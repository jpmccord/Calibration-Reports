check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

check.packages(c("tidyverse", "readxl", "investr", "broom"))

calibrateCmp <- function(Comp, dataset) {
  
  calCurve <- dplyr::filter(dataset, Sample.Type == "Standard", is.na(Peak.Status) | Peak.Status != "Excluded", Cmp == Comp)
  
  formula.lm <- formula(Area.Ratio ~ Exp.Amt)
  formula.qd <- formula(Area.Ratio ~ Exp.Amt + I(Exp.Amt^2)) 
  
  calCurve.model.lm1x <- lm(formula = formula.lm,
                            weights = 1/Exp.Amt,
                            data = as.data.frame(calCurve))
  
  calCurve.model.lm1x2 <- lm(formula = formula.lm,
                             weights = 1/Exp.Amt^2,
                             data = as.data.frame(calCurve))
  
  calCurve.model.qd1x <- lm(formula = formula.qd,
                            weights = 1/Exp.Amt,
                            data = as.data.frame(calCurve))
  
  calCurve.model.qd1x2 <- lm(formula = formula.qd,
                             weights = 1/Exp.Amt^2,
                             data = as.data.frame(calCurve))  
  model.labels = c("Linear weight = 1/x",
                   "Linear weight = 1/x^2",
                   "Quadratic weight  = 1/x",
                   "Qudratic weight = 1/x^2")
  
  model.list <- list(calCurve.model.lm1x, calCurve.model.lm1x2, calCurve.model.qd1x,calCurve.model.qd1x2)
  
  names(model.list) <- model.labels
  
  model_frame <- enframe(model.list, name = "modelName", value = "model")
  
  fits <- map(.x = model_frame$model, .f = broom::glance) %>%
    bind_rows()
  
  which.model = which(fits$adj.r.squared == max(fits$adj.r.squared))
  
  return = list(model.list[[which.model]])
  
  return(return)
}

calibrateDataset <- function(data) {
  quant_compounds <- data %>% group_by(Cmp) %>%
    summarize(ISTD.Response = mean(ISTD.Response)) %>%
    filter(!is.na(ISTD.Response) & ISTD.Response > 1)
  
  sapply(quant_compounds$Cmp, dataset = data, FUN = calibrateCmp)
}

getSamplePredictions <- function(dataset){
  
  dataset_models <- calibrateDataset(dataset)
  
  sample_measures <- dplyr::filter(dataset, Sample.Type != "Blank") %>%
    group_by(Cmp) %>%
    select(Cmp,Sample.Name,Sample.ID,Raw.Response,ISTD.Response) %>%
    mutate(Raw.Response = ifelse(is.na(Raw.Response), 0, Raw.Response)) %>%
    mutate(Area.Ratio = Raw.Response/ISTD.Response) %>%
    filter(Cmp %in% names(dataset_models))
  
  reverse.calibration <- function(subject, cmp, dataset) {
    measures <- dplyr::filter(dataset, Sample.ID == subject, Cmp == cmp)$Area.Ratio
    
    calCurve.model <- dataset_models[[cmp]]
    
    calCurve <- as.data.frame(calCurve.model$model)# extract original calCurve from model for inverse estimates

    lower.bound = min(calCurve$Exp.Amt) - 0.5*(max(calCurve$Exp.Amt)-min(calCurve$Exp.Amt))
    upper.bound = 2*max(calCurve$Exp.Amt) + (max(calCurve$Exp.Amt)-min(calCurve$Exp.Amt))
    
    results <- try(invest(calCurve.model,
                          y0 = measures,
                          interval = "Wald",
                          extendInt = "yes",
                          lower = lower.bound,
                          upper = upper.bound,
                          maxiter = 1e7),
                   silent = TRUE)
    
    if (inherits(results, "try-error")) {
      #print(paste("Prediction bounds not contained in the search interval (", lower.bound, 
      #           ", ", upper.bound, "). ", 
      #           "Try tweaking the values of lower and upper.", sep = ""))
      
      results.frame <- data.frame(subject = as.character(subject),
                                  estimate = as.numeric(NA),
                                  lower = as.numeric(NA),
                                  upper = as.numeric(NA),
                                  Cmp = cmp)
      
      return(results.frame)
      
    } else {
      
      results.frame <- tibble(subject = as.character(subject),
                              estimate = results$estimate,
                              lower = results$lower,
                              upper = results$upper,
                              Cmp = cmp)
      
      return(results.frame)
    }
  }
  
  subjects <- unique(sample_measures$Sample.ID)
  
  Cmps <- unique(sample_measures$Cmp)
  
  predictsubjects<- function(Compound) {lapply(subjects, cmp = Compound, FUN = reverse.calibration, dataset = sample_measures) %>%
      bind_rows()
  }
  
  predictions <- lapply(Cmps, FUN=predictsubjects) %>%
    bind_rows() %>%
    rename(`Sample.ID` = subject)
  
}

prettyPrint_calibration <- function(calibrations, Compound, band = "prediction", alpha = 0.95, stdunits =NULL, groupSample = FALSE, ...){
  
  calibration_model <- calibrations[[Compound]]
  
  if(is.null(calibration_model)) {
    stop("Calibration not found")
  }
  
  cal_data <- calibration_model$model
  
  cal_formula <- as.formula(calibration_model$terms)
  
  new_call <- lm(cal_formula,
                 data = cal_data,
                 weights = `(weights)`)
  
  plot_title = paste(Compound, "\n", "y =",
                     signif(calibration_model$coefficients["(Intercept)"],4), "+",
                     signif(calibration_model$coefficients["Exp.Amt"],4), "* X",
                     "+", signif(calibration_model$coefficients["I(Exp.Amt^2)"],4), "* X^2",
                     ", R^2 =", signif(summary(new_call)$r.squared, 4))
  
  x_label = paste("Std Concentration", stdunits)
  
  plotFit(new_call,
          interval = band,
          level = 0.95,
          ylab = "Response Factor",
          xlab = x_label,
          main = plot_title,
          ...)
}

prettySummary_calibration <- function(calibrations, Compound, groupSample = FALSE){
  
  calibration_model <- calibrations[[Compound]]
  
  if(is.null(calibration_model)) {
    stop("Calibration not found")
  }
  
  calCurve <- calibration_model$model %>% mutate(index = seq_len(nrow(.)))
  
  cal_formula <- as.formula(calibration_model$terms)
  
  calCurve.model  <- lm(cal_formula,
                        data = calCurve,
                        weights = `(weights)`)
  
  lower.bound = min(calCurve$Exp.Amt) - 0.5*(max(calCurve$Exp.Amt)-min(calCurve$Exp.Amt))
  upper.bound = 2*max(calCurve$Exp.Amt) + (max(calCurve$Exp.Amt)-min(calCurve$Exp.Amt))
  
  reverse.calibration <- function(measures) {
    
    results <- try(invest(calCurve.model,
                          y0 = measures,
                          interval = "Wald",
                          extendInt = "yes",
                          level = .99,
                          lower = lower.bound,
                          upper = upper.bound,
                          maxiter = 1e7),
                   silent = TRUE)
    
    if (inherits(results, "try-error")) {
      #print(paste("Prediction bounds not contained in the search interval (", lower.bound, 
      #           ", ", upper.bound, "). ", 
      #           "Try tweaking the values of lower and upper.", sep = ""))
      
      results.frame <- data.frame(estimate = as.numeric(NA),
                                  lower = as.numeric(NA),
                                  upper = as.numeric(NA),
                                  Cmp = Compound)
      
      return(results.frame)
      
    } else {
      
      results.frame <- tibble(estimate = results$estimate,
                              lower = results$lower,
                              upper = results$upper,
                              Cmp = Compound)
      
      return(results.frame)
    }
    
  }
  
  if (groupSample == TRUE) {
    cal_measures <- calCurve %>% select(-index) %>% group_by(Exp.Amt) 
  } else {
    cal_measures <- calCurve %>% group_by(index)
  }
  
  back_preds <- cal_measures %>% do(reverse.calibration(.$Area.Ratio)) %>% ungroup()
  
  compare <- left_join(back_preds,calCurve) %>%
    select(Cmp, Exp.Amt, Area.Ratio, estimate,upper,lower) %>%
    mutate(dev = round((Exp.Amt - estimate)/Exp.Amt * 100,2),
           inpred = (Exp.Amt > lower & Exp.Amt < upper))
  
  if (groupSample == TRUE) {
    compare <- compare %>% group_by(Cmp, Exp.Amt) %>% summarize_if(.predicate = is.numeric, .fun = mean) %>%
      mutate(dev = round((Exp.Amt - estimate)/Exp.Amt * 100,2),
             inpred = (Exp.Amt > lower & Exp.Amt < upper))
  } 
  
  return(compare)
}

