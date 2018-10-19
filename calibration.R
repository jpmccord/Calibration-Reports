check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

check.packages(c("tidyverse", "readxl", "investr", "broom"))

calibrateCmp <- function(Comp, dataset) {
  
  calCurve <- dplyr::filter(data, Sample.Type == "Standard", is.na(Peak.Status) | Peak.Status != "Excluded", Cmp == Comp)
  
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

calibrateDataset <- function(dataset) {
  quant_compounds <- dataset %>% group_by(Cmp) %>%
    summarize(ISTD.Response = mean(ISTD.Response)) %>%
    filter(!is.na(ISTD.Response))
  
  sapply(quant_compounds$Cmp, dataset = data, FUN = calibrateCmp)
}

getSamplePredictions <- function(dataset){
  
  dataset_models <- calibrateDataset(dataset)
  
  sample_measures <- dplyr::filter(dataset, Sample.Type == "Sample") %>%
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
    bind_rows()
  
}

