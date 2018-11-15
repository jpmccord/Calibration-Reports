source("readWatersQQQReport.R")
source("calibration.R")

input <- readXMLQQQReport("Metabolism Study/APR G4.xml")

output <- input 
  

calibrations <- calibrateDataset(output)

prettyPrint_calibration <- function(calibrations, Compound, band = "prediction", alpha = 0.95, stdunits =NULL, ...){

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

prettySummary_calibration <- function(calibrations, Compound){

  calibration_model <- calibrations[[Compound]]
  
  if(is.null(calibration_model)) {
    stop("Calibration not found")
  }
  
  calCurve <- calibration_model$model
  
  cal_formula <- as.formula(calibration_model$terms)
  
  calCurve.model  <- lm(cal_formula,
                 data = calCurve,
                 weights = `(weights)`)

  lower.bound = min(calCurve$Exp.Amt) - 0.5*(max(calCurve$Exp.Amt)-min(calCurve$Exp.Amt))
  upper.bound = 2*max(calCurve$Exp.Amt) + (max(calCurve$Exp.Amt)-min(calCurve$Exp.Amt))
  
  reverse.calibration <- function(measures) {
  
  results <- try(invest(calCurve.model,
                          y0 = measures,
                          interval = "inversion",
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
      
      results.frame <- data.frame(subject = as.character(subject),
                                  estimate = as.numeric(NA),
                                  lower = as.numeric(NA),
                                  upper = as.numeric(NA),
                                  Cmp = cmp)
      
      return(results.frame)
      
    } else {
      
      results.frame <- tibble(estimate = results$estimate,
                              lower = results$lower,
                              upper = results$upper,
                              Cmp = Compound)
      
      return(results.frame)
    }
  
  }

  back_preds <- lapply(FUN = reverse.calibration, calCurve$Area.Ratio) %>% bind_rows()
  
  compare <- cbind(back_preds,calCurve) %>%
    select(Cmp, Exp.Amt, Area.Ratio, estimate,upper,lower) %>%
    mutate(dev = round((Exp.Amt - estimate)/Exp.Amt * 100,2),
          inpred = (Exp.Amt > lower & Exp.Amt < upper))
  
  return(compare)
}

sapply(calCurve$Area.Ratio, FUN = results)


preds <- output %>%
  group_by(Batch) %>%
  do(getSamplePredictions(.)) %>%
  inner_join(output)

plot_table <- preds %>%
  mutate(group = paste0(substr(Sample.Desc,1,3), str_extract(Sample.Desc,"[a-z,A-Z]+")))

ggplot(plot_table) +
  theme_bw()+
  geom_point(aes(x = group, y= estimate)) +
  #geom_errorbar(aes(x = Sample.ID, ymin = lower, ymax = upper))+
  stat_summary(aes(x = group, y= estimate),
               fun.data=mean_sdl, 
               fun.args = list(mult=1), 
               color = "red",
               geom = "errorbar",
               width = 0.2)+
  facet_wrap(~Cmp)
