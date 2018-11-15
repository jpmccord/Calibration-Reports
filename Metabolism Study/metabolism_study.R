source("readWatersQQQReport.R")
source("calibration.R")

input <- readXMLQQQReport("Metabolism Study/APR G4.xml")

output <- input 
  

calibrations <- calibrateDataset(output)

prettyPrint_calibration <- function(calibrations, Compound, band = "prediction", alpha = 0.95, stdunits =NA, ...){

  calibration_model <- calibrations[[Compound]]
  
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

prettySummary <- function(calibrations, Compound){
  calibration_model <- calibrations[[Compound]]
  
  cal_data <- calibration_model$model
  
  cal_formula <- as.formula(calibration_model$terms)
  
  new_call <- lm(cal_formula,
                 data = cal_data,
                 weights = `(weights)`)
  
  summary(new_call)
  
}

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
