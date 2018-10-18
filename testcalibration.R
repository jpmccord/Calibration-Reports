
library(readxl)
library(tidyverse)
library(investr)
library(rafalib)

inputSheet <- "F:/Documents/R Scripts/Thermo Quan Reports/TestSheet.xlsx"

allSheets <- excel_sheets(inputSheet)

chosenCmp <- allSheets[1]

  subSheet <- read_excel(inputSheet, sheet = chosenCmp) %>% dplyr::rename_all(funs(make.names(.)))
  
  calCurve <- dplyr::filter(subSheet, Sample.Type == "Standard", Exp.Amt != 0)
  
  sampleCurve <- dplyr::filter(subSheet, Sample.Type == "Sample")
  
  qcCurve <- dplyr::filter(subSheet, Sample.Type == "QC")
  
  formula.lm <- formula(Area.Ratio ~ Std.Concentration)
  formula.qd <- formula(Area.Ratio ~ Std.Concentration + I(Std.Concentration^2)) 
  
  calCurve.model.lm1x <- lm(formula = formula.lm,
                            weights = 1/Std.Concentration,
                            data = as.data.frame(calCurve))
  
  calCurve.model.lm1x2 <- lm(formula = formula.lm,
                            weights = 1/Std.Concentration^2,
                            data = as.data.frame(calCurve))
  
  calCurve.model.qd1x <- lm(formula = formula.qd,
                             weights = 1/Std.Concentration,
                             data = as.data.frame(calCurve))
  
  calCurve.model.qd1x2 <- lm(formula = formula.qd,
                            weights = 1/Std.Concentration^2,
                            data = as.data.frame(calCurve))

  par(mfrow = c(2,2))
  
  plotFit(calCurve.model.lm1x, interval = "prediction", main = "Linear | weight = 1/x", extend.range = TRUE)
  plotFit(calCurve.model.lm1x2, interval = "prediction", main = "Linear | weight = 1/x^2",extend.range = TRUE)
  plotFit(calCurve.model.qd1x, interval = "prediction", main = "Quad | weight = 1/x",extend.range = TRUE)
  plotFit(calCurve.model.qd1x2, interval = "prediction", , main = "Quad | weight = 1/x^2",extend.range = TRUE)
  

  fitSummary <- tibble(
    model = c("Linear weight = 1/x",
              "Linear weight = 1/x^2",
              "Quadratic weight  = 1/x",
              "Qudratic weight = 1/x^2"),
    formula = c(paste0("y = ", round(calCurve.model.lm1x$coefficients[1], 4)," + ", round(calCurve.model.lm1x$coefficients[2],4), "x"),
                paste0("y = ", round(calCurve.model.lm1x2$coefficients[1], 4)," + ", round(calCurve.model.lm1x2$coefficients[2],4), "x"),
                paste0("y = ", round(calCurve.model.qd1x$coefficients[1], 4)," + ", round(calCurve.model.qd1x$coefficients[2],4), "x"),
                paste0("y = ", round(calCurve.model.qd1x2$coefficients[1], 4)," + ", round(calCurve.model.qd1x2$coefficients[2],4), "x")),
    R2 = c(summary(calCurve.model.lm1x)$r.squared,
           summary(calCurve.model.lm1x2)$r.squared,
           summary(calCurve.model.qd1x)$r.squared,
           summary(calCurve.model.qd1x2)$r.squared)) %>%
    mutate(R2 = round(R2,4))
  
  stored.models <- list(calCurve.model.lm1x,
                   calCurve.model.lm1x2,
                   calCurve.model.qd1x,
                   calCurve.model.qd1x2)
  
calCurve.model <- stored.models[[which(fitSummary$R2 == max(fitSummary$R2))]]

samples <- unique(sampleCurve$Sample)

reverse.calibration <- function(subject) {
  measures <- dplyr::filter(sampleCurve, Sample == subject)$Area.Ratio
  
  lower.bound = min(calCurve$Std.Concentration) - 0.5*(max(calCurve$Std.Concentration)-min(calCurve$Std.Concentration))
  upper.bound = 2*max(calCurve$Std.Concentration) + (max(calCurve$Std.Concentration)-min(calCurve$Std.Concentration))
    
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
                                upper = as.numeric(NA))
    
    return(results.frame)
    
  } else {
  
  results.frame <- data.frame(subject = as.character(subject),
                              estimate = results$estimate,
                              lower = results$lower,
                              upper = results$upper)
  
  return(results.frame)
  }
}

predictions <- plyr::ldply(samples, reverse.calibration)

ggplot(predictions) +
  geom_point(aes(x = subject, y = estimate)) +
  geom_errorbar(aes(x = subject, ymin = lower, ymax = upper), width = 0.1)
  

