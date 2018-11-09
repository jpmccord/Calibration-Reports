source("readWatersQQQReport.R")
source("calibration.R")

input <- readXMLQQQReport("Metabolism Study/APR G4.xml")

calibration <- calibrateDataset(input)


ggplot(filter(subset, calibref %in% c("Diclofenac", "-OH Diclofenac"))) + 
  geom_point(aes(x = as.numeric(stdconc), y = as.numeric(area))) +
  geom_smooth(aes(x = as.numeric(stdconc), y = as.numeric(area)), method = "lm") +
  facet_wrap(~type+calibref)
