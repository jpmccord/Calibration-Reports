source("readWatersQQQReport.R")
source("calibration.R")

input <- readXMLQQQReport("Metabolism Study/APR G4.xml")

output <- input 

calibrations <- calibrateDataset(output)

prettyPrint_calibration(calibrations, "Diclofenac")

prettySummary_calibration(calibrations,"Diclofenac")

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
