source("readWatersQQQReport.R")
source("calibration.R")

input <- readXMLQQQReport("Metabolism Study/APR G4.xml")

output <- input %>% 
  mutate(Cmp = calibref,
         Raw.Response = as.numeric(area1),
         ISTD.Response = as.numeric(area),
         Area.Ratio = Raw.Response/ISTD.Response,
         Sample.Type = type,
         Sample.Name = name,
         Sample.ID = id,
         Sample.Desc = desc,
         Exp.Amt = as.numeric(stdconc),
         Peak.Status = pkflags,
         Batch = basename(.attrs.filename))%>%
  select(Cmp,Raw.Response,ISTD.Response,Area.Ratio,Sample.Type,Sample.Name,Sample.Desc,Sample.ID,Exp.Amt,Peak.Status,Batch) %>%
  mutate(Sample.Type = case_when(Sample.Type %in% c("Standard", "Standard Bracket Sample", "Std", "Std Bracket Sample") ~ "Standard",
                                 Sample.Type %in% c("Blank", "Blank Sample") ~ "Blank",
                                 Sample.Type %in% c("Analyte", "Unknown Sample", "Unknown") ~ "Sample",
                                 Sample.Type %in% c("QC", "QC Sample") ~ "QC"))

calibrations <- calibrateDataset(output)

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
