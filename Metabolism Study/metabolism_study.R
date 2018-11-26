source("readWatersQQQReport.R")
source("calibration.R")

input <- readXMLQQQReport("Metabolism Study/APR G4.xml")

calibrations <- calibrateDataset(input)

for (i in 1:length(calibrations)) {
  the.Compounds = names(calibrations[i])
  
  preds <- input %>%
    filter(Cmp == the.Compounds) %>%
    group_by(Batch) %>%
    do(getSamplePredictions(.)) %>%
    inner_join(input) %>%
    ungroup() %>%
    select(Batch,Cmp,Sample.Name, Sample.Desc, Sample.ID, Raw.Response, ISTD.Response, estimate, lower, upper) %>%
    rename(`Estimated.Conc` = estimate,
           lower_est = lower,
           upper_est = upper,
           `Run Order` = Sample.ID)
  
  write_csv(preds,paste0(here::here("reports"),"/",the.Compounds,"_Report",".csv"))  
  
  rmarkdown::render(input = "Single_Cmp_MarkdownReport.Rmd",
                    output_format = "html_document",
                    output_file = paste0(the.Compounds,"_Report",".html"),
                    output_dir = "reports")
}

reports <- dir(here::here("reports"), pattern = ".csv", full.names = TRUE)

myMergedData <- 
  do.call(rbind,
          lapply(list.files(path = here::here("reports"), pattern = ".csv", full.names = TRUE), read_csv))

write_csv(myMergedData, paste0(here::here("reports"),"/","All_Compounds","_Report",".csv"))

#prettyPrint_calibration(calibrations, "Diclofenac")

#prettySummary_calibration(calibrations,"Diclofenac")

#preds <- output %>%
#  group_by(Batch) %>%
#  do(getSamplePredictions(.)) %>%
#  inner_join(output)

#plot_table <- preds %>%
#  mutate(group = paste0(substr(Sample.Desc,1,3), str_extract(Sample.Desc,"[a-z,A-Z]+")))

#ggplot(plot_table) +
#  theme_bw()+
#  geom_point(aes(x = group, y= estimate)) +
#  #geom_errorbar(aes(x = Sample.ID, ymin = lower, ymax = upper))+
#  stat_summary(aes(x = group, y= estimate),
#               fun.data=mean_sdl, 
#               fun.args = list(mult=1), 
#               color = "red",
#               geom = "errorbar",
#               width = 0.2)+
#  facet_wrap(~Cmp)
