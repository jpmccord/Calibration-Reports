source("readWatersQQQReport.R")
source("calibration.R")

check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

check.packages("here")

batches <- dir("Metabolism Study/Batches", full.names = TRUE)

for (i in 1:length(batches)) {
  
input <- readXMLQQQReport(batches[[i]])

calibrations <- calibrateDataset(input)

for (m in 1:length(calibrations)) {
  the.Compounds = names(calibrations[m])
  
  preds <- input %>%
    filter(Cmp == the.Compounds) %>%
    group_by(Batch) %>%
    do(getSamplePredictions(.)) %>%
    inner_join(input) %>%
    ungroup() %>%
    select(Batch,Cmp,Sample.Name, Sample.Desc, Sample.Type, Sample.ID, Raw.Response, ISTD.Response, estimate, lower, upper) %>%
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
}

myMergedData <- 
  do.call(rbind,
          lapply(list.files(path = here::here("reports"), pattern = ".csv", full.names = TRUE), read_csv)) %>%
  unique()

write_csv(myMergedData, paste0(here::here("reports/combined"),"/","All_Compounds","_Report",".csv"))

plot_table <- myMergedData %>%
  mutate(sample = substr(Sample.Desc, 1, 5) ,
         prep_type = substr(Sample.Desc, nchar(Sample.Desc)-1, nchar(Sample.Desc))) %>%
  dplyr::filter(prep_type %in% c("AP"," R")) %>%
  select(Cmp, sample, Estimated.Conc, prep_type) %>%
  spread(prep_type, Estimated.Conc) %>%
  mutate(short_sample = substr(sample,1,3)) %>%
  mutate(short_sample = ifelse(short_sample == "2.5", "3.0", short_sample)) %>%
  mutate(recovery = AP/` R`)

summary_table <- plot_table %>% group_by(Cmp,short_sample) %>% summarize(mean = mean(recovery))

ggplot(plot_table, aes(x = short_sample, y = recovery)) +
    theme_bw()+
  #geom_boxplot(aes(color = Cmp)) +
  geom_point(data = summary_table, aes(x = short_sample, y = mean), shape = 95, size = 10, color = "black")+
  geom_point(aes(color = Cmp), size = 2) +
  facet_wrap(~Cmp, ncol = 6) +
  guides(color = FALSE)


