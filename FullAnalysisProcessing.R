source("readWatersQQQReport.R")
source("makeCalibrations.R")

filelist <- c("APR G8.xml",
              "APR G1.xml",
              "APR G4.xml",
              "APR G5.xml",
              "APR G7.xml")

inputs <- map(filelist, readXMLQQQReport) %>%
  bind_rows() %>%
  filter(!is.na(ISTD.Response) & ISTD.Response > 1) %>%
  group_by(Batch,Cmp) %>%
  nest() %>%
  mutate(calibration_model = map(data, .f = makeCalibrationModel),
         sample_predictions = map2(.x = data, .y = calibration_model, .f= getSamplePredictions),
         results = map2(.x = data, .y = sample_predictions, .f = left_join))
         
results <- inputs %>%
  mutate(results = map2(.x = data, .y = sample_predictions, .f = left_join)) %>%
  select(Batch,Cmp,results) %>%
  unnest(results)


batch_n <- length(unique(results$Batch))

sample_n <- results %>%
  group_by(Batch, Sample.ID) %>%
  tally() %>%
  nrow()

QC_n <- results %>%
  dplyr::filter(Sample.Type == "QC") %>%
  group_by(Batch, Sample.ID) %>%
  tally() %>%
  nrow()
  
blank_n <- results %>%
  dplyr::filter(Sample.Type == "Blank") %>%
  group_by(Batch, Sample.ID) %>%
  tally() %>%
  nrow()


standard_n <- results %>%
  dplyr::filter(Sample.Type == "Standard") %>%
  group_by(Batch, Sample.ID) %>%
  tally() %>%
  nrow()

cmp_n <- results %>%
  dplyr::filter(Sample.Type == "Standard") %>%
  group_by(Batch, Cmp) %>%
  tally() %>%
  nrow()

subject_n <- sample_n - (blank_n + QC_n + standard_n)

rmarkdown::render(input = "FullReport.Rmd",
                  output_format = "html_document",
                  output_file = paste0("testReport",".html"))
