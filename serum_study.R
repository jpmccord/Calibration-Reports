check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

check.packages(c("tidyverse", "readxl", "investr", "broom"))

source("readThermoTargetedReport.R")
source("calibration.R")

study_files <- dir(path ="Serum Study/", full.names = TRUE)

all_readin <- readThermoBatchedExperiment(study_files)

subject_predictions2 <- all_readin %>%
  group_by(Batch) %>%
  do(getSamplePredictions(.))

plot_file <- subject_predictions2 %>%
  mutate(Cmp = as.factor(Cmp)) %>%
  mutate(color_me = ifelse(subject == "527", TRUE, FALSE)) %>%
  filter(Cmp %in% c("PFOA", "PFOS", "GenX", "Naf_bp2")) %>%
  mutate(estimate = ifelse(estimate < 0, 0 , estimate),
         estimate = ifelse(estimate > 30, 30, estimate))

ggplot() +
  theme_minimal()+
  geom_point(data = filter(plot_file, color_me == FALSE), aes(y = Cmp, x = estimate), position = position_jitter(height = 0.05), shape = 1, color = "blue", alpha = 0.25) +
  geom_point(data = filter(plot_file, color_me == TRUE), aes(y = Cmp, x = estimate), color = "green", shape = 16) +
  geom_tile(data = filter(plot_file, color_me == TRUE), aes(x = estimate, y = Cmp, width = (lower-upper)/2), height = 0.5, alpha = 0.1)

ggplot(plot_file) +
  theme_bw()+
  geom_histogram(aes(x=estimate), binwidth=2, fill = NA, color = "grey50") +
  #geom_density(aes(x = estimate), adjust = 5)+
  facet_wrap(~Cmp)+
  geom_vline(data = plot_file[plot_file$color_me == TRUE,], aes(xintercept = estimate)) +
  xlim(-1,31) +
  geom_text(data=arrow_pos, aes(label=name, x=yvar, y=y + 2), size=3) +

