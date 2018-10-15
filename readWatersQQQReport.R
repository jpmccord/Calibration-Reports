library(tidyverse)
library(readxl)

excel_input <- read_excel("F:/DIC test file 10-2-18.xlsm",
                          sheet= 1,
                          guess_max = 10000,
                          na = c("NaN",""))

calibration_info <- excel_input %>%
  select(id39,name40,type41,ref,rah,type42,origin,weighting,axistrans,curve,rsquared) %>%
  filter(!is.na(id39))

cal_matching <- calibration_info %>%
  filter(is.na(ref)) %>%
  mutate(ref = as.character(id39)) %>%
  select(ref, name40) %>%
  rename(`Internal Standard` = name40) %>%
  right_join(calibration_info) %>%
  select(id39,name40,type41,`Internal Standard`,rah,type42,origin,weighting,axistrans,curve,rsquared)

colnames(cal_matching) <- c("id","Analyte Name","Calibration Type","Internal Standard",
                                "Relative Response Ratio","Regression Fit","Include/Exclude Origin",
                                "Weighting","Axis Transformation","Regression Equation",
                                "Correlation Coefficient (r2)")

std_pairs <- select(cal_matching,`Analyte Name`,`Internal Standard`)

report_info <- excel_input %>%
  select(filename, filename2, creationdate, name, filename6, filename11,
         inletmethodname, msmethodname,tunemethodname,samplenumber)

colnames(report_info) <- c("XML Transfer File","QuanLynx Result File","Creation Date for QuanLynx Result File",
                           "QuanLynx Processing Method","QuanLynx Processing Method File Name",
                           "Sample List File Name","UPLC Method","MS MRM Method","MS Tune File",
                           "Sample List No.")

raws_data <- excel_input %>%
  select(samplenumber,name16,createdate,vial,injectvolume,desc,id17,name20,type,
         quantrace,foundrt,area,secondarytrace,stdconc,analconc,conccalc,pkflags)%>%
  filter(!name20 %in% unique(std_pairs$`Internal Standard`)) %>%
  rename(`Analyte Name` = name20) %>%
  left_join(std_pairs)

IS_data <- excel_input %>%
  select(samplenumber,name20,quantrace,foundrt,area) %>%
  filter(name20 %in% unique(std_pairs$`Internal Standard`)) %>%
  rename(`Internal Standard` = name20,
         `Internal Standard MRM` = quantrace,
         `Internal Standard Retention Time (min)` = foundrt,
         `Internal Standard Peak Area (counts)` = area)

final_data <- left_join(raws_data,IS_data)

colnames(final_data) <- c("Sample List No.","Raw Data File (.raw)","Date Analyzed","Vial no.","Injection Vol. (µL)","Sample ID","QuanLynx (.mdb) Method ID","Analyte Name","Sample Type","Quan Ion (m/z -> m/z)","Quan Ion RT (min)","Quan Ion Peak Area (counts)","Qual Ion (m/z -> m/z)", "Standard Concentration", "Theor. Conc.", "% Diff", "Quan Ion Flags", "Internal Standard","Internal Standard MRM","Internal Standard Retention Time (min)","Internal Standard Peak Area (counts)")

write_excel_csv(final_data, "QQQ Output.csv")



### Trying Calcurve predction intervals
diclofenac <- filter(final_data, `Analyte Name` == "Diclofenac", `Sample Type` == "Standard") %>%
  mutate(Response = `Quan Ion Peak Area (counts)`/`Internal Standard Peak Area (counts)`) %>% filter(`Standard Concentration` != 0 )

ggplot(diclofenac) +
  theme_bw() +
  geom_point(aes(x = as.numeric(`Standard Concentration`), y = Response))

calcurve <- lm(Response ~ `Standard Concentration` + I(`Standard Concentration`),
               data = diclofenac)

invest(calcurve, diclofenac$Response)

mod <- lm(Response ~ `Standard Concentration`/3,
          data= diclofenac)

QC.predictions <- sapply(diclofenac.QCs$Response, inverse.predict, object = mod)

lod(mod, data = diclofenac)

lm.weight.0 <- lm(Response ~ `Standard Concentration` + I(`Standard Concentration`^2),
                   data = diclofenac)

lm.weight.1x <- lm(Response ~ `Standard Concentration` + I(`Standard Concentration`^2),
                   weight = 1/`Standard Concentration`,
                  data = diclofenac)

lm.weight.1x2 <- lm(Response ~ `Standard Concentration` + I(`Standard Concentration`^2),
                   weight = 1/`Standard Concentration`^2,
                   data = diclofenac)

anova(lm.weight.1x,lm.weight.1x2,lm.weight.0,lm.weight.1x2_2)

lm.weight.1x2_2 <- lm(Response ~ `Standard Concentration`,
                    data = diclofenac)
