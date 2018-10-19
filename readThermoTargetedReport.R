
check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

check.packages(c("tidyverse", "readxl"))

readThermoTargetedSheet <- function(sheetname, file) {
options(warn = 0)
checkLong <- length(read_excel(file,
                        sheet = sheetname,
                        skip = 4,
                        trim_ws = TRUE,
                        n_max = 3)) > 25

if (checkLong) {
  
  the.sheet <- as.tibble(
      read_excel(file,
                 sheet = sheetname,
                 skip = 4,
                 trim_ws = TRUE,
                 col_types = c("text", "text", "text", "text", "numeric","numeric",
                               "text","text","text","numeric","text","numeric",
                               "text","text","numeric","numeric","numeric","numeric",
                               "numeric","numeric","numeric","numeric","numeric","numeric",
                               "numeric","numeric","text","numeric","numeric","numeric",
                               "date","numeric","text","text","text","numeric",
                               "numeric","numeric","numeric","numeric","numeric","numeric",
                               "text","text","text","text","text"),
                 na = c("","NA","NF","ND"))  )

} else {
  the.sheet <- as.tibble(
    read_excel(file,
               sheet = sheetname,
               skip = 4,
               trim_ws = TRUE,
               col_types = c("text","text","text","text","numeric","numeric",
                             "numeric","numeric","numeric","numeric","numeric","text",
                             "text","text","numeric","text"),
               na = c("","NA","NF","ND"))  )
}

result <- head(the.sheet,-6) %>%
  rename_all(make.names) %>%
  rename(Raw.Response = Area,
         ISTD.Response = ISTD.Area) %>%
  rename_at(vars(one_of("Amount", "Exp.Amount","Exp.Amt")), funs(paste0("Exp.Amt")))

result["Cmp"] <- sheetname

options(warn = 1)

return(result)
}

readThermoTargetedReport <- function(file) {
sheetnames <- excel_sheets(file)
n_sheets <- length(sheetnames) -2
  
vector <- vector("list", length=n_sheets)

for (i in 1:n_sheets) {
  vector[[i]] <- readThermoTargetedSheet(sheetname = sheetnames[i], file = file)
}

summary <- bind_rows(vector) %>%
  mutate(batch = basename(file)) %>%
  dplyr::rename_all(funs(make.names(.))) %>%
  mutate(Sample.Type = case_when(Sample.Type %in% c("Standard", "Standard Bracket Sample", "Std", "Std Bracket Sample") ~ "Standard",
                                 Sample.Type %in% c("Blank", "Blank Sample") ~ "Blank",
                                 Sample.Type %in% c("Aanlyte", "Unknown Sample", "Unknown") ~ "Sample",
                                 Sample.Type %in% c("QC", "QC Sample") ~ "QC")) %>%
  mutate(Batch = basename(file)) %>%
  select(Batch,Sample.ID,Sample.Name,Sample.Type,Exp.Amt,Cmp,Raw.Response,ISTD.Response)

}

readBatchedThermoExperiment <- function(filelist) {
  
batch <- lapply(filelist, FUN = readThermoTargetedReport)

bind_rows(batch)#does not work with mixed short and long report formats, extend functionality later

}

file <- c("Sample Long Report.xls")
#shortfile <- c("Sample Short Report.xls")

filist <- c(file,shortfile)

summary <- readThermoTargetedReport(file)

summary<- readBatchedThermoExperiment(filist)


