
check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

check.packages(c("tidyverse", "readxl"))

readThermoTargetedSheet <- function(sheetname, file) {
  
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

result <- head(the.sheet,-6)

result["Cmp"] <- sheetname

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
  mutate_at(vars(one_of("Exp Amt","Calc Amt","%Diff", "%RSD-AMT","Response Ratio","Area","Height","ISTD Area","ISTD Ht",
                        "RT","Rel RT", "Del RT", "S/N","Start Time","End Time","Start Height","End Height","Search Window",
                        "Duration","Sample Wt", "Sample Vol", "ISTD Base Amt","ISTD Calc Amt", "Dilution Factor")),
            funs(as.numeric)) %>%
  mutate(batch = basename(file))

}

readBatchedThermoExperiment <- function(filelist) {
  
batch <- sapply(filelist, FUN = readThermoTargetedReport)

bind_rows(batch) #does not work with mixed short and long report formats, extend functionality later
  
}

#file <- c("Sample Long Report.xls")
#shortfile <- c("Sample Short Report.xls")

#filist <- c(file,shortfile)

#summary <- readThermoTargetedReport(file)

#readBatchedThermoExperiment(filist)


