
check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

check.packages(c("tidyverse", "readxl"))

readThermoTargetedSheet <- function(sheetname, file) {

the.sheet <- as.tibble(
      read_excel(file,
                 sheet = sheetname,
                 skip = 4,
                 trim_ws = TRUE,
                 col_types = "text",
                 na = c("","NA","NF","ND"))  )

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
            funs(as.numeric))

}

#file <- c("C:/Users/Orbitrap-Processing/Documents/R Scripts/Calibration-Reports/Sample Long Report.xls")

#summary <- readThermoTargetedReport(file)
