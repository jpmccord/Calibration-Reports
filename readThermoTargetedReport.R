
check.packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}

packages<-c("tidyverse", "readxl")

check.packages(packages)

file <- c("NJ_SolvayX_042518_Long.xlsx")

readThermoTargetedReport <- function(file) {
sheetnames <- excel_sheets(file)

input.matrix <- as.tibble(read_excel(file, sheet = sheetnames[1], range = "A5:A43"))

for (sheet in 1:(length(sheetnames)-2)) {
  
  sheet.compound <- colnames(read_excel(file, sheet = sheetnames[sheet], range = "A3"))
  
  desiredframes <- c("Filename","cmp", "Area")
  
  input.sheet <- read_excel(file, sheet = sheetnames[sheet], skip = 4, na = c("NF", "NA", "")) %>%
    mutate(cmp = sheet.compound)
  
  output.sheet <- input.sheet[,desiredframes]
  
  output.sheet <- filter(output.sheet, Filename %in% input.matrix$Filename)
  
  input.matrix <- full_join(input.matrix,output.sheet)
  
}

compound.abuns <- input.matrix %>%
  spread(key = cmp, value = Area) %>%
  filter(!grepl("Blank",Filename)) %>%
  mutate(Id = as.numeric(str_extract(Filename,"[0-9]*$")))
}