#-------------------------------
# example 1:

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirinput <- paste0(thisdir,"/input/")
diroutput <- paste0(thisdir,"/output/")

#load function
source(paste0(thisdir,"/../CountPrevalence.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)
library(lubridate)


#load input
cohort <- fread(paste0(thisdir,"/input/cohort.csv"), sep = ",")
conditions <- fread(paste0(thisdir,"/input/medicines.csv"), sep = ",")

date_cols <- c("preg_start","tri1_end","tri2_start","tri2_end","tri3_start","tri3_end","preg_end")
conditions[,(date_cols) := lapply(.SD,function(x) (as.character(x))),.SDcols = date_cols]
conditions[,(date_cols) := lapply(.SD,function(x) (as.Date(x,"%Y%m%d"))),.SDcols = date_cols]
cohort[,date_dispensing := as.character(date_dispensing)]
cohort[,date_dispensing:= as.Date(date_dispensing,"%Y%m%d")]


#USE THE FUNCTION 

prevalent_individual = CountPrevalence(Dataset_cohort = cohort,
                                       Dataset_events = conditions,
                                       UoO_id = c("PREG_ID"),
                                       key = c("person_id"),
                                       Type_prevalence = "of use",
                                       Periods_of_time = list(list("preg_start","tri1_end"),list("tri2_start","tri2_end"),list("tri3_start","tri3_end")),
                                       Start_date = "preg_start",
                                       End_date = "preg_end",
                                       Start_study_time = "20100101",
                                       End_study_time = "20211231",
                                       Name_condition = "type_of_medicine",
                                       Date_dondition = "date_dispensing",
                                       Conditions = c("antiepileptic","antidepressant","antiasthma"),
                                       strata = c("region_at_pregnancy_start"),
                                       Aggregate = FALSE
                                       )


