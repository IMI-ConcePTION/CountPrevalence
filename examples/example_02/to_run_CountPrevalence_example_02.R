#-------------------------------
# example 2:

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirinput <- paste0(thisdir,"/input/")
diroutput <- paste0(thisdir,"/output/")


#load function
setwd('..')
setwd('..')
source("CountPrevalence.R")

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)
library(lubridate)


#load input
cohort <- fread(paste0(thisdir,"/input/cohort.csv"), sep = ",")
medicines <- fread(paste0(thisdir,"/input/medicines.csv"), sep = ",")

date_cols_cohort <- c("preg_start","tri1_end","tri2_start","tri2_end","tri3_start","tri3_end","preg_end")
cohort[,(date_cols_cohort) := lapply(.SD,as.character),.SDcols = date_cols_cohort]
cohort[,(date_cols_cohort) := lapply(.SD,as.Date),.SDcols = date_cols_cohort]

date_cols_medicines <- c("date_dispensing")
medicines[,(date_cols_medicines) := lapply(.SD,as.character),.SDcols = date_cols_medicines]
medicines[,(date_cols_medicines) := lapply(.SD,as.Date),.SDcols = date_cols_medicines]








#USE THE FUNCTION 

#se unità osservazione ha 2 righe controlla che non siano overlapping le date

#specificare Increment_period o Periods_of_time

prevalent_individual = CountPrevalence(Dataset_cohort = cohort,
                                       Dataset_events = medicines,
                                       UoO_id = c("preg_id"),
                                       key = c("person_id"),
                                       Type_prevalence = "of use",
                                       #Periods_of_time = list(c("preg_start","tri1_end"),c("tri2_start","tri2_end"),c("tri3_start","tri3_end")),
                                       Increment_period = "month",
                                       Start_date = "preg_start",
                                       End_date = "preg_end",
                                       Start_study_time = "20100101",
                                       End_study_time = "20121231",
                                       Name_condition = "type_of_medicine",
                                       Date_condition = "date_dispensing",
                                       Conditions = c("antiepileptic","antidepressant","antiasthma"),
                                       Strata = c("region_at_pregnancy_start"),
                                       Aggregate = FALSE
)



#Periods_of_time = list(list("preg_start","tri1_end"),list("tri2_start","tri2_end"),list("tri3_start","tri3_end")),
