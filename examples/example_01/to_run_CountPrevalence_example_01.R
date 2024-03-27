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
setwd('..')
setwd('..')
source("CountPrevalence.R")


# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)
library(lubridate)


#load input
cohort <- fread(paste0(thisdir,"/input/cohort.csv"), sep = ",")
conditions <- fread(paste0(thisdir,"/input/conditions.csv"), sep = ",")

conditions[,condition_end:=cond_date+10 ]

date_cols <- c("entry_date","exit_date","index_date")
cohort[,(date_cols) := lapply(.SD,as.character),.SDcols = date_cols]
cohort[,(date_cols) := lapply(.SD,as.Date),.SDcols = date_cols]


#prova<-dcast(setDT(conditions), person_id ~  cond_name, value.var= "cond_date")

# conditions[,cond_date := as.character(cond_date)]
# conditions[,cond_date:= as.Date(cond_date)]


#USE THE FUNCTION 

#specificare Increment_period o Periods_of_time

prevalent_individual = CountPrevalence(Dataset_cohort = cohort,
                                       Dataset_events = conditions,
                                       UoO_id = c("person_id"),
                                       Type_prevalence = "point",
                                       Increment = "year",
                                       #Points_in_time = c("index_date"),
                                       #Points_in_time = c("20190511", "20180224"),
                                       Start_study_time = "20190101",
                                       End_study_time = "20220101",
                                       Start_date = "entry_date",
                                       End_date = "exit_date",
                                       Name_condition = "cond_name",
                                       Date_condition = "cond_date",
                                       Conditions = c("hypertension","cardiovascular disease"),
                                       Date_end_condition="condition_end",
                                       Aggregate = F
)


