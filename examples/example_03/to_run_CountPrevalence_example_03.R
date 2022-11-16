#-------------------------------
# example 3:

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

date_cols <- c("entry_date","exit_date","birth_date")

cohort[,(date_cols) := lapply(.SD,as.character),.SDcols = date_cols]
cohort[,(date_cols) := lapply(.SD,as.Date),.SDcols = date_cols]

date_cond<-c("cond_date")
conditions[,(date_cond) := lapply(.SD,as.character),.SDcols = date_cond]
conditions[,(date_cond) := lapply(.SD,as.Date),.SDcols = date_cond]



#USE THE FUNCTION 

prevalent_individual = CountPrevalence(Dataset_cohort = cohort,
                                       Dataset_events = conditions,
                                       UoO_id = c("person_id"),
                                       Type_prevalence = "period",
                                       Increment_period = "year",
                                       Start_study_time = "20050101",
                                       End_study_time = "20191231",
                                       Start_date = "entry_date",
                                       End_date = "exit_date",
                                       Birth_date = "birth_date",
                                       Age_bands = c(15, 20, 25, 30, 35, 40, 45),
                                       include_remaning_ages = FALSE,
                                       Name_condition = "cond_name",
                                       Date_condition = "cond_date",
                                       Conditions = c("multiple sclerosis"),
                                       Aggregate = FALSE
)


