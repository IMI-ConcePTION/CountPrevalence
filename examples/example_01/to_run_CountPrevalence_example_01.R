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
conditions <- fread(paste0(thisdir,"/input/conditions.csv"), sep = ",")

date_cols <- c("start_date","end_date","date_birth")
conditions[,(date_cols) := lapply(.SD,function(x) (as.character(x))),.SDcols = date_cols]
conditions[,(date_cols) := lapply(.SD,function(x) (as.Date(x,"%Y%m%d"))),.SDcols = date_cols]
cohort[,date_event := as.character(date_event)]
cohort[,date_event:= as.Date(date_event,"%Y%m%d")]


#USE THE FUNCTION 

prevalent_individual = CountPrevalence(Dataset_cohort = cohort,
                                       Dataset_events = conditions,
                                       UoO_id = c("PREG_ID"),
                                       key = c("person_id"),
                                       Type_prevalence = "point",
                                       Points_in_time = c("preg_start","preg_end"),
                                       Start_date = "preg_start",
                                       End_date = "preg_end",
                                       Name_condition = "condition",
                                       Date_dondition = "date_condition",
                                       Conditions = c("epilepsia","depression","asthma"),
                                       strata = c("age_at_preg_start"),
                                       Aggregate = FALSE
                                       )


