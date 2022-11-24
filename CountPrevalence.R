
CountPrevalence<-function(Dataset_cohort, Dataset_events, UoO_id,key=NULL,Start_date, End_date,  Birth_date = NULL,Name_condition,Date_condition, Date_end_condition=NULL, Type_prevalence, Points_in_time=NULL, Increment = NULL, Periods_of_time=NULL, Increment_period=NULL, Conditions , Start_study_time, End_study_time, Age_bands = NULL, Unit_of_age = "year" ,include_remaning_ages = T, Strata =NULL, Aggregate = T){
  
  print("Version 1.0")
  # Check if demanded R packages are installed, install if not,  and activate
  ################################################################################################################################
  print("Check packages data.table and lubridate")
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  ################################################################################################################################
  
  #Set character input for study dates to date format
  ################################################################################################################################
  print("Assign date format to Start_study_time and End_study_time")
  Start_study_time<-as.IDate(as.character(Start_study_time),"%Y%m%d")
  End_study_time<-as.IDate(as.character(End_study_time),"%Y%m%d")
    ################################################################################################################################
  #create the object used as choosen key (between key and unit of observation)
  if(!is.null(key)) {
    choosen_key<-key
  }else{
    choosen_key<-UoO_id
  }
  
  #create the object containing the ids to keep
  if (UoO_id %in% choosen_key) {
    id_columns_tokeep<-choosen_key
  }else{
    id_columns_tokeep<-c(choosen_key,UoO_id)
  }


  # #check if study start and stop dates are valid
  # ################################################################################################################################
  # 
  # print("Check if Start_study_time and End_study_time are valid")
  # if(!sum(Start_study_time==seq.Date(as.Date("19000101","%Y%m%d"),Sys.Date(),by = Increment))==1){
  #   
  #   if(Increment == "year"){stop("Change the start date to the first of january. Wrong study start date can produce invalid results.")}
  #   if(Increment == "month"){stop("Change the start date to the first of month. Wrong study start date can produce invalid results.")}
  #   if(Increment == "week"){stop("Change the start date to a monday. Wrong study start date can produce invalid results.")}
  #   
  # }
  # 
  # if(!sum(End_study_time==seq.Date(as.Date("19000101","%Y%m%d"),end_date_new ,by = Increment)-1)==1){
  #   
  #   if(Increment == "year"){stop("Change the end date to the 31th of december. Wrong study start date can produce invalid results.")}
  #   if(Increment == "month"){stop("Change the end date to the last day of the month. Wrong study start date can produce invalid results.")}
  #   if(Increment == "week"){stop("Change the end date to a sunday. Wrong study start date can produce invalid results.")}
  #   
  # }
  
  gc()
  
  ################################################################################################################################
  if(!is.null(Birth_date)) {
    date_cols<-c(Start_date,End_date,Birth_date)
  }else{
    date_cols<-c(Start_date,End_date)
    if (!is.null(Periods_of_time)) {
      periods<-unlist(Periods_of_time)
      date_cols<-c(date_cols,periods[!(periods %in% date_cols)])
    }
  
    if (!is.null(Points_in_time)) {
      periods<-unlist(Points_in_time)
      date_cols<-c(date_cols,periods[!(periods %in% date_cols)])
    }

  }


  
  # Reduce memory size using integers
  Dataset_cohort[, c(date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
  #Dataset_cohort[, c(Strata) := lapply(.SD, as.integer), .SDcols = Strata]
  gc()
  
  
  #Check if start, end and birth date are all filled. If end date is not filled it will be replaced by the study end date
  ################################################################################################################################
  
  if(!is.null(Birth_date)) {
    print("Check if date columns in input data are valid and in correct order")
    if(sum(is.na(Dataset_cohort[,.(get(Start_date))]))>0){stop("Empty start dates")}
    if(!is.null(Age_bands)){if(sum(is.na(Dataset_cohort[,.(get(Birth_date))]))>0){stop("Empty birth dates")}}
    if(sum(is.na(Dataset_cohort[,.(get(End_date))]))>0){print(paste0(sum(is.na(Dataset_cohort[,.(get(End_date))]))," empty end dates will be filled with the end study date. This may cause overlapping intervals"))}
    Dataset_cohort[is.na(get(End_date)),eval(End_date) := End_study_time]
    
    gc()
    
    #Check the order of dates
    ################################################################################################################################
    wrong_End_date<-nrow(Dataset_cohort[get(Start_date)>get(End_date),])
    if (wrong_End_date>0){warning(paste0(wrong_End_date," end date(s) prior to start date"))}
    wrong_Start_date<-nrow(Dataset_cohort[get(Start_date)>Sys.Date(),])
    if (wrong_Start_date>0){warning(paste0(wrong_Start_date," start date(s) in future"))}
    
    if(!is.null(Age_bands)){
      wrong_Birth_date<-nrow(Dataset_cohort[get(Start_date)<get(Birth_date),])
      if (wrong_Birth_date>0){warning(paste0(wrong_Start_date," start date(s) before birth date"))}}
  }

  ################################################################################################################################
  #Check if the subjects have overlap in the time intervals (within strata???), deend_period_datesd by end-start date.
  ################################################################################################################################
  print("Check if observation periods do not have overlap")
  test_overlap<-Dataset_cohort[!is.na(get(End_date))&!is.na(get(End_date))&get(End_date)>get(Start_date),][,.(get(choosen_key), as.integer(get(Start_date)), as.integer(get(End_date)))]
  setkey(test_overlap,V1,V2,V3)
  test_overlap2<-as.data.table(foverlaps(test_overlap, test_overlap, type="any", which=TRUE))
  test_overlap2<-test_overlap2[xid!=yid,]
  test_overlap[,id:=as.integer(rownames(test_overlap))]
  overlap_subjects<-unlist(unique(test_overlap2[test_overlap, on = .(xid = id), nomatch=NULL][,.(V1)]))
  
  if(length(overlap_subjects) > 0){
    stop("Subjects have overlapping person time: ")
    stop(paste0(overlap_subjects," "))
  }
  
  rm(test_overlap,test_overlap2,overlap_subjects)
  gc()
  
  #Determine the ages at the beginning and end of all observation periods. Output is a starting point for calculation and splitting of
  # age bands
  ################################################################################################################################
  if(!is.null(Age_bands)){
    
    print(paste0("Calculate ages at the start and end of every observation period by ", Increment_period))
    
    
    if (nrow(Dataset_cohort) > 0){
      
      Dataset_cohort[, age_start := floor(time_length(interval(get(Birth_date), get(Start_date)), Unit_of_age)) ]
      Dataset_cohort[, age_end := floor(time_length(interval(get(Birth_date), get(End_date)), Unit_of_age)) ]
      
    } else{
      Dataset_cohort[,age_start := NA]
      Dataset_cohort[,age_end := NA]
    }   
    
  }
  
  #Calculate agebands in 2 steps ((1)split/recalculate start/end ages and (assign row to ageband) )
  ################################################################################################################################
  if(!is.null(Age_bands)){
    print("Create agebands")
    if(nrow(Dataset_cohort) > 0){
      #### New code from version 13.6
      
      #Produce a dataset with Agebands and the start and end age of that ageband. This can be used to merge top all cases in the Dataset that overlap with the start and end age. 
      Agebands_list <- list()
      
      for (k in 1:length(Age_bands)){
        
        if( k == 1) Agebands_list[[k]] <- paste0(Age_bands[k],"-",Age_bands[k+1])
        if( k > 1 &k!= length(Age_bands)) Agebands_list[[k]] <- paste0(Age_bands[k]+1,"-",Age_bands[k+1])
        if( k== length(Age_bands) & include_remaning_ages == T) Agebands_list[[k]] <- paste0(Age_bands[k]+1,"+")
        
      }
      
      Agebands_list <- as.data.table(do.call(rbind, Agebands_list))
      colnames(Agebands_list)<- "Ageband"
      
      Agebands_list[,row := row.names(Agebands_list) ]
      Agebands_list[,ST := as.numeric(gsub("[^[:digit:].]", "\\1",strsplit(as.character(Ageband),"-")[[1]][1])),by = row ]
      Agebands_list[,EN := as.numeric(gsub("[^[:digit:].]", "\\1",strsplit(as.character(Ageband),"-")[[1]][2])),by = row ]
      Agebands_list[is.na(EN),EN := 4000 ]
      
      #Merge the overlapping
      setkeyv(Dataset_cohort, c("age_start","age_end"))
      Dataset_cohort <- foverlaps(Agebands_list, Dataset_cohort, by.x = c("ST","EN"), nomatch = 0L, type = "any")
      
      # select the rows that doubled by the merge. In these, multiple agebands occur witing the obeservation period. So start and end dated need to be adapted
      Dataset_cohort <- Dataset_cohort[, row := row.names(Dataset_cohort)]
      Dataset_cohort <- Dataset_cohort[,"Start_date_temp" :=get(Start_date)]
      Dataset_cohort <- Dataset_cohort[age_start < ST  ,(Start_date) := as.IDate(add_with_rollback(get(Birth_date), period(ST,units = Unit_of_age),roll_to_first = T, preserve_hms = T)), by = row]
      Dataset_cohort <- Dataset_cohort[,"End_date_temp" :=get(End_date)]
      Dataset_cohort <- Dataset_cohort[age_end > EN  ,(End_date) := as.IDate(add_with_rollback(get(Birth_date), period(EN + 1,units = Unit_of_age), roll_to_first = T, preserve_hms = T)) - 1, by = row]
      Dataset_cohort <- Dataset_cohort[,':=' (age_start = NULL, age_end = NULL,ST = NULL, EN = NULL, row = NULL)]
    }
    Dataset_cohort[,birth_date:=NULL]
  }
  
  
  ################################################################################################################################

  #Remove records of Dataset_cohort that don’t overlap the window between Start_study_time and End_study_time
  Dataset_cohort<-Dataset_cohort[get(Start_date)>End_study_time | get(End_date)< Start_study_time, no_overlap:=1]
  rm_rows<-nrow( Dataset_cohort[no_overlap==1,])
  if(rm_rows>0) message(paste0(rm_rows, " records removed because do not overlap the window between start and end of the study"))
  Dataset_cohort<-Dataset_cohort[no_overlap!=1 | is.na(no_overlap),]
  Dataset_cohort<-Dataset_cohort[,no_overlap:=NULL]
  ####################################################
  

  #check all strings in the parameter conditions are in the unique(columns names) #######
  check<-Conditions %in% unique(Dataset_events[,get(Name_condition)])
  for (s in Conditions[!check]) warning(paste0("The string ",s, " indicated in the Condition parameter is not in the dataset" ))

  #######################

  #Compute statistics of censored persons which have spells ending before the START of the period or the point prevalence date.
  # browser()
  # if(!is.null(Periods_of_time))  {
  #   for( t in length(Periods_of_time)){
  #     if(!is.null(Periods_of_time)) Dataset_cohort<-Dataset_cohort[ get(End_date)< get(Periods_of_time)[[t]][[1]], spell_end_before_prevalenceperiod:=1]
  #   }
  # 
  # }
  # 
  # if(!is.null(Points_in_time))  {
  #   Dataset_cohort<-Dataset_cohort[ get(End_date)< get(Points_in_time[[1]]) , spell_end_before_prevalencepoint1:=1]
  # Dataset_cohort<-Dataset_cohort[ get(End_date)< get(Points_in_time[[2]]), spell_end_before_prevalencepoint2:=1]
  # Dataset_cohort<-Dataset_cohort[,spell_end_before_prevalencepoint1:=NULL]
  # Dataset_cohort<-Dataset_cohort[,spell_end_before_prevalencepoint2:=NULL]
  # # print(table(Dataset_cohort$spell_end_before_prevalencepoint1 ))
  # # print(table(Dataset_cohort$spell_end_before_prevalencepoint2 ))
  # }
    
    

    
    
  ##########################################
#POINT PREVALENCE
if (Type_prevalence == "point") {
  if (is.null(Points_in_time)){
    if (Increment!= "month") {
      Points_in_time<-seq.Date(Start_study_time,End_study_time ,by = Increment)
      if (Increment=="week") min_days_distance<-7
      if (Increment=="year") min_days_distance<-365
      diff<-as.numeric(difftime(End_study_time,tail(Points_in_time, n=1),units = "days"))
      if (diff<7 & diff!=0) Points_in_time<-head(Points_in_time, - 1) 
    }else{
      n_month<-floor(interval(Start_study_time, End_study_time) / months(1))
      Points_in_time<-add_with_rollback(Start_study_time, months(1:n_month), roll_to_first = F)
      Points_in_time<-c(Start_study_time,Points_in_time)
      new_vector<-Points_in_time < End_study_time
      Points_in_time<-Points_in_time[new_vector]
    }
  }else{
      points<-c()
      for (i in 1:length(Points_in_time)) {
        if (Points_in_time[[i]] %in% colnames(Dataset_cohort)) {
          points<-c(points,as.character(Dataset_cohort[[Points_in_time[[i]]]]))
          }else{
          stop("The column name provided in Points_in_time is not in the Dataset_cohort. Please check")
        }
        
      }
  Points_in_time<-points
  }

  #crea dt2 con 2 colonne identiche (Points_in_time)
  # DT2<- data.table(value1=start_period_dates,value2=end_period_dates)
  # 
  # setkeyv(Dataset_cohort,c(Start_date,End_date))
  # Dataset_cohort<-foverlaps(DT2,Dataset_cohort,by.x=colnames(DT2))
  
  Dataset_cohort<-rbindlist(lapply(Points_in_time, function(x) data.frame(Dataset_cohort, value=x)))

  Dataset_cohort[,in_population:=fifelse(is.na(get(Start_date)),0,1) ]
#inglobate nel foverlaps
  # Dataset_cohort<-Dataset_cohort[, in_population:=0 ]
  # Dataset_cohort<-Dataset_cohort[value<=get(End_date) & value>=get(Start_date), in_population:=1 ]
  

  dataset<-merge(Dataset_cohort,Dataset_events, by=choosen_key,all.x=T,allow.cartesian=T )

  dataset<-dataset[get(Date_condition)<=value & in_population==1 & !is.na(get( Date_condition)),constant:=1][is.na(constant),constant:=0]

  f = as.formula(sprintf('%s ~ %s', paste(c(choosen_key,Start_date,End_date,"value","in_population",Date_condition), collapse = "+ "), Name_condition))
  
  dataset<-dcast(dataset,f, value.var = "constant" ,fill=0,fun.aggregate = length)
  dataset<-dataset[,"NA":=NULL]
  cols_to_rename <- names(dataset)[names(dataset) %in% Conditions]
  setnames(dataset, cols_to_rename, paste0("prev_",cols_to_rename))


  setnames(dataset,"value","timeframe")
  cols<-paste0("prev_", Conditions)
  myvector<-c(choosen_key,UoO_id,Start_date,End_date,"timeframe","in_population",cols)
  dataset<-unique(dataset[,..myvector])

  dataset<-dataset[,(cols) := lapply(.SD, function(x)max(x)), .SDcols = cols,by=c("timeframe",choosen_key)]

  dataset<-unique(dataset[,..myvector])
}

  
# PREVALENCE OF USE
  if (Type_prevalence=="of use") {
    if (!is.null(Periods_of_time))  {
      start_period_dates<-unlist(lapply(Periods_of_time, `[[`, 1))
      end_period_dates<-unlist(lapply(Periods_of_time, `[[`, 2))


      Dataset_cohort<-melt(setDT(Dataset_cohort), id.vars = c(choosen_key,UoO_id,Start_date,End_date,Strata ), measure.vars = list(start_period_dates,end_period_dates))
          #Dataset_cohort<-rbindlist(lapply(c(start_period_dates), function(x) data.frame(Dataset_cohort, value=x)))


    }else{
      if (Increment_period!= "month") {
        start_period_dates<-seq.Date(Start_study_time,End_study_time ,by = Increment_period)
        if (Increment_period=="week") {
          end_period_dates=start_period_dates+6
          if (tail(start_period_dates, n=1)>=End_study_time) {
            end_period_dates<-end_period_dates[-length(end_period_dates)]
            start_period_dates<-start_period_dates[-length(start_period_dates)]
          }
        }
        if (Increment_period=="day") {
          end_period_dates=start_period_dates+1
          end_period_dates<-end_period_dates[-length(end_period_dates)]
          start_period_dates<-start_period_dates[-length(start_period_dates)]
        }
        if (Increment_period=="year") {
          end_period_dates<-start_period_dates-days(1)
          end_period_dates<-c(end_period_dates,ymd(paste0(year(End_study_time),"1231")))
          end_period_dates<-end_period_dates[-1]
        }
      }else{
        start_period_dates<-seq.Date(Start_study_time,End_study_time+month(1) ,by = Increment_period)
        end_period_dates<-start_period_dates-days(1)
        end_period_dates<-end_period_dates[-1]
        start_period_dates<-start_period_dates[-length(start_period_dates)]
        
      }


      Dataset_cohort<-rbindlist(lapply(1:length(start_period_dates), function(x) data.frame(Dataset_cohort, value1=start_period_dates[[x]], value2=end_period_dates[[x]])))
  }
    



    Dataset_cohort<-Dataset_cohort[ !is.na(value1),]
    if (!is.null(Periods_of_time)) {
      for (i in 1:length(Periods_of_time)) Dataset_cohort[variable==i,timeframe:=paste0(Periods_of_time[[i]][[1]],"-",Periods_of_time[[i]][[2]])]
    }else{
      Dataset_cohort<-Dataset_cohort[,timeframe:=paste0(value1,"-",value2)]
        
      }
    

    Dataset_cohort<-Dataset_cohort[, in_population:=0 ]

      
      #dataset<-dataset[value1<=ymd(Start_study_time) & value2>=ymd(Start_study_time) | value2>ymd(Start_study_time) & value2>=ymd(End_study_time) , in_population:=1 ]
    Dataset_cohort<-Dataset_cohort[value1>=ymd(Start_study_time) & value1<=ymd(Start_study_time) | value2>=ymd(Start_study_time) & value2<=ymd(End_study_time) , in_population:=1 ]
    
#prima del merge
    #togli le colonne inutili (tieni solo person_id, data e nome condizione )
#togliere tutto quello che c'è prima di study_start_time e dopo study_end_time
    #per ogni persona tieni la data minima della patologia per persona
      dataset<-merge(Dataset_cohort,Dataset_events, by=choosen_key,all.x=T,allow.cartesian=T )
      
      #dopo merge
      #raggruppa per patologia e person_id
      #df1[,median(purchaseAmt),by=c("adShown","url")]
      #tinei sol oquelle precedenti inizio studio
      #fai massimo
      #fai massimo tra le date per patologia rispetto al primo anno 
      #togli eventi dopo exit_date per persona
      
      # #name_nospaces<-gsub(" ", "",unique(Dataset_events[,get(Name_condition)]))
      # name_nospaces<-Conditions
      # for(i in  name_nospaces){
      #   name<-paste0("prev_",i)
      #   dataset[,(name):=0]
      #   dataset[,c(Name_condition) := lapply(.SD, function(x)gsub('\\s+', '', x)), .SDcols = Name_condition]
      #   dataset<-dataset[ i==get(Name_condition) & get(Date_condition)>=value1 & get(Date_condition)<=value2, (name):=1 ]
      #}
      
      name_nospaces<-gsub(" ", "",unique(Conditions))
      cols<-paste0("prev_",name_nospaces)
      
      dataset<-dataset[,(Name_condition) := lapply(.SD, function(x)gsub('\\s+', '', x)), .SDcols = Name_condition]
      dataset<-dataset[,(cols):=as.list(sub('.*_', '', cols))]
      dataset<-dataset[,(cols) := lapply(.SD,function(x)ifelse(x==get(Name_condition)& get(Date_condition)>=value1 & get(Date_condition)<=value2 & in_population==1 & !is.na(get( Date_condition)) ,1,0)),.SDcols=cols]
      
      
      myvector<-c(choosen_key,UoO_id,Start_date,End_date,"timeframe","in_population",cols,Strata)
      dataset<-unique(dataset[,..myvector])

      test_name<-paste0("prev_use_", name_nospaces)
      for (i in 1:length(name_nospaces)){
        dataset<-dataset[,test_name[[i]]:=max(get(cols[[i]])),by=c("timeframe",choosen_key,Strata)]
      }
      
      # dataset <- dataset[, lapply(.SD, max), .SDcols = paste0("prev_use_", name_nospaces), by=c("timeframe",choosen_key)]
      myvector<-c(choosen_key,UoO_id,Start_date,End_date,"timeframe","in_population",test_name,Strata)
      dataset<-unique(dataset[,..myvector])
    #}
    
    
  }

#PERIOD PREVALENCE
  if (Type_prevalence=="period") {
    print("Period prevalence computation")
    #Create the start and end of each period of interest
    #if Periods_of_time is provided by the users, extract the column names
    if (!is.null(Periods_of_time)) { 
      start_period_dates<-unlist(lapply(Periods_of_time, `[[`, 1))
      end_period_dates<-unlist(lapply(Periods_of_time, `[[`, 2))

      if(start_period_dates %in% names(Dataset_cohort)) {
        #columns are already in the what to check
        # dataset<-melt(setDT(Dataset_cohort), id.vars = c(choosen_key,Start_date,End_date,Strata ), measure.vars = list(start_period_dates,end_period_dates))
      }
    }else{
      #if Increment_period is provided, create couples of start and end dates basen on increment
      if (Increment_period!= "month") {
        start_period_dates<-seq.Date(Start_study_time,End_study_time ,by = Increment_period)
        if (Increment_period=="week") {
          end_period_dates=start_period_dates+6
          if (tail(start_period_dates, n=1)>=End_study_time) {
            end_period_dates<-end_period_dates[-length(end_period_dates)]
            start_period_dates<-start_period_dates[-length(start_period_dates)]
          }
        }
        if (Increment_period=="day") {
          end_period_dates=start_period_dates+1
          end_period_dates<-end_period_dates[-length(end_period_dates)]
          start_period_dates<-start_period_dates[-length(start_period_dates)]
        }
        if (Increment_period=="year") {
          end_period_dates<-start_period_dates-days(1)
          end_period_dates<-c(end_period_dates,ymd(paste0(year(End_study_time),"1231")))
          end_period_dates<-end_period_dates[-1]
        }
      }else{
        start_period_dates<-seq.Date(Start_study_time,End_study_time+month(1) ,by = Increment_period)
        end_period_dates<-start_period_dates-days(1)
        end_period_dates<-end_period_dates[-1]
        start_period_dates<-start_period_dates[-length(start_period_dates)]
      }
    }
    
    if(!(start_period_dates[[1]] %in% names(Dataset_cohort))) {
      CJ.dt = function(X,Y) {
        stopifnot(is.data.table(X),is.data.table(Y))
        k = NULL
        X = X[, c(k=1, .SD)]
        setkey(X, k)
        Y = Y[, c(k=1, .SD)]
        setkey(Y, NULL)
        X[Y, allow.cartesian=TRUE][, k := NULL][]
      }
      
      #tranform the 2 vectors start_period_dates and end_period_dates into a datatable
      DT2<- data.table(value1=start_period_dates,value2=end_period_dates)
      
      #compute all the overlaps between Dataset_cohort and the dataset containig the computed periods
      setkeyv(Dataset_cohort,c(Start_date,End_date))
      setkeyv(DT2,colnames(DT2))
      Dataset_cohort<-foverlaps(Dataset_cohort,DT2)
      
      #expand the dataset containig the computed periods (DT2) as many times as the number of unique id in Dataset_cohort
      tmp<-CJ.dt(unique(Dataset_cohort[,..choosen_key]),DT2)
      
      #merge Dataset_cohort and the expanded datatable to get all the possible combinations of periods
      Dataset_cohort<-merge(tmp,Dataset_cohort,all.x=T,by=c(choosen_key,"value1","value2"),allow.cartesian = T)
      
      rm(tmp)
    }
    #create the binary in_population checking if the Start_date exist in that row
    Dataset_cohort[,in_population:=fifelse(is.na(get(Start_date)),0,1) ]

    Dataset_events[,.(choosen_key,Name_condition,Date_condition)]
    Dataset_events<-Dataset_events[get(Date_condition)>=Start_study_time | get(Date_condition)<=End_study_time,]
    
    # #in future evaluate to add a filter to reduce the number of rows in events
    # Dataset_events<-merge(Dataset_events,Dataset_cohort[,.(choosen_key,Start_date)],all.x=T,by=choosen_key)
    
    #add the information on diagnosis (Dataset_events)
    Dataset_events[,cond_date2:=get(Date_condition)]
    setkeyv(Dataset_events,c(choosen_key,Date_condition,"cond_date2"))
    dataset<-foverlaps(Dataset_cohort,Dataset_events)

    dataset[,cond_date2:=NULL]
    
    #if the subject is not in population remove his Date_condition and Name_condition
    dataset<-unique(dataset[in_population==0,c(Name_condition,Date_condition):=as.list(rep(NA,2))])
    
    #prepare the dataset for the dcast:
    #converte Date_condition in integer 
    # add "prev_" in all the column Name_condition
    dataset[,(Date_condition):=as.integer(get(Date_condition))]
    dataset<-unique(dataset[!is.na(get(Date_condition)),(Date_condition):=1])
    dataset<-dataset[,(Name_condition):=paste0("prev_",gsub(" ", "",get(Name_condition)))]

    #define the formula for the dcast
    f = as.formula(sprintf('%s ~ %s', paste(c(id_columns_tokeep,"Start_date_temp","End_date_temp","value1","value2","in_population",Date_condition,"Ageband"), collapse = "+ "), Name_condition))
    
    dataset<-dcast(dataset,f, value.var = Date_condition ,fill=0)
    
    #create the timeframe column
    if (!is.null(Periods_of_time)) {
      print("No")
    }else{
      dataset<-dataset[,timeframe:=paste0(value1,"-",value2)]
      
    }
    
    #reorder and remove not necessary columns 
    setorder(dataset,"value1")
    dataset<-dataset[,(Date_condition):=NULL][,"value1":=NULL][,"value2":=NULL]
    if ("prev_NA" %in% colnames(dataset)) dataset<-dataset[,prev_NA:=NULL]
    
    #extract all the column names containing "prev_"
    cols<-colnames( dataset )[ grepl("^prev_", colnames( dataset )) ]
    dataset<-dataset[ in_population==1,(cols) := lapply(.SD, function(x)cummax(x)), .SDcols = cols,by=choosen_key] #non farlo per use
    
    #rename colmns containing Start_date and End_date, previously left apart
    setnames(dataset,c("Start_date_temp","End_date_temp"),c(Start_date,End_date))

    #se non c'è colonna prev_patologia aggiungila con tutti 0
  }
  

  if (Aggregate == T) {
    Aggr_variables<-c("timeframe")
    if (!is.null(Age_bands)) Aggr_variables<-c("Ageband")
    if (!is.null(Strata)) Aggr_variables<-c(Strata)
     dataset <- dataset[, lapply(.SD, sum), .SDcols=cols, by  = Aggr_variables]
  }
  
  return(dataset)
}













