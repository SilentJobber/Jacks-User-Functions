#basedir = getwd()
wd_setup <- function(basedir,save.levels = 2){
  #browser()
  dirs <- list.dirs(basedir)
  
  wds <- strsplit(dirs,split = "/")
  baselength <- length(unlist(strsplit(basedir,split = "/")))
  endlength <- baselength + save.levels
  
  
  wds <- lapply(wds,function(x)(x[1:endlength]))
  wds <- lapply(wds, function(x)(x[!is.na(x)]))
  wds <- unique(wds)

  foldernames <- unlist(lapply(wds, function(x)tail(x,1)))
  wdm <- lapply(wds,function(x)paste(x,collapse = "/"))
  names(wdm) <- foldernames
  return(wdm)
}





#### Start a Time Series ####
DefineTS <- function(from = "2016-12-31 01:00",to = "2020-1-2 00:00" ,TimeColumn = "DateTime",date = F, by = "hour", tzone = "UTC"){
  require(tidyverse)
  #browser()
  from <- as.POSIXct(from,tz = tzone)
  to <- as.POSIXct(to,tz = tzone)

    
  x <- as_tibble(seq.POSIXt(from,to,by = by))

    
  colnames(x) <- TimeColumn
  
  if(date){
    x <- x %>% mutate(Date = as.Date(!!sym(TimeColumn)),Hour = hour(!!sym(TimeColumn))) %>% 
      mutate(Hour = replace(Hour,Hour == 0, 24)) %>% mutate(Date = replace(Date, Hour == 24, Date[which(Hour == 24)] - 1))
  }
  
  return(x)
} 



#df <- as.data.frame(DefineTS())
## DateTime to Date/Hour in existing data.frame #####
DT_conversion <- function(df,format,TimeColumnName = "DateTime",Year = T,Month = T, DOW = F) {
  #browser()
  require(tidyverse)
  require(lubridate)
  
  #if(!is.data.frame(df)){stop("Input is not a data.frame")}
  
  paste(sum(as.integer(is.na(df[,TimeColumnName]))),"NA's due to DateTime in df.")
  
  df <- filter(df,!is.na(TimeColumnName))
  time_col <- df %>% pull(!!sym(TimeColumnName))
  
  ## Convert Datetime column into POSIXct if needed
  if(is.POSIXct(time_col)){
    
    x <- data.frame(DateTime = time_col, Date = as.Date(time_col),Hour = hour(time_col)) %>% distinct() %>%
      mutate(Hour = replace(Hour,Hour == 0, 24)) %>% mutate(Date = replace(Date, Hour == 24, Date[which(Hour == 24)] - 1)) %>% rename({{TimeColumnName}} := DateTime)
    names(x)[1] <- TimeColumnName
    
    if(Year){x$Year <- year(x$Date)}
    if(Month){x$Month <- month(x$Date)}
    if(DOW){x$DOW <- weekdays(x$Date)}
  } else(stop("Convert TimeColumnName to POSIXct before using this function"))
  
  df <- left_join(x,df)
  return(df)
}
#df.DT_conversion(df)


`%!in%` <- function(x,y)!('%in%'(x,y))

#Copy dir to another folder (Used for scenario traking)
dir.copy <- function(from, to){
  
  ## check if from and to directories are valid
  if (!dir.exists(from)){
    cat('from: No such Directory\n')
    return (FALSE)
  }
  else if (!dir.exists(to)){
    cat('to: No such Directory\n')
    return (FALSE)
  }
  
  ## extract the directory name from 'from'
  split_ans <- unlist(strsplit(from,'/'))
  
  dir_name <- split_ans[length(split_ans)]
  
  new_to <- paste(to,dir_name,sep='/')
  
  ## create the directory in 'to'
  dir.create(new_to)
  
  ## copy all files in 'to'
  file_inside <- list.files(from,full.names = T)
  
  file.copy(from = file_inside,to=new_to)
  
  ## copy all subdirectories
  dir_inside <- list.dirs(path=from,recursive = F)
  
  if (length(dir_inside) > 0){
    for (dir_name in dir_inside)
      dir.copy(dir_name,new_to)
  }
  
  return (TRUE)
}


# File Opened Function
file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path, 
               open = "w"), 
          silent = TRUE
      )
    )
  )
}


# NERC Holiday Adj
NERCHoliday.Adj <- function(x){
  require(timeDate)
  x <- x[,c("Date","DOW")]
  Holidays <- do.call(rbind,lapply(list(
    data.frame(holiday(unique(year(x$Date)),Holiday = "USNewYearsDay") - 3600*24,"DayBefore_USNewYearsDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USNewYearsDay"),"USNewYearsDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USMemorialDay"),"USMemorialDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USMemorialDay") + 3600*24,"DayAfter_USMemorialDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USIndependenceDay") - 3600*24,"DayBefore_USIndependenceDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USIndependenceDay"),"USIndependenceDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USLaborDay") - 3600*24,"DayBefore_USLaborDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USLaborDay"),"USLaborDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USLaborDay") + 3600*24,"DayAfter_USLaborDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USThanksgivingDay") - 3600*24,"DayBefore_USThanksgivingDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USThanksgivingDay"),"USThanksgivingDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USThanksgivingDay") + 3600*24,"DayAfter_USThanksgivingDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USChristmasDay") - 3600*24,"DayBefore_USChristmasDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USChristmasDay"),"USChristmasDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USChristmasDay") + 3600*24,"DayAfter_USChristmasDay")),
    setNames,c("Date","Holiday")))   
  x <- left_join(x,Holidays)
  
  x$DOW[x$Holiday %in% c("DayAfter_USMemorialDay", "DayBefore_USThanksgivingDay")]   <- "Monday" # Holidays Modeled as Monday
  x$DOW[x$Holiday %in% "DayAfter_USLaborDay"] <- "Thursday" # Holidays Modeled as Thursday
  x$DOW[x$Holiday %in% "DayBefore_USIndependenceDay"]   <-  "Friday" # Holidays Modeled as Friday
  x$DOW[x$Holiday %in% c("DayBefore_USNewYearsDay",
                         "USMemorialDay",
                         "DayBefore_USLaborDay",
                         "USLaborDay",
                         "USThanksgivingDay",
                         "DayAfter_USThanksgivingDay",
                         "DayBefore_USChristmasDay",
                         "DayAfter_USChristmasDay")] <-  "Saturday" #Holidays Modeled as Saturday
  x$DOW[x$Holiday %in% c("USNewYearsDay", "USIndependenceDay", "USChristmasDay","2DayBefore_USChristmasDay") & x$DOW %in% "Friday"] <-  "Saturday"
  x$DOW[x$Holiday %in% c("USNewYearsDay",  "USIndependenceDay", "USChristmasDay") & x$DOW != "Friday"] <-    "Sunday"
  
  return(x)
}


# Scenario Building
scenario.ws <- function(Fyear = 2024, 
                        OriginYear.Range = 2006:2022,
                        ShiftParameters.Days = -3:3){
  require(tidyverse)
  require(lubridate)
  #
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  hist.start <- as.POSIXct(paste(min(OriginYear.Range),"1-1 1:00",sep = "-"),tz = "UTC")
  hist.end <- as.POSIXct(paste(max(OriginYear.Range) + 1,"1-1 0:00",sep = "-"),tz = "UTC")
  
  
  
  ## Creating Forecast year DateTime range key
  Fyear.TS <- DefineTS(from = as.POSIXct(paste(Fyear,"1-1 1:00",sep = "-"),tz = "UTC"),
                       to   = as.POSIXct(paste(Fyear + 1,"1-1 0:00",sep = "-"),tz = "UTC"))
  
  DT.key <-  Fyear.TS %>% 
    group_by(DateTime) %>% 
    mutate(Scenario = list(OriginYear.Range)) %>% 
    unnest(cols = c(Scenario))
  
  ## Creating Scenario year data Origin.year range key
  scenariodata.TS <- DefineTS(from = hist.start + days(min(ShiftParameters.Days)),
                              to   = hist.end + days(max(ShiftParameters.Days)))
  
  Data  <- DT_conversion(scenariodata.TS) %>% 
    rename(Scenario = Year,
           Origin.DateTime = DateTime) %>% 
    select(-c(Month))
  
  year(Data$Date) <- Fyear #replace year in Date
  
  Data <- Data %>% 
    mutate(DateTime = as.POSIXct(paste(Date,Hour),format = "%Y-%m-%d %H",tz = "UTC"))  %>%
    mutate(DateTime = replace(DateTime,Origin.DateTime < hist.start | Origin.DateTime > hist.end,NA),
           Scenario = replace(Scenario,Origin.DateTime < hist.start | Origin.DateTime > hist.end,NA)) %>%
    select(Origin.DateTime,DateTime,Scenario) %>%
    full_join(DT.key) %>% 
    mutate(Shift.Days = 0) 
  
  Data <-   Data %>% 
    mutate(Origin.DateTime = if_else(is.na(Origin.DateTime),DateTime - days(1),Origin.DateTime)) # replace leap day with day before
  
  #Data %>%  filter(is.na(Origin.DateTime))
  
  shift = function(x, lag) {
    require(dplyr)
    switch(sign(lag)/2 + 1.5, lead(x, abs(lag)), lag(x, abs(lag)))
  }
  
  #Examples of how lag/lead works
  # (-) = lag  1 day lag example (-1): Forecast Date (12-31: December 31st) comes from (1-1: January 1st)
  # (+) = lead 1 day lead example (1): Forecast Date (12-31: December 31st) comes from (12-30: December 30th)
  ShiftParameters.Days <- -3:3
  Data.shift <- list()
  for(i in ShiftParameters.Days){
    #i <- -3
    Data.shift[[paste0("ShiftDays_",i)]] <- Data %>% 
      mutate(Origin.DateTime = shift(Origin.DateTime,lag = i * 24), Scenario = paste0("Scenario_OY",Scenario),Shift.Days = i) %>%
      filter(!is.na(DateTime))
    
  }
  Data <- do.call(rbind,Data.shift)
  #browser()
  
  Data <-   Data %>% 
    mutate(Origin.DateTime = if_else(is.na(Origin.DateTime),DateTime - days(1),Origin.DateTime)) # Hopefully is replacing only feb29th with feb 28th
 #Data <- Data %>% filter(is.na(Origin.DateTime))
 
  ## Cleaning Leap DateTimes from scenario Results
  if(Fyear %!in% seq(from = 1988, to = 2100,by = 4)){
    leap_dates <- expand.grid(c("1988-2-29","1992-2-29","1996-2-29","2000-2-29","2004-2-29","2008-2-29","2012-2-29","2016-2-29","2020-2-29","2024-2-29",
                                "2028-2-29","2032-2-29","2036-2-29","2040-2-29","2044-2-29","2048-2-29","2052-2-29","2056-2-29","2060-2-29","2064-2-29"),1:24) %>% 
      mutate(Leap_DateTimes = paste(Var1,Var2)) %>% mutate(Leap_DateTimes = as.POSIXct(Leap_DateTimes,format = "%Y-%m-%d %H",tz = "UTC"))
    Data <- filter(Data,DateTime %!in% leap_dates$Leap_DateTimes)
  }
  Data <- Data %>% arrange(Shift.Days,Scenario,DateTime)
  return(Data)
}
#ws_Forecast <- scenario.ws(2024,2006:2021,-3:3)
