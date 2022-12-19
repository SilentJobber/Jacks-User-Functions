rm(list = setdiff(ls(),c(lsf.str())))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
list.files()
library(tidyverse)
library(lubridate)
load.test <- read_csv("MID Total Demand - Hourly.csv")
temp.test <- read_csv("MID Station Temperature - Hourly.csv")


load.test %>% filter(`DR ADJ` > 0)

temp.test <- temp.test %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M", tz = "UTC")) 

## Weather shift

weather.shift <- function(df,TimeColumn = "DateTime",shift.days = -3:3){
 browser()
 old.dt <- paste0("Origin.",TimeColumn) 
 test <- list()
 for(shift in shift.days){
 test[[as.character(shift)]] <- df %>% 
   rename({{old.dt}} := {{TimeColumn}}) %>%
   mutate({{TimeColumn}} := !!sym(old.dt) + hours(24 * shift),
          shift.days = shift)
 }
 test <- do.call(rbind,test)
 return(test)
}

test2 <- weather.shift(df = temp.test)

(temp.test %>% pull(DateTime))[1] + hours(24)

######################

Scenarios.Building <- function(Fyear = 2021, 
                               HistDataTS = DefineTS(from = "2003-12-25 01:00",to =  "2021-01-09 0:00",TimeColumn = "Origin.DateTime"),
                               OriginYear.Range = 2004:2020,
                               ShiftParameters.Days = -7:7){
  require(tidyverse)
  require(lubridate)
  #browser()
  '%!in%' <- function(x,y)!('%in%'(x,y))
  Data <- HistDataTS
  Data  <- data.frame.DatetimeConversion(Data,TimeColumnName = "Origin.DateTime",Year = T,Month = F) %>% 
    mutate(Scenario = replace(Year,Year %!in% OriginYear.Range ,NA)) %>% mutate(Scenario = replace(Scenario,Year %!in% OriginYear.Range ,NA)) %>% select(-Year) 
  
  year(Data$Date) <- Fyear
  
  Data <- Data %>% mutate(DateTime = as.POSIXct(paste(Date,Hour),format = "%Y-%m-%d %H",tz = "UTC"),Shift.Days = 0) %>% select(Origin.DateTime,DateTime,Scenario,Shift.Days)
  
  
  
  shift = function(x, lag) {
    require(dplyr)
    switch(sign(lag)/2 + 1.5, lead(x, abs(lag)), lag(x, abs(lag)))
  }
  
  #Examples of how lag/lead works
  # (-) = lag  1 day lag example (-1): Forecast Date (12-31: December 31st) comes from (1-1: January 1st)
  # (+) = lead 1 day lead example (1): Forecast Date (12-31: December 31st) comes from (12-30: December 30th)
  
  # ShiftParameters.Days <- ShiftParameters.Days[ShiftParameters.Days!= 0]
  # Data.shift <- list(ShiftDays_0 = Data %>% mutate(Scenario = paste0("Scenario_OY",Scenario))) 
  Data.shift <- list()
  for(i in ShiftParameters.Days){
    
    Data.shift[[paste0("ShiftDays_",i)]] <- Data %>% mutate(Origin.DateTime = shift(Origin.DateTime,lag = i * 24), Scenario = paste0("Scenario_OY",Scenario),Shift.Days = i)
    
  }
  Data <- do.call(rbind,Data.shift)
  Data <- filter(Data,Scenario != "Scenario_OYNA") %>% filter(!is.na(DateTime))
  
  ## Cleaning Leap DateTimes from scenario Results
  leap_dates <- expand.grid(c("1988-2-29","1992-2-29","1996-2-29","2000-2-29","2004-2-29","2008-2-29","2012-2-29","2016-2-29","2020-2-29","2024-2-29",
                              "2028-2-29","2032-2-29","2036-2-29","2040-2-29","2044-2-29","2048-2-29","2052-2-29","2056-2-29","2060-2-29","2064-2-29"),1:24) %>% 
    mutate(Leap_DateTimes = paste(Var1,Var2)) %>% mutate(Leap_DateTimes = as.POSIXct(Leap_DateTimes,format = "%Y-%m-%d %H",tz = "UTC"))
  Data <- filter(Data,DateTime %!in% leap_dates$Leap_DateTimes)
  
  
  return(Data)
}


