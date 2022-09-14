
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
DefineTS <- function(from = "2016-12-31 01:00",to = "2020-1-2 00:00" ,TimeColumn = "DateTime",date = F){
  require(dplyr)
  require(timeDate)
  #browser()
  from <- as.POSIXct(from,tz = "UTC")
  to <- as.POSIXct(to,tz = "UTC")
  
  x <- as.data.frame(timeSequence(from,to,by = "hour"))
  
  colnames(x) <- TimeColumn
  
  if(date){
    x <- x %>% mutate(Date = as.Date(!!sym(TimeColumn)),Hour = hour(!!sym(TimeColumn))) %>% 
      mutate(Hour = replace(Hour,Hour == 0, 24)) %>% mutate(Date = replace(Date, Hour == 24, Date[which(Hour == 24)] - 1))
  }
  
  return(x)
} 

`%!in%` <- function(x,y)!('%in%'(x,y))

#df <- ts
## DateTime to Date/Hour in existing data.frame #####
data.frame.DatetimeConversion <- function(df,format,TimeColumnName = "DateTime",Year = T,Month = T, DOW = F) {
  if(!is.data.frame(df)){stop("Input is not a data.frame")}
  #browser()
  require(dplyr)
  require(lubridate)
  
  paste(sum(as.integer(is.na(df[,TimeColumnName]))),"NA's due to DateTime in df.")
  df <- filter(df,!is.na(TimeColumnName))
  x <- df
  ## Convert Datetime column into POSIXct if needed
  if(is.POSIXct(df[,TimeColumnName])){
    df <- data.frame(DateTime = df[,TimeColumnName], Date = as.Date(df[,TimeColumnName]),Hour = hour(df[,TimeColumnName])) %>% distinct() %>%
      mutate(Hour = replace(Hour,Hour == 0, 24)) %>% mutate(Date = replace(Date, Hour == 24, Date[which(Hour == 24)] - 1)) %>% rename({{TimeColumnName}} := DateTime)
    names(df)[1] <- TimeColumnName
    
    if(Year){df$Year <- year(df$Date)}
    if(Month){df$Month <- month(df$Date)}
    if(DOW){df$DOW <- weekdays(df$Date)}
  } else(stop("Convert TimeColumnName to POSIXct before using this function"))
  
  df <- left_join(df,x)
  return(df)
}



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

