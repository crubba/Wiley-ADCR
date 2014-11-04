HTTPdate <- function(time="now", type=c("rfc1123","rfc850","ascitime")[1]){
    if(time=="now") {
        tmp <- as.POSIXlt(Sys.time(),tz="GMT")
    }else{
        tmp <- as.POSIXlt(as.POSIXct(time),tz="GMT")
    }
    nday <- c("Sun", "Mon" , "Tue" , "Wed", "Thu" , "Fri" , "Sat")[tmp$wday+1]
    month <- tmp$mon+1
    nmonth <- c("Jan" , "Feb" , "Mar" , "Apr", "May" , "Jun" , "Jul" , "Aug", "Sep" , "Oct" , "Nov" , "Dec")[month]
    mday <- formatC(tmp$mday, width=2, flag="0")
    hour <- formatC(tmp$hour, width=2, flag="0")
    min  <- formatC(tmp$min , width=2, flag="0")
    sec  <- formatC(round(tmp$sec) , width=2, flag="0")
    if(type=="rfc1123"){
        return(paste0(nday,", ",mday," ",nmonth," ",tmp$year+1900," ",hour,":",min,":",sec," GMT"))
    }
    if(type=="rfc850"){
        message("not implemented yet")
        return(NULL)
        }
    if(type=="ascitime"){
        message("not implemented yet")
        return(NULL)
        }
}

file.date <- function(filename,timezone=Sys.timezone()) {
    as.POSIXlt(min(unlist(file.info(filename)[4:6])),origin = "1970-01-01", tz = timezone)
}

# usage: 
# HTTPdate()
# HTTPdate( file.date("812.pdf") ) 


