sourceDir <- getSrcDirectory(function(abc) {abc})
setwd(sourceDir)                                         # to set working directory same as the source of the code


library(xlsx)
library(xlsxjars)
df<-data.frame()

for (x in 1:56) 
  df <- rbind(df, read.xlsx2("weather.xlsx",sheetIndex = x))

# Creating data cubes for the three required data values
rainfall_cube=tapply(as.numeric(as.character(df$Mean.Rainfall.in)),df[,c("Station.Name","Month","Period")],FUN=function(x){return(mean(x))})
min_temp_cube=tapply(as.numeric(as.character(df$X. )),df[,c("Station.Name","Month","Period")],FUN=function(x){return(min(x))})
max_temp_cube=tapply(as.numeric(as.character(df$Mean.Temperature.oC)),df[,c("Station.Name","Month","Period")],FUN=function(x){return(max(x))})


# Performing operations  on the data-set:

# Slice:
cat("\n\nPrinting data for mean rainfall in Ahmedabad in 1901-2000 for all months: \n")
cat(rainfall_cube["Ahmedabad",,"1901-2000"])
cat("\n\nPrinting data for mean rainfall in Kolkata in January during 1901-2000 periods: \n")
cat(rainfall_cube["Kolkata (Alipur)","January","1901-2000"])

# Dice:
cat("\n\nPrinting data for minimum avg. temperature in Banglore and Kolkata in January,February,April during 1901-2000 periods: \n")
cat(min_temp_cube[c("Bangalore","Kolkata (Alipur)"),c("January","February","April"),"1901-2000"])

# Rollup
cat("\n\nPrinting data for average rainfall over Mumbai during entire recorded period averaged over ALL MONTHS \n")
cat(apply(rainfall_cube, c("Station.Name"),FUN=function(x){return(mean(x,na.rm=TRUE))})["Mumbai (Santa Cruz)"])

# Drlldown
cat("\n\nPrinting data for average rainfall over Mumbai during entire recorded period separately for each month \n")
cat(apply(rainfall_cube, c("Station.Name","Month"),FUN=function(x){return(mean(x,na.rm=TRUE))})["Mumbai (Santa Cruz)",])



