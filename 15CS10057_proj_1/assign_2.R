sourceDir <- getSrcDirectory(function(abc) {abc})
setwd(sourceDir)                                         # to set working directory same as the source of the code


library("xlsx")

rem_outlier <- function(x,q1,q2,q3)
{
  iqr=q3-q1
  ans<-x
  ans[x<q1-1.5*iqr]=NA
  ans[x>q3+1.5*iqr]=NA
  ans
}

data <- read.xlsx("EARTHQUAKE.xlsx",sheetIndex = 1)
#for depth data
min_depth=min(data$Depth.Km)
q1_depth=quantile(data$Depth.Km,prob=(0.25))
q2_depth=quantile(data$Depth.Km,prob=(0.5))
q3_depth=quantile(data$Depth.Km,prob=(0.75))
max_depth=max(data$Depth.Km)
mean_depth=mean(data$Depth.Km)

# for magnitude
min_mag=min(data$Magnitude)
q1_mag=quantile(data$Magnitude,prob=(0.25))
q2_mag=quantile(data$Magnitude,prob=(0.5))
q3_mag=quantile(data$Magnitude,prob=(0.75))
max_mag=max(data$Magnitude)
mean_mag=mean(data$Magnitude)

cat("On raw data:\n")
cat("Depth data: min,Q1,Q2,Q3,max, mean:", min_depth,",",q1_depth,",",q2_depth,",",q3_depth,",",max_depth,",",mean_depth,"\n")
cat("Magnitude data: min,Q1,Q2,Q3,max, mean:",min_mag,",",q1_mag,",",q2_mag,",",q3_mag,",",max_mag,",",mean_mag,"\n")

new_depth=rem_outlier(data$Depth.Km,q1_depth,q2_depth,q3_depth)
new_mag=rem_outlier(data$Magnitude,q1_mag,q2_mag,q3_mag)

jpeg("raw_depth.jpg")
boxplot(data$Depth.Km,main="Raw depth data",ylab="Depth--->")
dev.off()
jpeg("raw_mag.jpg")
boxplot(data$Magnitude,main="Raw magnitude data",ylab="Magnitude--->")
dev.off()
jpeg("new_depth.jpg")
boxplot(new_depth,main="Rectified depth data",ylab="Depth--->")
dev.off()
jpeg("new_mag.jpg")
boxplot(new_mag,main="Rectified magnitude data",ylab="Magnitude--->")
dev.off()
jpeg("Seismic Zone.jpg")
plot(data$Longitude,data$Latitude)
dev.off()





