sourceDir <- getSrcDirectory(function(dummy) {dummy})
setwd(sourceDir)

geoMean<-function(x){
  ans=exp(mean(log(x)))
}
harMean<-function(x){
  ans=1/mean(1/x)
}
data <- read.csv("CARS.csv")
GM_dist=geoMean(data$dist)
GM_speed=geoMean(data$speed)
HM_dist=harMean(data$dist)
HM_speed=harMean(data$speed)
mean_speed=mean(data$speed)
mean_dist=mean(data$dist)

jpeg("speed.jpg")
plot(data$speed,data$dist)
dev.off()

cat("Geometric mean for distance",GM_dist,"\n")
cat("Geometric speed for distance",GM_speed,"\n")
cat("Harmonnic speed for distance",HM_dist,"\n")
cat("Harmonic speed for distance",HM_speed,"\n")
cat("Mean speed for distance",mean_dist,"\n")
cat("Mean speed for distance",mean_speed,"\n")

idx=0
uniq_speed<-unique(data$speed)
sort(uniq_speed)
dist_mean<-array(dim=(length(uniq_speed)))
for (x in uniq_speed){
  dist_mean[idx]=sum(data$dist[data$speed==x])/sum(data$speed==x)
  idx=idx+1
}
jpeg("stop_dist.jpg")
plot(uniq_speed,dist_mean)
lines(uniq_speed,dist_mean)
dev.off()

rate_of_inc<-array(dim=(length(uniq_speed)))
for (i in (2:length(uniq_speed))){
  rate_of_inc[i] = (dist_mean[i]-dist_mean[i-1])/(uniq_speed[i]-uniq_speed[i-1])
}

jpeg("rate_of_increase_stop_dist.jpg")
plot(uniq_speed,rate_of_inc)
lines(uniq_speed,rate_of_inc)
dev.off()

