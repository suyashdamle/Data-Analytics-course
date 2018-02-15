sourceDir <- getSrcDirectory(function(dummy) {dummy})
setwd(sourceDir)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data<-read.csv("AUTOMOBILES.csv")
# Nominal measure
mode_make=getmode(data$make)
cat("Nominal: mode:",mode_make,"\n")


# Ordinal
mode_symbol=getmode(data$symboling)
median_symbol=median(data$symboling)
cat("Ordinal: mode,median",mode_symbol,",",median_symbol,"\n")

# Interval
mode_interval=getmode(data$compression.ratio)
median_interval=median(data$compression.ratio)
mean_interval=mean(data$compression.ratio)
cat("Interval: mode,median,mean ",mode_interval,median_interval, mean_interval,"\n")

# Ratio
mean_ratio=mean(data$curb.weight)
median_ratio=median(data$curb.weight)
mode_ratio=getmode(data$curb.weight)
cat("Ratio: mode,median,mean ",mode_ratio,median_ratio, mean_ratio,"\n")

data_mpg<- data$city.mpg
data_rpm <- data$peak.rpm
cat(as.numeric(as.character(data_rpm)),"\n")

jpeg("rpm_graph.jpg")
hist(as.numeric(as.character(data_rpm)))
dev.off()
jpeg("mpg_graph.jpg")
hist(data_mpg)
dev.off()