sourceDir <- getSrcDirectory(function(abc) {abc})
setwd(sourceDir)                                         # to set working directory same as the source of the code

library("xlsx")


conf_interval = function(data,s2,alpha) {
  size = nrow(data)
  lower = qchisq((1 - alpha)/2, size-1)
  upper = qchisq((1-alpha)/2, size-1, lower.tail = FALSE)  #, lower.tail = FALSE
  c((size-1) * s2/upper, (size-1) * s2/lower)
}

data <- read.xlsx("IRIS.xlsx",sheetIndex = 1)
sample_data<-data[sample(1:nrow(data),50),]

mean_sep_len=mean(sample_data$SepalLengthCm)
mean_sep_wid=mean(sample_data$SepalWidthCm)
mean_pet_len=mean(sample_data$PetalLengthCm)
mean_pet_wid=mean(sample_data$PetalWidthCm)

# sample variance being calculated here
var_sep_len=var(sample_data$SepalLengthCm)
var_sep_wid=var(sample_data$SepalWidthCm)
var_pet_len=var(sample_data$PetalLengthCm)
var_pet_wid=var(sample_data$PetalWidthCm)

# population variance
var_sep_len_pop=var(data$SepalLengthCm)
var_sep_wid_pop=var(data$SepalWidthCm)
var_pet_len_pop=var(data$PetalLengthCm)
var_pet_wid_pop=var(data$PetalWidthCm)

cat("Mean values: sepal length, width=",mean_sep_len,",",mean_sep_wid,"  petal length, width=",mean_pet_len,",",mean_pet_wid,"\n")
cat("Variance values: sepal length, width=",var_sep_len,",",var_sep_wid,"  petal length, width=",var_pet_len,",",var_pet_wid,"\n")

conf_inter_sep_len<-conf_interval(sample_data["SepalLengthCm"],var_sep_len,0.9)
conf_inter_sep_wid<-conf_interval(sample_data["SepalWidthCm"],var_sep_wid,0.9)
conf_inter_pet_len<-conf_interval(sample_data["PetalLengthCm"],var_pet_len,0.9)
conf_inter_pet_wid<-conf_interval(sample_data["PetalWidthCm"],var_pet_wid,0.9)

cat("\n\nconfidence interval(90%) for sepal length: ",conf_inter_sep_len," ; True population variance: ",var_sep_len_pop,"\n")
cat("confidence interval(90%) for sepal Width: ",conf_inter_sep_wid," ; True population variance: ",var_sep_wid_pop,"\n")
cat("confidence interval(90%) for petal length: ",conf_inter_pet_len," ; True population variance: ",var_pet_len_pop,"\n")
cat("confidence interval(90%) for petal width: ",conf_inter_pet_wid," ; True population variance: ",var_pet_wid_pop,"\n")






