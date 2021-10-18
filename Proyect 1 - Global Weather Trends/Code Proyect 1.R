
#dplyr use for basic functions like filtering. ggplot2 for the graph.
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(dplyr)
library(ggplot2)

#setting the working directory and reading the csv data.if executing in another computer, change path.
setwd("C:/Users/Jaime Auger Esterio/Desktop/Udacity/Proyect 1")
data_set <- read.csv(file = 'results.csv')

#Filter the temperatures because the city data has also the maximum teperatures of each year in the data and if i do a query for only accesing the global data, there is less than the half of the city data 
data_set <- filter(data_set, santiago_avg_temp<10)

#Then with the summary function we can find out if there is any missing value
summary(data_set)

#Creation of the moving averages (5 and 10 years) for the anual temperatures of Santiago and the world.
x<- rep(0,length(data_set$santiago_avg_temp))
y<- rep(0,length(data_set$santiago_avg_temp))
w<- rep(0,length(data_set$global_avg_temp))
z<- rep(0,length(data_set$global_avg_temp))

for (r in 1:length(data_set$santiago_avg_temp)){
  if (r>=5){
    x[r] <- (data_set$santiago_avg_temp[r] + data_set$santiago_avg_temp[r-1] + data_set$santiago_avg_temp[r-2] 
             + data_set$santiago_avg_temp[r-3] + data_set$santiago_avg_temp[r-4])/5
    w[r] <- (data_set$global_avg_temp[r] + data_set$global_avg_temp[r-1] + data_set$global_avg_temp[r-2] 
             + data_set$global_avg_temp[r-3] + data_set$global_avg_temp[r-4])/5
  }
  if (r>=10){
    y[r] <- (data_set$santiago_avg_temp[r] + data_set$santiago_avg_temp[r-1] + data_set$santiago_avg_temp[r-2] 
             + data_set$santiago_avg_temp[r-3] + data_set$santiago_avg_temp[r-4] + data_set$santiago_avg_temp[r-5] 
             + data_set$santiago_avg_temp[r-6] + data_set$santiago_avg_temp[r-7] + data_set$santiago_avg_temp[r-8] 
             + data_set$santiago_avg_temp[r-9])/10  
    z[r] <- (data_set$global_avg_temp[r] + data_set$global_avg_temp[r-1] + data_set$global_avg_temp[r-2] 
             + data_set$global_avg_temp[r-3] + data_set$global_avg_temp[r-4] + data_set$global_avg_temp[r-5] 
             + data_set$global_avg_temp[r-6] + data_set$global_avg_temp[r-7] + data_set$global_avg_temp[r-8] 
             + data_set$global_avg_temp[r-9])/10    
  }
}

data_set <- mutate(data_set, ma5_santiago = x)
data_set <- mutate(data_set, ma10_santiago = y)
data_set <- mutate(data_set, ma5_global = w)
data_set <- mutate(data_set, ma10_global = z)
data_set <- mutate(data_set, difference_10 = ma10_global - ma10_santiago)

#reshape the data matrix to make it suitable for the graphics
data_set2<-data.frame(Year = data_set$year, Temperature = c(data_set$ma5_santiago, data_set$ma10_santiago, data_set$ma5_global, data_set$ma10_global), Moving_Averages = c(rep("Santiago - 5 year",nrow(data_set)),rep("Santiago - 10 year",nrow(data_set)),rep("Global - 5 year",nrow(data_set)),rep("Global - 10 year",nrow(data_set))))
data_set2<-filter(data_set2, Temperature!=0)

#create the linegraph
linegraph <- ggplot(data_set2, aes(Year, Temperature, col = Moving_Averages)) + geom_line() + ggtitle("5/10-years moving average temperature comparison, Global Vs Santiago") + ylab("Temperature (°C)")

#Show the graph
linegraph

#Bonus - Calculate the differences between each mobile average
data_set_diff <- data.frame(data_set$year[2:159], 
                            diff_ma5_stgo=diff(data_set$ma5_santiago), 
                            diff_ma10_stgo=diff(data_set$ma10_santiago), 
                            diff_ma5_global=diff(data_set$ma5_global), 
                            diff_ma10_global=diff(data_set$ma10_global))

m<- rep(0,length(nrow(data_set_diff)))
n<- rep(0,length(nrow(data_set_diff)))
for (i in 1:length(data_set_diff$diff_ma5_global)){
  if (data_set_diff$diff_ma5_stgo[i]*data_set_diff$diff_ma5_global[i] > 0){
    m[i] <- TRUE
  }
  if (data_set_diff$diff_ma5_stgo[i]*data_set_diff$diff_ma5_global[i] <= 0){
    m[i] <- FALSE
  }
  if (data_set_diff$diff_ma10_stgo[i]*data_set_diff$diff_ma10_global[i] > 0){
    n[i] <- TRUE
  }
  if (data_set_diff$diff_ma10_stgo[i]*data_set_diff$diff_ma10_global[i] <= 0){
    n[i] <- FALSE
  }
}
data_set_diff <- mutate(data_set_diff, diff_5 = m)
data_set_diff <- mutate(data_set_diff, diff_10 = n)
summary(data_set_diff)



