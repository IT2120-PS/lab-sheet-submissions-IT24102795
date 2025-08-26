setwd("C:\\Users\\it24102795\\Desktop\\IT24102795_Lab04")

##1.Import the data set
branch_data <- read.table("Exercise.txt",header = TRUE,sep = ",")

##2.Structure of the data
str(branch_data)

##Get a summary of the data

summary(branch_data)

##Boxplot for sales
boxplot(branch_data$Sales_X1, 
        outline = TRUE,
        outpch=8,
        horizontal=TRUE,
        main = "sales distribution")

##4.Five number summary, min,max,q1,q2,q3
summary(branch_data$Advertising_X2)


##calculate IQR
IQR_advertising <- IQR(branch_data$Advertising)
IQR_advertising  ##Display the iqr


##5. Function to find outliers
find_outliers <- function(years){
  Q1 <- quantile(years)[2]
  Q3 <- quantile(years)[4]
  iqr <- Q3-Q1
  
  lower_bound <- Q1 - 1.5 * iqr
  upper_bound <-Q3 + 1.5 * iqr
  
  outliers <- years[years < lower_bound |  years > upper_bound]
  
  ##sort the outlier
  outliers = sort(outliers)
  
  print(paste("Upper Bound:",upper_bound))
  print(paste("Lower Bound:",lower_bound))
  print(paste("IQR:",iqr))
  print(paste("Outliers",paste(outliers,collapse = ",")))
}

##get outliers for the Years variable
find_outliers(branch_data$Years)




