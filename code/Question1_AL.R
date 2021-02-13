BikeShare = read.csv("YOUR .CSV PATH HERE")
library(dplyr)
library(lubridate)
library(ggplot2)
# Create Table for 2011
Month_Table_2011 <- BikeShare[which(year(BikeShare$dteday) == 2011) ,]

#This was a test; I built the for loops based on the main ideas behind this code
Jan_2011_Count <-  Month_Table_2011[which(Month_Table_2011$mnth == 1), ]
sum(Jan_2011_Count$cnt)

# Create for loops for 2011 and 2012
BikeShare_Unique <- unique(BikeShare$mnth)
BikeShare_Length <- length(unique(BikeShare$mnth))

  # Empty matrix to store results
Month_Store_2011 = matrix(NA, BikeShare_Length , 2)

  # loops for 2011
count = 0
for(ii in BikeShare_Unique){
  count = count + 1
  Month_Store_2011[count, 1] = ii
  X_table <-  Month_Table_2011[which(Month_Table_2011$mnth == ii), ]
  Month_Store_2011[count, 2] = sum(X_table$cnt)
  as.data.frame(Month_Store_2011)
}


  # empty matrix for storage
Month_Store_2012 = matrix(NA, BikeShare_Length , 2)  
Month_Table_2012 <- BikeShare[which(year(BikeShare$dteday) == 2012) ,]

  #loop for 2012 
count = 0
for(jj in BikeShare_Unique){
  count = count + 1
  Month_Store_2012[count, 1] = jj+12
  Y_table <-  Month_Table_2012[which(Month_Table_2012$mnth == jj), ]
  Month_Store_2012[count, 2] = sum(Y_table$cnt)
  as.data.frame(Month_Store_2012)
}

Count_per_Month <- data.frame(rbind(Month_Store_2011, Month_Store_2012))

names(Count_per_Month)[1] <- "Month"
names(Count_per_Month)[2] <-  "Riders"
ggplot(Count_per_Month, aes(x = Month, y = Riders)) + geom_point(alpha = 0.6)+
  stat_smooth (method = "lm", color = "red", se = FALSE)

#This will be run at the end of the chunk(S) to avoid conflicts w/ other package
detach("package:lubridate", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
