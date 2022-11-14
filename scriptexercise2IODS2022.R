# Ignacio Valero, 9/11/2022, Exercise 2: Data Wrangling
learning2014 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep = "\t", header = T)
dim(learning2014)
str(learning2014)

#Table dimensions are 183 x 60, with all variables but gender (chr) being integers (int)
learning2014$Attitude
#Grouping variables into the interest columns
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

deep_columns <- select(learning2014, one_of(deep_questions)) 
surface_columns <- select(learning2014, one_of(surface_questions))
strategic_columns <- select(learning2014, one_of(strategic_questions))

#adjusting the columns by averaging values depending on scale
learning2014$deep <- rowMeans(deep_columns)
learning2014$surface <- rowMeans(surface_columns)
learning2014$strategic <- rowMeans(strategic_columns)
learning2014$attitude <- learning2014$Attitude / 10
#checking that the names prunt correctly
colnames(learning2014)
library(dplyr)

#creating a final dataframe with the interest columns and required filters (points > 0)

keep_columns2 <- c("gender","Age","attitude", "deep", "strategic", "surface", "Points")
select(learning2014, one_of(keep_columns2))
keeptable <- select(learning2014, one_of(keep_columns2))

colnames(keeptable)


keeptable <- filter(keeptable, Points > 0)

str(keeptable)
dim(keeptable)

library(ggplot2)
keeptable

# CREATING A PLOT: the example uses Points as IV and Others as DV
ggplot(data = keeptable,
       mapping = aes(x = Points,
                     y = surface)) + 
  geom_point (size = 2)


ggplot(data = keeptable,
       mapping = aes(x = Points,
                     y = strategic)) + 
  geom_point (size = 2)

ggplot(data = keeptable,
       mapping = aes(x = Points,
                     y = deep)) + 
  geom_point (size = 2)


#Noting the shortened versions of the above
ggplot(keeptable, aes(Points, attitude)) + geom_point(size = 2)

# Using Pipe we can dive a bit deeper with the plotting
library(tidyverse)
keeptable %>%
  ggplot(aes(Points, attitude, colour = gender))+ 
  geom_point() + 
  geom_smooth(method = lm) 
  

keeptable %>%
  ggplot(aes(Points, surface, colour = gender))+ 
  geom_point() + 
  geom_smooth(method = lm)

keeptable %>%
  ggplot(aes(Points, deep, colour = gender))+ 
  geom_point() + 
  geom_smooth(method = lm)

keeptable %>%
  ggplot(aes(Points, strategic, colour = gender))+ 
  geom_point() + 
  geom_smooth(method = lm)

keeptable %>%
  ggplot(aes(Points, Age, colour = gender))+ 
  geom_point() + 
  geom_smooth(method = lm)


# Attempting to create a model

ggplot(data = keeptable, aes(Points, attitude))+ 
  geom_point() + 
  geom_smooth(method = lm)


