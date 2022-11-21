# Ignacio Valero, 16/11/2022, data from two questionnaires related to student performance (Source: UCI Machine Learning Repository)
#R package: Caret --> Used for cross-validation (making several groups/models and compare them to each other)

library(tidyverse)
getwd()

# Read both student-mat.csv and student-por.csv into R (from the data folder) and explore the structure and dimensions of the data.

data_math <- read.csv("C:/Users/innac/Documents/iods project/IODS-project/data/student-mat.csv", header = TRUE, sep = ";")
data_por <- read.csv("C:/Users/innac/Documents/iods project/IODS-project/data/student-por.csv", header = TRUE, sep = ";")

dim(data_math)
str(data_math)

dim(data_por)
str(data_por)

colnames(data_por)
colnames(data_math)

# Join the two data sets using all other variables than "failures", "paid", "absences", "G1", "G2", "G3" as (student) identifiers. Keep only the students present in both data sets. Explore the structure and dimensions of the joined data.

free_cols <- c("failures", "paid", "absences", "G1", "G2","G3")
join_cols <- setdiff(colnames(data_por), free_cols)
math_por <- inner_join(data_math, data_por, by = join_cols)

names(math_por)

colnames(math_por)
glimpse(math_por)

colnames(math_por)

# Get rid of the duplicate records in the joined data set.

# create a new data frame with only the joined columns
alc <- select(math_por, all_of(join_cols))

# print out the columns not used for joining (those that varied in the two data sets)

free_cols

# for every column name not used for joining...
for(col_name in free_cols) {
  # select two columns from 'math_por' with the same original name
  two_cols <- select(math_por, starts_with(col_name))
  # select the first column vector of those two columns
  first_col <- select(two_cols, 1)[[1]]
  
  # then, enter the if-else structure!
  # if that first column vector is numeric...
  if(is.numeric(first_col)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[col_name] <- round(rowMeans(two_cols))
  } else { # else (if the first column vector was not numeric)...
    # add the first column vector to the alc data frame
    alc[col_name] <- first_col
  }
}


glimpse(alc)


#Take the average of the answers related to weekday and weekend alcohol consumption to create a new column 'alc_use' to the joined data. 

alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Then use 'alc_use' to create a new logical column 'high_use' which is TRUE for students for which 'alc_use' is greater than 2 (and FALSE otherwise).

alc <- mutate(alc, high_use = if_else (alc_use > 2,
                                       TRUE,
                                       FALSE))
glimpse(alc)

write.csv(alc)

#Writing the final csv for rmd to be able to read it
write.csv(alc,"C:\\Users\\innac\\Desktop\\alc.csv", row.names = FALSE)

adsdasdsa