

#### ASSIGNMENT 6: Longitudinal Data

#DATA WRANGLING
library(dplyr)
library(tidyr)
library(FactoMineR)
library(ggplot2)
#1. Load the data sets (BPRS and RATS) into R using as the source the GitHub repository of MABS, where they are given in the wide form:


BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep =" ", header = T)

RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')


#As before, write the wrangled data sets to files in your IODS-project data-folder.

#Also, take a look at the data sets: check their variable names, view the data contents and structures, and create some brief summaries of the variables , so that you understand the point of the wide form data.

write.csv(BPRS, "C:/Users/innac/Documents/iods project/IODS-project/data/BPRS.csv")

write.csv(RATS, "C:/Users/innac/Documents/iods project/IODS-project/data/RATS.csv")



colnames(BPRS)
colnames(RATS)

summary(BPRS)
summary(RATS)

glimpse(BPRS)
glimpse(RATS)

#Convert the categorical variables of both data sets to factors.
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)


BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

#Convert the data sets to long form. Add a week variable to BPRS and a Time variable to RATS

BPRSL <-  pivot_longer(BPRS, cols = -c(treatment, subject),
                       names_to = "weeks", values_to = "bprs") %>%
  arrange(weeks) %>% 
  mutate(week = as.integer(substr(weeks, start = 5, stop = 5)))


RATSL <- pivot_longer(RATS, cols = -c(ID, Group), 
                      names_to = "WD",
                      values_to = "Weight") %>% 
  mutate(Time = as.integer(substr(WD, 3, 4))) %>%
  arrange(Time)



#Now, take a serious look at the new data sets and compare them with their wide form versions

summary(BPRSL)
summary(RATSL)






