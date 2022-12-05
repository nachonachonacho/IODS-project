
hd <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv")
gii <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv", na = "..")

library(dplyr)
library(tidyverse)


str(hd)
dim(hd)
summary(hd)

str(gii)
dim(gii)
summary(gii)


hd.renamed <- hd %>%
  rename(HDI = Human.Development.Index..HDI.
         , LEBirth = Life.Expectancy.at.Birth
         , EYEd = Expected.Years.of.Education
         , MeYEd = Mean.Years.of.Education
         , GNIpc = Gross.National.Income..GNI..per.Capita
         , GNIminusHDIR = GNI.per.Capita.Rank.Minus.HDI.Rank)

colnames(hd.renamed)


gii.renamed <- gii %>%
  rename(GII = Gender.Inequality.Index..GII.
         , MMR = Maternal.Mortality.Ratio
         , Ado.Birth = Adolescent.Birth.Rate
         , Parli.F = Percent.Representation.in.Parliament
         , Edu2.F = Population.with.Secondary.Education..Female.
         , Edu2.M = Population.with.Secondary.Education..Male.
         , Labo.F = Labour.Force.Participation.Rate..Female.
         , Labo.M = Labour.Force.Participation.Rate..Male.)

colnames(gii.renamed)

gii.renamed.with.ratios <- gii.renamed %>%
  mutate(Edu2.Ratio = Edu2.F/Edu2.M,
         Labo.Ratio = Labo.F/Labo.M)

colnames(gii.renamed.with.ratios)


human <- inner_join(hd.renamed, gii.renamed.with.ratios)


dim(human)
write.csv(human)
save(human,file = "human.Rdata")









#ASSIGNMENT 5: DATA WRANGLING


str(human)
dim(human)

#The data contains 195 observations over 19 different variables.

library(stringr)

str(human$GNIpc)


library(dplyr)

#Selecting our columns of interest and creating the reuslting data frame.

keep <- c("Country", "Edu2.Ratio", "Labo.Ratio", "LEBirth", "EYEd", "GNIpc", "MMR", "Ado.Birth", "Parli.F")


human <- select(human, one_of(keep))

include <- complete.cases(human)


data.frame(human[-1], comp = include)

human_ <- filter(human, include)

rownames(human_) <- human_$Country

human_ <- select(human_, -1)


#removing the regions and keeping only country data

tail(human_, n = 10)
last <- nrow(human_) - 7

human_ <- human_[1:last, ]

#Checking on the new data dimensions and structure
human_
human_$GNIpc <- gsub(",", "", human_$GNIpc) %>% as.numeric
str(human_)

#The filtered dataframe now has 155 observations across 8 variables.


#ANALYSIS
library(corrplot)
library(GGally)
#Show a graphical overview of the data and show summaries of the variables in the data. Describe and interpret the outputs, commenting on the distributions of the variables and the relationships between them.

ggpairs(human_)
cor(human_)

summary(human_)

#######From the correlation table, we can see several interesting dynamics between variables, such as a strong positive interrelation between expected years of schooling and Life Expectancy at Birth, or a clear positive correlation between Maternal Mortality Ratio and Adolescent Birth rate.


#Perform principal component analysis (PCA) on the raw (non-standardized) human data. Show the variability captured by the principal components. Draw a biplot displaying the observations by the first two principal components (PC1 coordinate in x-axis, PC2 coordinate in y-axis), along with arrows representing the original variables

pca_human_ <- prcomp(human_)
biplot(pca_human_, choices = 1:2, cex = c(0.9, 1), col = c("grey40", "deeppink2"))

#Standardize the variables in the human data and repeat the above analysis. Interpret the results of both analysis (with and without standardizing). 

human_std <- scale(human_)
human_std <- as.data.frame(human_std)

pca_human_std <- prcomp(human_std)
biplot(pca_human_std, choices = 1:2, cex = c(0.9, 1), col = c("grey40", "deeppink2"))

# The angle between the arrows can be interpreted as the correlation between the variables.
#The angle between a variable and a PC axis can be interpreted as the correlation between the two.
#The length of the arrows are proportional to the standard deviations of the variables.


##The results of the standardized and non standardized procedures are already quite distinct at first glance, with the countries being distributed much more homogeneously. Without standardizing, the GNI variable becomes the 











