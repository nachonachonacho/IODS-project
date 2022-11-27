
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
