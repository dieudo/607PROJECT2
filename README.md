607PROJECT2
===========
# This Data is from the Minnesota Breast Cancer Family Study. This contains extended 
#pedigrees from 426 families, each identified by a single proband in 1945-52, with 
#follow up for incident breast cancer.
#there are men and women in the data even the title infer only women(breast)
# for men instead of breast cancer ,there is prostate cancer
#you can find this data in the package (kinship2)
#I will try in the following codes belows to get useful informations about this data
# by applying what i have learn so far in this course "607"
install.packages("kinship2")
require(kingship2)
data(minnbreast)
View(minnbreast)
install.packages("dplyr")
library(dplyr)
library(plyr)
sdata<-tbl_df(minnbreast)
#this allow me to see dataframe locally
sdata
#selecting 2 columns
Educated <-select(minnbreast,education,yob)
sEducated<-tbl_df(Educated)
sEducated
#adding a column for death year#
#from a 15 variables(columns ,I get 16 variables)
newminnbreast<-mutate(minnbreast,yod=(yob+endage))
#getting the population of people with age less than 50
AgeLess50<-subset(minnbreast,endage<50,na.rm=True)
#i got 2915 obs,this just the population of the study less than 50
sAgeless50<-tbl_df(AgeLess50)
sAgeless50
#the number of people less than 50 age dead with cancer is : ## cancer==1 :
DeadCancerLess50<-subset(minnbreast,endage<50 & cancer==1,na.rm=True)
#number of people dead with cancer with age less than 50
#i got 372 obs this represent men and women,men(prostate cancer,women breast)
# to find the number of female actually dead with cancer at age less than 50 ,we do:
FemaleCancerless50<-subset(minnbreast,endage<50 & cancer==1 & sex=="F")
# I found 361 obs 
# is there a relationship between education and life,we compute and add a new col to the data
EdperLife<-transform(minnbreast,Educatio_age=education/endage)
head(EdperLife)
EdperLifeValue<-select(EdperLife,Educatio_age)
EdperLifeValue
#some may argue that the number of years in school is related to life expectency
plot(EdperLifeValue,main="Ratio of years of education to years lived ")
genderGroup<-group_by(minnbreast,sex)
#average age of female dying with cancer
AvFemaleCancer<-subset(minnbreast,sex=="F" & cancer==1,na.rm=True)
FemaleAgewithCancer<-mean(AvFemaleCancer$endage)
#I found the average age of female with cancer at death to be 58.3674
#average age of male dying with cancer is below
AvMaleCancer<-subset(minnbreast,sex=="M"& cancer==1,na.rm=True)
AvMaleCancer
summary(AvMaleCancer)
#I get 69.33 average age of male at death with cancer.

#the average education level of female dying with cancer is 3.499 year based on this summary
summary(AvFemaleCancer)
# the average number of breast affected on a female dying with cancer is 1
Female<-select(minnbreast,sex=="F")
plot(Female$endage~Female$education)
plot(Female$endage,Female$education,main="females with education",xlab="Female age",ylab="education",pch=15)
#from this picture I can see that there is no relationship between education and females
Male<-select(minnbreast,sex=="M")%>%
  filter(!is.na(education))
#this tell us the number of male which education has a value,I found 6195 cases
summary(Male)
# for those male that we know the level a education they have ,there average age is 
#63.43,there education average 3.683,and there is 6.5 cases of cancer between them

#set.seed(5689)
#qplot(Male$endage, binwidth=.5) this should work fine but 
#on my machine ploting with ggplot2 seems not to work
#ggplot(Female, aes(x=endage)) + geom_histogram(binwidth=.5, colour="black", fill="white")
# i got this error :cannot open file 'C:/Users/tt/Documents/R/win-library/3.1/proto/R/proto.rdb':
#In GeomHistogram$new : restarting interrupted promise evaluation
hist(Male$endage,freq=FALSE,main="Men ages")
hist(AvFemaleCancer$education,freq=FALSE,main="education of female with cancer")
#This project allowed me to understand how data can be analysed using R ,
# from basic R codes to packages codes many tools can be used to have 
# an understanding and a visualization of the data .
#Through what I have learned in this course I am able to manipulate 
# data  and give conclusions by using various tools





