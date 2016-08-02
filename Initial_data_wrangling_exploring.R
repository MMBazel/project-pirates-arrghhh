#Last updated 4pm at 2/25/2016


setwd("C:\\Users\\mbazeley\\Desktop\\Sec_512_Commentary\\New_Data_Export")


library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)


data2013<-read.csv("data2013.csv")
data2014<-read.csv("data2014.csv")
data2015<-read.csv("data2015.csv")


#Subsetting the relevant dataframes
data2013<-data2013[,1:4]
data2014<-data2014[,1:4]
data2015<-data2015[,1:4]


#Manual method of coding sets based on year to keep track
data2013$year<-'2013'
data2014$year<-'2014'
data2015$year<-'2015'


#Renaming columns
colnames(data2013)<-c("company_id","group_title","site","count","year")
colnames(data2014)<-c("company_id","group_title","site","count","year")
colnames(data2015)<-c("company_id","group_title","site","count","year")


#Setting names == to each other allows us to check that columns match for when we append the data frames to each other
#rbind() is what we use for appending
names(data2013)==names(data2014)
allTimeData<-rbind(data2013,data2014)

names(allTimeData)==names(data2015)
allTimeData<-rbind(allTimeData,data2015)

#Idally the number of records should equal the sum of all three data sets
print(nrow(data2013)+nrow(data2014)+nrow(data2015))
print(nrow(allTimeData))

allTimeData$company_id<-as.factor(allTimeData$company_id)

#---------------------------------------------------------------------------------------------------------
#This gives the total infringements per year
TotalInfrgsYear <- allTimeData %>%
  group_by(year) %>%
  summarize(n=n())

TotalInfrgsYear


#-----------------------------------------------------------------------------------------------------
#This gives the total inringements per company per year
TotalInfrgs.Company.Year <-allTimeData %>%
  group_by(company_id,year) %>%
  summarise(n=n())

View(TotalInfrgs.Company.Year)


#Creating a wide version
TotalInfrgs.Company.Year.wide<-spread(TotalInfrgs.Company.Year,year,n)
View(TotalInfrgs.Company.Year.wide)

#let's calculate year over year change from 2013 to 2014, 2014-2015
#add columns
TotalInfrgs.Company.Year.wide$YYchg14<-NA
TotalInfrgs.Company.Year.wide$YYchg15<-NA

colnames(TotalInfrgs.Company.Year.wide)<-c("company_id","y2013","y2014","y2015","YYchg14","YYchg15")

#Calculate YY 
TotalInfrgs.Company.Year.wide$YYchg14<-((TotalInfrgs.Company.Year.wide$y2014-TotalInfrgs.Company.Year.wide$y2013)/(TotalInfrgs.Company.Year.wide$y2013)*100)

TotalInfrgs.Company.Year.wide$YYchg15<-((TotalInfrgs.Company.Year.wide$y2015-TotalInfrgs.Company.Year.wide$y2014)/(TotalInfrgs.Company.Year.wide$y2014)*100)



View(TotalInfrgs.Company.Year.wide)

#Comments: In order to create a box-and-whisker plot of the statistics, I need to plot year (2013, 2014, 2015) against infringements, where each point represents a company's infringements for that year
#Comments (continued): I have a couple data.frames
#Comments: Data frame 1 ==> allTimeData is the long version of all three years combined with not filtering
#Comments: Data frame 3 ==> TotalInfrgs.Company.Year is the long form (not filtered) with Company ID | year | n
#Comments: Data frame 3 ==> TotalInfrgs.Company.Year.wide >>>>>> coID, y2013, y2014, y2015, yychg14, yychg15
#Comments: Data frame 2 ==> historic_Year_Comp {Only those companies we have data on all 3 years}
#Comments: Data rame 4 ==> TotalInfrgs.Company.Year.wide is the wide form
#Data frame 5 ==> 
#----------------------------------------------------------------------
historic_Year_Comp<-TotalInfrgs.Company.Year.wide %>%
  filter(!is.na(y2015)) %>%
  filter(!is.na(y2014)) %>%
  filter(!is.na(y2013))

View(historic_Year_Comp)

summary(historic_Year_Comp$YYchg14)
summary(historic_Year_Comp$YYchg15)


#-------------------------------------------------------------------
# Box Plot

#Ignore OUtliers: http://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot
#Plotting distributions
oldGraph=ggplot(data=TotalInfrgs.Company.Year,aes(y=n,x=year))+
  geom_boxplot()
               
ylim1=boxplot.stats(TotalInfrgs.Company.Year$n)$stats[c(1,5)]

newGraph = oldGraph+coord_cartesian(ylim=ylim1*1.05)

newGraph

#Boxplot is good but need to go back and limit the companies to those we have data on
#Action item: subset data set so that it's only the companies we have 3 years of data on

#--------------------------------------------
#Gather
#Resource 1: http://blog.rstudio.org/2014/07/22/introducing-tidyr/
#Resource 2: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

historic_Year_Comp.gathered<-historic_Year_Comp %>%
  select(1:4) 

historic_Year_Comp.gathered<-historic_Year_Comp.gathered %>%
  gather(year, n,y2013:y2015)

#Boxlot operations
oldGraph_actual=ggplot(data=historic_Year_Comp.gathered,aes(y=n,x=year))+
  geom_boxplot()
oldGraph_actual

ylim1=boxplot.stats(historic_Year_Comp.gathered$n)$stats[c(1,5)]

newGraph_actual = oldGraph_actual+coord_cartesian(ylim=ylim1*1.05)

newGraph_actual

