###--------Import libraries (R)--------###
library(highcharter)
library(leaflet)
library(ggmap)
library(readxl)
library(tidyr)
library(stringr)
library(rgeos)
library(maptools)
library(rgdal)
library(cluster)
library(plyr)
library(dplyr)
library(stats)
library(GISTools)
library(flexclust)
library(tibble)
library(lubridate)
library(forecast)
library(mefa)
library(reshape2)
library(wskm)

###--------------------------------###
install.packages("DT")
setwd("C:/Users/kenne/GIT/Capstone")

##-------SNF Aggragregate Report Data (2013, 2014, 2015)-------##
SNF_Location <- read_excel('Data/Inspection_Cycle/Inspection_Cycle_Deficiencies_SNF.xlsx')

SNF_Location  <- SNF_Location[c(1,2,13)]

names(SNF_Location) <- c("Provider_ID", "Facility_Name", "Location")

#View(SNF_Report_2015)

SNF_Report_2013 <- read_excel('Data/SNF/SNF_aggregate_report_2013.xlsx', sheet = "Provider")
SNF_Report_2013  <- SNF_Report_2013[c(1,2,3,4,5,6, 7, 8)]
names(SNF_Report_2013) <- c("Provider_ID", "Facility_Name", "Address", "City", "State", "Zip_Code", "Total_Stays", "Total_Medicare_Beneficiaries")
SNF_Report_2013$Percent_Medicare_Stays <- (as.numeric(SNF_Report_2013$Total_Medicare_Beneficiaries)/as.numeric(SNF_Report_2013$Total_Stays))

SNF_Report_2014 <- read_excel('Data/SNF/SNF_aggregate_report_2014.xlsx', sheet = "Provider")
SNF_Report_2014  <- SNF_Report_2014[c(1,2,3,4,5,6, 7, 8)]
names(SNF_Report_2014) <- c("Provider_ID", "Facility_Name", "Address", "City", "State", "Zip_Code", "Total_Stays", "Total_Medicare_Beneficiaries")
SNF_Report_2014$Percent_Medicare_Stays <- (as.numeric(SNF_Report_2014$Total_Medicare_Beneficiaries)/as.numeric(SNF_Report_2014$Total_Stays))

SNF_Report_2015 <- read_excel('Data/SNF/SNF_aggregate_report_2015.xlsx', sheet = "SNF_2015")
SNF_Report_2015  <- SNF_Report_2015[c(1,2,3,4,5,6, 7, 8)]
names(SNF_Report_2015) <- c("Provider_ID", "Facility_Name", "Address", "City", "State", "Zip_Code", "Total_Stays", "Total_Medicare_Beneficiaries")
SNF_Report_2015$Percent_Medicare_Stays <- (as.numeric(SNF_Report_2015$Total_Medicare_Beneficiaries)/as.numeric(SNF_Report_2015$Total_Stays))


SNF_Report_2013 <- merge(SNF_Report_2013,SNF_Location,by="Provider_ID")
SNF_Report_2014 <- merge(SNF_Report_2014,SNF_Location,by="Provider_ID")
SNF_Report_2015 <- merge(SNF_Report_2015,SNF_Location,by="Provider_ID")


SNF_2013_TN <- subset(SNF_Report_2013, State == "TN")
SNF_2014_TN <- subset(SNF_Report_2014, State == "TN")
SNF_2015_TN <- subset(SNF_Report_2015, State == "TN")

SNF_2013_TN <- unique(SNF_2013_TN)
SNF_2014_TN <- unique(SNF_2014_TN)
SNF_2015_TN <- unique(SNF_2015_TN)

SNF_2013_TN <- separate(SNF_2013_TN, Location, into = c("Lat", "Long"), sep = ",")
SNF_2013_TN$Lat <- as.numeric(gsub("\\(", "", SNF_2013_TN$Lat))
SNF_2013_TN$Long <- as.numeric(gsub("\\)", "", SNF_2013_TN$Long))

SNF_2014_TN <- separate(SNF_2014_TN, Location, into = c("Lat", "Long"), sep = ",")
SNF_2014_TN$Lat <- as.numeric(gsub("\\(", "", SNF_2014_TN$Lat))
SNF_2014_TN$Long <- as.numeric(gsub("\\)", "", SNF_2014_TN$Long))

SNF_2015_TN <- separate(SNF_2015_TN, Location, into = c("Lat", "Long"), sep = ",")
SNF_2015_TN$Lat <- as.numeric(gsub("\\(", "", SNF_2015_TN$Lat))
SNF_2015_TN$Long <- as.numeric(gsub("\\)", "", SNF_2015_TN$Long))

##---------Census_Tract_Data---------##

##--2010--##

TN_Tract_Population_2010 = read.csv("Data/TN_Tract_Data/ACS_10_5YR_DP05_with_ann.csv")

TN_Tract_Population_2010 <- TN_Tract_Population_2010[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC17", "HC01_VC18", "HC01_VC19")]

names(TN_Tract_Population_2010) <- c("ID1", "ID2", "Tract_Name_2010", "Tract_Population_2010", "Tract_Population_65_to_74_2010", "Tract_Population_75_to_84_2010", "Tract_Population_Over_85_2010")

TN_Tract_Population_2010 <- TN_Tract_Population_2010[-c(1), ]

rownames(TN_Tract_Population_2010) <- seq(length=nrow(TN_Tract_Population_2010)) 

TN_Tract_Population_2010$Tract_Population_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_2010))

TN_Tract_Population_2010$Tract_Population_2010 <- as.numeric(TN_Tract_Population_2010$Tract_Population_2010)

TN_Tract_Population_2010$Tract_Population_2010[which(TN_Tract_Population_2010$Tract_Population_2010 == 0)] <-  mean(TN_Tract_Population_2010$Tract_Population_2010)

TN_Tract_Population_2010$Tract_Population_65_to_74_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_65_to_74_2010))

TN_Tract_Population_2010$Tract_Population_75_to_84_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_75_to_84_2010))

TN_Tract_Population_2010$Tract_Population_Over_85_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_Over_85_2010))

TN_Tract_Population_2010$Target_Demographic_Population_2010 <- as.numeric(as.character(TN_Tract_Population_2010$Tract_Population_65_to_74_2010+TN_Tract_Population_2010$Tract_Population_75_to_84_2010+TN_Tract_Population_2010$Tract_Population_Over_85_2010))

TN_Tract_Population_2010$Tract_Population_Density_2010 <- round((TN_Tract_Population_2010$Target_Demographic_Population_2010/TN_Tract_Population_2010$Tract_Population_2010), 2)

TN_Tract_Population_2010$Tract_Population_Density_2010[which(TN_Tract_Population_2010$Tract_Population_Density_2010 > 1)] <-  1


##--2011--##

TN_Tract_Population_2011 = read.csv("Data/TN_Tract_Data/ACS_11_5YR_DP05_with_ann.csv")

TN_Tract_Population_2011 <- TN_Tract_Population_2011[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC17", "HC01_VC18", "HC01_VC19")]

names(TN_Tract_Population_2011) <- c("ID1", "ID2", "Tract_Name_2011", "Tract_Population_2011", "Tract_Population_65_to_74_2011", "Tract_Population_75_to_84_2011", "Tract_Population_Over_85_2011")

TN_Tract_Population_2011 <- TN_Tract_Population_2011[-c(1), ]

rownames(TN_Tract_Population_2011) <- seq(length=nrow(TN_Tract_Population_2011)) 

TN_Tract_Population_2011$Tract_Population_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_2011))

TN_Tract_Population_2011$Tract_Population_2011[which(TN_Tract_Population_2011$Tract_Population_2011 == 0)] <-  mean(TN_Tract_Population_2011$Tract_Population_2011)

TN_Tract_Population_2011$Tract_Population_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_2011))

TN_Tract_Population_2011$Tract_Population_65_to_74_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_65_to_74_2011))

TN_Tract_Population_2011$Tract_Population_75_to_84_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_75_to_84_2011))

TN_Tract_Population_2011$Tract_Population_Over_85_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_Over_85_2011))

TN_Tract_Population_2011$Target_Demographic_Population_2011 <- as.numeric(as.character(TN_Tract_Population_2011$Tract_Population_65_to_74_2011+TN_Tract_Population_2011$Tract_Population_75_to_84_2011+TN_Tract_Population_2011$Tract_Population_Over_85_2011))

TN_Tract_Population_2011$Tract_Population_Density_2011 <- round((TN_Tract_Population_2011$Target_Demographic_Population_2011/TN_Tract_Population_2011$Tract_Population_2011), 2)

TN_Tract_Population_2011$Tract_Population_Density_2011[which(TN_Tract_Population_2011$Tract_Population_Density_2011 > 1)] <-  1


##--2012--##

TN_Tract_Population_2012 = read.csv("Data/TN_Tract_Data/ACS_12_5YR_DP05_with_ann.csv")

TN_Tract_Population_2012 <- TN_Tract_Population_2012[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC17", "HC01_VC18", "HC01_VC19")]

names(TN_Tract_Population_2012) <- c("ID1", "ID2", "Tract_Name_2012", "Tract_Population_2012", "Tract_Population_65_to_74_2012", "Tract_Population_75_to_84_2012", "Tract_Population_Over_85_2012")

TN_Tract_Population_2012 <- TN_Tract_Population_2012[-c(1), ]

rownames(TN_Tract_Population_2012) <- seq(length=nrow(TN_Tract_Population_2012)) 

TN_Tract_Population_2012$Tract_Population_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_2012))

TN_Tract_Population_2012$Tract_Population_2012[which(TN_Tract_Population_2012$Tract_Population_2012 == 0)] <-  mean(TN_Tract_Population_2012$Tract_Population_2012)

TN_Tract_Population_2012$Tract_Population_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_2012))

TN_Tract_Population_2012$Tract_Population_65_to_74_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_65_to_74_2012))

TN_Tract_Population_2012$Tract_Population_75_to_84_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_75_to_84_2012))

TN_Tract_Population_2012$Tract_Population_Over_85_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_Over_85_2012))

TN_Tract_Population_2012$Target_Demographic_Population_2012 <- as.numeric(as.character(TN_Tract_Population_2012$Tract_Population_65_to_74_2012+TN_Tract_Population_2012$Tract_Population_75_to_84_2012+TN_Tract_Population_2012$Tract_Population_Over_85_2012))

TN_Tract_Population_2012$Tract_Population_Density_2012 <- round((TN_Tract_Population_2012$Target_Demographic_Population_2012/TN_Tract_Population_2012$Tract_Population_2012), 2)

TN_Tract_Population_2012$Tract_Population_Density_2012[which(TN_Tract_Population_2012$Tract_Population_Density_2012 > 1)] <-  1


##--2013--##

TN_Tract_Population_2013 = read.csv("Data/TN_Tract_Data/ACS_13_5YR_DP05_with_ann.csv")

TN_Tract_Population_2013 <- TN_Tract_Population_2013[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2013) <- c("ID1", "ID2", "Tract_Name_2013", "Tract_Population_2013", "Tract_Population_65_to_74_2013", "Tract_Population_75_to_84_2013", "Tract_Population_Over_85_2013")

TN_Tract_Population_2013 <- TN_Tract_Population_2013[-c(1), ]

rownames(TN_Tract_Population_2013) <- seq(length=nrow(TN_Tract_Population_2013)) 

TN_Tract_Population_2013$Tract_Population_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_2013))

TN_Tract_Population_2013$Tract_Population_2013[which(TN_Tract_Population_2013$Tract_Population_2013 == 0)] <-  mean(TN_Tract_Population_2013$Tract_Population_2013)

TN_Tract_Population_2013$Tract_Population_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_2013))

TN_Tract_Population_2013$Tract_Population_65_to_74_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_65_to_74_2013))

TN_Tract_Population_2013$Tract_Population_75_to_84_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_75_to_84_2013))

TN_Tract_Population_2013$Tract_Population_Over_85_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_Over_85_2013))

TN_Tract_Population_2013$Target_Demographic_Population_2013 <- as.numeric(as.character(TN_Tract_Population_2013$Tract_Population_65_to_74_2013+TN_Tract_Population_2013$Tract_Population_75_to_84_2013+TN_Tract_Population_2013$Tract_Population_Over_85_2013))

TN_Tract_Population_2013$Tract_Population_Density_2013 <- round((TN_Tract_Population_2013$Target_Demographic_Population_2013/TN_Tract_Population_2013$Tract_Population_2013), 2)

TN_Tract_Population_2013$Tract_Population_Density_2013[which(TN_Tract_Population_2013$Tract_Population_Density_2013 > 1)] <-  1


##--2014--##

TN_Tract_Population_2014 = read.csv("Data/TN_Tract_Data/ACS_14_5YR_DP05_with_ann.csv")

TN_Tract_Population_2014 <- TN_Tract_Population_2014[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2014) <- c("ID1", "ID2", "Tract_Name_2014", "Tract_Population_2014", "Tract_Population_65_to_74_2014", "Tract_Population_75_to_84_2014", "Tract_Population_Over_85_2014")

TN_Tract_Population_2014 <- TN_Tract_Population_2014[-c(1), ]

rownames(TN_Tract_Population_2014) <- seq(length=nrow(TN_Tract_Population_2014)) 

TN_Tract_Population_2014$Tract_Population_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_2014))

TN_Tract_Population_2014$Tract_Population_2014[which(TN_Tract_Population_2014$Tract_Population_2014 == 0)] <-  mean(TN_Tract_Population_2014$Tract_Population_2014)

TN_Tract_Population_2014$Tract_Population_2014 <- as.numeric(Tas.character(N_Tract_Population_2014$Tract_Population_2014))

TN_Tract_Population_2014$Tract_Population_65_to_74_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_65_to_74_2014))

TN_Tract_Population_2014$Tract_Population_75_to_84_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_75_to_84_2014))

TN_Tract_Population_2014$Tract_Population_Over_85_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_Over_85_2014))

TN_Tract_Population_2014$Target_Demographic_Population_2014 <- as.numeric(as.character(TN_Tract_Population_2014$Tract_Population_65_to_74_2014+TN_Tract_Population_2014$Tract_Population_75_to_84_2014+TN_Tract_Population_2014$Tract_Population_Over_85_2014))

TN_Tract_Population_2014$Tract_Population_Density_2014 <- round((TN_Tract_Population_2014$Target_Demographic_Population_2014/TN_Tract_Population_2014$Tract_Population_2014), 2)

TN_Tract_Population_2014$Tract_Population_Density_2014[which(TN_Tract_Population_2014$Tract_Population_Density_2014 > 1)] <-  1

#View(TN_Tract_Population_2014)

##--2015--##

TN_Tract_Population_2015 = read.csv("Data/TN_Tract_Data/ACS_15_5YR_DP05_with_ann.csv")

TN_Tract_Population_2015 <- TN_Tract_Population_2015[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2015) <- c("ID1", "ID2", "Tract_Name_2015", "Tract_Population_2015", "Tract_Population_65_to_74_2015", "Tract_Population_75_to_84_2015", "Tract_Population_Over_85_2015")

TN_Tract_Population_2015 <- TN_Tract_Population_2015[-c(1), ]

rownames(TN_Tract_Population_2015) <- seq(length=nrow(TN_Tract_Population_2015)) 

TN_Tract_Population_2015$Tract_Population_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_2015))

TN_Tract_Population_2015$Tract_Population_2015[which(TN_Tract_Population_2015$Tract_Population_2015 == 0)] <-  mean(TN_Tract_Population_2015$Tract_Population_2015)

TN_Tract_Population_2015$Tract_Population_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_2015))

TN_Tract_Population_2015$Tract_Population_65_to_74_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_65_to_74_2015))

TN_Tract_Population_2015$Tract_Population_75_to_84_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_75_to_84_2015))

TN_Tract_Population_2015$Tract_Population_Over_85_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_Over_85_2015))

TN_Tract_Population_2015$Target_Demographic_Population_2015 <- as.numeric(as.character(TN_Tract_Population_2015$Tract_Population_65_to_74_2015+TN_Tract_Population_2015$Tract_Population_75_to_84_2015+TN_Tract_Population_2015$Tract_Population_Over_85_2015))

TN_Tract_Population_2015$Tract_Population_Density_2015 <- round((TN_Tract_Population_2015$Target_Demographic_Population_2015/TN_Tract_Population_2015$Tract_Population_2015), 2)

TN_Tract_Population_2015$Tract_Population_Density_2015[which(TN_Tract_Population_2015$Tract_Population_Density_2015 > 1)] <-  1

#View(TN_Tract_Population_2015)

##--2016--#
TN_Tract_Population_2016 = read.csv("Data/TN_Tract_Data/ACS_16_5YR_DP05_with_ann.csv")

TN_Tract_Population_2016 <- TN_Tract_Population_2016[,c("GEO.id","GEO.id2","GEO.display.label","HC01_VC03","HC01_VC18", "HC01_VC19", "HC01_VC20")]

names(TN_Tract_Population_2016) <- c("ID1", "ID2", "Tract_Name_2016", "Tract_Population_2016", "Tract_Population_65_to_74_2016", "Tract_Population_75_to_84_2016", "Tract_Population_Over_85_2016")

TN_Tract_Population_2016 <- TN_Tract_Population_2016[-c(1, 2), ]

rownames(TN_Tract_Population_2016) <- seq(length=nrow(TN_Tract_Population_2016)) 

TN_Tract_Population_2016$Tract_Population_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_2016))

TN_Tract_Population_2016$Tract_Population_2016[which(TN_Tract_Population_2016$Tract_Population_2016 == 0)] <-  mean(TN_Tract_Population_2016$Tract_Population_2016)

TN_Tract_Population_2016$Tract_Population_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_2016))

TN_Tract_Population_2016$Tract_Population_65_to_74_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_65_to_74_2016))

TN_Tract_Population_2016$Tract_Population_75_to_84_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_75_to_84_2016))

TN_Tract_Population_2016$Tract_Population_Over_85_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_Over_85_2016))

TN_Tract_Population_2016$Target_Demographic_Population_2016 <- as.numeric(as.character(TN_Tract_Population_2016$Tract_Population_65_to_74_2016+TN_Tract_Population_2016$Tract_Population_75_to_84_2016+TN_Tract_Population_2016$Tract_Population_Over_85_2016))

TN_Tract_Population_2016$Tract_Population_Density_2016 <- round((TN_Tract_Population_2016$Target_Demographic_Population_2016/TN_Tract_Population_2016$Tract_Population_2016), 2)

TN_Tract_Population_2016$Tract_Population_Density_2016[which(TN_Tract_Population_2016$Tract_Population_Density_2016 > 1)] <-  1

#View(TN_Tract_Population_2016)

##--Total--##
TN_Tract_Population <- cbind(TN_Tract_Population_2011, TN_Tract_Population_2012, TN_Tract_Population_2013, TN_Tract_Population_2014, TN_Tract_Population_2015, TN_Tract_Population_2016)

#View(TN_Tract_Population)

##--Tract_Target_Demographic_Population_Predictions
Tract_Demographic_Predictions <- data.frame(
  Target_Demographic_Population_2017=numeric(),
  Target_Demographic_Population_2018=numeric(),
  Target_Demographic_Population_2019 = numeric(),
  Target_Demographic_Population_2020=numeric(),
  Target_Demographic_Population_2021=numeric())

Median_2011 <- median(TN_Tract_Population$Target_Demographic_Population_2011)

Median_2012 <- median(TN_Tract_Population$Target_Demographic_Population_2012)

Median_2013 <- median(TN_Tract_Population$Target_Demographic_Population_2013)

Median_2014 <- median(TN_Tract_Population$Target_Demographic_Population_2014)

Median_2015 <- median(TN_Tract_Population$Target_Demographic_Population_2015)

Median_2016 <- median(TN_Tract_Population$Target_Demographic_Population_2016)

optim.control = list(maxit = 2000) 

for(i in unique(TN_Tract_Population$ID2[1:1497])) 
{
  TN_Tract_Selection <- subset(TN_Tract_Population, ID2 == i)
  
  
  if (TN_Tract_Selection$Target_Demographic_Population_2011 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2012 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2013 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2014 == 0 &  
      TN_Tract_Selection$Target_Demographic_Population_2015 == 0 &
      TN_Tract_Selection$Target_Demographic_Population_2016 == 0)  
    
  {
    TN_Tract_Selection$Target_Demographic_Population_2011 <-  Median_2011
    
    TN_Tract_Selection$Target_Demographic_Population_2012 <-  Median_2012
    
    TN_Tract_Selection$Target_Demographic_Population_2013 <-  Median_2013
    
    TN_Tract_Selection$Target_Demographic_Population_2014 <-  Median_2014
    
    TN_Tract_Selection$Target_Demographic_Population_2015 <-  Median_2015
    
    TN_Tract_Selection$Target_Demographic_Population_2016 <-  Median_2016
  }
  
  median(TN_Tract_Selection$Target_Demographic_Population_2016)
  
  TN_Tract_Selection <- as.data.frame(t(TN_Tract_Selection[c("Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")]))
  
  names(TN_Tract_Selection) <- c("Population")
  
  x_tract <- arima(TN_Tract_Selection$Population, order=c(0,2,2), method="ML")
  
  y_tract <- as.data.frame(predict(x_tract, n.ahead=5))
  
  names(y_tract) <- c("Population", "Standard_Error")
  
  y_tract$Population <- round(as.numeric(as.character(y_tract$Population)), 0)
  
  z_tract <- as.data.frame(t(y_tract$Population))
  
  names(z_tract) <- c("Target_Demographic_Population_2017", "Target_Demographic_Population_2018", "Target_Demographic_Population_2019","Target_Demographic_Population_2020", "Target_Demographic_Population_2021")
  
  Tract_Demographic_Predictions <- rbind(Tract_Demographic_Predictions, z_tract)
}

TN_Tract_Population <- cbind(TN_Tract_Population, Tract_Demographic_Predictions)

##--Tract_Population_Predictions
Tract_Population_Predictions <- data.frame(
  Tract_Population_2017=numeric(),
  Tract_Population_2018=numeric(),
  Tract_Population_2019 = numeric(),
  Tract_Population_2020=numeric(),
  Tract_Population_2021=numeric())

Median_2011 <- median(TN_Tract_Population$Tract_Population_2011)

Median_2012 <- median(TN_Tract_Population$Tract_Population_2012)

Median_2013 <- median(TN_Tract_Population$Tract_Population_2013)

Median_2014 <- median(TN_Tract_Population$Tract_Population_2014)

Median_2015 <- median(TN_Tract_Population$Tract_Population_2015)

Median_2016 <- median(TN_Tract_Population$Tract_Population_2016)

optim.control = list(maxit = 2000) 

for(i in unique(TN_Tract_Population$ID2[1:1497])) 
{
  TN_Tract_Selection <- subset(TN_Tract_Population, ID2 == i)
  
  
  if (TN_Tract_Selection$Tract_Population_2011 == 0 &
      TN_Tract_Selection$Tract_Population_2012 == 0 &
      TN_Tract_Selection$Tract_Population_2013 == 0 &
      TN_Tract_Selection$Tract_Population_2014 == 0 &  
      TN_Tract_Selection$Tract_Population_2015 == 0 &
      TN_Tract_Selection$Tract_Population_2016 == 0)  
    
  {
    TN_Tract_Selection$Tract_Population_2011 <-  Median_2011
    
    TN_Tract_Selection$Tract_Population_2012 <-  Median_2012
    
    TN_Tract_Selection$Tract_Population_2013 <-  Median_2013
    
    TN_Tract_Selection$Tract_Population_2014 <-  Median_2014
    
    TN_Tract_Selection$Tract_Population_2015 <-  Median_2015
    
    TN_Tract_Selection$Tract_Population_2016 <-  Median_2016
  }
  
  median(TN_Tract_Selection$Target_Demographic_Population_2016)
  
  TN_Tract_Selection <- as.data.frame(t(TN_Tract_Selection[c("Tract_Population_2011", "Tract_Population_2012", "Tract_Population_2013", "Tract_Population_2014", "Tract_Population_2015", "Tract_Population_2016")]))
  
  names(TN_Tract_Selection) <- c("Population")
  
  x_tract <- arima(TN_Tract_Selection$Population, order=c(0,2,2), method="ML")
  
  y_tract <- as.data.frame(predict(x_tract, n.ahead=5))
  
  names(y_tract) <- c("Population", "Standard_Error")
  
  y_tract$Population <- round(as.numeric(as.character(y_tract$Population)), 0)
  
  z_tract <- as.data.frame(t(y_tract$Population))
  
  names(z_tract) <- c("Tract_Population_2017", "Tract_Population_2018", "Tract_Population_2019","Tract_Population_2020", "Tract_Population_2021")
  
  Tract_Population_Predictions <- rbind(Tract_Population_Predictions, z_tract)
}

TN_Tract_Population <- cbind(TN_Tract_Population, Tract_Population_Predictions)

TN_Tract_Population$Tract_Population_Density_2017 <- round((TN_Tract_Population$Target_Demographic_Population_2017/TN_Tract_Population$Tract_Population_2017), 2)

TN_Tract_Population$Tract_Population_Density_2017[which(TN_Tract_Population$Tract_Population_Density_2017 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2018 <- round((TN_Tract_Population$Target_Demographic_Population_2018/TN_Tract_Population$Tract_Population_2018), 2)

TN_Tract_Population$Tract_Population_Density_2018[which(TN_Tract_Population$Tract_Population_Density_2018 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2019 <- round((TN_Tract_Population$Target_Demographic_Population_2019/TN_Tract_Population$Tract_Population_2019), 2)

TN_Tract_Population$Tract_Population_Density_2019[which(TN_Tract_Population$Tract_Population_Density_2019 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2020 <- round((TN_Tract_Population$Target_Demographic_Population_2020/TN_Tract_Population$Tract_Population_2020), 2)

TN_Tract_Population$Tract_Population_Density_2020[which(TN_Tract_Population$Tract_Population_Density_2020 > 1)] <-  1


TN_Tract_Population$Tract_Population_Density_2021 <- round((TN_Tract_Population$Target_Demographic_Population_2021/TN_Tract_Population$Tract_Population_2021), 2)

TN_Tract_Population$Tract_Population_Density_2021[which(TN_Tract_Population$Tract_Population_Density_2021 > 1)] <-  1

#View(TN_Tract_Population)

##---------County_Data---------##

##--2011--##
TN_County_Population_2011 = read.csv("Data/TN_counties/PEP_2011_PEPAGESEX_with_ann.csv")

#View(TN_County_Population_2011)

TN_County_Population_2011<- TN_County_Population_2011[,c("GEO.id","GEO.id2","GEO.display.label","est72011sex0_age999","est72011sex0_age65to69","est72011sex0_age70to74","est72011sex0_age75to79","est72011sex0_age80to84","est72011sex0_age85plus")]

names(TN_County_Population_2011) <- c("ID1", "ID2", "County_Name_2011", "County_Population_2011", "County_Population_65_to_69_2011", "County_Population_70_to_74_2011", "County_Population_75_to_79_2011","County_Population_80_to_84_2011","County_Population_Over_85_2011")

TN_County_Population_2011<- TN_County_Population_2011[-c(1), ]

rownames(TN_County_Population_2011) <- seq(length=nrow(TN_County_Population_2011)) 

TN_County_Population_2011$County_Population_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_2011))

TN_County_Population_2011$County_Population_2011[which(TN_County_Population_2011$County_Population_2011 == 0)] <-  mean(TN_County_Population_2011$County_Population_2011)

TN_County_Population_2011$County_Population_65_to_69_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_65_to_69_2011))

TN_County_Population_2011$County_Population_70_to_74_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_70_to_74_2011))

TN_County_Population_2011$County_Population_75_to_79_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_75_to_79_2011))

TN_County_Population_2011$County_Population_80_to_84_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_80_to_84_2011))

TN_County_Population_2011$County_Population_Over_85_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_Over_85_2011))

TN_County_Population_2011$Target_Demographic_Population_2011 <- as.numeric(as.character(TN_County_Population_2011$County_Population_65_to_69_2011+TN_County_Population_2011$County_Population_70_to_74_2011+TN_County_Population_2011$County_Population_75_to_79_2011+TN_County_Population_2011$County_Population_80_to_84_2011+TN_County_Population_2011$County_Population_Over_85_2011))

TN_County_Population_2011$County_Population_Density_2011 <- round((TN_County_Population_2011$Target_Demographic_Population_2011/TN_County_Population_2011$County_Population_2011), 2)

TN_County_Population_2011$Year_2011 <- '2011'

#View(TN_County_Population_2011)

##--2012--##
TN_County_Population_2012 = read.csv("Data/TN_counties/PEP_2012_PEPAGESEX_with_ann.csv")

#View(TN_County_Population_2012)

TN_County_Population_2012<- TN_County_Population_2012[,c("GEO.id","GEO.id2","GEO.display.label","est72012sex0_age999","est72012sex0_age65to69","est72012sex0_age70to74","est72012sex0_age75to79","est72012sex0_age80to84","est72012sex0_age85plus")]

names(TN_County_Population_2012) <- c("ID1", "ID2", "County_Name_2012", "County_Population_2012", "County_Population_65_to_69_2012", "County_Population_70_to_74_2012", "County_Population_75_to_79_2012","County_Population_80_to_84_2012","County_Population_Over_85_2012")

TN_County_Population_2012<- TN_County_Population_2012[-c(1), ]

rownames(TN_County_Population_2012) <- seq(length=nrow(TN_County_Population_2012)) 

TN_County_Population_2012$County_Population_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_2012))

TN_County_Population_2012$County_Population_2012[which(TN_County_Population_2012$County_Population_2012 == 0)] <-  mean(TN_County_Population_2012$County_Population_2012)

TN_County_Population_2012$County_Population_65_to_69_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_65_to_69_2012))

TN_County_Population_2012$County_Population_70_to_74_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_70_to_74_2012))

TN_County_Population_2012$County_Population_75_to_79_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_75_to_79_2012))

TN_County_Population_2012$County_Population_80_to_84_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_80_to_84_2012))

TN_County_Population_2012$County_Population_Over_85_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_Over_85_2012))

TN_County_Population_2012$Target_Demographic_Population_2012 <- as.numeric(as.character(TN_County_Population_2012$County_Population_65_to_69_2012+TN_County_Population_2012$County_Population_70_to_74_2012+TN_County_Population_2012$County_Population_75_to_79_2012+TN_County_Population_2012$County_Population_80_to_84_2012+TN_County_Population_2012$County_Population_Over_85_2012))

TN_County_Population_2012$County_Population_Density_2012 <- round((TN_County_Population_2012$Target_Demographic_Population_2012/TN_County_Population_2012$County_Population_2012), 2)

TN_County_Population_2012$Year_2012 <- '2012'


##--2013--##
TN_County_Population_2013 = read.csv("Data/TN_counties/PEP_2013_PEPAGESEX_with_ann.csv")

TN_County_Population_2013<- TN_County_Population_2013[,c("GEO.id","GEO.id2","GEO.display.label","est72013sex0_age999","est72013sex0_age65to69","est72013sex0_age70to74","est72013sex0_age75to79","est72013sex0_age80to84","est72013sex0_age85plus")]

names(TN_County_Population_2013) <- c("ID1", "ID2", "County_Name_2013", "County_Population_2013", "County_Population_65_to_69_2013", "County_Population_70_to_74_2013", "County_Population_75_to_79_2013","County_Population_80_to_84_2013","County_Population_Over_85_2013")

TN_County_Population_2013<- TN_County_Population_2013[-c(1), ]

rownames(TN_County_Population_2013) <- seq(length=nrow(TN_County_Population_2013)) 

TN_County_Population_2013$County_Population_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_2013))

TN_County_Population_2013$County_Population_2013[which(TN_County_Population_2013$County_Population_2013 == 0)] <-  mean(TN_County_Population_2013$County_Population_2013)

TN_County_Population_2013$County_Population_65_to_69_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_65_to_69_2013))

TN_County_Population_2013$County_Population_70_to_74_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_70_to_74_2013))

TN_County_Population_2013$County_Population_75_to_79_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_75_to_79_2013))

TN_County_Population_2013$County_Population_80_to_84_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_80_to_84_2013))

TN_County_Population_2013$County_Population_Over_85_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_Over_85_2013))

TN_County_Population_2013$Target_Demographic_Population_2013 <- as.numeric(as.character(TN_County_Population_2013$County_Population_65_to_69_2013+TN_County_Population_2013$County_Population_70_to_74_2013+TN_County_Population_2013$County_Population_75_to_79_2013+TN_County_Population_2013$County_Population_80_to_84_2013+TN_County_Population_2013$County_Population_Over_85_2013))

TN_County_Population_2013$County_Population_Density_2013 <- round((TN_County_Population_2013$Target_Demographic_Population_2013/TN_County_Population_2013$County_Population_2013), 2)

TN_County_Population_2013$Year_2013 <- '2013'

##--2014--##
TN_County_Population_2014 = read.csv("Data/TN_counties/PEP_2014_PEPAGESEX_with_ann.csv")

TN_County_Population_2014<- TN_County_Population_2014[,c("GEO.id","GEO.id2","GEO.display.label","est72014sex0_age999","est72014sex0_age65to69","est72014sex0_age70to74","est72014sex0_age75to79","est72014sex0_age80to84","est72014sex0_age85plus")]

names(TN_County_Population_2014) <- c("ID1", "ID2", "County_Name_2014", "County_Population_2014", "County_Population_65_to_69_2014", "County_Population_70_to_74_2014", "County_Population_75_to_79_2014","County_Population_80_to_84_2014","County_Population_Over_85_2014")

TN_County_Population_2014<- TN_County_Population_2014[-c(1), ]

rownames(TN_County_Population_2014) <- seq(length=nrow(TN_County_Population_2014)) 

TN_County_Population_2014$County_Population_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_2014))

TN_County_Population_2014$County_Population_2014[which(TN_County_Population_2014$County_Population_2014 == 0)] <-  mean(TN_County_Population_2014$County_Population_2014)

TN_County_Population_2014$County_Population_65_to_69_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_65_to_69_2014))

TN_County_Population_2014$County_Population_70_to_74_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_70_to_74_2014))

TN_County_Population_2014$County_Population_75_to_79_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_75_to_79_2014))

TN_County_Population_2014$County_Population_80_to_84_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_80_to_84_2014))

TN_County_Population_2014$County_Population_Over_85_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_Over_85_2014))

TN_County_Population_2014$Target_Demographic_Population_2014 <- as.numeric(as.character(TN_County_Population_2014$County_Population_65_to_69_2014+TN_County_Population_2014$County_Population_70_to_74_2014+TN_County_Population_2014$County_Population_75_to_79_2014+TN_County_Population_2014$County_Population_80_to_84_2014+TN_County_Population_2014$County_Population_Over_85_2014))

TN_County_Population_2014$County_Population_Density_2014 <- round((TN_County_Population_2014$Target_Demographic_Population_2014/TN_County_Population_2014$County_Population_2014), 2)

TN_County_Population_2014$Year_2014 <- '2014'

##--2015--##
TN_County_Population_2015 = read.csv("Data/TN_counties/PEP_2015_PEPAGESEX_with_ann.csv")

TN_County_Population_2015<- TN_County_Population_2015[,c("GEO.id","GEO.id2","GEO.display.label","est72015sex0_age999","est72015sex0_age65to69","est72015sex0_age70to74","est72015sex0_age75to79","est72015sex0_age80to84","est72015sex0_age85plus")]

names(TN_County_Population_2015) <- c("ID1", "ID2", "County_Name_2015", "County_Population_2015", "County_Population_65_to_69_2015", "County_Population_70_to_74_2015", "County_Population_75_to_79_2015","County_Population_80_to_84_2015","County_Population_Over_85_2015")

TN_County_Population_2015<- TN_County_Population_2015[-c(1), ]

rownames(TN_County_Population_2015) <- seq(length=nrow(TN_County_Population_2015)) 

TN_County_Population_2015$County_Population_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_2015))

TN_County_Population_2015$County_Population_2015[which(TN_County_Population_2015$County_Population_2015 == 0)] <-  mean(TN_County_Population_2015$County_Population_2015)

TN_County_Population_2015$County_Population_65_to_69_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_65_to_69_2015))

TN_County_Population_2015$County_Population_70_to_74_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_70_to_74_2015))

TN_County_Population_2015$County_Population_75_to_79_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_75_to_79_2015))

TN_County_Population_2015$County_Population_80_to_84_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_80_to_84_2015))

TN_County_Population_2015$County_Population_Over_85_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_Over_85_2015))

TN_County_Population_2015$Target_Demographic_Population_2015 <- as.numeric(as.character(TN_County_Population_2015$County_Population_65_to_69_2015+TN_County_Population_2015$County_Population_70_to_74_2015+TN_County_Population_2015$County_Population_75_to_79_2015+TN_County_Population_2015$County_Population_80_to_84_2015+TN_County_Population_2015$County_Population_Over_85_2015))

TN_County_Population_2015$County_Population_Density_2015 <- round((TN_County_Population_2015$Target_Demographic_Population_2015/TN_County_Population_2015$County_Population_2015), 2)

TN_County_Population_2015$Year_2015 <- '2015'

##--2016--##
TN_County_Population_2016 = read.csv("Data/TN_counties/PEP_2016_PEPAGESEX_with_ann.csv")

TN_County_Population_2016<- TN_County_Population_2016[,c("GEO.id","GEO.id2","GEO.display.label","est72016sex0_age999","est72016sex0_age65to69","est72016sex0_age70to74","est72016sex0_age75to79","est72016sex0_age80to84","est72016sex0_age85plus")]

names(TN_County_Population_2016) <- c("ID1", "ID2", "County_Name_2016", "County_Population_2016", "County_Population_65_to_69_2016", "County_Population_70_to_74_2016", "County_Population_75_to_79_2016","County_Population_80_to_84_2016","County_Population_Over_85_2016")

TN_County_Population_2016<- TN_County_Population_2016[-c(1), ]

rownames(TN_County_Population_2016) <- seq(length=nrow(TN_County_Population_2016)) 

TN_County_Population_2016$County_Population_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_2016))

TN_County_Population_2016$County_Population_2016[which(TN_County_Population_2016$County_Population_2016 == 0)] <-  mean(TN_County_Population_2016$County_Population_2016)

TN_County_Population_2016$County_Population_65_to_69_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_65_to_69_2016))

TN_County_Population_2016$County_Population_70_to_74_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_70_to_74_2016))

TN_County_Population_2016$County_Population_75_to_79_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_75_to_79_2016))

TN_County_Population_2016$County_Population_80_to_84_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_80_to_84_2016))

TN_County_Population_2016$County_Population_Over_85_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_Over_85_2016))

TN_County_Population_2016$Target_Demographic_Population_2016 <- as.numeric(as.character(TN_County_Population_2016$County_Population_65_to_69_2016+TN_County_Population_2016$County_Population_70_to_74_2016+TN_County_Population_2016$County_Population_75_to_79_2016+TN_County_Population_2016$County_Population_80_to_84_2016+TN_County_Population_2016$County_Population_Over_85_2016))

TN_County_Population_2016$County_Population_Density_2016 <- round((TN_County_Population_2016$Target_Demographic_Population_2016/TN_County_Population_2016$County_Population_2016), 2)

TN_County_Population_2016$Year_2016 <- '2016'

##--2017--##
TN_County_Population_2017 = read.csv("Data/TN_counties/PEP_2017_PEPAGESEX_with_ann.csv")

TN_County_Population_2017 <- TN_County_Population_2017[,c("GEO.id","GEO.id2","GEO.display.label","est72010sex0_age999","est72017sex0_age65to69","est72017sex0_age70to74","est72017sex0_age75to79","est72017sex0_age80to84","est72017sex0_age85plus")]

names(TN_County_Population_2017) <- c("ID1", "ID2", "County_Name_2017", "County_Population_2017", "County_Population_65_to_69_2017", "County_Population_70_to_74_2017", "County_Population_75_to_79_2017","County_Population_80_to_84_2017","County_Population_Over_85_2017")

TN_County_Population_2017 <- TN_County_Population_2017[-c(1, 2), ]

rownames(TN_County_Population_2017) <- seq(length=nrow(TN_County_Population_2017)) 

TN_County_Population_2017$County_Population_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_2017))

TN_County_Population_2017$County_Population_2017[which(TN_County_Population_2017$County_Population_2017 == 0)] <-  mean(TN_County_Population_2017$County_Population_2017)

TN_County_Population_2017$County_Population_65_to_69_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_65_to_69_2017))

TN_County_Population_2017$County_Population_70_to_74_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_70_to_74_2017))

TN_County_Population_2017$County_Population_75_to_79_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_75_to_79_2017))

TN_County_Population_2017$County_Population_80_to_84_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_80_to_84_2017))

TN_County_Population_2017$County_Population_Over_85_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_Over_85_2017))

TN_County_Population_2017$Target_Demographic_Population_2017 <- as.numeric(as.character(TN_County_Population_2017$County_Population_65_to_69_2017+TN_County_Population_2017$County_Population_70_to_74_2017+TN_County_Population_2017$County_Population_75_to_79_2017+TN_County_Population_2017$County_Population_80_to_84_2017+TN_County_Population_2017$County_Population_Over_85_2017))

TN_County_Population_2017$County_Population_Density <- round((TN_County_Population_2017$Target_Demographic_Population_2017/TN_County_Population_2017$County_Population_2017), 2)

TN_County_Population_2017$Year_2017 <- '2017'

#View(TN_County_Population_2017)

##--Total--##

TN_County_Population <- cbind(TN_County_Population_2011, TN_County_Population_2012, TN_County_Population_2013, TN_County_Population_2014, TN_County_Population_2015, TN_County_Population_2016, TN_County_Population_2017)

#View(TN_County_Population)

##--County_Target_Demographic_Population_Predictions

County_Predictions <- data.frame(
  Target_Demographic_Population_2017=numeric(),
  Target_Demographic_Population_2018=numeric(),
  Target_Demographic_Population_2019 = numeric(),
  Target_Demographic_Population_2020=numeric(),
  Target_Demographic_Population_2021=numeric())

Median_2011 <- median(TN_County_Population$Target_Demographic_Population_2011)

Median_2012 <- median(TN_County_Population$Target_Demographic_Population_2012)

Median_2013 <- median(TN_County_Population$Target_Demographic_Population_2013)

Median_2014 <- median(TN_County_Population$Target_Demographic_Population_2014)

Median_2015 <- median(TN_County_Population$Target_Demographic_Population_2015)

Median_2016 <- median(TN_County_Population$Target_Demographic_Population_2016)

optim.control = list(maxit = 2000) 

for(i in unique(TN_County_Population$ID2[1:95])) 
{
  TN_County_Selection <- subset(TN_County_Population, ID2 == i)
  
  
  if (TN_County_Selection$Target_Demographic_Population_2011 == 0 &
      TN_County_Selection$Target_Demographic_Population_2012 == 0 &
      TN_County_Selection$Target_Demographic_Population_2013 == 0 &
      TN_County_Selection$Target_Demographic_Population_2014 == 0 &  
      TN_County_Selection$Target_Demographic_Population_2015 == 0 &
      TN_County_Selection$Target_Demographic_Population_2016 == 0)  
    
  {
    TN_County_Selection$Target_Demographic_Population_2011 <-  Median_2011
    
    TN_County_Selection$Target_Demographic_Population_2012 <-  Median_2012
    
    TN_County_Selection$Target_Demographic_Population_2013 <-  Median_2013
    
    TN_County_Selection$Target_Demographic_Population_2014 <-  Median_2014
    
    TN_County_Selection$Target_Demographic_Population_2015 <-  Median_2015
    
    TN_County_Selection$Target_Demographic_Population_2016 <-  Median_2016
  }
  
  median(TN_County_Selection$Target_Demographic_Population_2016)
  
  TN_County_Selection <- as.data.frame(t(TN_County_Selection[c("Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")]))
  
  names(TN_County_Selection) <- c("Population")
  
  x_County <- arima(TN_County_Selection$Population, order=c(0,2,2), method="ML")
  
  y_County <- as.data.frame(predict(x_County, n.ahead=5))
  
  names(y_County) <- c("Population", "Standard_Error")
  
  y_County$Population <- round(as.numeric(as.character(y_County$Population)), 0)
  
  z_County <- as.data.frame(t(y_County$Population))
  
  names(z_County) <- c("Target_Demographic_Population_2017", "Target_Demographic_Population_2018", "Target_Demographic_Population_2019","Target_Demographic_Population_2020", "Target_Demographic_Population_2021")
  
  County_Predictions <- rbind(County_Predictions, z_County)
}


##---------Quality_Data---------##

Quality_Score = read.csv("Data/Quality_Measure_Data/Star_Ratings.csv")

Quality_Score <- Quality_Score[c(1,2,3,4)]

Quality_Score_TN <- subset(Quality_Score, Provider.State == "TN")

rownames(Quality_Score_TN) <- seq(length=nrow(Quality_Score_TN))

names(Quality_Score_TN) <- c("Provider_ID","Facility_Name","State","Quality_Rating")

#View(Quality_Score_TN)

SNF_2013_TN <- merge(SNF_2013_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

SNF_2014_TN <- merge(SNF_2014_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

SNF_2015_TN <- merge(SNF_2015_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

##--Quality_Subsets--##

SNF_2013_TN_Quality_Rating_1 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==1)
SNF_2013_TN_Quality_Rating_1 <- SNF_2013_TN_Quality_Rating_1 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_1) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_1)) 

SNF_2013_TN_Quality_Rating_2 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==2)
SNF_2013_TN_Quality_Rating_2 <- SNF_2013_TN_Quality_Rating_2 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_2) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_2)) 

SNF_2013_TN_Quality_Rating_3 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==3)
SNF_2013_TN_Quality_Rating_3 <- SNF_2013_TN_Quality_Rating_3 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_3) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_3)) 

SNF_2013_TN_Quality_Rating_4 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==4)
SNF_2013_TN_Quality_Rating_4 <- SNF_2013_TN_Quality_Rating_4 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_4) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_4)) 

SNF_2013_TN_Quality_Rating_5 <- subset(SNF_2013_TN[,c("Long","Lat")], SNF_2013_TN$Quality_Rating ==5)
SNF_2013_TN_Quality_Rating_5 <- SNF_2013_TN_Quality_Rating_5 %>% drop_na()
rownames(SNF_2013_TN_Quality_Rating_5) <- seq(length=nrow(SNF_2013_TN_Quality_Rating_5)) 

##--
SNF_2014_TN_Quality_Rating_1 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==1)

SNF_2014_TN_Quality_Rating_2 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==2)

SNF_2014_TN_Quality_Rating_3 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==3)

SNF_2014_TN_Quality_Rating_4 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==4)

SNF_2014_TN_Quality_Rating_5 <- subset(SNF_2014_TN[,c("Long","Lat")], SNF_2014_TN$Quality_Rating ==5)

##--
SNF_2015_TN_Quality_Rating_1 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==1)

SNF_2015_TN_Quality_Rating_2 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==2)

SNF_2015_TN_Quality_Rating_3 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==3)

SNF_2015_TN_Quality_Rating_4 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==4)

SNF_2015_TN_Quality_Rating_5 <- subset(SNF_2015_TN[,c("Long","Lat")], SNF_2015_TN$Quality_Rating ==5)


##---------Census Tract Shape File---------##
TN = readOGR("Data/TN_County_Shp/TN_counties.shp")
TN_Tracts = readOGR("Data/cb_2017_47_tract_500k.shp")

TN_Tract_Centroids <- as.data.frame(SpatialPointsDataFrame(gCentroid(TN_Tracts, byid=TRUE), 
                                                           TN_Tracts@data, match.ID=FALSE))

TN_Tract_Centroids <- TN_Tract_Centroids[, c("GEOID","x","y")]

names(TN_Tract_Centroids) <- c("id","long", "lat")

#View(TN_Tract_Centroids)

Centroid_Population_Data <- merge(TN_Tract_Centroids, TN_Tract_Population, by.x=c("id"), by.y=c(2), all.x=TRUE) 

#View(Centroid_Population_Data)

##-------Plotting Tools------##

ggtract<-fortify(TN_Tracts, region = "GEOID")

centroid <- fortify(getSpPPolygonsLabptSlots(TN_Tract), region = "GEOID")

ggtract<-merge(ggtract, TN_Tract_Population, by.x=c("id"), by.y=c(2), all.x=TRUE) 
#View(ggtract)

polyFunc<-function(groupname, dat){
  poly<-filter(dat, id==groupname) %>% 
    dplyr::select(long, lat)
  return(Polygons(list(Polygon(poly)), groupname))
}

#View(tracts)
tracts <- unique(ggtract[c("id","Tract_Population_Density_2013")])
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$Tract_Population_Density_2013),]

pal <- colorNumeric(
  palette = "YlGnBu", ##try viridis
  domain = df.polygon$Tract_Population_Density)

#View(ggtract)

#--Tract_Population_Density_Subsets--##

#hist(subset(ggtract$Tract_Population_Density_2017, ggtract$Tract_Population_Density_2021>0))

GG_Tract_2013_Low <- as.data.frame(subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2013 < 0.1))

GG_Tract_2013_Mid <- as.data.frame(subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= Centroid_Population_Data$Tract_Population_Density_2013 & Centroid_Population_Data$Tract_Population_Density_2013 <= 0.2))

GG_Tract_2013_High <- as.data.frame(subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2013 > 0.2))


GG_Tract_2014_Low <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2014 < 0.1)

GG_Tract_2014_Mid <- subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= Centroid_Population_Data$Tract_Population_Density_2014 & Centroid_Population_Data$Tract_Population_Density_2014 <= 0.2)

GG_Tract_2014_High <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2014 > 0.2)


GG_Tract_2015_Low <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2015 < 0.1)

GG_Tract_2015_Mid <- subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= Centroid_Population_Data$Tract_Population_Density_2015 & Centroid_Population_Data$Tract_Population_Density_2015 <= 0.2)

GG_Tract_2015_High <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2015 > 0.2)


GG_Tract_2016_Low <- subset(Centroid_Population_Data[,c("long", "lat")], Centroid_Population_Data$Tract_Population_Density_2016 < 0.1)

GG_Tract_2016_Mid <- subset(Centroid_Population_Data[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2016 & ggtract$Tract_Population_Density_2016 <= 0.2)

GG_Tract_2016_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2016 > 0.2)


GG_Tract_2017_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2017 < 0.1)

GG_Tract_2017_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2017 & ggtract$Tract_Population_Density_2017 <= 0.2)

GG_Tract_2017_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2017 > 0.2)


GG_Tract_2018_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2018 < 0.1)

GG_Tract_2018_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2018 & ggtract$Tract_Population_Density_2018 <= 0.2)

GG_Tract_2018_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2018 > 0.2)


GG_Tract_2019_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2019 < 0.1)

GG_Tract_2019_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2019 & ggtract$Tract_Population_Density_2019 <= 0.2)

GG_Tract_2019_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2019 > 0.2)


GG_Tract_2020_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2020 < 0.1)

GG_Tract_2020_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2020 & ggtract$Tract_Population_Density_2020 <= 0.2)

GG_Tract_2020_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2020 > 0.2)


GG_Tract_2021_Low <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2021 < 0.1)

GG_Tract_2021_Mid <- subset(ggtract[,c("long", "lat")], 0.1 <= ggtract$Tract_Population_Density_2021 & ggtract$Tract_Population_Density_2021 <= 0.2)

GG_Tract_2021_High <- subset(ggtract[,c("long", "lat")], ggtract$Tract_Population_Density_2021 > 0.2)


##---Weighted Clustering sandbox---##

cl1 <- (cclust(SNF_2013_TN_Quality_Rating_1, k=38, weights =c(3,3),method="hardcl"))
cl1 <- as.data.frame(parameters(cl1))
names(cl1) <- c("Long","Lat")
#plot(cl1)

cl2 <- (cclust(SNF_2013_TN_Quality_Rating_2, k=46, weights =c(1,1),method="hardcl"))
cl2 <- as.data.frame(parameters(cl2))
names(cl2) <- c("Long","Lat")

#plot(cl2)

cl3 <- (cclust(SNF_2013_TN_Quality_Rating_3, k=53, weights =c(0.25,0.25),method="hardcl"))
cl3 <- as.data.frame(parameters(cl3))
names(cl3) <- c("Long","Lat")

#plot(cl3)

cl4 <- (cclust(SNF_2013_TN_Quality_Rating_4, k=60, weights =c(0.20,0.20),method="hardcl"))
cl4 <- as.data.frame(parameters(cl4))
names(cl4) <- c("Long","Lat")

#plot(cl4)

cl5 <- (cclust(SNF_2013_TN_Quality_Rating_5, k=37, weights =c(0.01,0.01),method="hardcl"))
cl5 <- as.data.frame(parameters(cl5))
names(cl5) <- c("Long","Lat")

#plot(cl5)

cl6 <- (cclust(GG_Tract_2013_Low, k=95, weights =c(0.05,0.05),method="hardcl"))
cl6 <- as.data.frame(parameters(cl6))
names(cl6) <- c("Long","Lat")

#plot(cl6)

cl7 <- (cclust(GG_Tract_2013_Mid, k=95, weights =c(0.5,0.5),method="hardcl"))
cl7 <- as.data.frame(parameters(cl7))
names(cl7) <- c("Long","Lat")

#plot(cl7)

cl8 <- (cclust(GG_Tract_2013_High, k=95, weights =c(5,5),method="hardcl"))
cl8 <- as.data.frame(parameters(cl8))
names(cl8) <- c("Long","Lat")

#plot(cl8)
Test <- rbind(cl1,cl2,cl3,cl4,cl5,cl6,cl7,cl8)

km <- kmeans(Test, 95)

##---------Define body of dashboard---------##

##---------UI Definition-------##




ui <- dashboardbody(fluidRow(
  column(3
         
  ),
  column(9
         
  )
),
theme = shinytheme("darkly"),

titlePanel("Predicting Optimal Locations for Skilled Nursing Facilities in TN", 
           windowTitle = "Optimal Analysis"),
sidebarLayout(
  sidebarPanel(
    h4("Select Predictive Year"),
    checkboxInput("plot1","Make Plot 1", value=T),
    checkboxInput("plot2","Make Plot 2", value=F),
    selectInput(
      inputId ="causes",
      label="characteristics  :",
      choices = c("State"= "STATE",
                  "Gender" = "GENDER",
                  "On Highway"="NHS",
                  "Rural vs Urban"= "Land_Use",
                  "Weather Condition"="WEATHER_CONDITION",
                  "Distraction Involved"="DISTRACTED",
                  "Age Group"="AGE_GROUP",
                  "Drunk Driver"="DR_DRINK",
                  "License Status"="LICENSE_STATUS",
                  "Speed Involved"="SPEED_INVOLVED",
                  "Hit and Run"="HIT_RUN"),
      selected="GENDER"
    ),
    
    selectInput(
      inputId = "color",
      label = "Color by:",
      choices = c("State"= "STATE",
                  "Gender" = "GENDER",
                  "On Highway"="NHS",
                  "Rural vs Urban"= "Land_Use",
                  "Weather Condition"="WEATHER_CONDITION",
                  "Distraction Involved"="DISTRACTED",
                  "Age Group"="AGE_GROUP",
                  "Drunk Driver"="DR_DRINK",
                  "License Status"="LICENSE_STATUS",
                  "Speed Involved"="SPEED_INVOLVED",
                  "Hit and Run"="HIT_RUN"),
      selected="AGE_GROUP"
    ),
    selectInput(
      inputId = "time",
      label = "Time of Accident:",
      choices = c("Hour"="HOUR",
                  "Day of the Week"="DAY_WEEK",
                  "Month"="MONTH"
      ),
      selected="HOUR"
    ),
    
    br(),
    br(),
    
    #sliderInput(inputId = "alpha", 
    #label = "Alpha:", 
    # min = 0, max = 1, 
    #  value = 0.5),
    
    #sliderInput(inputId = "bins", 
    #label = "Number of bins:", 
    #min = 0, max = 24, 
    #value = 1),
    #textInput(inputId = "plot_title",
    #label = "Plot title",
    # placeholder = "Enter text to be used as plot title")
    
    selectInput(inputId = "selected_state",
                label = "Select State:",
                choices = c("Alabama", "Alaska", "American Samoa","Arizona", "Arkansas", 
                            "California", "Colorado","Connecticut","Delaware", "District of Columbia", "Florida","Georgia", "Guam", 
                            "Hawaii", "Idaho","Illinois","Indiana", "Iowa","Kansas","Kentucky", "Louisiana", "Maine","Maryland", "Massachusetts",
                            "Michigan", "Minnesota","Mississippi","Missouri", "Montanaa", "Nebraska","Nevada", "New Hampshire",
                            "New Jersey", "New Mexico","New York","North Carolina", "North Dakota","Ohio","Oklahoma", "Oregon", "Pennsylvania","Puerto Rico", "Rhode Island",
                            "South Carolina", "South Dakota","Tennessee","Texas", "Utah", "Vermont","Virginia", "Virgin Islands","Washington","West Virginia", "Wisconsin","Wyoming"),
                
                selected = "Tennessee")
  ),
  #Main panel
  mainPanel(tabsetPanel(id = "tabspanel", type = "tabs",
                        tabPanel(title = "Plots",
                                 
                                 h5("Barplot of charachterstics for US "),
                                 plotOutput(outputId = "Barplot"),
                                 br(), br()
                                 
                                 # h5("Barplot of charachterstics for States "),
                                 # plotOutput(outputId = "Barplot")
                        ),
                        
                        tabPanel(title="Accident by time",
                                 h5("Accidents Distribution by time for US"),
                                 plotOutput(outputId = "Histogram")),
                        
                        tabPanel(title = "Visualizing Tennessee",
                                 plotOutput(outputId = "GeoMapping", width ="100%")),
                        
                        tabPanel(title = "Data Source",
                                 htmlOutput(outputId = "Description"))
                        
  )
  
  )
)
)





# Define server logic 
server <- function(input, output) {
content<- "A"
    output$SNF_Provider_Map <- renderLeaflet({
    leaflet(SNF_2015_TX) %>% addPopups(SNF_2015_TX$lat, SNF_2015_TX$long, content,
                                       options = popupOptions(closeButton = FALSE)) %>%
                                         addTiles() %>% addMarkers(lat = ~long, lng = ~lat)})
    
     output$SNF_Provider_Map <- renderLeaflet({
       leaflet(SNF_2015_TX) %>% 
         addTiles() %>%
           addCircleMarkers(lng = SNF_2015_TX$lat,lat = SNF_2015_TX$long, 
                                        
                                          popup = ~Cost_Per_Day
                                         ) 
        })
      
  
    #output$PacProviderTable <- renderTable({DT::datatable(data = SNF_2015_TX,
                                                                #options = list(pageLength = 10),
                                                                #rownames = FALSE)})
    
    output$PacProviderTable <- renderDataTable(SNF_2015_TX)
}
# Run the application 
shinyApp(ui = ui, server = server)

