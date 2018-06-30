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
###--------------------------------###

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

View(TN_Tract_Population)

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

View(TN_County_Population_2011)

##--2012--##
TN_County_Population_2012 = read.csv("Data/TN_counties/PEP_2012_PEPAGESEX_with_ann.csv")

View(TN_County_Population_2012)

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

View(TN_County_Population)

##---------Quality_Data---------##

Quality_Score = read.csv("Data/Quality_Measure_Data/Star_Ratings.csv")

Quality_Score <- Quality_Score[c(1,2,3,4)]

Quality_Score_TN <- subset(Quality_Score, Provider.State == "TN")

rownames(Quality_Score_TN) <- seq(length=nrow(Quality_Score_TN))

names(Quality_Score_TN) <- c("Provider_ID","Facility_Name","State","Quality_Rating")

View(Quality_Score_TN)

SNF_2013_TN <- merge(SNF_2013_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

SNF_2014_TN <- merge(SNF_2014_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

SNF_2015_TN <- merge(SNF_2015_TN, Quality_Score_TN, by = "Provider_ID", sort = TRUE)

##---------Census Tract Shape File---------##
TN = readOGR("Data/TN_County_Shp/TN_counties.shp")
TN_Tracts = readOGR("Data/cb_2017_47_tract_500k.shp")

TN_Tract_centroids <- as.data.frame(getSpPPolygonsLabptSlots(TN_Tracts))

names(TN_Tract_centroids) <- c("Long", "Lat")

#View(TN_Tract_centroids$Long)


##-------Plotting Tools------##

ggtract<-fortify(TN_Tracts, region = "GEOID")

ggtract<-merge(ggtract, TN_Tract_Population, by.x=c("id"), by.y=c(2), all.x=TRUE) 
#View(ggtract)

polyFunc<-function(groupname, dat){
  poly<-filter(dat, id==groupname) %>% 
    dplyr::select(long, lat)
  return(Polygons(list(Polygon(poly)), groupname))
}

View(tracts)
tracts <- unique(ggtract[c("id","Tract_Population_Density_2013")])
tractname <- tracts$id
polygons<-lapply(tractname, function(x) polyFunc(x, dat=ggtract)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=tractname, tracts))
df.polygon <- df.polygon[order(df.polygon$Tract_Population_Density),]

pal <- colorNumeric(
  palette = "YlGnBu", ##try viridis
  domain = df.polygon$Tract_Population_Density)


##-------Leaflet Maps of TN-------##
SNF_Test <- SNF_2013_TN[,c("Long", "Lat")] %>% drop_na()

km <- kmeans(SNF_Test, 95)

leaflet(SNF_2013_TN) %>% 
  addTiles() %>%
  addPolygons(data=df.polygon, fillColor = ~pal(Tract_Population_Density_2013),color = "#b2aeae",fillOpacity = 1, 
              weight = 0.3, 
              smoothFactor = 0.2) %>%
  addPolygons(data=TN,weight=0.5,col = 'black', fillColor = "Transparent") %>%
  addCircles(lng = SNF_2013_TN$Long,lat = SNF_2013_TN$Lat, color = "red", 
                   popup = paste("<b>","Facility:","</b>", "<i>",SNF_2013_TN$Facility_Name.x,"</i>", "<br>",
                                 "<b>","Quality Rating:","</b>", SNF_2013_TN$Quality_Rating, "<br>")) %>%
  addCircles(lng = km$centers[,c(1)], lat = km$centers[,c(2)], color = "blue") %>%
  addCircles(lng = TN_Tract_centroids$Long, lat = TN_Tract_centroids$Lat, color = "transparent")
  addLegend(pal = pal, 
            values = df.polygon$Tract_Population_Density_2013, 
            position = "bottomright", 
            title = "Population Density",
            labFormat = labelFormat(suffix = "%")) 
                     
leaflet(SNF_2014_TN) %>% 
  addTiles() %>%
  addPolygons(data=TN_Tracts,weight=1,col = 'grey') %>%
  addPolygons(data=TN,weight=2,col = 'grey') %>%
  addCircles(lng = SNF_2014_TN$Long,lat = SNF_2014_TN$Lat, color = "red",
             popup = paste("<b>","Facility:","</b>", "<i>",SNF_2014_TN$Facility_Name.x,"</i>", "<br>",
                           "<b>","Quality Rating:","</b>", SNF_2014_TN$Quality_Rating, "<br>")) 

leaflet(SNF_2015_TN) %>% 
  addTiles() %>%
  addPolygons(data=TN_Tracts,weight=1,col = 'grey') %>%
  addPolygons(data=TN,weight=2,col = 'grey') %>%
  addCircles(lng = SNF_2015_TN$Long,lat = SNF_2015_TN$Lat, color = "red",
             popup = paste("<b>","Facility:","</b>", "<i>",SNF_2015_TN$Facility_Name.x,"</i>", "<br>",
                           "<b>","Quality Rating:","</b>", SNF_2015_TN$Quality_Rating, "<br>")) 


#View(SNF_2013_TN)
#View(SNF_2014_TN)
#View(SNF_2015_TN)

##--Highcharter Maps--##

##--Tract Level 

View(TN_Tract_Test)

TN_Tract_Test <- subset(TN_Tract_Population, ID2 == "47001020100")
TN_Tract_Test <- as.data.frame(t(TN_Tract_Test[c("Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016")]))

TN_Tract_Test$Year <- c("2011", "2012", "2013", "2014", "2015", "2016")

names(TN_Tract_Test) <- c("Population", "Year")

rownames(TN_Tract_Test) <- seq(length=nrow(TN_Tract_Test)) 

hc <- highchart() %>% 
  hc_title(text = "Anderson County: Census Tract 201 Population", align="center") %>% 
  hc_xAxis(categories = TN_Tract_Test$Year) %>% 
  hc_add_series(name="Population", data = TN_Tract_Test$Population) %>%
  hc_add_theme(hc_theme_538())

hc

##--County Level
TN_County_Test <- subset(TN_County_Population, ID2 == "47001")
TN_County_Test <- as.data.frame(t(TN_County_Test[c("Target_Demographic_Population_2011", "Target_Demographic_Population_2012", "Target_Demographic_Population_2013", "Target_Demographic_Population_2014", "Target_Demographic_Population_2015", "Target_Demographic_Population_2016", "Target_Demographic_Population_2017")]))

TN_County_Test$Year <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017")

names(TN_County_Test) <- c("Population", "Year")

rownames(TN_County_Test) <- seq(length=nrow(TN_County_Test)) 

hc <- highchart() %>% 
  hc_title(text = "County Population", align="center") %>% 
  hc_xAxis(categories = TN_County_Test$Year) %>% 
  hc_add_series(name="Population", data = TN_County_Test$Population) %>%
  hc_add_theme(hc_theme_538())

hc

View(TN_County_Test)

##--Sandbox--##

plot(SNF_2013_TN$Long,SNF_2013_TN$Lat)

SNF_Test <- SNF_2013_TN[,c("Long", "Lat")] %>% drop_na()

str(SNF_Test)

is.numeric(SNF_Test$Lat)

View(SNF_Test)

set.seed(101)
km <- kmeans(SNF_Test, 10)
plot(SNF_Test$Long, SNF_Test$Lat, col=km$cluster)
points(km$centers[,c(1,2)], col=1:3, pch=1, cex=2)

View(km$centers)

plot(hybrid$longitude, hybrid$latitude, col = two$cluster,
     main = "Hybrid: Two-cluster kiosks in three-cluster locations", pch = hybrid$hybrid_shape, cex = 1.1,
     xlab = "Longitude",  ylab = "Latitude", asp = 1)

View(iris[,-5])

cclust(iris[,-5], k=3, save.data=TRUE,weights =c(1,0.5,1,0.1),method="hardcl")

x <- forecast(TN_County_Test$Population, h = 5, level = 99)

hchart(x)


x <- arima(TN_County_Test$Population, order=c(1,1,2))
#print(x)

#View(TN_County_Test$Population)

y <- as.data.frame(predict(x, n.ahead=5))

View(y)


