Libraries {
  ####  ---- Libraries ---- ####
  
  library(tidyverse)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(ggExtra)
  library(ggpubr)
  library(data.table)
  library(magrittr)
  library(mmtable2)
  library(gapminder)
  library(purrr)
  library(gt)
  library(formattable)
  library(DT)
  library(patchwork) # To display 2 charts together
  library(hrbrthemes)
  library(showtext)
  library(ggpattern)
  library(sf)
  library(broom)
  library(purrr)
  library(multcompView)
  library(ggrepel)
}



## Github Directory .zip file download
# 
# ## Set working directory to folder in computer where you want data
# setwd(dir = "~/Documents/~ Classes/ABE6933 - Data Management/Final Project")
# 
# ## Download .zip file from Github 
# download.file(url = "https://github.com/JeanPompeo/Greenhouse-Hydroponic-Saline-Mustard-Greens-DBMS/archive/refs/heads/main.zip"
#               , destfile = "Greenhouse-Hydroponic-Saline-Mustard-Greens-DBMS-main.zip")
# 
# ## Unzip file in directory, and creeate new folder as "destfile"
# unzip(zipfile = "Greenhouse-Hydroponic-Saline-Mustard-Greens-DBMS-main")
# 
# ## Set new directory as new folder with unzipped github file
# setwd(dir = "~/Documents/~ Classes/ABE6933 - Data Management/Final Project/Greenhouse-Hydroponic-Saline-Mustard-Greens-DBMS-main/Data_Collection_Files")



setwd(dir = "~//Documents/~ Classes/ABE6933 - Data Management/Final Project/Data/")


## Read .csv  Files
Plant = as_tibble(read_csv("GH Mustard Greens - Plant.csv")) 
Weather = as_tibble(read_csv("GH Mustard Greens - WeatherData.csv"))

## Create list of column names
Plant_Cols = as.character(Plant[3, ])
Weather_Cols = as.character(Weather[3, ])
  

## Rename Columns, convert data types, delete non-data entities
Plant_1 <- Plant %>%
  dplyr::rename(c("ID.Plant" = "* PLANT", "ID.Experiment" = ...2, "ID.Cultivar" = ...3, "Harvest_Date" = ...4, "Plant_Age" = ...5, 
                  "SFW" = ...6, "Width" = ...7, "Height" = ...8, "Leaf_Area" = ...9, "Leaf_Number" = ...10, "Stem_Weight" = ...11, 
                  "SDW" = ...12, "Chlorophyl" = ...13, "Stomatal_Conductivity" = ...14, "Na/K Ratio" = ...15)) %>%
  mutate(ID.Plant = as.numeric(ID.Plant), 
         ID.Experiment = as.character(ID.Experiment), 
         ID.Cultivar = as.character(ID.Cultivar), 
         Harvest_Date = as.numeric(Harvest_Date), 
         Plant_Age = as.numeric(Plant_Age), 
         SFW = as.numeric(SFW), 
         Width = as.numeric(Width), 
         Height = as.numeric(Height), 
         Leaf_Area = as.numeric(Leaf_Area), 
         Leaf_Number = as.numeric(Leaf_Number), 
         Stem_Weight = as.numeric(Stem_Weight), 
         SDW = as.numeric(SDW), 
         Chlorophyl = as.numeric(Chlorophyl), 
         Stomatal_Conductivity = as.numeric(Stomatal_Conductivity), 
         `Na/K Ratio` = as.numeric(`Na/K Ratio`))%>%
  slice(-c(1:4))

Weather_1 <- Weather %>%
  dplyr::rename(c("ID.Weather" = "* WEATHER DATA", "ID.Experiment" = ...2, "ID.WeatherSensor" = ...3, "Date" = ...4, "Time" = ...5, 
                  "Air_Temp" = ...6, "RH" = ...7, "CO2" = ...8, "PP" = ...9, "DLI" = ...10, "ePAR" = ...11, 
                  "DP" = ...12, "VPD" = ...13, "Atm" = ...14)) %>%
  mutate(Date_Time = paste(Date, Time)) %>%
  mutate(ID.Weather = as.character(ID.Weather), 
         ID.Experiment = as.character(ID.Experiment), 
         ID.WeatherSensor = as.character(ID.WeatherSensor), 
         Date_Time = lubridate::ymd_hms(Date_Time), 
         Air_Temp = as.numeric(Air_Temp), 
         RH = as.numeric(RH), 
         CO2 = as.numeric(CO2), 
         PP = as.numeric(PP), 
         DLI = as.numeric(DLI), 
         ePAR = as.numeric(ePAR), 
         DP = as.numeric(DP), 
         VPD = as.numeric(VPD), 
         Atm = as.numeric(Atm))%>%
  mutate(Year = as.character(year(Date_Time)), 
         Month = as.character(month(Date_Time)),
         Day = as.character(day(Date_Time)),
         Hour = as.character(hour(Date_Time))) %>%
  slice(-c(1:4))




## Graph Plant Data
Plant_SFW = Plant_1 %>%
  ggplot(aes(x=SFW, y=Plant_Age , color=ID.Cultivar) ) + geom_smooth(se=F) + geom_point()
Plant_SFW

Plant_Height = Plant_1 %>%
  ggplot(aes(x=Height, y=Plant_Age , color=ID.Cultivar) ) + geom_smooth(se=F) ; Plant_Height

Plant_LA = Plant_1 %>%
  filter(Plant_Age == 35) %>%
  ggplot(aes(x=Leaf_Area, y=Plant_Age , color=ID.Cultivar) ) + geom_boxplot(se=F) ; Plant_LA

Plant_LN = Plant_1 %>%
  filter(Plant_Age == 35) %>%
  ggplot(aes(x=Leaf_Number, y=Plant_Age , color=ID.Cultivar) ) + geom_boxplot(se=F) ; Plant_LN





## Graph Weather Data
Weather_Temp = Weather_1 %>%
  ggplot(aes(x=Date_Time, y=Air_Temp, color=Day) ) + geom_line() ; Weather_Temp

Weather_RH = Weather_1 %>%
  ggplot(aes(x=Date_Time, y=RH, color=Day) ) + geom_line() ; Weather_RH

Weather_CO2 = Weather_1 %>%
  ggplot(aes(x=Date_Time, y=CO2, color=Day) ) + geom_line() ; Weather_CO2






