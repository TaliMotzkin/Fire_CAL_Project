setwd("C:\\Users\\Tali\\Documents\\study\\science\\Project\\Data")
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(Hmisc)
library(ggcorrplot)
library(corrplot)
library(ggcorrplot)
library(corrplot)
library(Hmisc)
library(moments)




####Load Weather and Cause of fire tables, and CIMIS station data from the gdb project
Weather_data=read.csv("Raw_tables\\Weather_data.csv")
Cause_code = read.csv("Raw_tables\\Cause_code.csv")
gdb_project = st_read("Raw_Layers\\Project.gdb")
project_layers = st_layers(dsn = "Raw_Layers\\Project.gdb")
CIMIS_New = st_read("Raw_Layers\\Project.gdb", layer="CIMIS_new")


#### Comparing between the spatial station data (CIMIS_new) and the weather station 
#### data (Weather_data) and making a new layer which contains only the spatial 
#### stations that have weather data
CIMIS_clean1 = CIMIS_new[CIMIS_new$ID %in%  Weather_data$Stn.Id,]
st_write(CIMIS_clean1, "CIMIS_clean1.shp")
class(CIMIS_clean1)
str(CIMIS_clean1)
str(Burn_centroid)

#### reading the gdb project of the two layers: burn areas and the clean meteorological 
#### spatial station. Each burn area has the nearest station (determined by the FID column)
Burn_areas = st_read("Raw_Layers\\Project.gdb", layer="Burn_areas")
CIMIS_clean1 = st_read("Raw_Layers\\Project.gdb", layer = "CIMIS_clean")
CIMIS_clean1$FID = 0:(length(CIMIS_clean1$ID)-1) 
#We couldn't load the FID column from the ARCGIS so we made one manually



#### cleaning and tiding the Data of the three tables
Burn_areas_clean = Burn_areas %>%
  select(AGENCY:FIRE_NAME, ALARM_DATE:CAUSE,
         GIS_ACRES, Shape_Length:Shape_Area,NEAR_FID:Shape) %>%
  mutate(treat_duration = difftime(CONT_DATE, ALARM_DATE, units="days")) %>%
  separate(ALARM_DATE, into=c("year", "month","day_time"), sep="-", 
           convert = TRUE) %>%
  separate(day_time, into =c("day", "time"), sep =2,convert= TRUE) %>%
  separate(CONT_DATE, into=c("Cont_year", "Cont_month","Cont_day_time"), sep="-", 
           convert = TRUE) %>% #seperate for the cont_date
  separate(Cont_day_time, into =c("Cont_day", "Cont_time"), sep =2,convert= TRUE) %>%
  select(-time, -Cont_time)
#1. we selected the relevant columns
#2. we calculated a new column which contains the treatment 
#duration of the fire contamination(days)
#3. we separated the starting and the ending dates of the fires to
# three different columns: "day", "month" and "year" and de-select unnecessary columns


Weather_data_clean = Weather_data %>%
  select(Stn.Id:Total.ETo..mm., Total.Precip..mm.,Avg.Sol.Rad..W.sq.m.
         ,Avg.Vap.Pres..kPa.,Avg.Max.Air.Temp..C.,Avg.Min.Air.Temp..C.
         , Avg.Air.Temp..C., Avg.Max.Rel.Hum....,Avg.Min.Rel.Hum....,Avg.Rel.Hum....
         ,Avg.Dew.Point..C.,Avg.Wind.Speed..m.s.,Avg.Soil.Temp..C.) %>%
  rename(Total_ETo_mm = Total.ETo..mm., Tot_Precip_mm = Total.Precip..mm.,
         Avg_Sol_Rad_Wsqm = Avg.Sol.Rad..W.sq.m., Avg_Vap_pres_kPa=Avg.Vap.Pres..kPa.,
         Avg_Max_Air_T_C=Avg.Max.Air.Temp..C., Avg_Min_Air_Temp_C= Avg.Min.Air.Temp..C.,
        Avg_Air_T_C = Avg.Air.Temp..C., Avg_Max_Rel_Hum= Avg.Max.Rel.Hum....,
       Avg_Min_Rel_Hum = Avg.Min.Rel.Hum...., Avg_Rel_Hum= Avg.Rel.Hum....,
       Avg_DPoint_c=Avg.Dew.Point..C., Avg_Wind_S_ms=Avg.Wind.Speed..m.s.,
       Avg_Soil_Temp_C=Avg.Soil.Temp..C.) %>%
  separate(Month.Year , into=c("month", "year_time"), sep="/", 
           convert = TRUE) %>%
  separate(year_time, into =c("year", "time"), sep =4,convert= TRUE) %>%
  select(-time)
#1. we selected the relevant columns
#2. we renamed some of the columns and separated the measuring date
# to 2 columns: "month" and "year" and de-select unnecessary columns


#### join
CIMIS_data_clean = CIMIS_clean1 %>%
  st_drop_geometry() %>%
  select(ID, FID)
#1. we drop Geometry for the join
#2. we selected the relevant columns

Burn_Weather_join = Burn_areas_clean %>%
  inner_join(CIMIS_data_clean, by = c("NEAR_FID"="FID")) %>%
  inner_join(Weather_data_clean, by = c("ID"="Stn.Id", "month", "year")) %>%
  left_join(Cause_code ,by = c("CAUSE"= "Code")) %>%
  select(-NEAR_FID)
#we joined the 4 tables into one main table that includes the fires
#data, meteorological station data and the cause of the fire

#we converted the sf data frame to tibble format for convenience while working with big data
tibble_Burn_weather = as.tibble(Burn_Weather_join)

#show how much fires were per year since 1983 till 2020
tibble_Burn_weather %>%
  group_by(year)%>%
  count() %>% 
  ggplot(aes(year,n))+
  geom_point(col = "black")+
  labs(title = "Numbers of fires per year",
       x="Year", y="Number of fires")+
  geom_smooth(method = lm, col="red")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))

#sum the total burned area per year
tibble_Burn_weather %>%
  group_by(year)%>%
  summarise(total = sum(GIS_ACRES)) %>% 
  ggplot(aes(year,total))+
  geom_point(col = "blue")+
  labs(title = "size of burned area per year",
       x="Year", y="Size of burned area (acres)")+
  geom_smooth(method = lm, col="red")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))


  
#plot of lightning (natural cause)
tibble_Burn_weather %>%
group_by(year,Desc)%>%
  count() %>%
  filter(Desc == "Lightning") %>% 
  ggplot(aes(year,n))+
  geom_point(col = "red")+
  labs(title = "Numbers of fires per year by natural 
       cause (lightning)",
       x="Year", y="Number of fires")+
  geom_smooth(method = lm, col="blue")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))
  


##ignition from power lines, vehicle, equipment use and arson
tibble_Burn_weather %>%
  group_by(year,Desc)%>%
  count() %>%
  filter(Desc %in% c("Power Line","Arson","Vehicle", "Equipment Use")) %>% 
  ggplot(aes(year,n))+
  geom_point(col = "red")+
  labs(title = "Numbers of fires per year by 4 human ignition causes",
       x="Year", y="Number of fires")+
  geom_smooth(method = lm, col="blue")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))+
  facet_wrap(~Desc, nrow=1)


#Numbers of fires per year by human cause
tibble_Burn_weather %>%
  filter(Desc %in% c("Aircraft","Arson","Campfire", "Debris",
                     "Equipment Use", "Escaped Prescribed Burn",
                     "Firefighter Trainning","Illigal Alien Campfire",
                     "Non-Firefighter Training", "Playing with Fire", 
                     "Power Line", "Railroad", "Smoking",
                     "Structure", "Vehicle")) %>%
  group_by(year)%>%
  count() %>%
  ggplot(aes(year,n))+
  geom_point(col = "red")+
  labs(title = "Numbers of fires per year by human cause",
       x="Year", y="Number of fires")+
  geom_smooth(method = lm, col="blue")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))


#decide which fires are small\big
Burn_size = tibble_Burn_weather %>% 
  filter(!is.na(GIS_ACRES)) %>% 
  mutate(godel = case_when(GIS_ACRES <= 500 ~ "Small (<=500)",
                           GIS_ACRES > 500 ~ "Big (>500)"))

#make a plot of big and small fires by month as accumulation
#of all years
Burn_size %>% 
  group_by(month,godel)%>%
  count() %>% 
  ggplot(aes(x= as.factor(month)))+
  geom_col(aes(fill = as.factor(godel), y=n), position = "dodge")+
  scale_fill_brewer(palette="Set2")+
  labs(title = "Fire Size distribution by Months (Acres Burned)",
       subtitle = "During 1983-2020",
       y="Number of Fires", x="Month",fill="Fire size (Acre)")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))


## Correlation - Between climatic factors
hello=tibble_Burn_weather %>% 
  select(GIS_ACRES, Tot_Precip_mm, Total_ETo_mm, 
         Avg_Sol_Rad_Wsqm, Avg_Air_T_C, Avg_Rel_Hum, Avg_Wind_S_ms) 

cor1=rcorr(as.matrix(hello[,c(1:7)]))
cor1_df=as.data.frame(cor1$r)
cor1_df

ggcorrplot(cor1_df, hc.order = TRUE,
           type="lower",
           lab=TRUE,digits = 3,
           lab_size=2.5,
           method="circle",
           colors = c("#6D9EC1", "white", "#E46726"))

             
#Treat duration of fires per year 
tibble_Burn_weather %>%
  filter(treat_duration >= 0) %>%
  group_by(year)%>%
  summarise(Avg_Treat_Duration =mean(treat_duration )) %>% 
  ggplot(aes(year,Avg_Treat_Duration))+
  geom_point(col = "red")+
  labs(title = "Average Treat Duration per Year",
       x="Year", y="Average treat duration")+
  geom_smooth(method = lm, col="blue")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))


#Treat duration of fires per Size of fire 
tibble_Burn_weather %>%
  filter(treat_duration >= 0, GIS_ACRES < 500000) %>%
  ggplot(aes(GIS_ACRES,treat_duration))+
  geom_point(col = "red")+
  labs(title = "Treat Duration per Fire size",
       x="Fire size (GIS Acres)", y="treat duration")+
  geom_smooth(method = lm, col="blue")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))

##Checking Normality
par(mar=c(5.1, 4.1, 4.1, 2.1))
hist(tibble_Burn_weather$GIS_ACRES)
skewness(tibble_Burn_weather$GIS_ACRES, na.rm = TRUE)
kurtosis(tibble_Burn_weather$GIS_ACRES, na.rm = TRUE)

###??????
tibble_Burn_weather1 = tibble_Burn_weather %>% 
  filter(GIS_ACRES >0, na.rm=TRUE) %>% 
  filter(Avg_Air_T_C >0 , na.rm =TRUE) %>% 
  filter(Avg_Wind_S_ms >0, na.rm = TRUE) %>% 
  filter(Avg_Rel_Hum >0 , na.rm =TRUE) %>% 
  filter(Total_ETo_mm >0 , na.rm =TRUE) %>% 
  filter(Avg_Sol_Rad_Wsqm >0 , na.rm =TRUE) %>% 
  filter(Tot_Precip_mm >0 , na.rm =TRUE) 
tibble_Burn_weather1 = na.omit(tibble_Burn_weather1)  
  
climate_model4 = glm(formula = GIS_ACRES ~ Tot_Precip_mm+
                       Total_ETo_mm + Avg_Sol_Rad_Wsqm + Avg_Air_T_C +
                       Avg_Rel_Hum + Avg_Wind_S_ms, 
                     family = Gamma(link = "identity"), 
                     data = tibble_Burn_weather1)
climate_model2 = glm(GIS_ACRES ~ Avg_Air_T_C ,data = tibble_Burn_weather1, Gamma(link = "log"))
climate_model2 = glm(GIS_ACRES ~ Avg_Wind_S_ms ,data = tibble_Burn_weather1, family= "Gamma")
climate_model2 = glm(GIS_ACRES ~ Avg_Rel_Hum ,data = tibble_Burn_weather1, family= "Gamma")

summary(climate_model4)


cor(tibble_Burn_weather$GIS_ACRES, tibble_Burn_weather$Avg_Air_T_C,
    method = "kendall", use="na.or.complete") 


#linear regression of precipitation, evapotranspiration, temperature,
#wind speed, humidity, solar
climate_model = lm(data = tibble_Burn_weather, GIS_ACRES ~ Tot_Precip_mm+
                     Total_ETo_mm + Avg_Sol_Rad_Wsqm + Avg_Air_T_C +
                     Avg_Rel_Hum + Avg_Wind_S_ms)
summary(climate_model)

##checking the strong correlations:
cor(tibble_Burn_weather$GIS_ACRES, tibble_Burn_weather$Avg_Air_T_C,
    method = "kendall", use="na.or.complete") 
cor(tibble_Burn_weather$GIS_ACRES, tibble_Burn_weather$Avg_Sol_Rad_Wsqm,
    method = "kendall", use="na.or.complete") 
cor(tibble_Burn_weather$GIS_ACRES, tibble_Burn_weather$Avg_Rel_Hum,
    method = "kendall", use="na.or.complete") 


#filter summer months and run regression
summer = filter(tibble_Burn_weather, month %in% c(5:10))
summer_model = lm(data = summer, GIS_ACRES ~ Tot_Precip_mm+
                     Total_ETo_mm + Avg_Sol_Rad_Wsqm + Avg_Air_T_C +
                     Avg_Rel_Hum + Avg_Wind_S_ms)
summary(summer_model)
cor(summer$GIS_ACRES, summer$Avg_Air_T_C, method = "kendall", use="na.or.complete") 
cor(summer$GIS_ACRES, summer$Avg_Wind_S_ms, method = "kendall", use="na.or.complete") 
cor(summer$GIS_ACRES, summer$Avg_Rel_Hum, method = "kendall", use="na.or.complete")

#filter fall month and run regression
fall = filter(tibble_Burn_weather, month %in% c(11,12,1,2))
fall_model = lm(data = fall, GIS_ACRES ~ Tot_Precip_mm+
                    Total_ETo_mm + Avg_Sol_Rad_Wsqm + Avg_Air_T_C +
                    Avg_Rel_Hum + Avg_Wind_S_ms)
summary(fall_model)
cor(fall$GIS_ACRES, fall$Total_ETo_mm , method = "kendall", use="na.or.complete") 
cor(fall$GIS_ACRES, fall$Avg_Sol_Rad_Wsqm, method = "kendall", use="na.or.complete") 





##writing for ArcGis
shapefile = Burn_centroid
shapefile = shapefile[,-15]
shapefile = shapefile %>% 
  rename(AMATC=Avg_Min_Air_Temp_C, AMAATC=Avg_Max_Air_T_C)

st_write(shapefile, "sep_coords13.shp")


#load the california counties data
cal_county = st_read("Raw_Layers\\California_County_Boundaries\\cnty19_1.shp")
cal_crs = st_transform(cal_county, 4269)

#finding the fires centroid and plotting the results
Burn_centroid = st_centroid(Burn_Weather_join)
ggplot()+
  geom_sf(data =Burn_centroid, color = 'red')+
  labs(title = "Centroid of fires", x="lon", y="lat")+
  theme(plot.title = element_text(hjust=0.5))

#finding the lat and lon attributes of the centroid for the ggplot plotting
sep_coords <- Burn_centroid %>%
  mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

#plotting the density of fires
ggplot() +
  geom_sf(data =sep_coords, color = 'white', alpha = 0.2)+
  stat_density_2d(data = sep_coords, 
                  mapping = aes(x = lon,
                                y = lat,
                                fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.8) +
  scale_fill_viridis(option = "A", direction = -1) +
  geom_sf(data = cal_crs, fill = NA) +
  geom_sf_text(data = cal_crs, aes(label= COUNTY_ABB), colour = 'green4', size = 2.5)+
  labs(title = "Density of fires")+
  theme(plot.title = element_text(hjust=0.5)) 

sep_coords$Desc

#decide which fires are small\big
Burn_size = tibble_Burn_weather %>% 
  filter(!is.na(GIS_ACRES)) %>% 
  mutate(godel = case_when(GIS_ACRES <= 500 ~ "Small (<=500)",
                           GIS_ACRES > 500 ~ "Big (>500)"))

#make a plot of big and small fires by month as accumulation
#of all years
Burn_size %>% 
  group_by(month,godel)%>%
  count() %>% 
  ggplot(aes(x= as.factor(month)))+
  geom_col(aes(fill = as.factor(godel), y=n), position = "dodge")+
  scale_fill_brewer(palette="Set2")+
  labs(title = "Fire Size distribution by Months (Acres Burned)",
       subtitle = "During 1983-2020",
       y="Number of Fires", x="Month",fill="Fire size (Acre)")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

#prepering the data for ploting per human VS natural cause

human = sep_coords %>% 
  filter(Desc %in% c("Aircraft","Arson","Campfire", "Debris",
                     "Equipment Use", "Escaped Prescribed Burn",
                     "Firefighter Trainning","Illigal Alien Campfire",
                     "Non-Firefighter Training", "Playing with Fire", 
                     "Power Line", "Railroad", "Smoking",
                     "Structure", "Vehicle")) 

natural = sep_coords %>% 
  filter(Desc == "Lightning" )


#plotting the density of fires per human\natural cause
ggplot() +
  geom_sf(data =human, color = 'white', alpha = 0.2)+
  stat_density_2d(data = human, 
                  mapping = aes(x = lon,
                                y = lat,
                                fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.8) +
  scale_fill_viridis(option = "A", direction = -1) +
  geom_sf(data = cal_crs, fill = NA) +
  geom_sf_text(data = cal_crs, aes(label= COUNTY_ABB), colour = 'green4', size = 2.5)+
  labs(title = "Density of fires by human cause")+
  theme(plot.title = element_text(hjust=0.5)) 


ggplot() +
  geom_sf(data =natural, color = 'white', alpha = 0.2)+
  stat_density_2d(data = natural, 
                  mapping = aes(x = lon,
                                y = lat,
                                fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.8) +
  scale_fill_viridis(option = "A", direction = -1) +
  geom_sf(data = cal_crs, fill = NA) +
  geom_sf_text(data = cal_crs, aes(label= COUNTY_ABB), colour = 'green4', size = 2.5)+
  labs(title = "Density of fires by natural cause")+
  theme(plot.title = element_text(hjust=0.5)) 


###Chi-Square

hum_nat = tibble_Burn_weather %>% 
  filter(Desc %in% c("Aircraft","Arson","Campfire", "Debris",
                     "Equipment Use", "Escaped Prescribed Burn",
                     "Firefighter Trainning","Illigal Alien Campfire",
                     "Non-Firefighter Training", "Playing with Fire", 
                     "Power Line", "Railroad", "Smoking",
                     "Structure", "Vehicle", "Lightning")) %>%
  mutate(binary = case_when(Desc == "Lightning"~ 1, TRUE ~ 0))

chisq.test(hum_nat$GIS_ACRES, hum_nat$binary)
chisq.test(hum_nat$GIS_ACRES, hum_nat$Avg_Air_T_C)
chisq.test(hum_nat$GIS_ACRES, hum_nat$Total_ETo_mm)
chisq.test(hum_nat$GIS_ACRES, hum_nat$Tot_Precip_mm)
chisq.test(hum_nat$GIS_ACRES, hum_nat$Avg_Wind_S_ms)
chisq.test(hum_nat$GIS_ACRES, hum_nat$Avg_Rel_Hum)


