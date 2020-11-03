#libraries and not reactive processes 

library(tidyverse)
library(lubridate)
library(plotly)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(RColorBrewer)
library(DT)
library(leaflet)
library(geojsonio)


rm(list=ls())
df_covid<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

sa_countries<-c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Paraguay",
                "Peru","Uruguay","Venezuela","Suriname","Guyana")
df_covid<- df_covid[df_covid$countriesAndTerritories %in% sa_countries,]
df_covid$dateRep <- as.Date(df_covid$dateRep, "%d/%m/%Y")

df_covid$countriesAndTerritories<-as.factor(df_covid$countriesAndTerritories)

nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)


sa_sp <- ne_countries(continent = 'south america',returnclass = "sf")
sa_sp<-sa_sp[-7,]
#sa_sp<-  geojson_read("custom.geo.json", what='sp')

#sa_sp<-sa_sp[-8,]

pal1<- colorNumeric("viridis", NULL)
pal2<-colorNumeric("YlOrRd",NULL)
pal3<-colorNumeric("YlGnBu",NULL)