#Hiya! Here's the R script version without comments! (:

## Import Data------------------------------------------------------------------------------------------------------------------------
read.delim("data_raw/dicamptodon_tenebrosus_gbif/dicamptodon_tenebrosus_gbif.csv",sep = "\t", stringsAsFactors = T)
sp<-read.delim("data_raw/dicamptodon_tenebrosus_gbif/dicamptodon_tenebrosus_gbif.csv",sep = "\t", stringsAsFactors = T)


## Data Inspection------------------------------------------------------------------------------------------------------------------------
str(sp)
head(sp)
names(sp)


## Data Subsetting------------------------------------------------------------------------------------------------------------------------
sp[1:6, ]
sp["eventDate"]
sp_date<-sp["eventDate"]
sp$eventDate
sp_date_vec<-sp$eventDate
sp[c("eventDate","institutionCode", "basisOfRecord")]


## Using Functions and Dealing with NAs------------------------------------------------------------------------------------------------------------------------
mean(sp$coordinateUncertaintyInMeters)
mean(sp$coordinateUncertaintyInMeters, na.rm = TRUE)
round(mean(sp$coordinateUncertaintyInMeters, na.rm = TRUE))
avg<-mean(sp$coordinateUncertaintyInMeters, na.rm = TRUE)
round(avg)


## Working with Logical Operators------------------------------------------------------------------------------------------------------------------------
sp_inat <- sp[sp$institutionCode=="iNaturalist", ]
sp_inat_cas <- sp[sp$institutionCode=="iNaturalist" | sp$institutionCode=="CAS", ]


## Using Functions from Packages and Removing columns------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
sp_less <- sp[,-c(7,8)]
sp_less_tidy <- select(sp, -recordNumber)
