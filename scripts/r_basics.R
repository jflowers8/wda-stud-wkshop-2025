
#Hiya! Here's the R script version with comments! (:

## Import Data------------------------------------------------------------------------------------------------------------------------
#sp<-read.csv("data_raw/dicamptodon_tenebrosus_gbif/dicamptodon_tenebrosus_gbif.csv", stringsAsFactors = T)
#Why does this give us an error? Let's look at the data file...our actual file is a TSV not a CSV

#?read.csv #look here to see which function is more suitable for our data type

read.delim("data_raw/dicamptodon_tenebrosus_gbif/dicamptodon_tenebrosus_gbif.csv",sep = "\t", stringsAsFactors = T)

sp<-read.delim("data_raw/dicamptodon_tenebrosus_gbif/dicamptodon_tenebrosus_gbif.csv",sep = "\t", stringsAsFactors = T) #Reads in and looks good! Take a gander!

#More details about the standardized GBIF columns here: https://dwc.tdwg.org/terms/

#Here you can uncomment the below line to try a different dataset on your own!
#sp<-read.delim("data_raw/cervus_canadensis_gbif/cervus_canadensis_gbif.csv",sep = "\t", stringsAsFactors = T)



## Data Inspection------------------------------------------------------------------------------------------------------------------------

str(sp)
head(sp)
names(sp)



## Data Subsetting------------------------------------------------------------------------------------------------------------------------
#NOTE: This is referencing data frame object types. These may require different syntax/may work differently for non data frame objects.

sp[1:6, ] #first 6 rows, all columns

sp["eventDate"] #pulls out the column named "eventDate"

sp_date<-sp["eventDate"] #now its a named object in our environment pane (still a data frame object type)


sp$eventDate

sp_date_vec<-sp$eventDate #Why does this look different? It's because when we subset in this way R returns sp_date_vec as a vector of values we can compare using class()


sp[c("eventDate","institutionCode", "basisOfRecord")] #will pull out all of these columns as a data frame object type in the order you specify them in



## Using Functions and Dealing with NAs------------------------------------------------------------------------------------------------------------------------

#Using Functions
mean(sp$coordinateUncertaintyInMeters) #result is NA :(


mean(sp$coordinateUncertaintyInMeters, na.rm = TRUE) #we tell the function to ignore NAs and now we get the mean! thats a high average coordinate uncertainty (in meters)!


round(mean(sp$coordinateUncertaintyInMeters, na.rm = TRUE)) #this will return the same result as...

avg<-mean(sp$coordinateUncertaintyInMeters, na.rm = TRUE)
round(avg)


#Filtering NAs

#!is.na(sp$coordinateUncertaintyInMeters) #returns a logical vector that tells us which item in the sp$coordinateUncertaintyInMeters is NOT an NA value. is.na() should return the opposite result.




## Working with Logical Operators------------------------------------------------------------------------------------------------------------------------
sp_inat <- sp[sp$institutionCode=="iNaturalist", ] #takes all rows where the institution code returned TRUE for the value "iNaturalist"

#sp$institutionCode=="iNaturalist" #here you can see exactly what the input looks like for above, it will return a logical vector (true or false)


sp_inat_cas <- sp[sp$institutionCode=="iNaturalist" | sp$institutionCode=="CAS", ] #takes rows where the logical vector returns true for iNaturalist OR CAS



## Using Functions from Packages and Removing columns------------------------------------------------------------------------------------------------------------------------
#install.packages("tidyverse") #uncomment if you need to install the package
library(tidyverse) #Here we load a library. We need to install a package and load a library to use its functions in R.

sp_less <- sp[,-c(7,8)]  #here we remove columns by number index NOTE this isn't the best method because number indices change. Putting in "hard" numbers like this is considered "hard" coding. We want to "soft" code wherever possible so we have to make fewer changes to our code when other parts of our code change down the line

#For example this may be a better "soft" coding option because it will take out the relevant column by name rather than by column number index

sp_less_tidy <- select(sp, -recordNumber) #select here comes from the package dplyr or tidyverse



