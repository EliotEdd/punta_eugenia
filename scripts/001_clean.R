### Clean up data for Punta Eugenia
## created by Rodrigo Beas 
#######################################

rm(list=ls()) # delete all


### packages ####
#require(reshape)
library(ggplot2)
require(plyr)
require(lattice)
require(doBy)
require(gsubfn)
library(gtools)
library(tidyverse)

g<- read.csv("data_raw/2018_04_05_data_gavilanes_all.csv") # load gavilaes file and call it g
pb<-read.csv("data_raw/Punta_Eugenia/2018_04_05_data_piedra_blanca_all.csv") # load piedra blanca and callit pb

pe<-list(g, pb)# make a list of these two tables to manage them easy

cols<- c("Year", "Month", "Day", "Site", "Level", "Observer", "Transect_type", 
         "Transect_number", "Transect_section", "Depth_m", "Lat", "Long", 
         "Genusspecies", "Frequency", "Size_cm") ## these are the columns all the tables should have

lapply(pe, function(x) {cols %in% colnames(x[1:length(x)])  }) # check if the column names of all tables are there


cbind(cols, cols %in%colnames(pe[[2]])) # check the column names specific for each table

### correct colnames ####
pe <- lapply(pe, function(x) {colnames(x) <- gsub("^Lat.nort$", "Lat", colnames(x)); x})  # gsub substituye el segundo valor por el primero. Nota ^$ quieredecir toda la palabra.

#### Merge all the items in the list. They all have to match or it does not work. 
datosPE = do.call(rbind,pe)

# View(datosPE) ## look at the data in a sortable table

errors<-lapply(X = datosPE[,], FUN = unique ), errors## print all the unique values for each of the columns  

errors.df <- do.call("rbind", lapply(errors, as.data.frame))  # makes a data frame from a list in order to print it
write.csv(errors.df, "sandbox/0001_punta_eugenia_unique_values.csv") # prints a table with the unique values.


#### correct column errors ####


# Level
datosPE$Level<-sapply(datosPE$Level, function(x) gsub("^mid$", "Mid", x)) 

# Observer
#as.data.frame(sort(unique(datosPE$Observer))) # print a table with the unique values in a column organized by name
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Nur A.$", "Nur_Arafeh", x)) 
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Memo T$", "Guillermo_Torres", x)) 
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Memo T.$", "Guillermo_Torres", x)) 
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Ainoa V. $", "Ainoa_Vilalta", x)) 
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Ainoa V.$", "Ainoa_Vilalta", x)) 
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Carlos H.$", "Carlos_Hernandez", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Daniel D$", "Daniel_Diaz", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Eliot D.$", "Eliot_DelaCruz", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Eliot. D.$", "Eliot_DelaCruz", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Fernanda A.$", "Fernanda_Aldaco", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Fernanda A. $", "Fernanda_Aldaco", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^H\202ctor H.$", "Hector_Hernandez", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Memo T. $", "Guillermo_Torres", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Nur$", "Nur_Arafeh", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Rodrigo B$", "Rodrigo_Beas", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Rodrigo C.$", "Rodrigo_Castro", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Rodrigo C. $", "Rodrigo_Castro", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^$", "", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^$", "", x))

#### After each column is cleaned, you can print the new clened tables like this
write.csv(datosPE, "data_clean/2018_04_08_subtidal_punta_eugenia_clean_data.csv")