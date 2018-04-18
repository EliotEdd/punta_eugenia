### Clean up data for Punta Eugenia
## created by Rodrigo Beas 
#######################################

rm(list=ls()) # delete all


### packages ####
require(reshape)
library(ggplot2)
require(plyr)
require(lattice)
require(doBy)
require(gsubfn)
library(gtools)
library(tidyr)

g<- read.csv("data_raw/2018_04_05_data_gavilanes_all.csv") # load gavilaes file and call it g
pb<-read.csv("data_raw/2018_04_05_data_piedra_blanca_all.csv") # load piedra blanca and callit pb

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

#View(datosPE) ## look at the data in a sortable table

errors<-lapply(X = datosPE[,], FUN = unique ) 
errors## print all the unique values for each of the columns 

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
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Daniel D.$", "Daniel_Diaz", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Eliot D.$", "Eliot_DelaCruz", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Eliot. D.$", "Eliot_DelaCruz", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Fernanda A.$", "Fernanda_Aldaco", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Fernanda A. $", "Fernanda_Aldaco", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^H\202ctor H.$", "Hector_Hernandez", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Memo T. $", "Guillermo_Torres", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Nur$", "Nur_Arafeh", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Rodrigo B.$", "Rodrigo_Beas", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Rodrigo C.$", "Rodrigo_Castro", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Rodrigo C. $", "Rodrigo_Castro", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^Hâctor H.$", "Hector_Hernandez", x))
datosPE$Observer<-sapply(datosPE$Observer, function(x) gsub("^H_ctor H.$", "Hector_Hernandez", x))


#Transect_type ####
datosPE$Transect_type<-sapply(datosPE$Transect_type, function(x) gsub("^Algae$", "algae", x))
datosPE$Transect_type<-sapply(datosPE$Transect_type, function(x) gsub("^Inverts$", "inverts", x))

#Transect_section #### 
datosPE$Transect_section<-sapply(datosPE$Transect_section, function(x) gsub("^oct-20$", "10-20", x))
datosPE$Transect_section<-sapply(datosPE$Transect_section, function(x) gsub("^Na$", NA, x))

#size_cm ###
datosPE$Size_cm<-sapply(datosPE$Size_cm, function(x) gsub("^Na$", NA, x))

# Genusspecies ####

datosPE$Genusspecies<-sapply(datosPE$Genusspecies, function(x) gsub("^macrocystis_pyrifera$", "Macrocystis_pyrifera", x))
datosPE$Genusspecies<-sapply(datosPE$Genusspecies, function(x) gsub("^macrocystis_pyrifera_stipes$", "Macrocystis_pyrifera_stipes", x))
datosPE$Genusspecies<-sapply(datosPE$Genusspecies, function(x) gsub("^sargassum_muticum$", "Sargassum_muticum", x))
datosPE$Genusspecies<-sapply(datosPE$Genusspecies, function(x) gsub("^undaria_pinnatifida$", "Undaria_pinnatifida", x))




## Genusspecies column ####
fg<- read.csv("data_raw/Genusspecies_dictionary.csv") # load dictionary
# look for the wrong names and replace them with the correct names
head(fg)
colnames(fg)[colnames(fg)=="Genus_species"] <- "Genusspecies"  ### rename the column to match both tables

Punta_Eugenia<- merge(datosPE, fg,all.x=T) ## merge the data table with the dictionary.

as.data.frame(sort(unique(Punta_Eugenia$Genusspecies)))## unique values for genusspeuces column 
as.data.frame(sort(unique(Punta_Eugenia$translates_to))) ## unique values for the new genus spp names.

errors<-lapply(X =Punta_Eugenia[,], FUN = unique ) 
errors## print all the unique values for each of the columns 

#### After each column is cleaned, you can print the new clened tables like this
write.csv(Punta_Eugenia, "data_clean/2018_04_08_subtidal_punta_eugenia_clean_data.csv")

