#### transform data to fit MDS ###

rm(list=ls()) # remove all clean all

### Relevant packages and scripts
require(reshape)
require(plyr)
require(lattice)
require(doBy)
require(gsubfn)
require(dplyr)


### load data #####################

NDatos<-read.csv("data_clean/2018_04_08_subtidal_punta_eugenia_clean_data.csv")
head(NDatos)

## subset data into different taxa ####

### Transect_typeLevel
# Generate 4 data sheets per Transect_type. Named by:
#   "Transect_typelevel" example:
#     "algaeSha" meaning Transect_type="algae" and Level="Shallow".
# By not specifying the Level means all of the data in such 
#Transect_type. 

### algaeLevel
algae <- subset(NDatos, Transect_type == "algae")
algaeMid <- subset(algae, Level == "Mid")
algaeSha <- subset(algae, Level == "Shallow")
algaeDee <- subset(algae, Level == "Deep")

###invertslevel
inverts <- subset(NDatos, Transect_type == "inverts")
invertsMid <- subset(inverts, Level == "Mid")
invertsSha <- subset(inverts, Level == "Shallow")
invertsDee <- subset(inverts, Level == "Deep")

###fishlevel
fish <- subset(NDatos, Transect_type == "fish")
fishMid <- subset(fish, Level == "Mid")
fishSha <- subset(fish, Level == "Shallow")
fishDee <- subset(fish, Level == "Deep")


upc <- subset(NDatos, Transect_type == "upc")
upcMid <- subset(upc, Level == "Mid")
upcSha <- subset(upc, Level == "Shallow")
upcDee <- subset(upc, Level == "Deep")



### summary data by Month and level
# x,y,z and k will be asigned respectivley to algae,
# inverts,fish and upc followed by the fisrt letter of the 
# corresponding level finally a number being none,1,2 or 3. 

algaes <-  algae %>%              #x1,x2,x3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

algaesMid <-  algaeMid %>%        #xm1,xm2,xm3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

algaesSha <-  algaeSha %>%        #xs1,xs2,xs3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

algaesDee <-  algaeDee %>%        #xd1,xd2,xd3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

invertss <-  inverts %>%          #y1,y2,y3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

invertssMid <-  invertsMid %>%    #ym1,ym2,ym3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

invertssSha <-  invertsSha %>%    #ys1,ys2,ys3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

invertssDee <-  invertsDee %>%    #yd1,yd2,yd3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

fishes <-  fish %>%               #z1,z2,z3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_120m3 = mean(Frequency))

fishesMid <-  fishMid %>%         #zm1,zm2,zm3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_120m3 = mean(Frequency))

fishesSha<-  fishSha %>%         #zs1,zs2,zs3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_120m3 = mean(Frequency))

fishesDee <-  fishDee %>%         #zd1,zd2,zd3.
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_120m3 = mean(Frequency))


upcs <-  upc %>%                  #k1,k2,k3
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))


upcsMid <-  upcMid %>%            #km1,km2,km3
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

upcsSha <-  upcSha %>%            #ks1,ks2,ks3
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))

upcsDee <-  upcDee %>%            #kd1,kd2,kd3
  ungroup() %>%
  group_by(Site, Year, Month,  Genusspecies) %>%
  summarise(Mean_density_30m2 = mean(Frequency))





## Bring togehter the name of the rows
x1<-unite_(algaes, "Site", c("Site","Year", "Month"))
xm1<-unite_(algaesMid, "Site", c("Site","Year", "Month"))
xs1<-unite_(algaesSha, "Site", c("Site","Year", "Month"))
xd1<-unite_(algaesDee, "Site", c("Site","Year", "Month"))

y1<-unite_(invertss, "Site", c("Site","Year", "Month"))
ym1<-unite_(invertssMid, "Site", c("Site","Year", "Month"))
ys1<-unite_(invertssSha, "Site", c("Site","Year", "Month"))
yd1<-unite_(invertssDee, "Site", c("Site","Year", "Month"))

z1<-unite_(fishes, "Site", c("Site","Year", "Month"))
zm1<-unite_(fishesMid, "Site", c("Site","Year", "Month"))
zs1<-unite_(fishesSha, "Site", c("Site","Year", "Month"))
zd1<-unite_(fishesDee, "Site", c("Site","Year", "Month"))

k1<-unite_(upcs, "Site", c("Site","Year", "Month"))
km1<-unite_(upcsMid, "Site", c("Site","Year", "Month"))
ks1<-unite_(upcsSha, "Site", c("Site","Year", "Month"))
kd1<-unite_(upcsDee, "Site", c("Site","Year", "Month"))

## Change table from long to wide
x2<-algaes %>% 
  spread(Genusspecies, Mean_density_30m2 )
xm2<-algaesMid %>% 
  spread(Genusspecies, Mean_density_30m2 )
xs2<-algaesSha %>% 
  spread(Genusspecies, Mean_density_30m2 )
xd2<-algaesDee %>% 
  spread(Genusspecies, Mean_density_30m2 )

y2<-invertss %>% 
  spread(Genusspecies, Mean_density_30m2 )
ym2<-invertssMid %>% 
  spread(Genusspecies, Mean_density_30m2 )
ys2<-invertssSha %>% 
  spread(Genusspecies, Mean_density_30m2 )
yd2<-invertssDee %>% 
  spread(Genusspecies, Mean_density_30m2 )

z2<-fishes %>% 
  spread(Genusspecies, Mean_density_120m3 )
zm2<-fishesMid %>% 
  spread(Genusspecies, Mean_density_120m3 )
zs2<-fishesSha %>% 
  spread(Genusspecies, Mean_density_120m3 )
zd2<-fishesDee %>% 
  spread(Genusspecies, Mean_density_120m3 )

k2<-upcs %>% 
  spread(Genusspecies, Mean_density_30m2 )
ks2<-upcsMid %>% 
  spread(Genusspecies, Mean_density_30m2 )
km2<-upcsSha %>% 
  spread(Genusspecies, Mean_density_30m2 )
kd2<-upcsDee %>% 
  spread(Genusspecies, Mean_density_30m2 )

## Put zeros in all the NAs
x3<- mutate_each(x2, funs(replace(., is.na(.), 0)))
xm3<- mutate_each(xm2, funs(replace(., is.na(.), 0)))
xs3<- mutate_each(xs2, funs(replace(., is.na(.), 0)))
xd3<- mutate_each(xd2, funs(replace(., is.na(.), 0)))

y3<- mutate_each(y2, funs(replace(., is.na(.), 0)))
ym3<- mutate_each(ym2, funs(replace(., is.na(.), 0)))
ys3<- mutate_each(ys2, funs(replace(., is.na(.), 0)))
yd3<- mutate_each(yd2, funs(replace(., is.na(.), 0)))

z3<- mutate_each(z2, funs(replace(., is.na(.), 0)))
zm3<- mutate_each(zm2, funs(replace(., is.na(.), 0)))
zs3<- mutate_each(zs2, funs(replace(., is.na(.), 0)))
zd3<- mutate_each(zd2, funs(replace(., is.na(.), 0)))

k3<- mutate_each(k2, funs(replace(., is.na(.), 0)))
km3<- mutate_each(km2, funs(replace(., is.na(.), 0)))
ks3<- mutate_each(ks2, funs(replace(., is.na(.), 0)))
kd3<- mutate_each(kd2, funs(replace(., is.na(.), 0)))


## Write table ####
write.table(x3, "data_clean/2018_04_punta_eugenia_wide_msd_algae_data.csv", sep=",", col.names=T, row.names=F)
write.table(xm3, "data_clean/2018_04_punta_eugenia_wide_msd_algae_Mid_data.csv", sep=",", col.names=T, row.names=F)
write.table(xs3, "data_clean/2018_04_punta_eugenia_wide_msd_algae_Shallow_data.csv", sep=",", col.names=T, row.names=F)
write.table(xd3, "data_clean/2018_04_punta_eugenia_wide_msd_algae_Deep_data.csv", sep=",", col.names=T, row.names=F)

write.table(y3, "data_clean/2018_04_punta_eugenia_wide_msd_inverts_data.csv", sep=",", col.names=T, row.names=F)
write.table(ym3, "data_clean/2018_04_punta_eugenia_wide_msd_inverts_Mid_data.csv", sep=",", col.names=T, row.names=F)
write.table(ys3, "data_clean/2018_04_punta_eugenia_wide_msd_inverts_Shallow_data.csv", sep=",", col.names=T, row.names=F)
write.table(yd3, "data_clean/2018_04_punta_eugenia_wide_msd_inverts_Deep_data.csv", sep=",", col.names=T, row.names=F)

write.table(z3, "data_clean/2018_04_punta_eugenia_wide_msd_fish_data.csv", sep=",", col.names=T, row.names=F)
write.table(zm3, "data_clean/2018_04_punta_eugenia_wide_msd_fish_Mid_data.csv", sep=",", col.names=T, row.names=F)
write.table(zs3, "data_clean/2018_04_punta_eugenia_wide_msd_fish_Shallow_data.csv", sep=",", col.names=T, row.names=F)
write.table(zd3, "data_clean/2018_04_punta_eugenia_wide_msd_fish_Deep_data.csv", sep=",", col.names=T, row.names=F)

write.table(k3, "data_clean/2018_04_punta_eugenia_wide_msd_upc_data.csv", sep=",", col.names=T, row.names=F)
write.table(km3, "data_clean/2018_04_punta_eugenia_wide_msd_upc_Mid_data.csv", sep=",", col.names=T, row.names=F)
write.table(ks3, "data_clean/2018_04_punta_eugenia_wide_msd_upc_Shallow_data.csv", sep=",", col.names=T, row.names=F)
write.table(kd3, "data_clean/2018_04_punta_eugenia_wide_msd_upc_Deep_data.csv", sep=",", col.names=T, row.names=F)

