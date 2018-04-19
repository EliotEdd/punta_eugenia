library(dplyr)
head(NDatos)
baja <-  NDatos %>%
  ungroup() %>%
  group_by(Site, Year, Commuunity, Genusspecies) %>%
  summarise(Mean = mean(Frequency),
            SD = sd(Frequency),
            CV=(sd(Frequency)/mean(Frequency)) ,
            SE=sqrt(var(Frequency)/length(Frequency)),
            N=length(Frequency),
            TC_max = max(TotalCounts))
