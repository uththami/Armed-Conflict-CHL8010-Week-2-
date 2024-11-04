library(dplyr)
library(here)
here()
disasterdata<-read.csv(here("original","disaster.csv"),header=TRUE)

#cleaning disaster data
disasterdata1<-filter(disasterdata, Year %in% c(2000,2019))
disasterdata2<-filter(disasterdata1, Disaster.Type=="Earthquake"|Disaster.Type=="Drought")
disasterdata3<- disasterdata2 %>% select(Year, ISO, Disaster.Type)
disasterdata3$drought<-ifelse(disasterdata3$Disaster.Type=="Drought",1,0)
disasterdata3$earthquake<-ifelse(disasterdata3$Disaster.Type=="Earthquake",1,0)

#grouping and summarizing
clean_disaster<-disasterdata3 %>% group_by(Year,ISO) %>% summarize(drought=max(drought),earthquake=max(earthquake))%>%ungroup()
head(clean_disaster)
