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
summarized_data<-disasterdata3 %>% group_by(Year,ISO) %>% summarize(drought=max(drought),earthquake=max(earthquake))%>%ungroup()

#loading in other datasets
maternal<-read_csv("~/Armed Conflict CHL8018 update/original/maternalmortality.csv")
infant<-read_csv("~/Armed Conflict CHL8018 update/original/infantmortality.csv")
neonatal<-read_csv("~/Armed Conflict CHL8018 update/original/neonatalmortality.csv")
under5<-read_csv("~/Armed Conflict CHL8018 update/original/under5mortality.csv")

#checking data
list_mortality<-list(maternal, infant, neonatal, under5)
lapply(list_mortality, FUN=summary)


library(tidyr)
maternallong<-maternal %>%
  pivot_longer(
    cols = (`2000`:`2019`),
    names_to = "Year",
    names_prefix = "X",
    values_to = "MatMor"
  )

