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
summarized_disaster_data<-disasterdata3 %>% group_by(Year,ISO) %>% summarize(drought=max(drought),earthquake=max(earthquake))%>%ungroup()
head(summarized_disaster_data)
#loading in other datasets
maternal<-read.csv(here("original","maternalmortality.csv"),header=TRUE)
infant<-read.csv(here("original","infantmortality.csv"),header=TRUE)
neonatal<-read.csv(here("original","neonatalmortality.csv"),header=TRUE)
under5<-read.csv(here("original","under5mortality.csv"),header=TRUE)


#Creating functions
library(tidyr)

clean_data<-function(data,datamor){data %>% select(Country.Name,X2000:X2019) %>% pivot_longer(cols=starts_with("X"),names_to="Year",names_prefix="X",values_to=datamor) %>% mutate(Year=as.numeric(Year))}

#applying functions
clean_maternal<-clean_data(maternal,datamor="MatMor")
clean_infant<-clean_data(infant,datamor="InfMor")
clean_neonatal<-clean_data(neonatal,datamor="NeoMor")
clean_under5<-clean_data(under5,datamor="Under5Mor")

#merging datasets
library(purrr)
list_mortality<-list(clean_maternal,clean_infant,clean_neonatal,clean_under5)
all_mortality<-reduce(list_mortality,full_join,by=c("Year","Country.Name"))

#countrycode
library(countrycode)
all_mortality$ISO<-countrycode(all_mortality$Country.Name,origin="country.name",destination="iso3c")

