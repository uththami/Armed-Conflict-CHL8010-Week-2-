library(here)
here()
finaldata<-read.csv(here("finaldata.csv"),header=TRUE)
attach(finaldata)
loggdp<-log(gdp1000)
finaldata<-finaldata %>% mutate(loggdp=log(gdp1000))
head(finaldata)
library(plm)
library(dplyr)
library(mice)

midata<-finaldata |>
  mutate(ISOnum=as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name,-ISO)
mice0<-mice(midata,seed=100,m=5,maxit=0,print=F)
meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "popdens")] <- "2l.lmer"
pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "popdens"), "ISOnum"] <- -2
mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)
plot(mice.multi.out)

mimatmor<-with(mice.multi.out,lm(matmor~armconf1 + loggdp + OECD + popdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + earthquake + drought, data=finaldata))
miun5mor<-with(mice.multi.out,lm(un5mor~armconf1 + loggdp + OECD + popdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + earthquake + drought, data=finaldata))
miinfmor<-with(mice.multi.out,lm(infmor~armconf1 + loggdp + OECD + popdens + urban + 
                                     agedep + male_edu + temp + rainfall1000 + earthquake + drought, data=finaldata))
mineomor<-with(mice.multi.out,lm(neomor~armconf1 + loggdp + OECD + popdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + earthquake + drought, data=finaldata))

preds <- as.formula(" ~ armconf1 + loggdp + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought")
matmormod <- plm(update.formula(preds, matmor ~ .),index=c("ISO","year"),effect="twoways",model="within",data=finaldata)
un5mormod <- plm(update.formula(preds, un5mor ~ .),index=c("ISO","year"),effect="twoways",model="within",data=finaldata)
infmormod <- plm(update.formula(preds, infmor ~ .),index=c("ISO","year"),effect="twoways",model="within",data=finaldata)
neomormod <- plm(update.formula(preds, neomor ~ .),index=c("ISO","year"),effect="twoways",model="within",data=finaldata)

library(texreg)
screenreg(list(matmormod,un5mormod,infmormod,neomormod,mimatmor,miun5mor,miinfmor,mineomor),
          custom.model.names=c("CC Maternal Mortality","CC Under 5 Mortality","CC Infant Mortality","CC Neonatal Mortality",
                               "MI Maternal Mortality","MI Under 5 Mortality","MI Infant Mortality","MI Neonatal Mortality"))
CCtable<-screenreg(list(matmormod,un5mormod,infmormod,neomormod),custom.model.names=c("CC Maternal Mortality","CC Under 5 Mortality","CC Infant Mortality","CC Neonatal Mortality"))
MItable<-screenreg(list(pool(mimatmor),pool(miun5mor),pool(miinfmor),pool(mineomor)),custom.model.names=c("MI Maternal Mortality","MI Under 5 Mortality","MI Infant Mortality","MI Neonatal Mortality"))
print(CCtable,MItable)
