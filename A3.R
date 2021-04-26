#remove variables
rm(list=ls())
#import needed libraries
library("bayesm")
library(tidyverse)
library(tidyr)
library(dplyr)
library(nnet)

df<-read.csv("C:\\Users\\kstan\\Documents\\613 data\\population.csv")
df2<-read.csv("C:\\Users\\kstan\\Documents\\613 data\\officers.csv")
df3<-read.csv("C:\\Users\\kstan\\Documents\\613 data\\crime_long.csv")
#Question 1 crimes by month
a=aggregate(df3['crimes'],by=df3['crime_month'],FUN=sum)
ggplot(data=a, aes(x=crime_month, y=crimes, group=1)) +
  geom_line()
#Question 2 create a merge
df6=left_join(df,df3,by=c("month"="crime_month","district"="district"))
#Question 3
temp=aggregate(df3['crimes'],by=c(df3['crime_month'],df3['district']),FUN=sum)
temp=temp %>% rename(tot_crime=crimes)
temp1=df3[(df3['crime_type']=='violent'),]
temp2=aggregate(temp1['crimes'],by=c(temp1['crime_month'],temp1['district']),FUN=sum)
temp2=temp2 %>% rename(viol_crime=crimes)
temp3=left_join(temp,temp2,by=c("crime_month","district"))
temp1=df3[(df3['crime_type']=='property'),]
temp2=aggregate(temp1['crimes'],by=c(temp1['crime_month'],temp1['district']),FUN=sum)
temp2=temp2 %>% rename(prop_crime=crimes)
temp3=left_join(temp3,temp2,by=c("crime_month","district"))
df4=left_join(df,temp3,by=c("district"="district","month"="crime_month"))
df4['perc_white']=df4['tot_white']/df4['tot_pop']
df4['perc_black']=df4['tot_black']/df4['tot_pop']
df4['perc_hisp']=df4['tot_hisp']/df4['tot_pop']
df5=df4%>%select('month','district','p50_inc','tot_crime','viol_crime','prop_crime','perc_white','perc_black','perc_hisp')

library(dummies)
temp=dummy(df2$unit)
df2[colnames(temp)]=temp

df2['month2']=format(as.Date(df2$month),"%m")
df2['year']=format(as.Date(df2$month),"%Y")

temp=unique(df2$month2)
for (x in temp){
  df2[as.character(x)]=as.numeric(df2$month2==x)
}
temp=unique(df2$year)
for (x in temp){
  df2[as.character(x)]=as.numeric(df2$year==x)
}

df6=left_join(df2,df5,by=c("unit"="district","month"="month"))
colnames(df6)
fit <- lm(arrest ~ tenure + tot_crime + p50_inc+perc_white+perc_black+perc_hisp, data=df6)
summary(fit)
a=""
for (x in colnames(df6))
{a=paste(paste(a,"+"),x)
}



# Just to indicate that I know how to code fixed effects, I would normally run the following, which is an indicator for each unit, each month, and each year (with one of each excluded for an intercept) but for size reasons, that doesn't run
fit <- lm(arrest ~ tenure + tot_crime + p50_inc+perc_white+perc_black+perc_hisp+unit1 + unit2 + unit3 + unit4 + unit5 + unit6 + unit7 + unit8 + unit9 + unit10 + unit11 + unit12 + unit13 + unit14 + unit15 + unit16 + unit17 + unit18 + unit19 + unit20 + unit21 + unit22 + unit23 + unit24 + `01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`2007`+`2008`+`2009`+`2010`+`2011`+`2012`+`2013`+`2014`+`2015`+`2016`, data=df6)
summary(fit)

temp=df6 %>% group_by(NUID) %>% summarize(tenurem=mean(tenure),arrestm=mean(arrest),p50_incm=mean(p50_inc),perc_whitem=mean(perc_white),perc_blackm=mean(perc_black),perc_hispm=mean(perc_hisp),tot_crimem=mean(tot_crime))
fit <- lm(arrestm ~ tenurem + tot_crimem + p50_incm+perc_whitem+perc_blackm+perc_hispm, data=temp)
summary(fit)

df7=left_join(df6,temp,by="NUID")
df7[c('arrestd','tenured','tot_crimed','p50_incd','perc_whited','perc_blackd','perc_hispd')]=df7[c('arrest','tenure','tot_crime','p50_inc','perc_white','perc_black','perc_hisp')]-df7[c('arrestm','tenurem','tot_crimem','p50_incm','perc_whitem','perc_blackm','perc_hispm')]
fit <- lm(arrestd ~ tenured + tot_crimed + p50_incd+perc_whited+perc_blackd+perc_hispd, data=df7)
summary(fit)

temp=df6[c('NUID','year','month2','arrest','tenure','tot_crime','p50_inc','perc_white','perc_black','perc_hisp')]
temp=arrange(temp,NUID,year,month2)
temp <- 
  temp %>%
  group_by(NUID) %>%
mutate(lag.arrest = dplyr::lag(arrest, n = 1, default = NA),lag.tenure = dplyr::lag(tenure, n = 1, default = NA),lag.tot_crime = dplyr::lag(tot_crime, n = 1, default = NA),lag.p50_inc = dplyr::lag(p50_inc, n = 1, default = NA),lag.perc_white = dplyr::lag(perc_white, n = 1, default = NA),lag.perc_black = dplyr::lag(perc_black, n = 1, default = NA),lag.perc_hisp = dplyr::lag(perc_hisp, n = 1, default = NA))
temp[c('arrestd','tenured','tot_crimed','p50_incd','perc_whited','perc_blackd','perc_hispd')]=temp[c('arrest','tenure','tot_crime','p50_inc','perc_white','perc_black','perc_hisp')]-temp[c('lag.arrest','lag.tenure','lag.tot_crime','lag.p50_inc','lag.perc_white','lag.perc_black','lag.perc_hisp')]
fit <- lm(arrestd ~ tenured + tot_crimed + p50_incd+perc_whited+perc_blackd+perc_hispd, data=temp)
summary(fit)




temp=unique(df6['NUID'])
row.names(temp) <- NULL
temp['unuid']=as.numeric(rownames(temp))
df6=left_join(df6,temp,by="NUID")


cols=c('tenure',"p50_inc" ,       "tot_crime",      "viol_crime" ,    "prop_crime"  ,   "perc_white"  , "perc_black"  ,   "perc_hisp" )

param=runif(length(cols)+length(unique(df6[,'year']))+length(unique(df6[,'unuid']))+length(unique(df6[,'month2']))+length(unique(df6[,'unit'])),0,1)


flike = function(param,df6,cols)
{
  year=param[as.numeric(df6[,'year'])-2006+length(cols)]
  unuid=param[as.numeric(df6[,'unuid'])-1+length(cols)+length(unique(df6[,'year']))]
  month=param[as.numeric(df6[,'month2'])-1+length(cols)+length(unique(df6[,'year']))+length(unique(df6[,'unuid']))]
  unit=param[as.numeric(df6[,'unit'])-1+length(cols)+length(unique(df6[,'year']))+length(unique(df6[,'unuid']))+length(unique(df6[,'month2']))]
  epsilon=df6['arrest']-(rowSums(df6[,cols]*param[1:length(cols)])+year+unuid+month+unit)

  q=c()
  for (x in cols){
    q=append(q,as.numeric(colMeans(epsilon*df6[x],na.rm=TRUE)))
  }
  
  return(-sum(q)-sum(year)-sum(unuid)-sum(month)-sum(unit))
}

res  = optim(param,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),df6=df6,cols=cols)



