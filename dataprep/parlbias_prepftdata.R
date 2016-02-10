require(lubridate)
require(dplyr)
require(magrittr)
require(ggplot2)
require(reshape2)
require(stargazer)
require(readr)
require(zoo)

#win
#setwd("C:/Users/fh/Documents/GitHub/parlbias/data")
#mac
setwd("~/GitHub/parlbias/data")

#read in data
ft<-rbind(readRDS(file="ft10to13.rds"),readRDS(file="ft14to15.rds"))

#clean up data a bit
#assign PM's to parties
table(ft$party)
ft$party[substr(as.character(ft$party),1,22)=="Helle Thorning-Schmidt"]<-"S"
ft$party[substr(as.character(ft$fullname),1,22)=="Helle Thorning-Schmidt"]<-"S"
ft$party[ft$party=="Lars Loekke Rasmussen"]<-"V"
ft$party[ft$fullname=="Lars Loekke Rasmussen"]<-"V"
ft$party<-as.character(ft$party)
table(ft$party)

#measure of seconds
ft$secs<-as.numeric(difftime(ft$endtime,ft$starttime))

#does speaking time exceed the limit?
ft$secsgt60<-ifelse(ft$secs>60,1,0)

#remove breaks
ft<-ft[-which(is.na(ft$party) & ft$secs>300),]

#code the party and name of the presiding chairman
ft$chairparty<-ifelse(ft$chair==1,as.character(ft$party),NA)
ft$chairname<-ifelse(ft$chair==1,as.character(ft$fullname),NA)

#fill in empty slots
ft$chairparty<-na.locf(ft$chairparty)
ft$chairname<-na.locf(ft$chairname)



#variable for whether the speaker is ingroup wrt the chairman
ft$copartisan<-ifelse(as.character(ft$party)==ft$chairparty,1,0)
ft$copartisan<-ifelse(ft$chair==1,NA,ft$copartisan)

# #cutpoint of 150 secs very effectively weeds out spokesperson speeches
# require(ggplot2)
# ggplot(ft,aes(x=secs)) +
#   geom_density() +
#   geom_vline(xintercept=150) +
#   theme_bw()

#get time of day
ft$timeofday<-hour(ft$starttime)+minute(ft$starttime)/60+second(ft$starttime)/3600
ft$timeofday<-ifelse(ft$timeofday<8,ft$timeofday+24,ft$timeofday)
summary(ft$timeofday)

#speech length by time of day
ggplot(ft,aes(x=timeofday,y=secs)) +
  geom_point(alpha=.2) +
  geom_smooth() +
  theme_bw()

#dummy for whether speaker party is in parl. leadership ("pr?sidiet")
ft$leadshipparty<-ifelse(ft$party %in% names(table(ft$chairparty)),1,0)
table(ft$party[ft$leadshipparty==1])

#create simpler party variable
ft$coarseparty<-ifelse(ft$party %in% c("IA","JF","SIU","SP","T","TF","KD","UFG"),"Other",ft$party)

#is the pm speaking?
subset(ft,substr(ft$fullname,1,nchar("Statsministeren"))=="Statsministeren")
ft$pm<-ifelse(substr(ft$fullname,1,nchar("Statsministeren"))=="Statsministeren",1,0)

#fix prime minister names
pmfullnames<-as.character(names(table(droplevels(ft$fullname[ft$pm==1]))))
ft$fullname[ft$fullname==pmfullnames[1]]<-"Helle Thorning-Schmidt"
ft$fullname[ft$fullname==pmfullnames[2]]<-"Lars Loekke Rasmussen"
ft$fullname[ft$fullname==pmfullnames[3]]<-"Helle Thorning-Schmidt"




#get parties placements from voter estimates in most recent election survey
FV11<-read.csv("../rawdata/ElectionStudy-2011_F1.csv",sep=",",dec=".")
FV11<-FV11 %>%
  select(num_range("v",228:235)) %>%
  melt() %>%
  filter(value<11) %>%
  group_by(variable) %>%
  summarise(intpos=mean(value)) %>%
  mutate(ordpos=rank(intpos),party=c("S","RV","KF","SF","LA","DF","V","EL")) %>%
  arrange(ordpos) %>%
  select(intpos:party)

#assign interval and ordinal positions to speakers and chairmen
ft$spkrintpos<-NA
ft$chairintpos<-NA
ft$spkrordpos<-NA
ft$chairordpos<-NA

for(i in 1:nrow(ft)){
  if(ft$party[i] %in% FV11$party){
    ft$spkrintpos[i]<-subset(FV11,party==ft$party[i])$intpos
    ft$spkrordpos[i]<-subset(FV11,party==ft$party[i])$ordpos
  }
  if(ft$chairparty[i] %in% FV11$party){
    ft$chairintpos[i]<-subset(FV11,party==ft$chairparty[i])$intpos
    ft$chairordpos[i]<-subset(FV11,party==ft$chairparty[i])$ordpos
  }
}

#calculate political distance measure, based on voter placements in FV11
ft$intposdif<-ifelse(ft$chair==0,abs(ft$spkrintpos-ft$chairintpos),NA)
ft$ordposdif<-ifelse(ft$chair==0,abs(ft$spkrordpos-ft$chairordpos),NA)

#bloc variable
table(ft$party)
table(ft$chairparty)
ft$leftspkr<-ifelse(ft$party=="S" | ft$party=="RV" | ft$party=="SF" | ft$party=="EL",1,0)
ft$leftchair<-ifelse(ft$chairparty=="S"| ft$chairparty=="SF" | ft$chairparty=="RV",1,0)
ft$cobloc<-abs(abs(ft$leftspkr-ft$leftchair)-1)
ft$cobloc[ft$chair==1]<-NA

#factor for debate type
ft$debatetype<-"Closing"
ft$debatetype[month(ft$starttime)==10]<-"Opening"

#factor for specific debate
names(ft)
ft<-ft %>%
  mutate(debate=paste(debatetype,year(starttime),sep=" "))
table(ft$debate)

#three odd time stamps (all chairmen, so no effect on results)
subset(ft,debate=="Closing 1900")

#read in background data on politicians
require(XML)
pols<-xmlParse("http://hvemstemmerhvad.dk/api/api_politikere.php")
pols<-xmlToDataFrame(pols)[1:491,]
pols$fullname<-as.character(pols$fulde_navn,Encoding="UTF-8")
pols$fullname<-gsub("æ","ae",pols$fullname)
pols$fullname<-gsub("ø","oe",pols$fullname)
pols$fullname<-gsub("å","aa",pols$fullname)
pols$fullname<-gsub("Æ","Ae",pols$fullname)
pols$fullname<-gsub("Ø","Oe",pols$fullname)
pols$fullname<-gsub("Å","Aa",pols$fullname)
#add in gender variable
pols$female<-as.numeric(read.table("../rawdata/polsgenders.txt")$V1)
pols<-select(pols,fullname,female)

polsgenders<-data.frame(fullname=unique(ft$fullname)) %>%
  left_join(.,pols,by="fullname")

# #export the gender data frame so I can edit it manually
# write_csv(polsgenders,"../rawdata/polsgenders2.csv")

#read back in edited polsgenders
polsgenders2<-read_csv("../rawdata/polsgenders2.csv")

#merge in
ft<-left_join(ft,polsgenders2,by="fullname")

# #calculate age in years at time of speaking
# ft$speakerage<-(year(ft$starttime)+month(ft$starttime)/12)-(year(ymd(ft$birthday))+month(ymd(ft$birthday))/12)  


#save version for easy reload
ftall<-ft
saveRDS(ftall,"ftall.rds")
write_csv(ftall,"ftall.csv")
