library(ggplot2)
library(zoo)
library(stargazer)
library(gridExtra)
library(reshape2)
library(effects)

#win
setwd("C:/Users/fh/Documents/GitHub/thesis/rawdata")
#mac
#setwd("~/Google Drive/InProgress/ParliamentBias")

############################
# JUMP TO LINE 300
############################

#get 2014 documents (different format)
require(XML)
doc <- htmlParse("afdb14.htm", useInternalNodes=T)
content <- doc["//meta/@content"]
content<-as.character(content[33:9764]) #this part is the actual debate
ftab14af<-data.frame(1:length(content),rawcontent=content)

#remove later date stamps
require(lubridate)
ftab14af$isstamp<-ifelse(substr(ftab14af$rawcontent,1,4)=="2014",1,0)
ftab14af$date<-ifelse(ftab14af$isstamp==1,ymd(substr(ftab14af$rawcontent,1,10)),NA)
ftab14af$laterdate<-ifelse(ftab14af$date>ftab14af$date[8],1,0)
ftab14af$laterdate[is.na(ftab14af$laterdate)]<-0
ftab14af<-subset(ftab14af,laterdate!=1 & rawcontent!="Proofed")

#shift function for lag/leads (crazy that this isn't in base R)
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

ftab14af$midstamp<-ifelse(shift(ftab14af$isstamp,1)==1 & shift(ftab14af$isstamp,-1)==1,1,0)
ftab14af<-subset(ftab14af,midstamp==0)

#find breaks (easier to handle manually)
as.numeric(rownames(subset(test,content=="Pause")))
breakrows<-c(1688:1695,5882:5889)

#remove breakrows
ftab14af<-ftab14af[-breakrows,]

#merge in variable names
fields<-c("speakerID","firstname","lastname","party","title","starttime","endtime")
ftab14af<-data.frame(field=c(rep(fields,nrow(ftab14af)/length(fields))),content=ftab14af$rawcontent)

#cast to wide form 
require(reshape2)
ftab14af$obs<-sort(rep(1:(nrow(ftab14af)/length(fields)),length(fields)))
ftab14af<-dcast(ftab14af,obs~field,value.var="content")


#parse html documents
ftab11af<-readLines("afdb11.html",encoding="UTF-8")
ftab12af<-readLines("afdb12.htm",encoding="UTF-8")
ftab13af<-readLines("afdb13.htm",encoding="UTF-8")

ftab10ab<-readLines("abdb10.htm",encoding="UTF-8")
ftab11ab<-readLines("abdb11.htm",encoding="UTF-8")
ftab12ab<-readLines("abdb12.htm",encoding="UTF-8")
ftab13ab<-readLines("abdb13.htm",encoding="UTF-8")



ftab11af[100:125]
ftab12af[100:125]
ftab13af[100:125]

ftab10ab[1:150]
ftab11ab[95:105]
ftab12ab[95:105]
ftab13ab[95:105]

#start at which line?
lstartaf<-115
lstartab<-100

#how many speeches?
nspeechs11af<-1225
nspeechs12af<-1309
nspeechs13af<-1533

nspeechs10ab<-999
nspeechs11ab<-1372
nspeechs12ab<-1077
nspeechs13ab<-1289

#which lines to read?
speechlines11af<-seq(from=lstartaf+3,to=lstartaf+3+nspeechs11af*2-4,by=2)
speechlines12af<-seq(from=lstartaf,to=lstartaf+nspeechs12af*2-4,by=2)
speechlines13af<-seq(from=lstartaf,to=lstartaf+nspeechs13af*2-4,by=2)

speechlines10ab<-seq(from=lstartab,to=lstartab+nspeechs10ab*2-4,by=2)
speechlines11ab<-seq(from=lstartab,to=lstartab+nspeechs11ab*2-4,by=2)
speechlines12ab<-seq(from=lstartab,to=lstartab+nspeechs12ab*2-4,by=2)
speechlines13ab<-seq(from=lstartab,to=lstartab+nspeechs13ab*2-4,by=2)

#parse out speechitems
ftab<-c(ftab10ab[speechlines10ab],ftab11af[speechlines11af],ftab11ab[speechlines11ab],ftab12af[speechlines12af],ftab12ab[speechlines12ab],ftab13af[speechlines13af],ftab13ab[speechlines13ab])

ftabaf<-c(ftab11af[speechlines11af],ftab12af[speechlines12af],ftab13af[speechlines13af])

write(ftab,file="ftab.txt")

#remove other objects
rm(ftab10ab,ftab11ab,ftab11af,ftab12ab,ftab12af,ftab13af,ftab13ab)

#clean up
ftab<-gsub("new SpeachItem","",ftab,fixed=T)
ftab<-gsub("\"","",ftab,fixed=T)
strsplit(ftab[1],",")

#create data
ftdt<-data.frame(no=seq(1:length(ftab)))
ftdt$statxt<-as.character("")
ftdt$endtxt<-as.character("")
ftdt$spktxt<-as.character("")
ftdt$type<-as.character("")

strsplit(ftab[20],",")[[1]]
strsplit(ftab[20],",")[[1]][3]
strsplit(ftab[20],",")[[1]][5]
strsplit(ftab[20],",")[[1]][6]

for (i in 1:nrow(ftdt)){
  ftdt$statxt[i]<-strsplit(ftab[i],",")[[1]][3]
  ftdt$endtxt[i]<-strsplit(ftab[i],",")[[1]][4]
  ftdt$spktxt[i]<-strsplit(ftab[i],",")[[1]][5]
  ftdt$type[i]<-strsplit(ftab[i],",")[[1]][6]
}

#reformat non-ascii letters - we need this for later
ftdt$spktxt<-gsub("æ","ae",ftdt$spktxt)
ftdt$spktxt<-gsub("ø","oe",ftdt$spktxt)
ftdt$spktxt<-gsub("å","aa",ftdt$spktxt)
ftdt$spktxt<-gsub("Æ","Ae",ftdt$spktxt)
ftdt$spktxt<-gsub("Ø","Oe",ftdt$spktxt)
ftdt$spktxt<-gsub("Å","Aa",ftdt$spktxt)
ftdt$type<-gsub("æ","ae",ftdt$type)
ftdt$type<-gsub("ø","oe",ftdt$type)

#clean up speech type variable
ftdt$type<-gsub(")","",ftdt$type)

#get speech length in secs
ftdt$secs<-as.numeric(strptime(ftdt$endtxt,"%Y/%m/%d %T")-strptime(ftdt$statxt,"%Y/%m/%d %T"))


#create variable for chairman and pm
ftdt$chair<-NA
#ftdt$pm<-NA
for (i in 1:nrow(ftdt)){
  ftdt$chair[i]<-ifelse(length(agrep("formand",ftdt$spktxt[i]))==1,1,0)
  #  ftdt$pm[i]<-ifelse(length(agrep("Statsministeren",ftdt$spktxt[i]))==1,1,0)
}

#create variable for party
ftdt$party<-NA
for (i in 1:nrow(ftdt)){
  ftdt$party[i]<-ifelse(ftdt$chair[i]==1,"Chair",strsplit(ftdt$spktxt[i],"(",fixed=T)[[1]][2]
  )
}

ftdt$party<-gsub(")","",ftdt$party,fixed=T)

#save this version to compare with scraping of old format
saveRDS(ftdt,file="ftdt.rds")

#fill in parties for chairmen - CAREFUL with this, can mess the whole thing up if not coded correctly
table(ftdt$spktxt[ftdt$chair==1])
spkrnames<-names(table(ftdt$spktxt[ftdt$chair==1]))
spkrnames

ftdt$party[ftdt$spktxt==spkrnames[1]]<-"DF"
ftdt$party[ftdt$spktxt==spkrnames[2]]<-"DF"
ftdt$party[ftdt$spktxt==spkrnames[3]]<-"DF"
ftdt$party[ftdt$spktxt==spkrnames[4]]<-"S"
ftdt$party[ftdt$spktxt==spkrnames[5]]<-"S"
ftdt$party[ftdt$spktxt==spkrnames[6]]<-"SF"
ftdt$party[ftdt$spktxt==spkrnames[7]]<-"KF"
ftdt$party[ftdt$spktxt==spkrnames[8]]<-"SF"
ftdt$party[ftdt$spktxt==spkrnames[9]]<-"V"
ftdt$party[ftdt$spktxt==spkrnames[10]]<-"S"
ftdt$party[ftdt$spktxt==spkrnames[11]]<-"S"
ftdt$party[ftdt$spktxt==spkrnames[12]]<-"V"
ftdt$party[ftdt$spktxt==spkrnames[13]]<-"RV"
ftdt$party[ftdt$spktxt==spkrnames[14]]<-"SF"
ftdt$party[ftdt$spktxt==spkrnames[15]]<-"RV"


#full name in FT data - sluggish for loop, but probably necessary
ftdt$full_name<-NA
for (i in 1:nrow(ftdt)){
  if (ftdt$chair[i]==0){
    ftdt$full_name[i]<-substr(ftdt$spktxt[i],1,nchar(ftdt$spktxt[i])-4)
    ftdt$full_name[i]<-gsub("\\s+$","",ftdt$full_name[i])
  }
  if (ftdt$chair[i]==1){
    ftdt$full_name[i]<-gsub(".*\\(|\\).*", "", ftdt$spktxt[i])
  }
}

#get starttime and endtime
require(lubridate)
ftdt$starttime<-ymd_hms(ftdt$statxt)
ftdt$endtime<-ymd_hms(ftdt$endtxt)

ft10to13<-data.frame(fullname=ftdt$full_name,
                     party=ftdt$party,
                     starttime=ftdt$starttime,
                     endtime=ftdt$endtime,
                     chair=ftdt$chair)

saveRDS(ft10to13,file="../data/ft10to13.rds")