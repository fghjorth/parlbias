require(dplyr)
require(magrittr)

#win
setwd("C:/Users/fh/Documents/GitHub/parlbias/rawdata")
#mac
#setwd("~/Google Drive/InProgress/ParliamentBias")

#get 2014 and 2015 documents (different format from earlier)
require(XML)
doc <- htmlParse("afdb14.htm", useInternalNodes=T)
content <- doc["//meta/@content"]
content<-as.character(content[33:9764]) #this part is the actual debate
ftaf14<-data.frame(no=1:length(content),rawcontent=content)

doc <- htmlParse("abdb14.htm", useInternalNodes=T)
content <- doc["//meta/@content"]
content<-as.character(content[33:10495]) #this part is the actual debate
ftab14<-data.frame(no=1:length(content),rawcontent=content)

#the closing debate in 2015 (ftaf15) did not take place because of the election

doc <- htmlParse("abdb15.htm", useInternalNodes=T)
content <- doc["//meta/@content"]
content<-as.character(content[68:10675]) #this part is the actual debate
ftab15<-data.frame(no=1:length(content),rawcontent=content)


#remove later date stamps
require(lubridate)
ftaf14$isstamp<-ifelse(substr(ftaf14$rawcontent,1,4)=="2014",1,0)
ftaf14$date<-ifelse(ftaf14$isstamp==1,ymd(substr(ftaf14$rawcontent,1,10)),NA)
ftaf14$laterdate<-ifelse(ftaf14$date>ftaf14$date[8],1,0)
ftaf14$laterdate[is.na(ftaf14$laterdate)]<-0
ftaf14<-subset(ftaf14,laterdate!=1 & rawcontent!="Proofed")

ftab14$isstamp<-ifelse(substr(ftab14$rawcontent,1,4)=="2014",1,0)
ftab14$date<-ifelse(ftab14$isstamp==1,ymd(substr(ftab14$rawcontent,1,10)),NA)
ftab14$laterdate<-ifelse(ftab14$date>ftab14$date[10463],1,0)
ftab14$laterdate[is.na(ftab14$laterdate)]<-0
ftab14<-subset(ftab14,laterdate!=1 & rawcontent!="Proofed")

ftab15$isstamp<-ifelse(substr(ftab15$rawcontent,1,4)=="2015",1,0)
ftab15$date<-ifelse(ftab15$isstamp==1,ymd(substr(ftab15$rawcontent,1,10)),NA)
ftab15$laterdate<-ifelse(ftab15$date>ftab15$date[8] | substr(ftab15$rawcontent,1,4)=="2016",1,0)
ftab15$laterdate[is.na(ftab15$laterdate)]<-0
ftab15<-subset(ftab15,laterdate!=1 & rawcontent!="Proofed")



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

ftaf14$midstamp<-ifelse(shift(ftaf14$isstamp,1)==1 & shift(ftaf14$isstamp,-1)==1,1,0)
ftaf14<-subset(ftaf14,midstamp==0)

ftab14$midstamp<-ifelse(shift(ftab14$isstamp,1)==1 & shift(ftab14$isstamp,-1)==1,1,0)
ftab14<-subset(ftab14,midstamp==0)

ftab15$midstamp<-ifelse(shift(ftab15$isstamp,1)==1 & shift(ftab15$isstamp,-1)==1,1,0)
ftab15<-subset(ftab15,midstamp==0)



#find breaks (easier to handle manually)
rownames(ftaf14)<-1:nrow(ftaf14)
rownames(ftab14)<-1:nrow(ftab14)
rownames(ftab15)<-1:nrow(ftab15)

as.numeric(rownames(subset(ftaf14,rawcontent=="Pause")))
as.numeric(rownames(subset(ftab14,rawcontent=="Pause")))
as.numeric(rownames(subset(ftab15,rawcontent=="Pause")))

breakrowsaf14<-c(1688:1695,5882:5889)
breakrowsab14<-c(1324:1339,5085:5092)
breakrowsab15<-c(1789:1796,7073:7979)

#remove breakrows
ftaf14<-ftaf14[-breakrowsaf14,]
ftab14<-ftab14[-breakrowsab14,]
ftab15<-ftab15[-breakrowsab15,]

#gather data
ft14to15<-as.data.frame(rbind(ftaf14,ftab14[1:8050,],ftab15[1:8335,]))

#filter out rows that say 'Released'
ft14to15<-filter(ft14to15,rawcontent!="Released")

#filter out triple time stamps
require(lubridate)

ft14to15$triplestamp<-0
ft14to15$year<-0
for (i in 2:nrow(ft14to15)){
  if (ft14to15$isstamp[i]==1){
    ft14to15$year[i]<-year(ymd(substr(as.character(ft14to15$rawcontent[i]),1,10)))
  }
  if (ft14to15$isstamp[i]==1 & ft14to15$isstamp[i-1]==0){
    t0<-hms(substr(as.character(ft14to15$rawcontent[i]),12,19))
    t1<-hms(substr(as.character(ft14to15$rawcontent[i+1]),12,19))
    if (t1<t0 & ft14to15$year[i]==2015){
      ft14to15$triplestamp[i]<-1
    }
  }
}
ft14to15<-filter(ft14to15,triplestamp!=1)

#remove some specific sections where breaks create several time stamps
ft14to15$quadruplestamp<-0
for (i in 1:nrow(ft14to15)){
  ft14to15$quadruplestamp[i]<-ft14to15$isstamp[i]*ft14to15$isstamp[i+3]
}

which(ft14to15$quadruplestamp==1)
wrongstamps<-c(16297:16305,17111:17119,17904:17912,19488:19496,20176:20184,20864:20872,21713:21720,21728:21736,21842:21844,21901:21909,22029:22047)
               
#merge in variable names
fields<-c("speakerID","firstname","lastname","party","title","starttime","endtime")
rawcontent<-as.character(ft14to15$rawcontent[-wrongstamps])

ft14to15df<-data.frame(field=c(rep(fields,length(rawcontent)/length(fields))),content=rawcontent)

#cast to wide form 
require(reshape2)
ft14to15$obs<-sort(rep(1:(nrow(ft14to15)/length(fields)),length(fields)))
ft14to15<-dcast(ft14to15,obs~field,value.var="content")


#reformat non-ascii letters - we need this for later
ft14to15$fullname<-paste(ft14to15$firstname,ft14to15$lastname)
Encoding(ft14to15$fullname)<-"UTF-8"

ft14to15$fullname<-gsub("æ","ae",ft14to15$fullname)
ft14to15$fullname<-gsub("ø","oe",ft14to15$fullname)
ft14to15$fullname<-gsub("å","aa",ft14to15$fullname)
ft14to15$fullname<-gsub("Æ","Ae",ft14to15$fullname)
ft14to15$fullname<-gsub("Ø","Oe",ft14to15$fullname)
ft14to15$fullname<-gsub("Å","Aa",ft14to15$fullname)

#save data
ft14to15<-data.frame(fullname=ft14to15$fullname,
                 party=ft14to15$party,
                 starttime=ymd_hms(ft14to15$starttime),
                 endtime=ymd_hms(ft14to15$endtime),
                 chair=ifelse(ft14to15$title=="formand",1,0))

saveRDS(ft14to15,file="../data/ft14to15.rds")


# #parse html documents
# ftab11af<-readLines("afdb11.html",encoding="UTF-8")
# ftab12af<-readLines("afdb12.htm",encoding="UTF-8")
# ftab13af<-readLines("afdb13.htm",encoding="UTF-8")
# 
# ftab10ab<-readLines("abdb10.htm",encoding="UTF-8")
# ftab11ab<-readLines("abdb11.htm",encoding="UTF-8")
# ftab12ab<-readLines("abdb12.htm",encoding="UTF-8")
# ftab13ab<-readLines("abdb13.htm",encoding="UTF-8")