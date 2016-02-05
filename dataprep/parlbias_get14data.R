
#win
setwd("C:/Users/fh/Documents/GitHub/thesis/rawdata")
#mac
#setwd("~/Google Drive/InProgress/ParliamentBias")

#get 2014 documents (different format)
require(XML)
doc <- htmlParse("afdb14.htm", useInternalNodes=T)
content <- doc["//meta/@content"]
content<-as.character(content[33:9764]) #this part is the actual debate
ftaf14<-data.frame(no=1:length(content),rawcontent=content)

doc <- htmlParse("abdb14.htm", useInternalNodes=T)
content <- doc["//meta/@content"]
content<-as.character(content[33:10495]) #this part is the actual debate
ftab14<-data.frame(no=1:length(content),rawcontent=content)


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

#find breaks (easier to handle manually)
rownames(ftaf14)<-1:nrow(ftaf14)
rownames(ftab14)<-1:nrow(ftab14)

as.numeric(rownames(subset(ftaf14,rawcontent=="Pause")))
as.numeric(rownames(subset(ftab14,rawcontent=="Pause")))

breakrowsaf<-c(1688:1695,5882:5889)
breakrowsab<-c(1324:1339,5085:5092)

#remove breakrows
ftaf14<-ftaf14[-breakrowsaf,]
ftab14<-ftab14[-breakrowsab,]

#gather data
ft14<-as.data.frame(rbind(ftaf14,ftab14[1:8050,]))

#merge in variable names
fields<-c("speakerID","firstname","lastname","party","title","starttime","endtime")
ft14<-data.frame(field=c(rep(fields,nrow(ft14)/length(fields))),content=ft14$rawcontent)

#cast to wide form 
require(reshape2)
ft14$obs<-sort(rep(1:(nrow(ft14)/length(fields)),length(fields)))
ft14<-dcast(ft14,obs~field,value.var="content")


#reformat non-ascii letters - we need this for later
ft14$fullname<-paste(ft14$firstname,ft14$lastname)
Encoding(ft14$fullname)<-"UTF-8"

ft14$fullname<-gsub("æ","ae",ft14$fullname)
ft14$fullname<-gsub("ø","oe",ft14$fullname)
ft14$fullname<-gsub("å","aa",ft14$fullname)
ft14$fullname<-gsub("Æ","Ae",ft14$fullname)
ft14$fullname<-gsub("Ø","Oe",ft14$fullname)
ft14$fullname<-gsub("Å","Aa",ft14$fullname)

#save data
ft14<-data.frame(fullname=ft14$fullname,
                 party=ft14$party,
                 starttime=ymd_hms(ft14$starttime),
                 endtime=ymd_hms(ft14$endtime),
                 chair=ifelse(ft14$title=="formand",1,0))

saveRDS(ft14,file="../data/ft14.rds")


#parse html documents
ftab11af<-readLines("afdb11.html",encoding="UTF-8")
ftab12af<-readLines("afdb12.htm",encoding="UTF-8")
ftab13af<-readLines("afdb13.htm",encoding="UTF-8")

ftab10ab<-readLines("abdb10.htm",encoding="UTF-8")
ftab11ab<-readLines("abdb11.htm",encoding="UTF-8")
ftab12ab<-readLines("abdb12.htm",encoding="UTF-8")
ftab13ab<-readLines("abdb13.htm",encoding="UTF-8")