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

ftall<-readRDS("ftall.rds")

#remove very short and very long speeches
ft<-subset(ftall,secs<150 & secs>10 & year(starttime)>2000)
ft<-arrange(ft,starttime)


#check sample of data
sample_n(ft,30)

ggplot(subset(ft,chair==0),aes(x=secs)) + geom_density()

ggplot(ft,aes(x=timeofday,y=secs)) + geom_point() + facet_grid(debate~.)

View(subset(ft,debate=="Opening 2015" & copartisan!=0))  

### TABLES

summary(m1<-lm(secs~copartisan,data=ft))
summary(m2<-lm(secs~copartisan+timeofday,data=ft))
summary(m3<-lm(secs~copartisan+timeofday+female,data=ft))
summary(m4<-lm(secs~copartisan+timeofday+female+factor(chairparty),data=ft))
summary(m5<-lm(secs~copartisan+timeofday+female+factor(coarseparty),data=ft))
summary(m6<-lm(secs~copartisan+timeofday+female+debate,data=ft))
summary(m7<-lm(secs~copartisan+timeofday+female+factor(coarseparty),data=subset(ft,leadshipparty==1)))

stargazer(m1,m2,m3,m4,m5,m6,m7,type="text",omit=c("factor","debate"),omit.stat=c("f","ser"))
stargazer(m1,m2,m3,m4,m5,m6,m7,type="text",omit=c("debate"),omit.stat=c("f","ser"))


summary(m4intposdifltmedian<-lm(secs~copartisan+pm+timeofday+debatetype+female+speakerage+factor(coarseparty)+factor(chairparty),data=subset(ft,intposdif<=2.34)))

summary(m4ordposdifltmedian<-lm(secs~copartisan+pm+timeofday+debatetype+female+speakerage+factor(coarseparty)+factor(chairparty),data=subset(ft,ordposdif<=2)))

summary(m4cobloc<-lm(secs~copartisan+pm+timeofday+debatetype+female+speakerage+factor(coarseparty)+factor(chairparty),data=subset(ft,cobloc==1)))

effectests<-data.frame(est=rep(NA,10),se=rep(NA,10),method=c(rep("OLS",5),rep("Logit",5)),model=rep(1:5,2))
effectests[1,1:2]<-summary(m1)$coefficients[2,1:2]
effectests[2,1:2]<-summary(m2)$coefficients[2,1:2]
effectests[3,1:2]<-summary(m3)$coefficients[2,1:2]
effectests[4,1:2]<-summary(m4)$coefficients[2,1:2]
effectests[5,1:2]<-summary(m5)$coefficients[2,1:2]

#what is the effect size in standardized terms? get cohen's d
cohensd <- effectests$est / sd(ft$secs)
barplot(cohensd)

head(ft)
require(dplyr)
require(lubridate)

#get average number of speeches
lengths<-ft %>%
  group_by(year(starttime),debatetype,chairname) %>%
  summarise(speeches=sum(chair)) %>%
  filter(speeches>0)

meanspeechesperchair<-mean(lengths$speeches)

#how many minutes does 2.8 seconds add up to over the average debate per chair?
effectests$est[4]*meanspeechesperchair/60

summary(m1.logit<-glm(secsgt60~copartisan,data=ft,family=binomial()))
summary(m2.logit<-glm(secsgt60~copartisan+pm+timeofday,data=ft,family=binomial()))
summary(m3.logit<-glm(secsgt60~copartisan+pm+timeofday+debatetype+female+speakerage,data=ft,family=binomial()))
summary(m4.logit<-glm(secsgt60~copartisan+pm+timeofday+debatetype+female+speakerage+factor(coarseparty)+factor(chairparty),data=ft,family=binomial()))
summary(m5.logit<-glm(secsgt60~copartisan+pm+timeofday+debatetype+female+speakerage+factor(coarseparty)+factor(chairparty),data=subset(ft,leadshipparty==1),family=binomial()))

require(mfx)
effectests[6,1:2]<-logitmfx(m1.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[7,1:2]<-logitmfx(m2.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[8,1:2]<-logitmfx(m3.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[9,1:2]<-logitmfx(m4.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[10,1:2]<-logitmfx(m5.logit$formula,data=subset(ft,leadshipparty==1))$mfxest[1,1:2]*100

require(rms)
m1rob<-robcov(ols(secs~copartisan,data=ft,x=T,y=T),cluster=ft$chairname)
m2rob<-robcov(ols(secs~copartisan+pm+timeofday,data=ft,x=T,y=T),cluster=ft$chairname)
m3rob<-robcov(ols(secs~copartisan+pm+timeofday+debatetype+female+speakerage,data=ft,x=T,y=T),cluster=ft$chairname)
m4rob<-robcov(ols(secs~copartisan+pm+timeofday+debatetype+female+speakerage+factor(coarseparty)+factor(chairparty),data=ft,x=T,y=T),cluster=ft$chairname)
m5rob<-robcov(ols(secs~copartisan+pm+timeofday+debatetype+female+speakerage+factor(coarseparty)+factor(chairparty),data=subset(ft,leadshipparty==1),x=T,y=T),cluster=subset(ft,leadshipparty==1)$chairname)

print(m4rob)
print(m5rob)

?rms
require(stargazer)
checkmarks<-c("Speaker party fixed effects & & & & \\checkmark & \\checkmark \\\\", "Chair party fixed effects & & & & \\checkmark & \\checkmark \\\\", "Restricted sample & & & & & \\checkmark \\\\")
?stargazer
regtab1<-stargazer(m1rob,m2rob,m3rob,m4rob,m5rob,style="apsr",omit.stat=c("f","chi2"),omit=7:21,dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",label="parlbias_regtab1",column.sep.width="5pt",covariate.labels=c("Copartisan","Prime minister","Time of day","Debate type: Opening","Gender (female)","Age of speaker","Intercept"),star.cutoffs=c(.1,.05,.01),align=T,title="OLS models of speaking time",digits=2) #,type="text")
length(regtab1)
regtab1[30]
regtab1<-c(regtab1[1:27],checkmarks,regtab1[28:34])
writeLines(regtab1,con="../tables/parlbias_regtab1.txt")

regtab1logit<-stargazer(m1.logit,m2.logit,m3.logit,m4.logit,m5.logit,style="apsr",omit.stat=c("f","chi2"),omit=7:21,dep.var.labels="Dummy: speaking time exceeds 60 seconds",dep.var.labels.include=T,font.size="footnotesize",label="parlbias_regtab1logit",column.sep.width="5pt",covariate.labels=c("Copartisan","Prime minister","Time of day","Debate type: Opening","Gender (female)","Age of speaker","Intercept"),star.cutoffs=c(.1,.05,.01),align=T,title="Logit models of exceeding standard speaking time",digits=2) #,type="text")
regtab1logit<-c(regtab1logit[1:27],checkmarks,regtab1logit[28:34])
writeLines(regtab1logit,con="../tables/parlbias_regtab1logit.txt")

modscolumnlabels<-c("Full","Distance$\\leq$median (int.)","Distance$\\leq$median (ord.)","Same bloc")

regtabmods<-stargazer(m4,m4intposdifltmedian,m4ordposdifltmedian,m4cobloc,style="apsr",omit.stat=c("f","chi2"),df=F,omit=7:21,dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",label="parlbias_regtabmods",column.sep.width="5pt",covariate.labels=c("Copartisan","Prime minister","Time of day","Debate type: Opening","Gender (female)","Age of speaker","Intercept"),star.cutoffs=c(.1,.05,.01),align=T,title="Tests of political moderators",digits=2,column.labels=modscolumnlabels)

regtabmods<-c(regtabmods[1:28],c("Speaker party fixed effects & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\", "Chair party fixed effects & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\"),regtabmods[28:36])

writeLines(regtabmods,con="../tables/parlbias_regtabmods.txt")

modeffectests<-data.frame(est=rep(NA,4),se=rep(NA,4),model=modscolumnlabels)
modeffectests[1,1:2]<-summary(m4)$coefficients[2,1:2]
modeffectests[2,1:2]<-summary(m4intposdifltmedian)$coefficients[2,1:2]
modeffectests[3,1:2]<-summary(m4ordposdifltmedian)$coefficients[2,1:2]
modeffectests[4,1:2]<-summary(m4cobloc)$coefficients[2,1:2]

#sub in <= for latex symbols
modeffectests$model<-modeffectests$model %>%
  as.character() %>%
  function(x){gsub("\\$\\\\leq\\$","<=",x)}

### SUMMARY STATS TABLES
ftall$type<-NA
ftall$type[ftall$secs<150]<-"Brief remark"
ftall$type[ftall$secs>=150]<-"Spokesperson speech"
ftall$type[ftall$secs>=150 & ftall$pm==1]<-"PM speech"

require(dplyr)

totaln<-nrow(subset(ftall,chair==0 & year(starttime)>2000))
totalsecs<-sum(subset(ftall,chair==0 & year(starttime)>2000)$secs)

sumstatstab<- subset(ftall,chair==0 & year(starttime)>2000) %>%
  group_by(type) %>%
  summarise(count=n(),nshare=100*(count/totaln),secshare=100*sum(secs)/totalsecs)

sumstatstabtex<-stargazer(sumstatstab,summary=F,digits=1,title="Types of speeches in opening and closing debates in the Folketing",label="parlbias_sumstatstab",font.size="footnotesize",align=T,colnames=T,rownames=F)

sumstatstabtex[12]<-"\\multicolumn{1}{l}{Type} & \\multicolumn{1}{r}{Number} & \\multicolumn{1}{r}{Share (numeric)} & \\multicolumn{1}{r}{Share (time-weighted)} \\\\ "

#awkward left alignment of labels
sumstatstabtex<-gsub("column\\{1\\}\\{c\\}\\{","column{1}{l}{",sumstatstabtex)

writeLines(sumstatstabtex,con="../tables/parlbias_sumstatstab.txt")


### PLOTS

#reorder factor levels
effectests<-within(effectests,method<-factor(method,levels=c("OLS","Logit")))

#plot estimates
ggplot(effectests2,aes(x=est,y=reorder(model,rep(5:1,2)))) +
  geom_point(size=2.5) +
  facet_grid(.~method,scales="free_x") +
  geom_errorbarh(aes(xmin=est-1.96*se,xmax=est+1.96*se),height=0,size=.5) +
  geom_errorbarh(aes(xmin=est-1.65*se,xmax=est+1.65*se),height=0,size=1.2) +
  expand_limits(x=0) +
  geom_vline(xintercept=0,linetype="dashed") +
  xlab("Estimate (seconds/percentage points)") +
  ylab("Model") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

ggsave(file="../figures/parlbias_effectplot.pdf",height=4,width=9)

ggplot(modeffectests,aes(x=est,y=reorder(model,c(4,3,2,1)))) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=est-1.96*se,xmax=est+1.96*se),height=0,size=.5) +
  geom_errorbarh(aes(xmin=est-1.65*se,xmax=est+1.65*se),height=0,size=1.2) +
  expand_limits(x=0) +
  geom_vline(xintercept=0,linetype="dashed") +
  xlab("Estimate (seconds)") +
  ylab("Model") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

ggsave(file="../figures/parlbias_modeffectplot.pdf",height=4,width=9)

ft$copartisan_factor<-factor(ft$copartisan,labels=c("Non-copartisan","Co-partisan"))
ft$copartisan_factor<-factor(ft$copartisan_factor,levels(ft$copartisan_factor)[c(2,1)])

ggplot(subset(ft,chair==0 & !is.na(copartisan)),aes(x=secs)) +
  geom_density(fill="gray",alpha=.5) +
  facet_grid(.~copartisan_factor) +
  geom_vline(xintercept=60,linetype="dashed") +
  xlab("Speech duration (seconds)") +
  ylab("") +
  scale_y_continuous(breaks=c(0,.005,.01,.015),labels=c("0",".005",".01",".015")) +
  scale_x_continuous(breaks=c(0,30,60,90,120),labels=c("0","30","60","90","120")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

ggsave(file="../figures/parlbias_dens.pdf",height=5,width=9)

plot(density(ft$timeofday))

### PLOTS


######################## UNUSED

# # Get data on first votes to create familiarity variable
# firstvotes<-read.csv("firstvotes.csv",sep="\t",stringsAsFactors=F,encoding="UTF-8")
# 
# #assign parties
# firstvotes$party[firstvotes$party_id==1]<-"S"
# firstvotes$party[firstvotes$party_id==2]<-"DF"
# firstvotes$party[firstvotes$party_id==3]<-"SF"
# firstvotes$party[firstvotes$party_id==4]<-"RV"
# firstvotes$party[firstvotes$party_id==5]<-"V"
# firstvotes$party[firstvotes$party_id==6]<-"EL"
# firstvotes$party[firstvotes$party_id==7]<-"KF"
# firstvotes$party[firstvotes$party_id==14]<-"LA"
# 
# #reformat non-ascii letters - we need this for later
# firstvotes$full_name<-gsub("æ","ae",firstvotes$full_name)
# firstvotes$full_name<-gsub("ø","oe",firstvotes$full_name)
# firstvotes$full_name<-gsub("å","aa",firstvotes$full_name)
# firstvotes$full_name<-gsub("Æ","Ae",firstvotes$full_name)
# firstvotes$full_name<-gsub("Ø","Oe",firstvotes$full_name)
# firstvotes$full_name<-gsub("Å","Aa",firstvotes$full_name)
# 
# #merge in
# names(firstvotes)[5]<-"fullname"
# names(firstvotes)[1]<-"mindate"
# ft<-merge(ft,firstvotes[,c(1,4,5,7)],by=c("fullname","party"),all.x=T)
# 
# ft$comptime<-difftime(ft$starttime,ymd(ft$mindate),units="days")

