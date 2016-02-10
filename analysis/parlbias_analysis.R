require(lubridate)
require(dplyr)
require(magrittr)
require(ggplot2)
require(reshape2)
require(stargazer)
require(readr)
require(tidyr)
require(broom)
require(lme4)
require(arm)

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


### REGRESSION MODELS

#define basic models
m1<-lm(m1f<-as.formula(secs~copartisan),data=ft)
m2<-lm(m2f<-as.formula(secs~copartisan+timeofday+female),data=ft)
m3<-lm(m3f<-as.formula(secs~copartisan+timeofday+female+factor(chairparty)),data=ft)
m4<-lm(m4f<-as.formula(secs~copartisan+timeofday+female+debate),data=ft)
m5<-lm(m5f<-as.formula(secs~copartisan+timeofday+female+debate+factor(chairparty)+factor(coarseparty)),data=ft)

stargazer(m1,m2,m3,m4,m5,type="text",omit=c("factor","debate"),omit.stat=c("f","ser"))
stargazer(m1,m2,m3,m4,m5,type="text",omit=c("debate"),omit.stat=c("f","ser"))

summary(mx<-lm(secs~copartisan+timeofday+female+factor(coarseparty)+factor(chairparty)+debate,data=ft))


summary(m5intposdifltmedian<-lm(m5f,data=subset(ft,intposdif<=2.34)))
summary(m5ordposdifltmedian<-lm(m5f,data=subset(ft,ordposdif<=2)))
summary(m5cobloc<-lm(m5f,data=subset(ft,cobloc==1)))

tidy(m1)

effectests<-data.frame(est=rep(NA,10),se=rep(NA,10),method=c(rep("OLS",5),rep("Logit",5)),model=rep(1:5,2))
effectests[1,1:2]<-tidy(m1)[2,2:3]
effectests[2,1:2]<-tidy(m2)[2,2:3]
effectests[3,1:2]<-tidy(m3)[2,2:3]
effectests[4,1:2]<-tidy(m4)[2,2:3]
effectests[5,1:2]<-tidy(m5)[2,2:3]

#what is the effect size in standardized terms? get cohen's d
cohensd <- effectests$est / sd(ft$secs)
barplot(cohensd)

#get average number of speeches
lengths<-ft %>%
  group_by(debate,chairname) %>%
  summarise(speeches=sum(chair)) %>%
  filter(speeches>0)

meanspeechesperchair<-mean(lengths$speeches)

#how many minutes does 2.8 seconds add up to over the average debate per chair?
effectests$est[4]*meanspeechesperchair/60

summary(m1.logit<-glm(secsgt60~copartisan,data=ft,family=binomial()))
summary(m2.logit<-glm(secsgt60~copartisan+timeofday+female,data=ft,family=binomial()))
summary(m3.logit<-glm(secsgt60~copartisan+timeofday+female+factor(chairparty)+factor(coarseparty),data=ft,family=binomial()))
summary(m4.logit<-glm(secsgt60~copartisan+timeofday+female+debate,data=ft,family=binomial()))
summary(m5.logit<-glm(secsgt60~copartisan+timeofday+female+debate+factor(chairparty)+factor(coarseparty),data=ft,family=binomial()))

require(mfx)
effectests[6,1:2]<-logitmfx(m1.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[7,1:2]<-logitmfx(m2.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[8,1:2]<-logitmfx(m3.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[9,1:2]<-logitmfx(m4.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[10,1:2]<-logitmfx(m5.logit$formula,data=ft)$mfxest[1,1:2]*100

#for the main models, cluster observations by presiding chair
require(rms)
m1rob<-robcov(ols(m1f,data=ft,x=T,y=T),cluster=ft$chairname)
m2rob<-robcov(ols(m2f,data=ft,x=T,y=T),cluster=ft$chairname)
m3rob<-robcov(ols(m3f,data=ft,x=T,y=T),cluster=ft$chairname)
m4rob<-robcov(ols(m4f,data=ft,x=T,y=T),cluster=ft$chairname)
m5rob<-robcov(ols(m5f,data=ft,x=T,y=T),cluster=ft$chairname)

#setup up varying slopes model to test how the bias varies by 
mlm3<-lmer(secs~copartisan+timeofday+female+(1|coarseparty)+(1+copartisan|chairparty),data=ft)
ranef(mlm3)
fixef(mlm3)
partyranefs<-data.frame(party=rownames(ranef(mlm3)$chairparty),coef=fixef(mlm3)[2]+ranef(mlm3)$chairparty[,2],se=se.ranef(mlm3)$chairparty[,2])
partyranefs

partyranefs<-ft %>%
  group_by(coarseparty) %>%
  summarise(ordpos=mean(chairordpos)) %>%
  rename(party=coarseparty) %>%
  left_join(partyranefs,.,by="party")

#varying slopes by chairman name
mlm3_chairs<-lmer(secs~copartisan+timeofday+female+(1|coarseparty)+(1+copartisan|chairname),data=ft)
ranef(mlm3_chairs)
fixef(mlm3_chairs)

chairranefs<-data.frame(chair=rownames(ranef(mlm3_chairs)$chairname),coef=fixef(mlm3)[2]+ranef(mlm3_chairs)$chairname[,2],se=se.ranef(mlm3_chairs)$chairname[,2])

chairranefs<-ft %>%
  group_by(chairname,chairparty) %>%
  summarise(ordpos=mean(chairordpos)) %>%
  rename(party=chairparty,chair=chairname) %>%
  left_join(chairranefs,.,by="chair") %>%
  arrange(.,ordpos,-coef) %>%
  mutate(chairorder=1:20,chairparty=paste(chair," ","(",party,")",sep=""))


### TABLES

checkmarks<-c("Speaker party fixed effects & & & $\\checkmark$ & & $\\checkmark$ \\\\", "Chair party fixed effects & & & $\\checkmark$ & & $\\checkmark$ \\\\", "Debate fixed effects & & & & $\\checkmark$ & $\\checkmark$ \\\\")
covarlabs<-c("Copartisan","Time of day","Gender (female)","Intercept")
 
regtab1<-stargazer(m1rob,m2rob,m3rob,m4rob,m5rob,style="apsr",omit=c("coarse","chair","debate"),omit.stat=c("f","chi2"),
                   dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",
                   label="parlbias_regtab1",column.sep.width="5pt",star.cutoffs=c(.1,.05,.01),align=T,title="OLS models of speaking time",digits=2,
                   covariate.labels=covarlabs)

regtab1<-c(regtab1[1:22],checkmarks,regtab1[23:length(regtab1)])
writeLines(regtab1,con="../tables/parlbias_regtab1.txt")

regtab1logit<-stargazer(m1.logit,m2.logit,m3.logit,m4.logit,m5.logit,style="apsr",omit=c("coarse","chair","debate"),omit.stat=c("f","chi2"),
                        dep.var.labels="Dummy: speaking time exceeds 60 seconds",dep.var.labels.include=T,font.size="footnotesize",
                        label="parlbias_regtab1logit",column.sep.width="5pt",covariate.labels=covarlabs,star.cutoffs=c(.1,.05,.01),
                        align=T,title="Logit models of exceeding standard speaking time",digits=2)

regtab1logit<-c(regtab1logit[1:22],checkmarks,regtab1logit[23:length(regtab1logit)])
writeLines(regtab1logit,con="../tables/parlbias_regtab1logit.txt")

modscolumnlabels<-c("Full","Distance$\\leq$median (int.)","Distance$\\leq$median (ord.)","Same bloc")

modscheckmarks<-c("Speaker party fixed effects & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\", "Chair party fixed effects & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\", "Debate fixed effects & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\")

regtabmods<-stargazer(m5,m5intposdifltmedian,m5ordposdifltmedian,m5cobloc,style="apsr",omit=c("coarse","chair","debate"),omit.stat=c("f","chi2","ser"),
                      dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",
                      label="parlbias_regtabmods",column.sep.width="0pt",covariate.labels=covarlabs,star.cutoffs=c(.1,.05,.01),
                      align=T,title="Tests of political moderators",digits=2,column.labels=modscolumnlabels)

regtabmods<-c(regtabmods[1:23],modscheckmarks,regtabmods[24:length(regtabmods)])

regtabmods[11]<-"\\\\[-1.8ex] & \\multicolumn{4}{c}{Speaking time (seconds)} \\\\ " #fix strange dep var labeling error

writeLines(regtabmods,con="../tables/parlbias_regtabmods.txt")

modeffectests<-data.frame(est=rep(NA,4),se=rep(NA,4),model=modscolumnlabels)
modeffectests[1,1:2]<-tidy(m5)[2,2:3]
modeffectests[2,1:2]<-tidy(m5intposdifltmedian)[2,2:3]
modeffectests[3,1:2]<-tidy(m5ordposdifltmedian)[2,2:3]
modeffectests[4,1:2]<-tidy(m5cobloc)[2,2:3]

#sub in <= for latex symbols
modeffectests$model<-modeffectests$model %>%
  as.character() %>%
  gsub("\\$\\\\leq\\$","<=",.)

### SUMMARY STATS TABLES
ftall$type<-NA
ftall$type[ftall$secs<150]<-"Brief remark"
ftall$type[ftall$secs>=150]<-"Spokesperson speech"
ftall$type[ftall$secs>=150 & ftall$pm==1]<-"PM speech"

require(dplyr)

totaln<-nrow(subset(ftall,chair==0 & year(starttime)>2000))
totalsecs<-sum(subset(ftall,chair==0 & year(starttime)>2000)$secs)

options(digits=2)
sumstatstab<- subset(ftall,chair==0 & year(starttime)>2000) %>%
  group_by(type) %>%
  summarise(count=n(),nshare=100*(count/totaln),secshare=100*sum(secs)/totalsecs)


sumstatstabtex<-stargazer(sumstatstab,summary=F,digits=2,title="Types of speeches in opening and closing debates in the Folketing",label="parlbias_sumstatstab",font.size="footnotesize",align=T,colnames=T,rownames=F)

sumstatstabtex[12]<-"\\multicolumn{1}{l}{Type} & \\multicolumn{1}{r}{Number} & \\multicolumn{1}{r}{Share (numeric)} & \\multicolumn{1}{r}{Share (time-weighted)} \\\\ "

#awkward left alignment of labels
sumstatstabtex<-gsub("column\\{1\\}\\{c\\}\\{","column{1}{l}{",sumstatstabtex)

writeLines(sumstatstabtex,con="../tables/parlbias_sumstatstab.txt")

options(digits=7)

### PLOTS

#reorder factor levels
effectests<-within(effectests,method<-factor(method,levels=c("OLS","Logit")))

#plot estimates
ggplot(effectests,aes(x=est,y=reorder(model,rep(5:1,2)))) +
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

## varying coefficients by party

ggplot(subset(partyranefs,party!="Other"),aes(x=coef,y=reorder(party,-ordpos))) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=coef-1.96*se,xmax=coef+1.96*se),height=0,size=.5) +
  geom_errorbarh(aes(xmin=coef-1.65*se,xmax=coef+1.65*se),height=0,size=1.2) +
  expand_limits(x=0) +
  geom_vline(xintercept=0,linetype="dashed") +
  geom_vline(xintercept=fixef(mlm3)[2],linetype="dashed",color="grey40") +
  xlab("Estimate (seconds)") +
  ylab("Chairman's party") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

ggsave(file="../figures/parlbias_partyranefs.pdf",height=4,width=9)

## varying coefficients by chairman

ggplot(chairranefs,aes(x=coef,y=reorder(chairparty,-chairorder))) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=coef-1.96*se,xmax=coef+1.96*se),height=0,size=.5) +
  geom_errorbarh(aes(xmin=coef-1.65*se,xmax=coef+1.65*se),height=0,size=1.2) +
  expand_limits(x=0) +
  geom_vline(xintercept=0,linetype="dashed") +
  geom_vline(xintercept=fixef(mlm3)[2],linetype="dashed",color="grey40") +
  xlab("Estimate (seconds)") +
  ylab("Chairman") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

ggsave(file="../figures/parlbias_chairranefs.pdf",height=6,width=9)

### PLOTS

