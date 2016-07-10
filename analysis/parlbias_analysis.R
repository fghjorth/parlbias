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
require(rms)
require(mfx)

#win
#setwd("C:/Users/fh/Documents/GitHub/parlbias/data")
#mac
setwd("~/GitHub/parlbias/data")

### READ IN DATA

ft<-readRDS("ft.rds")


### REGRESSION MODELS

#define basic models
m1<-lm(m1f<-as.formula(secs~copartisan),data=ft)
m2<-lm(m2f<-as.formula(secs~copartisan+timeofday),data=ft)
m3<-lm(m3f<-as.formula(secs~copartisan+timeofday+female+debatetype),data=ft)
m4<-lm(m4f<-as.formula(secs~copartisan+timeofday+female+debatetype+coarseparty),data=ft)
m5<-lm(m5f<-as.formula(secs~copartisan+timeofday+female+debatetype+coarseparty+chairparty),data=ft)

stargazer(m1,m2,m3,m4,m5,type="text",omit=c("factor","debate"),omit.stat=c("f","ser"))
stargazer(m1,m2,m3,m4,m5,type="text",omit=c("debate"),omit.stat=c("f","ser"))

summary(mx<-lm(secs~copartisan+timeofday+female+factor(coarseparty)+factor(chairparty)+debate,data=ft))

#quick robustness checks
summary(lm(m5f,data=ft))
summary(lm(m5f,data=sample_frac(ft,.5,replace=F)))
summary(lm(m5f,data=subset(ft,timeofday>11)))
summary(lm(m5f,data=subset(ft,pm==0)))

# identify followups
ft$followup<-0
for (i in 5:nrow(ft)){
  if (ft$chair[i]==0 & ft$fullname[i]==ft$fullname[i-2] & !(ft$fullname[i]==ft$fullname[i-4])){
    ft$followup[i]<-1
  }
}

table(ft$followup)

ggplot(subset(ft,copartisan<2),aes(x=secs)) +
  geom_density() +
  facet_grid(copartisan~followup) +
  theme_bw()

summary(lm(m5f,data=subset(ft,followup==0)))
#conclusion: data is robust to the exclusion of followups, but they do not (solely) explain the bimodal distribution


#collect effect estimates in data frame
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
maxspeechesperchair<-max(lengths$speeches)

#how many seconds does 2.8 seconds add up to over the average debate per chair?
effectests$est[4]*maxspeechesperchair

summary(m1.logit<-glm(secsgt60~copartisan,data=ft,family=binomial()))
summary(m2.logit<-glm(secsgt60~copartisan+timeofday,data=ft,family=binomial()))
summary(m3.logit<-glm(secsgt60~copartisan+timeofday+female+debatetype,data=ft,family=binomial()))
summary(m4.logit<-glm(secsgt60~copartisan+timeofday+female+debatetype+factor(coarseparty),data=ft,family=binomial()))
summary(m5.logit<-glm(secsgt60~copartisan+timeofday+female+debatetype+factor(coarseparty)+factor(chairparty),data=ft,family=binomial()))

effectests[6,1:2]<-logitmfx(m1.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[7,1:2]<-logitmfx(m2.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[8,1:2]<-logitmfx(m3.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[9,1:2]<-logitmfx(m4.logit$formula,data=ft)$mfxest[1,1:2]*100
effectests[10,1:2]<-logitmfx(m5.logit$formula,data=ft)$mfxest[1,1:2]*100

#magnitude check
sd(ft$secs,na.rm=T)
3/sd(ft$secs,na.rm=T)

#for the main models, cluster observations by presiding chair
m1rob<-robcov(ols(m1f,data=ft,x=T,y=T),cluster=ft$chairname)
m2rob<-robcov(ols(m2f,data=ft,x=T,y=T),cluster=ft$chairname)
m3rob<-robcov(ols(m3f,data=ft,x=T,y=T),cluster=ft$chairname)
m4rob<-robcov(ols(m4f,data=ft,x=T,y=T),cluster=ft$chairname)
m5rob<-robcov(ols(m5f,data=ft,x=T,y=T),cluster=ft$chairname)

m5intposdifltmedian<-robcov(ols(m5f,data=subset(ft,intposdif<=2.34),x=T,y=T),cluster=subset(ft,intposdif<=2.34)$chairname)
m5ordposdifltmedian<-robcov(ols(m5f,data=subset(ft,ordposdif<=2),x=T,y=T),cluster=subset(ft,ordposdif<=2)$chairname)
m5cobloc<-ols(m5f,data=subset(ft,cobloc==1),x=T,y=T)

#setup up varying slopes model to test how the bias varies by 
mlm3<-lmer(secs~copartisan+timeofday+female+(1|coarseparty)+(1+copartisan|chairparty),data=ft)
ranef(mlm3)
fixef(mlm3)
partyranefs<-data.frame(party=rownames(ranef(mlm3)$chairparty),coef=fixef(mlm3)[2]+ranef(mlm3)$chairparty[,2],se=se.ranef(mlm3)$chairparty[,2])
partyranefs

partyranefs<- ft %>%
  group_by(chairparty) %>%
  summarise(ordpos=mean(chairordpos,na.rm=T)) %>%
  rename(party=chairparty) %>%
  left_join(partyranefs,.,by="party")
partyranefs

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

#robustness check 1: only leadership parties
m1robrs<-robcov(ols(m1f,data=subset(ft,leadshipparty==1),x=T,y=T),cluster=subset(ft,leadshipparty==1)$chairname)
m2robrs<-robcov(ols(m2f,data=subset(ft,leadshipparty==1),x=T,y=T),cluster=subset(ft,leadshipparty==1)$chairname)
m3robrs<-robcov(ols(m3f,data=subset(ft,leadshipparty==1),x=T,y=T),cluster=subset(ft,leadshipparty==1)$chairname)
m4robrs<-robcov(ols(m4f,data=subset(ft,leadshipparty==1),x=T,y=T),cluster=subset(ft,leadshipparty==1)$chairname)
m5robrs<-robcov(ols(m5f,data=subset(ft,leadshipparty==1),x=T,y=T),cluster=subset(ft,leadshipparty==1)$chairname)

#robustness check 2: debate fixed effects
m1fdfe<-as.formula(secs~copartisan+debate)
m2fdfe<-as.formula(secs~copartisan+timeofday+debate)
m3fdfe<-as.formula(secs~copartisan+timeofday+female+debate)
m4fdfe<-as.formula(secs~copartisan+timeofday+female+coarseparty+debate)
m5fdfe<-as.formula(secs~copartisan+timeofday+female+coarseparty+chairparty+debate)

m1robdfe<-robcov(ols(m1fdfe,data=ft,x=T,y=T),cluster=ft$chairname)
m2robdfe<-robcov(ols(m2fdfe,data=ft,x=T,y=T),cluster=ft$chairname)
m3robdfe<-robcov(ols(m3fdfe,data=ft,x=T,y=T),cluster=ft$chairname)
m4robdfe<-robcov(ols(m4fdfe,data=ft,x=T,y=T),cluster=ft$chairname)
m5robdfe<-robcov(ols(m5fdfe,data=ft,x=T,y=T),cluster=ft$chairname)

#robustness check 3: exclude prime ministers
m1robexpm<-robcov(ols(m1f,data=subset(ft,pm==0),x=T,y=T),cluster=subset(ft,pm==0)$chairname)
m2robexpm<-robcov(ols(m2f,data=subset(ft,pm==0),x=T,y=T),cluster=subset(ft,pm==0)$chairname)
m3robexpm<-robcov(ols(m3f,data=subset(ft,pm==0),x=T,y=T),cluster=subset(ft,pm==0)$chairname)
m4robexpm<-robcov(ols(m4f,data=subset(ft,pm==0),x=T,y=T),cluster=subset(ft,pm==0)$chairname)
m5robexpm<-robcov(ols(m5f,data=subset(ft,pm==0),x=T,y=T),cluster=subset(ft,pm==0)$chairname)


#get list of chairmen by debate
allchairmen<-ftall %>%
  filter(chair==1) %>%
  group_by(chairname,debate) %>%
  summarise(remarks=length(chairname))

seatvec<-c(16,rep(22,2),rep(47,7),rep(8,3),rep(18,2),47,23,16,23,16,9,47,44,34,17,17,13,45,rep(47,3),45,rep(47,4),rep(22,4),37,25,22,25,22,16,14,rep(46,2)) #careful! here I manually type in the seats for each chairman's party by debate
allchairmen$seats<-seatvec

allchairmen<-allchairmen %>%
  group_by(chairname) %>%
  summarise(remarks=sum(remarks),debates=length(debate),avgseats=mean(seats)) %>%
  mutate(president=ifelse(chairname %in% c("Thor Pedersen","Mogens Lykketoft","Pia Kjaersgaard"),1,0))

#can we predict chairman activity?
summary(remarksm1<-lm(remarks~president+debates,data=allchairmen))
summary(remarksm2<-lm(remarks~president+debates+avgseats,data=allchairmen))
summary(remarksm3<-lm(remarks~president+avgseats,data=allchairmen))

allchairmen$exest<-NA
allchairmen$exse<-NA
for (i in 1:nrow(allchairmen)){
  exm<-robcov(ols(m5f,data=subset(ft,chairname!=allchairmen$chairname[i]),x=T,y=T),cluster=subset(ft,chairname!=allchairmen$chairname[i])$chairname)
  allchairmen[i,6:7]<-c(exm$coefficients[2],sqrt(diag(exm$var))[2])
}





### TABLES

checkmarks<-c("Speaker party FE & & & & $\\checkmark$ & $\\checkmark$ \\\\", "Chair party FE & & & & & $\\checkmark$ \\\\")
#, "Debate FE & & & & $\\checkmark$ & $\\checkmark$ \\\\")
covarlabs<-c("Copartisan","Time of day","Gender (female)","Debate type (Opening)","Intercept")
 
regtab1<-stargazer(m1rob,m2rob,m3rob,m4rob,m5rob,style="apsr",omit=c("coarse","chair"),
                   dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",
                   label="parlbias_regtab1",column.sep.width="-5pt",star.cutoffs=c(.1,.05,.01),align=T,title="OLS models of speaking time",digits=2,
                   covariate.labels=covarlabs)

regtab1<-c(regtab1[1:24],checkmarks,regtab1[25:length(regtab1)])
regtab1
writeLines(regtab1,con="../tables/parlbias_regtab1.txt")

regtab1logit<-stargazer(m1.logit,m2.logit,m3.logit,m4.logit,m5.logit,style="apsr",omit=c("coarse","chair","debate"),
                        dep.var.labels="Dummy: speaking time exceeds 60 seconds",dep.var.labels.include=T,font.size="footnotesize",
                        label="parlbias_regtab1logit",column.sep.width="-5pt",covariate.labels=covarlabs,star.cutoffs=c(.1,.05,.01),
                        align=T,title="Logit models of exceeding standard speaking time",digits=2)
regtab1logit
regtab1logit<-c(regtab1logit[1:22],checkmarks,regtab1logit[23:length(regtab1logit)])
writeLines(regtab1logit,con="../tables/parlbias_regtab1logit.txt")

modscolumnlabels<-c("Full","Distance$\\leq$median (interval)","Distance$\\leq$median (ordinal)","Same bloc")

modscheckmarks<-c("Speaker party FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\", "Chair party FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\")
#, "Debate FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\")

regtabmods<-stargazer(m5rob,m5intposdifltmedian,m5ordposdifltmedian,m5cobloc,style="apsr",omit=c("coarse","chair","debate"),
                      dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",
                      label="parlbias_regtabmods",column.sep.width="-5pt",covariate.labels=covarlabs,star.cutoffs=c(.1,.05,.01),
                      align=T,title="Tests of political moderators",digits=2,column.labels=modscolumnlabels)

regtabmods
regtabmods<-c(regtabmods[1:23],modscheckmarks,regtabmods[24:length(regtabmods)])
regtabmods

writeLines(regtabmods,con="../tables/parlbias_regtabmods.txt")

modeffectests<-data.frame(est=rep(NA,4),se=rep(NA,4),model=modscolumnlabels)

#get coef and se on copartisan for each model
modeffectms<-list(m5rob,m5intposdifltmedian,m5ordposdifltmedian,m5cobloc)

for (i in 1:4){
  modeffectests[i,1]<-coef(modeffectms[[i]])["copartisan"]
  modeffectests[i,2]<-sqrt(diag(modeffectms[[i]]$var))["copartisan"]
}

modeffectests

#sub in <= for latex symbols
modeffectests$model<-modeffectests$model %>%
  as.character() %>%
  gsub("\\$\\\\leq\\$","<=",.)

#robustness check 1: restricted sample
regtabrs<-stargazer(m1robrs,m2robrs,m3robrs,m4robrs,m5robrs,style="apsr",omit=c("coarse","chair"),
                   dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",
                   label="parlbias_regtabrs",column.sep.width="-5pt",star.cutoffs=c(.1,.05,.01),align=T,title="Results for only members of leadership parties",digits=2,
                   covariate.labels=covarlabs)

regtabrs<-c(regtabrs[1:24],checkmarks,regtabrs[25:length(regtabrs)])
regtabrs
writeLines(regtabrs,con="../tables/parlbias_regtabrs.txt")

#robustness check 2: debate-specific fixed effects
dfecheckmarks<-c("Speaker party FE & & & $\\checkmark$ & & $\\checkmark$ \\\\", "Chair party FE & & & & & $\\checkmark$ \\\\",
                 "Debate FE & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\")
dfecovarlabs<-c("Copartisan","Time of day","Gender (female)","Intercept")

regtabdfe<-stargazer(m1robdfe,m2robdfe,m3robdfe,m4robdfe,m5robdfe,style="apsr",omit=c("coarse","chair","debate"),
                   dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",
                   label="parlbias_regtabdfe",column.sep.width="-5pt",star.cutoffs=c(.1,.05,.01),align=T,title="Results with debate-specific fixed effects",digits=2,
                   covariate.labels=dfecovarlabs)

regtabdfe<-c(regtabdfe[1:22],dfecheckmarks,regtabdfe[23:length(regtabdfe)])
regtabdfe
writeLines(regtabdfe,con="../tables/parlbias_regtabdfe.txt")


#robustness check 3: excluding pm's
regtabexpm<-stargazer(m1robexpm,m2robexpm,m3robexpm,m4robexpm,m5robexpm,style="apsr",omit=c("coarse","chair"),
                    dep.var.labels="Speaking time (seconds)",dep.var.labels.include=T,font.size="footnotesize",
                    label="parlbias_regtabexpm",column.sep.width="-5pt",star.cutoffs=c(.1,.05,.01),align=T,title="Results excluding prime ministers",digits=2,
                    covariate.labels=covarlabs)

regtabexpm<-c(regtabexpm[1:24],checkmarks,regtabexpm[25:length(regtabexpm)])
regtabexpm
writeLines(regtabexpm,con="../tables/parlbias_regtabexpm.txt")


#table for reg predicting remarks presided over
remarksregtab<-stargazer(remarksm1,remarksm2,remarksm3,style="apsr",
                      dep.var.labels="Number of remarks enforced",dep.var.labels.include=T,font.size="footnotesize",
                      label="parlbias_remarksregtab",column.sep.width="-5pt",star.cutoffs=c(.1,.05,.01),
                      align=T,title="Model predicting number of remarks enforced by chairmen",digits=2,
                      covariate.labels=c("Head chairman","No. of debates in leadership","No. of party seats"))

writeLines(remarksregtab,con="../tables/parlbias_remarksregtab.txt")

### SUMMARY STATS TABLES
ftall$type<-NA
ftall$type[ftall$secs<150 & ftall$secs>10 & !is.na(ftall$copartisan)]<-"Brief remark"
ftall$type[ftall$secs>=150]<-"Spokesperson speech"
ftall$type[ftall$secs>=150 & ftall$pm==1]<-"PM speech"

require(dplyr)

totaln<-nrow(subset(ftall,chair==0 & year(starttime)>2000 & !is.na(type)))
totalsecs<-sum(subset(ftall,chair==0 & year(starttime)>2000 & !is.na(type))$secs)

options(digits=2)
sumstatstab<- ftall %>% 
  filter(chair==0 & year(starttime)>2000) %>%
  filter(!is.na(type)) %>% 
  group_by(type) %>%
  summarise(count=n(),nshare=100*(count/totaln),secshare=100*sum(secs)/totalsecs)

sumstatstab

#add total line at bottom
sumstatstab[4,]<-c("Total",colSums(sumstatstab[,2:4]))
class(sumstatstab$nshare)<-"numeric"
class(sumstatstab$secshare)<-"numeric"
sumstatstab$nshare<-as.character(format(sumstatstab$nshare,digits=2))
sumstatstab$secshare<-as.character(format(sumstatstab$secshare,digits=2))

sumstatstab

str(sumstatstab)

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
ggsave(file="../figures/parlbias_effectplot.png",height=4,width=9)

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
ggsave(file="../figures/parlbias_modeffectplot.png",height=4,width=9)

#factors for copartisanship
ft$copartisan_factor<-factor(ft$copartisan,labels=c("Non-copartisan","Co-partisan"))
ft$copartisan_factor<-factor(ft$copartisan_factor,levels(ft$copartisan_factor)[c(2,1)])

ft<-ft %>%
  mutate(copartisancobloc=factor(copartisan+cobloc,labels=c("Non-copartisan, other bloc","Non-copartisan, same bloc","Co-partisan")))

ggplot(subset(ft,chair==0 & !is.na(copartisancobloc)),aes(x=secs)) +
  geom_density(fill="gray",alpha=.5,adjust=1.5) +
  facet_grid(.~copartisancobloc) +
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
ggsave(file="../figures/parlbias_dens.png",height=5,width=9)

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
ggsave(file="../figures/parlbias_partyranefs.png",height=4,width=9)

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
ggsave(file="../figures/parlbias_chairranefs.png",height=6,width=9)


# estimate coefficient excluding each chairman
ggplot(allchairmen,aes(x=exest,y=chairname)) +
  geom_point(size=2.5) +
  geom_errorbarh(aes(xmin=exest-1.96*exse,xmax=exest+1.96*exse),height=0,size=.5) +
  geom_errorbarh(aes(xmin=exest-1.65*exse,xmax=exest+1.65*exse),height=0,size=1.2) +
  expand_limits(x=0) +
  geom_vline(xintercept=0,linetype="dashed") +
  geom_vline(xintercept=m5rob$coefficients[2],linetype="dashed",color="grey40") +
  xlab("Estimate excluding chairman (seconds)") +
  ylab("Chairman") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

ggsave(file="../figures/parlbias_exchairests.pdf",height=6,width=9)
ggsave(file="../figures/parlbias_exchairests.png",height=6,width=9)

## plot predicted remarks presided over
seatsprdf1<-data.frame(expand.grid(debates=1:9,president=0:1,avgseats=21)) %>%
  bind_cols(.,as.data.frame(predict(remarksm2,newdata=.,se.fit=T))) %>%
  dplyr::select(debates,president,fit,se.fit)




