set.seed(1234)
##################################################################################################
source("/home/bd/Dropbox/projects/binaryprediction/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/binaryprediction/data/hrs/df.Rdata")
df$nhome<-ifelse(df$nhome=='1.yes',1,0)

df<-df[df$age>60 & df$age<=90,]

conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr","proxy","dead")
#conds<-"heart"## tmp<-df[,c("hhidpn","wave",conds)]
## tmp$wave<-tmp$wave -1
## names(tmp)[-(1:2)]<-paste(conds,".next",sep='')
## df<-merge(df,tmp,all.x=TRUE)

fm1<-formula("outcome~1")
fm2a<-formula("outcome~age")
ff<-function(fm1,fm2,df) {
    #conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr","proxy","dead")
    out<-list()
    for (cond in conds) {
        df[[cond]]->df$outcome
        out[[cond]]<-bigfun(df[!is.na(df$age),],fm1,fm2)
    }
    ew<-do.call("rbind",out)
    ew[,4]
}
ew1<-ff(fm1,fm2a,df)

## fm2b<-formula("outcome~age*ragender")
## ew2<-ff(fm1,fm2b,df)
## library(splines)
## spl<-bs(df$age,degree=5)
## for (i in 1:ncol(spl)) df[[paste("spl",i,sep='')]]<-spl[,i]
## fm2c<-formula("outcome~(spl1+spl2+spl3+spl4+spl5)*ragender")
## ew3<-ff(fm1,fm2c,df)
## fm2d<-formula("outcome~(spl1+spl2+spl3+spl4+spl5)*ragender*raedyrs")
## ew4<-ff(fm1,fm2d,df)

## cbind(ew1,ew2,ew3,ew4)
