library(foreign)
read.dta("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/randhrs1992_2016v1.dta")->x ##rand file, available here: https://www.rand.org/well-being/social-and-behavioral-policy/centers/aging/dataprod.html

##hhidpn,sex, race, education
df<-x[,c("hhidpn","ragender","raracem","raedyrs","raddate","rabyear")]
#year, cog, 
L<-list()
for (w in 3:12) {
    age<-x[[paste("r",w,"agey_m",sep='')]]
    cog<-x[[paste("r",w,"cogtot",sep="")]]
    prox<-x[[paste("r",w,"status",sep="")]]
    cog<-ifelse(prox=="1.cog meas",cog,NA)
    int.date<-x[[paste("r",w,"iwend",sep="")]]
    cesd<-x[[paste("r",w,"cesd",sep="")]]
    lonely<-x[[paste("r",w,"flone",sep="")]]
    iadl<-x[[paste("r",w,"iadlza",sep="")]]
    nhome<-x[[paste("r",w,"nhmliv",sep="")]]
    if (w>3) wealth<-x[[paste("h",w,"atotb",sep="")]] else rep(NA,nrow(x))->wealth
    conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr")
    for (nm in conds) assign(nm,x[[paste("r",w,nm,sep="")]])
    ##
    tmp<-data.frame(hhidpn=x$hhidpn,cog=cog,int.date=int.date,wave=w,age=age,cesd=cesd,iadl=iadl,wealth=wealth,nhome=nhome,lonely=lonely)
    for (nm in conds) tmp[[nm]]<-get(nm)
    L[[as.character(w)]]<-merge(df,tmp,all.x=TRUE)
}
df<-data.frame(do.call("rbind",L))
df<-df[!is.na(df$cog),]

for (nm in conds) {
    df[[nm]]->z
    df[[nm]]<-ifelse(z=="0.no",0,NA)
    df[[nm]]<-ifelse(z=="1.yes",1,df[[nm]])
}

#status
#proxy respondent
L<-list()
for (w in 1:13) {
    prox<-x[[paste("r",w,"proxy",sep="")]]
    prox<-ifelse(prox=="1.proxy",1,0)
    L[[as.character(w)]]<-data.frame(hhidpn=x$hhidpn,prox=prox,w=w)
}
prox<-data.frame(do.call("rbind",L))
prox<-prox[!is.na(prox$prox) & prox$prox==1,]
id<-paste(prox$hhidpn,prox$w-1)
df$proxy<-ifelse(paste(df$hhidpn,df$wave) %in% id,1,0)
##death
as.POSIXct(df$raddate*24*60^2,origin = "1960-01-01")->df$ddate.raw
as.POSIXct(df$int.date*24*60^2,origin = "1960-01-01")->df$iwend.raw
delta<-df$ddate.raw - df$iwend.raw
df$dead<-ifelse(delta<365*2,1,0)
##next test
L<-list()
for (w in 3:12) {
    prox<-x[[paste("r",w,"proxy",sep="")]]
    int.date<-x[[paste("r",w,"iwend",sep="")]]
    tmp<-data.frame(hhidpn=x$hhidpn,prox=prox,w=w,int.date=int.date)
    L[[as.character(w)]]<-tmp
}
tmp<-data.frame(do.call("rbind",L))
tmp<-tmp[!is.na(tmp$int.date),]
tmp<-tmp[tmp$prox=='0.not proxy',]
id<-paste(tmp$hhidpn,tmp$w-1)
df$inhrs<-ifelse(paste(df$hhidpn,df$wave) %in% id,1,0)

##status
st<-ifelse(df$inhrs==1 & df$proxy==0,1,NA)
st<-ifelse(df$proxy==1,2,st)
st<-ifelse(df$dead==1 & is.na(st),3,st)
st<-ifelse(is.na(st),4,st)
table(df$wave,st)
df$status<-c("eligible","proxy","dead","attrit")[st]




##grip & gait
load("/home/bd/Dropbox/projects/hrs/grip_gait/grip_gait.Rdata")
df<-merge(df,x,all.x=TRUE)
##now add next wave health obs
conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr")
for (nm in conds) {
    tmp<-df[,c("hhidpn","wave",nm)]
    names(tmp)[3]<-paste(nm,'.next',sep='')
    tmp$wave<- tmp$wave - 1 
    df<-merge(df,tmp,all.x=TRUE)
}

#df<-df[df$wave<=11,]

conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr")
tmp<-df[,c("hhidpn","wave",conds)]
tmp$wave<-tmp$wave -1
#names(tmp)[-(1:2)]<-paste(conds,".next",sep='')
for (nm in conds) {
    ii<-grep(nm,names(df))
    df<-df[,-ii]
}
df<-merge(df,tmp,all.x=TRUE)

save(df,file="/home/bd/Dropbox/projects/binaryprediction/data/df.Rdata")
