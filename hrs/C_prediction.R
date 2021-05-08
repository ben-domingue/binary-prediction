##################################################################################################
source("/home/bd/Dropbox/projects/binaryprediction/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/binaryprediction/data/hrs/df.Rdata")
df$nhome<-ifelse(df$nhome=='1.yes',1,0)

df<-df[df$age>=60,]

conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr","proxy","dead")
## tmp<-df[,c("hhidpn","wave",conds)]
## tmp$wave<-tmp$wave -1
## names(tmp)[-(1:2)]<-paste(conds,".next",sep='')
## df<-merge(df,tmp,all.x=TRUE)

fm1<-formula("outcome~1")
fm2<-formula("outcome~age")
conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr","proxy","dead")
out<-list()
for (cond in conds) {
    df[[cond]]->df$outcome
    out[[cond]]<-bigfun(df[!is.na(df$age),],fm1,fm2)
}
ew<-do.call("rbind",out)

wrapper<-function(df,fm1,fm2,conds) {
    out<-list()
    window<-1.5
    for (yr in seq(60,90,by=1)) {
        print(yr)
        df.age<-df[abs(df$age-yr)<window,]
        for (cond in conds) {
            x<-df.age
            x[[paste(cond,sep='')]]->x$outcome
            tmp<-x[,all.vars(fm2)]
            x<-x[rowSums(is.na(tmp))==0,]
            if (nrow(x)>1000) {
                ew<-bigfun(x,fm1=fm1,fm2=fm2)
                out[[paste(cond,yr)]]<-c(cond,yr,ew[c(1,4)],nrow(x))
            }
        }
    }
    out
}

fm1<-formula("outcome~1")
fm2<-formula("outcome~raracem+ragender")
out1<-wrapper(df,fm1,fm2,conds=conds)
out1a<-do.call("rbind",out1)

fm1<-formula("outcome~raracem+ragender")
fm2<-formula("outcome~raracem+ragender+raedyrs")
out1<-wrapper(df,fm1,fm2,conds=conds)
out1b<-do.call("rbind",out1)
fm1<-formula("outcome~raracem+ragender+raedyrs")
fm2<-formula("outcome~raracem+ragender+raedyrs+cog")
out1<-wrapper(df,fm1,fm2,conds=conds)
out1c<-do.call("rbind",out1)
## fm1<-formula("outcome~1")
## fm2<-formula("outcome~raracem+ragender")
## out2<-wrapper(df,fm1,fm2,conds=conds)
## out2a<-do.call("rbind",out2)
## fm1<-formula("outcome~raracem+ragender")
## fm2<-formula("outcome~raracem+ragender+raedyrs")
## out2<-wrapper(df,fm1,fm2,conds=conds)
## out2b<-do.call("rbind",out2)
fm1<-formula("outcome~raracem+ragender+raedyrs")
fm2<-formula("outcome~raracem+ragender+raedyrs+grip+gait")
out2<-wrapper(df,fm1,fm2,conds=conds)
out2c<-do.call("rbind",out2)


pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/hrs_age.pdf",width=7,height=5)
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(.5,4))
pf<-function(out,yt,ceiling=0.06) {
    plot(NULL,xlim=c(55,95),ylim=c(-.02,ceiling),xlab="Age",ylab=yt,xaxt='n',bty='n',yaxt='n')
    axis(side=1,seq(60,90,by=5))
    axis(side=2)
    L<-split(data.frame(out),out[,1])
    cols<-colorRampPalette(c("blue","red"))(length(L))
    for (i in 1:length(L)) {
        z<-L[[i]]
        lines(z[,2],z[,4],lwd=2,col=cols[i])
        a0<-min(z[,2])
        a1<-max(z[,2])
        text(a0,as.numeric(z[1,4]),names(L)[i],cex=.7,col=cols[i],pos=2)
        text(a1,as.numeric(z[nrow(z),4]),names(L)[i],cex=.7,col=cols[i],pos=4)
        if (as.numeric(z[1,4])>ceiling) mtext(at=as.numeric(z[1,2]),paste(sep=', ',names(L)[i],round(as.numeric(z[1,4]),digits=2)),cex=.7,col=cols[i],line=0.1)
        if (as.numeric(z[nrow(z),4])>ceiling) mtext(at=as.numeric(z[nrow(z),2]),paste(sep=', ',names(L)[i],round(as.numeric(z[nrow(z),4]),digits=2)),cex=.7,col=cols[i],line=0.1)
    }
    abline(h=0,col='black',lwd=.5)
    #abline(h=.0099,col='black')
    #text(60,.0099,pos=3,'blackjack',col='black')
    games<-list(`Sports Book`=0.091,Baccarat=0.048,Blackjack=0.0099)
    for (ii in 1:length(games)) {
        game<-games[[ii]]
        abline(h=game,col='darkgray',lwd=1)
    }
}
pf(out1a,yt="A: E(W), Demographics")
pf(out1b,yt="B: E(W), Education")
pf(out1c,yt="C: E(W), Cognition")
#pf(out2a,yt="E(W), Demographics")
#pf(out2b,yt="E(W), Education")
pf(out2c,yt="D: E(W), Grip+Gait")
games<-list(`Sports Book`=0.091,Baccarat=0.048,Blackjack=0.0099)
for (ii in 1:length(games)) {
    game<-games[[ii]]
    abline(h=game,col='darkgray',lwd=1)
    text(90,game,names(games)[ii],cex=.7,col='black',pos=3)
}
dev.off()

##for table
f<-function(x) {
    ii<-which.max(as.numeric(x[,4]))
    x[ii,]
}
f(out1a)
f(out1b)
f(out1c)
f(out2c)

zz<-rbind(out1a,out1b,out1c,out2c)
ii<-grep("dead 90",rownames(zz))
zz[ii,]

zz<-rbind(out1a,out1b,out1c,out2c)
ii<-grep("hibp 63",rownames(zz))
zz[ii,]

         
