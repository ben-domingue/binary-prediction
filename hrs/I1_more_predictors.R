
##################################################################################################
source("/home/bd/Dropbox/projects/binaryprediction/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/binaryprediction/data/df.Rdata")
results<-list()

fm0<-formula("outcome~factor(wave)+age*raracem*ragender")
fm1<-formula("outcome~factor(wave)+age*raracem*ragender+raedyrs+wealth")

## 1. how well can cognitive functioning predict death, nursing home admission, dementia dx in next 2y?
for (st in c("proxy","attrit","dead")) {
    tmp<-df[df$status %in% c(st,"eligible"),]
    tmp$outcome<-ifelse(tmp$status==st,1,0)
    results[[paste(st,"",sep='')]]<-bigfun(tmp,fm1=fm0,fm2=fm1)
}
##older respondents
for (st in c("proxy","attrit","dead")) {
    tmp<-df[df$status %in% c(st,"eligible"),]
    tmp$outcome<-ifelse(tmp$status==st,1,0)
    results[[paste(st,".75+",sep='')]]<-bigfun(tmp[tmp$age>=75,],fm1=fm0,fm2=fm1)
}

##nursing home
df$nhome<-ifelse(df$nhome=='1.yes',1,0)
tmp<-df[,c("hhidpn","wave","nhome")]
tmp$wave<-tmp$wave-1
df2<-df
NULL->df2$nhome
df2<-merge(df2,tmp)
df2$outcome<-df2$nhome
results[["nhome.75+"]]<-bigfun(df2[df2$age>=75,],fm1=fm0,fm2=fm1)

## 3. how well does education predict the full range of health outcomes in the hrs (the self-reported variables)? [look at 0-> 1 between waves]
conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr")
for (nm.tmp in conds) {
    nm<-paste(nm.tmp,".next",sep="")
    df$outcome<-df[[nm]]
    print(table(df$outcome))
    results[[nm.tmp]]<-bigfun(df,fm1=fm0,fm2=fm1)
}


################plot, log
mat<-do.call("rbind",results)
mat<-mat[order(mat[,4]),,drop=FALSE]


pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/ses.pdf",width=8,height=2.5)
layout(matrix(c(1,1,2),nrow=1))
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))

      
games<-list(`Sports Book`=0.091,Baccarat=0.048,Blackjack=0.0099)
ran<-c(mat[,4],unlist(games))
ran<-ran[ran>0]
plot(NULL,xlim=c(-4,-.8),ylim=c(0,.75),ylab="",yaxt='n',xlab="log10(E(W))",bty='n')
yv<-seq(.1,.7,by=.1)
for (i in 1:nrow(mat)) {
    yy<-yv[((i-1) %% length(yv))+1]
    text(log10(mat[i,4]),yy,rownames(mat)[i],cex=1.4,pos=3)
    segments(log10(mat[i,4]),0,log10(mat[i,4]),yy,lwd=.7,col='gray')
}
legend("topleft",bty="n",c("log10(-1)=10 cents","log10(-2)=1 cent","log10(-3)=0.1 cents","log10(-4)=0.01 cents"),title="E(W) for $1 bet",cex=.8)
for (i in 1:length(games)) {
    segments(log10(games[[i]]),0,log10(games[[i]]),.05,lwd=.7,col='red')
    text(log10(games[[i]]),0.05,names(games)[i],col='red',cex=.6,pos=3)
}
#for (i in 1:nrow(mat)) mtext(side=2,at=(i-1)/nrow(mat),paste(i,rownames(mat)[i]),cex=.8,las=2)

##get results from H_...
r00<-do.call("rbind",results00)

tmp<-merge(r00[,4,drop=FALSE],mat[,4,drop=FALSE],by=0)
plot(tmp[,-1],type='n',xlab="E(W), baseline",ylab="E(W), SES",xlim=c(0,.04),ylim=c(0,.04))
text(tmp[,2],tmp[,3],tmp[,1],cex=.85)
dev.off()
