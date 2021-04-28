

##################################################################################################
source("/home/bd/Dropbox/projects/binaryprediction/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/binaryprediction/data/df.Rdata")
results<-list()

fm0<-formula("outcome~factor(wave)")
fm1<-formula("outcome~factor(wave)+age*raracem*ragender")

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

#par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))

## mat->tmp
## tmp[,4]<-tmp[,4]/max(tmp[,4])
## tmp[,5]<-tmp[,5]/max(tmp[,5])
## plot(tmp[,1],tmp[,5],pch=19,col='red')
## points(tmp[,1],tmp[,4],pch=19,col='blue')
## arrows(tmp[,1],tmp[,5],tmp[,1],tmp[,4])
## cor(mat[,c(1,4,5)])


pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/baseline.pdf",width=8,height=2.5)
layout(matrix(c(1,1,2),nrow=1))
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))

games<-list(`Sports Book`=0.091,Baccarat=0.048,Blackjack=0.0099)
ran<-c(mat[,4],unlist(games))
ran<-ran[ran>0]
plot(NULL,xlim=c(-4,-.8),ylim=c(0,.75),ylab="",yaxt='n',xlab="log10(E(W))",bty='n')
yv<-seq(.1,.7,by=.1)
for (i in 1:nrow(mat)) {
    yy<-yv[((i-1) %% length(yv))+1]
    #text(mat[i,4],yy,rownames(mat)[i],cex=.8)
    text(log10(mat[i,4]),yy,rownames(mat)[i],cex=1.4,pos=3)
    segments(log10(mat[i,4]),0,log10(mat[i,4]),yy,lwd=.7,col='gray')
}
legend("topleft",bty="n",c("log10(-1)=10 cents","log10(-2)=1 cent","log10(-3)=0.1 cents","log10(-4)=0.01 cents"),title="E(W) for $1 bet",cex=.8)
for (i in 1:length(games)) {
    segments(log10(games[[i]]),0,log10(games[[i]]),.05,lwd=.7,col='red')
    text(log10(games[[i]]),0.05,names(games)[i],col='red',cex=.6,pos=3)
}
#for (i in 1:nrow(mat)) mtext(side=2,at=(i-1)/nrow(mat),paste(i,rownames(mat)[i]),cex=.8,las=2)

plot(mat[,1],mat[,4],xlab="Prevalence",ylab="E(W)",type='n')
text(mat[,1],mat[,4],rownames(mat),cex=.8)
dev.off()

results00 <-
list(proxy = c(0.0372207402467489, 0.963256134001304, 0.966518384390779, 
0.00338669049105727, 0.0334709768468893), attrit = c(0.058739634481775, 
0.941450379101723, 0.941642457619044, 0.000204024047985403, 0.00235443784330339
), dead = c(0.0646189979651545, 0.935863782464477, 0.944652853711258, 
0.00939140012837726, 0.0667691661540926), `proxy.75+` = c(0.0661262925768096, 
0.934556043545068, 0.93884191198876, 0.00458599403780449, 0.0321008188045713
), `attrit.75+` = c(0.0618207945394126, 0.938766027572523, 0.939156731209198, 
0.000416188512578811, 0.006397857920001), `dead.75+` = c(0.120805745707552, 
0.879034299783657, 0.890269691074946, 0.0127815163686495, 0.0541693565157868
), `nhome.75+` = c(0.0290395728183747, 0.970909017616774, 0.974628506503838, 
0.00383093453616655, 0.0349000319105947), hibp = c(0.649879442575161, 
0.661979134886986, 0.675316074894637, 0.0201470700582259, 0.03325413367882
), diab = c(0.221605563432584, 0.782046981043355, 0.787748047790418, 
0.00728992871944565, 0.0245301836455318), cancr = c(0.19134132719663, 
0.80967055904431, 0.813854698443634, 0.00516770599175902, 0.0153178210595554
), lung = c(0.120556246956334, 0.879626415525445, 0.880023381445933, 
0.000451289221743928, 0.00211015707223124), heart = c(0.324842067921027, 
0.676740539723039, 0.694889798030686, 0.0268186361571816, 0.0309341954073605
), strok = c(0.0851381020789852, 0.914821334312699, 0.916556214136058, 
0.0018964138223363, 0.00816574674377936), psych = c(0.145103208359945, 
0.855339773942866, 0.860392140210288, 0.0059068529505316, 0.0200645392038731
), arthr = c(0.704433295512023, 0.70759719174462, 0.721839031177544, 
0.0201270434635425, 0.0316976049542574))
