source("/home/bd/Dropbox/projects/binaryprediction/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/binaryprediction/gss/df_gss.Rdata")
results<-list()

df$year<-as.numeric(df$year)
df$age<-as.numeric(df$age)

#########################################################
fm1<-formula("gop~1")
fm2<-formula("gop~white+sex+age")
out<-list()
for (yr in 1972:2018) {
    x<-df[df$year==yr,]
    tmp<-x[,all.vars(fm2)]
    x<-x[rowSums(is.na(tmp))==0,]
    if (nrow(x)>100) {
        ew<-bigfun(x,fm1=fm1,fm2=fm2)
        out[[as.character(yr)]]<-c(yr,ew[c(1,4)],nrow(x))
    }
}
out->out1

#########################################################
fm1<-formula("gop~1")
fm2<-formula("gop~white*sex*age")
out<-list()
for (yr in 1972:2018) {
    x<-df[df$year==yr,]
    tmp<-x[,all.vars(fm2)]
    x<-x[rowSums(is.na(tmp))==0,]
    if (nrow(x)>100) {
        ew<-bigfun(x,fm1=fm1,fm2=fm2)
        out[[as.character(yr)]]<-c(yr,ew[c(1,4)],nrow(x))
    }
}
out->out2

#########################################################
pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/gss.pdf",width=5,height=5)
layout(matrix(c(1,1,2),nrow=3))
par(mar=c(3,3,1,1),mgp=c(2,1,0),oma=c(.5,2,.5,.5))
mat<-do.call("rbind",out1)
plot(mat[,1],mat[,3],type='b',ylim=c(0,.35),pch=19,col='red',lwd=2,xlab="Year",ylab="",xlim=c(1970,2020))
games<-list(`Sports Book`=0.091,Baccarat=0.048,Blackjack=0.0099)
for (ii in 1:length(games)) {
    game<-games[[ii]]
    abline(h=game,col='darkgray',lwd=1)
    text(2015,game,names(games)[ii],cex=.7,col='black',pos=3)
}
mtext(side=2,line=2,"E(W) based on demographics\nrelative to prevalence")
mat<-do.call("rbind",out2)
lines(mat[,1],mat[,3],type='b',col='blue',lwd=2,pch=19)
abline(h=0)
legend("topleft",bty="n",fill=c("red","blue"),c("Additive","Interactive"))
z<-aggregate(df$gop,list(df$year),mean,na.rm=TRUE)
plot(z,xlab="Year",ylab="Proportion GOP",type="l",xlim=c(1970,2020),ylim=c(.3,.7))
abline(h=.5,col='gray')
dev.off()

       
