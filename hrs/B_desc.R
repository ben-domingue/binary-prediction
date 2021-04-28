load(file="/home/bd/Dropbox/projects/binaryprediction/data/df.Rdata")

df<-df[df$age>=60,]
dim(df)

L<-split(df,df$wave)
L$all<-df

f<-function(x) {
    age<-c(mean(x$age,na.rm=TRUE),sd(x$age,na.rm=TRUE))
    #cog<-c(mean(x$cog,na.rm=TRUE),sd(x$cog,na.rm=TRUE))
    x$dead<-ifelse(x$status=="dead",1,0)
    x$attrit<-ifelse(x$status=="attrit",1,0)
    x$prox<-ifelse(x$status=="proxy",1,0)
    conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr")
    M<-numeric()
    for (nm in c("dead","attrit","prox",conds)) {
        M[[nm]]<-mean(x[[nm]],na.rm=TRUE)
    }
    c(nrow(x),M)
}
tab<-lapply(L,f)
tab<-do.call("rbind",tab)

library(xtable)
print(xtable(tab,digits=2),include.rownames=TRUE)


pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/desc.pdf",width=9,height=4)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,4),oma=rep(.5,4))
##
conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr","proxy","dead")
plot(NULL,xlim=c(60,90),ylim=c(0,1),xlab='age',ylab='Prevalence',bty='n')
cols<-colorRampPalette(c("blue","red"))(length(conds))
for (i in 1:length(conds)) {
    nm<-conds[i]
    lb<-ifelse(nm=="gait",65,50)
    tmp<-df[df$age>=lb & df$age<=90,]
    z<-tmp[[nm]]
    z<-aggregate(z,list(tmp$age),mean,na.rm=TRUE)
    lines(z,lwd=2,col=cols[i])
    ii<-which(z[,1]==90)
    mtext(side=4,las=2,line=.1,at=z[ii,2],nm,col=cols[i],cex=.7)
}
##
plot(NULL,xlim=c(60,90),ylim=c(-1.1,1.1),xlab='age',ylab='Standardized Mean',bty='n')
abline(h=0,col='gray')
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
cols<-c("blue","red","darkblue")
nms<-c("cog","grip","gait")
for (i in 1:length(nms)) {
    nm<-nms[i]
    lb<-ifelse(nm=="gait",65,50)
    tmp<-df[df$age>=lb & df$age<=90,]
    z<-std(tmp[[nm]])
    z<-aggregate(z,list(tmp$age),mean,na.rm=TRUE)
    lines(z,lwd=2,col=cols[i])
    ii<-which(z[,1]==90)
    mtext(side=4,las=2,line=.1,at=z[ii,2],nm,col=cols[i],cex=.7)
}
dev.off()

