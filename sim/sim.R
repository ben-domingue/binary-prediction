simdata<-function(N,b0,b1,b2) {
    gen.data<-function(x,z,b0=0,b1=.5,b2=1) {
        k<-exp(b0+b1*x+b2*z)
        #k<-exp(.3+.5*x+.2*z)
        p<-k/(1+k)
        y<-rbinom(length(x),1,p)
        y
    }
    ##
    x<-rnorm(N)
    z<-rnorm(N)
    train<-data.frame(x=x,y=gen.data(x,b0=b0,b1=b1,b2=b2,z=z),z=z)
    x<-rnorm(N)
    z<-rnorm(N)
    test<-data.frame(x=x,y=gen.data(x,b0=b0,b1=b1,b2=b2,z=z),z=z)
    list(train=train,test=test)
}

set.seed(10101010)
b0L<-c(0,1.5,runif(1000,min=0,max=5))
b1L<-c(1,1,runif(1000,min=1,max=5))
b2L<-c(1,1,runif(1000,min=1,max=5))
dat<-list()
for (i in 1:length(b0L)) simdata(N=2000,b0=b0L[i],b1=b1L[i],b2=b2L[i])->dat[[i]]

simfun<-function(df.tr,df.test,fm1,fm2) {
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(z)
        exp(z)
    }    
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    ##
    m0<-glm(fm1,df.tr,family="binomial") #baseline
    m1<-glm(fm2,df.tr,family="binomial")
    prev<-mean(df.tr$y)
    #get predictions in new data
    y<-df.test$y
    p0<-predict(m0,type="response",data.frame(x=df.test$x,z=df.test$z))
    p1<-predict(m1,type="response",data.frame(x=df.test$x,z=df.test$z))
    a0<-ll(x=y,p=p0)
    a1<-ll(x=y,p=p1)
    print(c(a0,a1))
    coin0<-nlminb(.5,f,lower=0.001,upper=.999,a=a0)$par
    coin1<-nlminb(.5,f,lower=0.001,upper=.999,a=a1)$par
    ##the single-blind bet
    ew<-(coin1-coin0)/coin0
    ##r2
    r2<-1-sum((y-p1)^2)/sum((y-mean(df.tr$y))^2)
    r2.0<-1-sum((y-p0)^2)/sum((y-mean(df.tr$y))^2)
    r2.del<-(r2-r2.0)/r2.0
    ##auc
    library(pROC)
    #ro<-roc(y,p1)
    au<-pROC::auc(response=y,predictor=p1)
    ro0<-roc(y,p0)
    au.del<-(au-auc(ro0))/auc(ro0)
    ##f1
    library(MLmetrics)
    pred<-ifelse(p1>.5,1,0)
    if (length(unique(pred))==1) f1<-NA else f1<-F1_Score(y_pred=pred, y_true=y)
    ##
    tr<-list(prev=prev,ew=ew,
             #ew2=ew2,
             r2=r2,
             r2.del=r2.del,
             auc=au,
             auc.del=au.del,
             f1=f1
             )
    unlist(tr)
}

results.noz<-results.wz<-list()
for (i in 1:length(dat)) {
    r1<-simfun(df.tr=dat[[i]]$train,df.test=dat[[i]]$test,fm1="y~1",fm2="y~x")
    r2<-simfun(df.tr=dat[[i]]$train,df.test=dat[[i]]$test,fm1="y~x",fm2="y~x+z")
    results.noz[[i]]<-c(b0=b0L[i],b1=b1L[i],b2=b2L[i],r1)
    results.wz[[i]]<-c(b0=b0L[i],b1=b1L[i],b2=b2L[i],r2)
}

mat.noz<-data.frame(do.call("rbind",results.noz))
mat.wz<-data.frame(do.call("rbind",results.wz))

save.image(file="simresults.Rdata")


##
rows<-c(1,2)
col<-ifelse (1:nrow(mat.noz) %in% rows,'red','black')
cex<-ifelse(col=='black',.25,1.9)

t1<-mat.noz[rows,]
t2<-mat.wz[rows,]
tab<-rbind(t1[1,],t2[1,],t1[2,],t2[2,])
index<-grepl(".del",colnames(tab),fixed=TRUE)
library(xtable)
xtable(tab[,!index])

c1<-cor(mat.noz[,c("b0","b1","b2","ew","r2","auc","f1")],use='p')
mat.wz$r2.del<-(mat.wz$r2-mat.noz$r2)/mat.noz$r2
mat.wz$auc.del<-(mat.wz$auc-mat.noz$auc)/mat.noz$auc
mat.wz$f1.del<-(mat.wz$f1-mat.noz$f1)/mat.noz$f1
c2<-cor(mat.wz[,c("b0","b1","b2","ew","r2.del","auc.del","f1.del")],use='p')

tab<-rbind(c1[1:3,4:7],c2[1:3,4:7])
xtable(tab)

    
## ran<-range(c(mat.noz$ew,mat.wz$ew))
## par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
## plot(mat.noz$b0,mat.noz$ew,pch=19,xlab=expression(beta[0]),ylab="E(W)",ylim=ran,cex=cex,col=col)
## plot(mat.noz$b1,mat.noz$ew,pch=19,xlab=expression(beta[1]),ylab="E(W)",ylim=ran,cex=cex,col=col)
## plot(mat.noz$b2,mat.noz$ew,pch=19,xlab=expression(beta[2]),ylab="E(W)",ylim=ran,cex=cex,col=col)
## plot(mat.wz$b0,mat.wz$ew,pch=19,xlab=expression(beta[0]),ylab="E(W)",ylim=ran,cex=cex,col=col)
## plot(mat.wz$b1,mat.wz$ew,pch=19,xlab=expression(beta[1]),ylab="E(W)",ylim=ran,cex=cex,col=col)
## plot(mat.wz$b2,mat.wz$ew,pch=19,xlab=expression(beta[2]),ylab="E(W)",ylim=ran,cex=cex,col=col)

## par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
## plot(mat.noz$ew,mat.noz$r2,pch=19,xlab="E(W)",ylab="r2",ylim=c(0,1),cex=cex,col=col)
## plot(mat.noz$ew,mat.noz$auc,pch=19,xlab="E(W)",ylab="AUC",ylim=c(.5,1),cex=cex,col=col)
## plot(mat.noz$ew,mat.noz$f1,pch=19,xlab="E(W)",ylab="F1",ylim=c(0,1),cex=cex,col=col)
## plot(mat.wz$ew,mat.wz$r2,pch=19,xlab="E(W)",ylab="r2",ylim=c(0,1),cex=cex,col=col)
## plot(mat.wz$ew,mat.wz$auc,pch=19,xlab="E(W)",ylab="AUC",ylim=c(.5,1),cex=cex,col=col)
## plot(mat.wz$ew,mat.wz$f1,pch=19,xlab="E(W)",ylab="F1",ylim=c(0,1),cex=cex,col=col)

## par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
## for (nm in c("r2","auc","f1")) {
##     plot(mat.noz$b0,mat.noz[[nm]],pch=19,xlab=expression(beta[0]),ylab=nm)
##     plot(mat.noz$b1,mat.noz[[nm]],pch=19,xlab=expression(beta[1]),ylab=nm)
##     plot(mat.noz$b2,mat.noz[[nm]],pch=19,xlab=expression(beta[2]),ylab=nm)
## }

## par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
## cc<-col2rgb("blue")
## c1<-rgb(cc[1],cc[2],cc[3],alpha=20,maxColorValue=255)
## cc<-col2rgb("red")
## c2<-rgb(cc[1],cc[2],cc[3],alpha=20,maxColorValue=255)
## lp<-function(x,y) {
##     m<-loess(y~x)
##     tmp<-cbind(m$x,fitted(m))
##     tmp[order(tmp[,1]),]
## }
## for (nm in c("r2","auc","f1")) {
##     plot(mat.noz$b0,mat.noz$ew,pch=19,cex=.6,col=c1,ylim=c(0,1),xlab=expression(beta[0]),ylab=paste("Metric (alt=",nm,")",sep=""))
##     if (nm=='r2') legend("topright",bty='n',fill=c("blue","red"),c("IMV","Alt"),title="Metric")
##     lines(lp(mat.noz$b0,mat.noz$ew),col='blue',lwd=2)
##     points(mat.noz$b0,mat.noz[[nm]],pch=19,cex=.6,col=c2)
##     lines(lp(mat.noz$b0,mat.noz[[nm]]),col='red',lwd=2)
## }
## for (nm in c("r2","auc","f1")) {
##     plot(mat.noz$b1,mat.noz$ew,pch=19,cex=.6,col=c1,ylim=c(0,1),xlab=expression(beta[1]),ylab=paste("Metric (alt=",nm,")",sep=""))
##     lines(lp(mat.noz$b1,mat.noz$ew),col='blue',lwd=2)
##     points(mat.noz$b1,mat.noz[[nm]],pch=19,cex=.6,col=c2)
##     lines(lp(mat.noz$b1,mat.noz[[nm]]),col='red',lwd=2)
## }
## ## for (nm in c("r2","auc","f1")) {
## ##     plot(mat.wz$b2,mat.noz$ew,pch=19,cex=.6,col=c1,ylim=c(0,1),xlab=expression(beta[2]),ylab=paste("Metric (alt=",nm,")",sep=""))
## ##     lines(lp(mat.wz$b2,mat.noz$ew),col='blue',lwd=2)
## ##     points(mat.wz$b2,mat.noz[[nm]],pch=19,cex=.6,col=c2)
## ##     lines(lp(mat.wz$b2,mat.noz[[nm]]),col='red',lwd=2)
## ## }


