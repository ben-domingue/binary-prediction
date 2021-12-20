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
    list(train=train,test=test,b0=b0,b1=b1,b2=b2)
}

set.seed(10101010)
b0L<-c(0,1)
b1L<-c(.5,1)
b2<-0
z<-expand.grid(b0L,b1L,b2)
dat<-list()
for (i in 1:nrow(z)) {
    tmp<-list()
    for (j in 1:100) tmp[[j]]<-simdata(N=2000,b0=z[i,1],b1=z[i,2],b2=z[i,3])
    dat[[i]]<-tmp
}


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

x<-list()
for (i in 1:length(dat)) {
    r1<-list()
    for (j in 1:length(dat[[i]])) r1[[paste(dat[[i]][[j]]$b0,dat[[i]][[j]]$b1,j)]]<-simfun(df.tr=dat[[i]][[j]]$train,df.test=dat[[i]][[j]]$test,fm1="y~1",fm2="y~x")
    x[[i]]<-do.call("rbind",r1)
}

par(mfrow=c(4,3),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.1,4),pch=19)
for (i in 1:length(x)) {
    z<-data.frame(x[[i]])
    plot(z$ew,z$r2,xlim=c(0,1),ylim=c(0,1))
    plot(z$ew,z$auc,xlim=c(0,1),ylim=c(0,1))
    plot(z$ew,z$f1,xlim=c(0,1),ylim=c(0,1))
}
