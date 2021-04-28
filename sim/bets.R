simdata<-function(N1,N2,b0,b1,b2) {
    gen.data<-function(x,z,b0=0,b1=.5,b2=1) {
        k<-exp(b0+b1*x+b2*z)
        #k<-exp(.3+.5*x+.2*z)
        p<-k/(1+k)
        y<-rbinom(length(x),1,p)
        y
    }
    ##
    x<-rnorm(N1)
    z<-rnorm(N1)
    train<-data.frame(x=x,y=gen.data(x,b0=b0,b1=b1,b2=b2,z=z),z=z)
    x<-rnorm(N2)
    z<-rnorm(N2)
    test<-data.frame(x=x,y=gen.data(x,b0=b0,b1=b1,b2=b2,z=z),z=z)
    list(train=train,test=test)
}
##ew
ew<-function(x,p0,p1) {
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(z)
        exp(z)
    }    
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    ##
    a0<-ll(x=x$y,p=p0)
    a1<-ll(x=x$y,p=p1)
    coin0<-nlminb(.5,f,lower=0.001,upper=.999,a=a0)$par
    coin1<-nlminb(.5,f,lower=0.001,upper=.999,a=a1)$par
    ##the single-blind bet
    ew<-(coin1-coin0)/coin0
    ew
}
##auc
auc<-function(x,p0,p1) { ##auc
    ##
    library(pROC)
    au<-pROC::auc(response=x$y,predictor=p1)
    ro0<-pROC::auc(x$y,p0)
    au.del<-(au-ro0)/ro0
    au.del
}
##
r2<-function(x,p0,p1) {
    ##r2
    y<-x$y
    r2<-1-sum((y-p1)^2)/sum((y-mean(x$y))^2)
    r2.0<-1-sum((y-p0)^2)/sum((y-mean(x$y))^2)
    r2.del<-(r2-r2.0)/r2.0
    r2.del
}
##ew
bce<-function(x,p0,p1) {
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(z)
        exp(z)
    }    
    ##
    #get predictions in new data
    a0<-ll(x=x$y,p=p0)
    a1<-ll(x=x$y,p=p1)
    (a1-a0)/a0
}
##
simwin<-function(p.bet,p.side,h) { #this will tell us how much you won based on actual gambling
    #p.bet and p.side both give probability of head (x$y=1)
    bet<-ifelse(p.side<.5,0,1) #you bet on T if p.side<.5 and H otherwise
    p.bet<-ifelse(bet==0,1-p.bet,p.bet) #flip this as needed so that p.bet is the probability of the side you betted
    o.bet<-p.bet/(1-p.bet)
    w<-ifelse(h==bet,1/o.bet,-1)
    sum(w)/length(h) #to scale as E(W)
}
win<-function(m,dat,p0,p1) {
    ##
    m1<-m(dat[[1]]$test,p0=p0[[1]],p1=p1[[1]])
    m2<-m(dat[[2]]$test,p0=p0[[2]],p1=p1[[2]])
    print(c(m1,m2))
    ##choose s1 or s2
    s<-rbinom(nrow(dat[[1]]$test),1,m1/(m1+m2))
    s<-ifelse(s==0,2,s)
    #
    p.bet<-cbind(p0[[1]],p0[[2]])
    p.bet<-ifelse(s==1,p.bet[,1],p.bet[,2])
    k<-exp(p.bet)
    p.bet<-k/(1+k)
    p.side<-cbind(p1[[1]],p1[[2]])
    p.side<-ifelse(s==1,p.side[,1],p.side[,2])
    k<-exp(p.side)
    p.side<-k/(1+k)
    hh<-cbind(dat[[1]]$test$y,dat[[2]]$test$y)
    h<-ifelse(s==1,hh[,1],hh[,2])
    simwin(p.bet,p.side,h=h)
}

set.seed(10101010)
b0L<-c(0,1.5)
b1L<-c(1,1)
b2L<-c(1,1)

b0<-c(0,1)
b1<-c(0,1)
b2<-0

df<-list()
N2<-100000
for (ii in 1:length(b0)) for (jj in 1:length(b1)) {
                             tab<-list()
                             for (delta in seq(0,1,length.out=5)) {
                                 b0L<-c(b0[ii],b0[ii]+delta)
                                 b1L<-rep(b1[jj],2) #c(b1[jj],b1[jj]+runif(1,-.25,.25))
                                 b2L<-rep(b2,2)
                                 #b1L<-c(1,1)
                                 #b2L<-c(1,1)
                                 dat<-list()
                                 for (i in 1:length(b0L)) simdata(N1=2000,N2=N2,b0=b0L[i],b1=b1L[i],b2=b2L[i])->dat[[i]]
                                 out<-list()
                                 ##need same true p0 for everyone
                                 ## p0<-list()
                                 ## k<-b0L[1]+b1L[1]*dat[[1]]$train$x
                                 ## p0[[1]]<-exp(k)/(1+exp(k))
                                 ## k<-b0L[2]+b1L[2]*dat[[2]]$train$x
                                 ## p0[[2]]<-exp(k)/(1+exp(k))
                                 p0<-list()
                                 m0<-glm(y~1,dat[[1]]$train,family='binomial')
                                 p0[[1]]<-predict(m0,type="response",data.frame(x=dat[[1]]$test$x))
                                 m0<-glm(y~1,dat[[2]]$train,family='binomial')
                                 p0[[2]]<-predict(m0,type="response",data.frame(x=dat[[2]]$test$x))
                                 p1<-list()
                                 m1<-glm(y~x,dat[[1]]$train,family='binomial')
                                 p1[[1]]<-predict(m1,type="response",data.frame(x=dat[[1]]$test$x))
                                 m1<-glm(y~x,dat[[2]]$train,family='binomial')
                                 p1[[2]]<-predict(m1,type="response",data.frame(x=dat[[2]]$test$x))
                                 ##
                                 out$ew<-win(ew,dat,p0,p1=p1)
                                 out$auc<-win(auc,dat,p0,p1=p1)
                                 out$r2<-win(r2,dat,p0,p1=p1)
                                 out$bce<-win(bce,dat,p0,p1=p1)
                                 tab[[as.character(delta)]]<-c(b0=b0[ii],b1=b1[jj],delta=delta,unlist(out))
                             }
                             df[[paste(ii,jj)]]<-do.call("rbind",tab)
                         }

par(mfrow=c(length(b0),length(b1)),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.5,4))
cols<-c("black","red","green","blue")
for (i in 1:length(df)) {
    z<-data.frame(df[[i]])
    tmp<-z[,c("ew","auc","r2","bce")]
    ran<-range(tmp,na.rm=TRUE)
    M<-max(tmp,na.rm=TRUE)
    plot(NULL,xlim=range(z$delta),ylim=ran); abline(h=0,col='gray')
    for (j in 1:ncol(tmp)) lines(z$delta,tmp[,j],col=cols[j],lwd=2)
    title<-paste(unique(z$b0),unique(z$b1))
    legend("bottomright",bty='n',title=title,names(tmp),fill=cols)
}
