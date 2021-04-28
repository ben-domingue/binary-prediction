##different prev
set.seed(10101010)
x<-rnorm(5000)
tab<-list()
for (b0 in seq(0,3,by=.005)) {
    k<-exp(b0+.5*x)
    p<-k/(1+k)
    y<-rbinom(length(x),1,p)
    df<-data.frame(x=x,y=y,oos=rbinom(1:length(x),1,.1))
    m0<-glm(y~1,df,family="binomial")
    m1<-glm(y~x,df,family="binomial")
    df$p0<-predict(m0,type="response")
    df$p1<-predict(m1,type="response")
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(z)
        exp(z)
    }    
    ##baseline prediction
    a0<-ll(x=df$y,p=df$p0)
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    p0<-nlminb(.5,f,lower=0.001,upper=.999,a=a0)$par
    ##enhanced prediction
    a1<-ll(x=df$y,p=df$p1)
    p1<-nlminb(.5,f,lower=0.001,upper=.999,a=a1)$par
    ##the single-blind bet
    ew<-(p1-p0)/p0
    tab[[as.character(b0)]]<-c(mean(df$y),ew)
}

z<-do.call("rbind",tab)
m<-loess(z[,2]~z[,1])
plot(m$x,predict(m),type='l',lwd=2)


############################################################3
############################################################3
############################################################3
############################################################3
##same ew
ew<- 0.1
set.seed(10101010)
x<-rnorm(5000)
tab<-list()
for (b0 in seq(0,6,by=.1)) {
    tmp<-list()
    print(b0)
    for (b1 in seq(0,10,by=.1)) {
        k<-exp(b0+b1*x)
        p<-k/(1+k)
        y<-rbinom(length(x),1,p)
        df<-data.frame(x=x,y=y,oos=rbinom(1:length(x),1,.1))
        m0<-glm(y~1,df,family="binomial")
        m1<-glm(y~x,df,family="binomial")
        df$p0<-predict(m0,type="response")
        df$p1<-predict(m1,type="response")
        ll<-function(x,p) {
            z<-log(p)*x+log(1-p)*(1-x)
            z<-sum(z)/length(z)
            exp(z)
        }    
        ##baseline prediction
        a0<-ll(x=df$y,p=df$p0)
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        p0<-nlminb(.5,f,lower=0.001,upper=.999,a=a0)$par
        ##enhanced prediction
        a1<-ll(x=df$y,p=df$p1)
        p1<-nlminb(.5,f,lower=0.001,upper=.999,a=a1)$par
        ##the single-blind bet
        ew.tmp<-(p1-p0)/p0
        tmp[[as.character(b1)]]<-c(mean(df$y),b1,ew.tmp)
    }
    tmp<-do.call("rbind",tmp)
    ii<-which.min(abs(tmp[,3]-ew))
    tab[[as.character(b0)]]<-tmp[ii,c(1,2)]
}

z<-do.call("rbind",tab)
m<-loess(z[,2]~z[,1])
plot(m$x,predict(m),type='l',lwd=2)
points(z[,1],z[,2],col='gray',cex=.5)

