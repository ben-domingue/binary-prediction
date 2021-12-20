##binomial x
ff<-function(args) {#b0,b1=b1,N=N,p0) {
    for (i in 1:length(args)) assign(names(args)[i],args[[i]])
    imv<-function(y,p1,p2) {
        ##
        ll<-function(x,p) {
            z<-log(p)*x+log(1-p)*(1-x)
            z<-sum(z)/length(x)
            exp(z)
        }    
        loglik1<-ll(y,p1)
        loglik2<-ll(y,p2)
        getcoins<-function(a) {
            f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
            nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
        }
        c1<-getcoins(loglik1)
        c2<-getcoins(loglik2)
        ew<-function(p1,p0) (p1-p0)/p0
        imv<-ew(c2,c1)
        imv
    }
    x<-rbinom(N,1,p0)
    p<-1/(1+exp(-1*(b0+b1*x)))
    y<-rbinom(length(x),1,p)
    y2<-rbinom(length(x),1,p)
    df<-data.frame(x=x,p=p,y=y,y2=y2)
    ##
    m<-glm(y~x,df,family="binomial")
    df$fit<-predict(m,type="response")
    ##
    om<-imv(df$y2,rep(mean(df$y),nrow(df)),df$fit)
    df0<-df[df$y==0,]
    om0<-imv(df0$y2,rep(mean(df$y),nrow(df0)),df0$fit)
    df1<-df[df$y==1,]
    om1<-imv(df1$y2,rep(mean(df$y),nrow(df1)),df1$fit)
    ##
    c(p=p0,b0=b0,b1=b1,prev=mean(df$y),om=om,om0=om0,om1=om1)
}

b0<-c(-5) #c(seq(-6,-4,length=15),seq(4,6,length=15))
N<-50000
p0<-c(.999,.99,.01,.001)
b1<-c(2,4)
pars<-expand.grid(b0=b0,N=N,p0=p0,b1=b1,nn=1:50)
pars<-split(pars,1:nrow(pars))

library(parallel)
x<-mclapply(pars,ff,mc.cores=25)
df<-data.frame(do.call("rbind",x))
df$om0om<-df$om0/df$om
df$om1om<-df$om1/df$om
df<-df[is.finite(df$om0om),]

id<-paste(df$p,df$b0,df$b1)
L<-split(df,id)
t(sapply(L,colMeans))

  m<-by(df$om0om,id,mean)
prev<-by(df$prev,id,mean)
s<-by(df$om0om,id,max)
cbind(prev,m,s)
       
                                                                



#######################################
ff<-function(b0,b1=b1,N=N) {
    imv<-function(y,p1,p2) {
        ##
        ll<-function(x,p) {
            z<-log(p)*x+log(1-p)*(1-x)
            z<-sum(z)/length(x)
            exp(z)
        }    
        loglik1<-ll(y,p1)
        loglik2<-ll(y,p2)
        getcoins<-function(a) {
            f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
            nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
        }
        c1<-getcoins(loglik1)
        c2<-getcoins(loglik2)
        ew<-function(p1,p0) (p1-p0)/p0
        imv<-ew(c2,c1)
        imv
    }
    x<-rnorm(N)
    p<-1/(1+exp(-1*(b0+b1*x)))
    y<-rbinom(length(x),1,p)
    y2<-rbinom(length(x),1,p)
    df<-data.frame(x=x,p=p,y=y,y2=y2)
    ##
    m<-glm(y~x,df,family="binomial")
    df$fit<-predict(m,type="response")
    ##
    om<-imv(df$y2,rep(mean(df$y),nrow(df)),df$fit)
    df0<-df[df$y==0,]
    om0<-imv(df0$y2,rep(mean(df$y),nrow(df0)),df0$fit)
    df1<-df[df$y==1,]
    om1<-imv(df1$y2,rep(mean(df$y),nrow(df1)),df1$fit)
    ##
    c(b0,mean(df$y),om,om0,om1)
}

par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
b0L<-seq(-5,5,length=35)
for (b1 in c(.25,.5,1,2)) {
    plot(NULL,xlim=c(0,1),ylim=c(0,.8),xlab="Prevalence",ylab="IMV")
    legend("topright",bty="n",fill=c("black","red","blue"),legend=c(expression(omega),expression(omega[0]),expression(omega[1])),title=as.character(b1))
    N<-5000
    tab<-list()
    for (i in 1:length(b0L)) {
        tmp<-list()
        for (ii in 1:20) tmp[[ii]]<-ff(b0L[i],b1=b1,N=N)
        tmp<-do.call("rbind",tmp)
        tab[[i]]<-colMeans(tmp)
    }
    x<-do.call("rbind",tab)
    lines(x[,2],x[,3],col='black')
    lines(x[,2],x[,4],col='red')
    lines(x[,2],x[,5],col='blue')
}
