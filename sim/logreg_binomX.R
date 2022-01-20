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
    ##x<-rbinom(N,1,p0)
    test<-runif(N,0,1)
    x<-abs(rnorm(N))
    x<-ifelse(test<p0,x,0)
    ##
    p<-1/(1+exp(-1*(b0+b1*x)))
    y<-rbinom(length(x),1,p)
    y2<-y #rbinom(length(x),1,p)
    df<-data.frame(x=x,p=p,y=y,y2=y2)
    ##
    m<-glm(y~x,df,family="binomial")
    df$fit<-predict(m,type="response")
    ##
    om<-imv(df$y2,rep(mean(df$y),nrow(df)),df$fit)
    df0<-df[df$y2==0,]
    om0<-imv(df0$y2,rep(mean(df$y),nrow(df0)),df0$fit)
    df1<-df[df$y2==1,]
    om1<-imv(df1$y2,rep(mean(df$y),nrow(df1)),df1$fit)
    ##
    c(p=p0,b0=b0,b1=b1,prev=mean(df$y),om=om,om0=om0,om1=om1)
}

b0<-c(-3) #c(seq(-6,-4,length=15),seq(4,6,length=15))
N<-50000
p0<-c(.1)
b1<-c(2,5)
pars<-expand.grid(b0=b0,N=N,p0=p0,b1=b1,nn=1:25)
pars<-split(pars,1:nrow(pars))

library(parallel)
x<-mclapply(pars,ff,mc.cores=25)
df<-data.frame(do.call("rbind",x))

id<-paste(df$p,df$b0,df$b1)
L<-split(df,id)
df<-data.frame(do.call("rbind",lapply(L,colMeans)))
df

                                                                
