ff<-function(args) {
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
    df0<-df[df$y2==0,]
    om0<-imv(df0$y2,rep(mean(df$y),nrow(df0)),df0$fit)
    df1<-df[df$y2==1,]
    om1<-imv(df1$y2,rep(mean(df$y),nrow(df1)),df1$fit)
    ##
    c(b0=b0,b1=b1,prev=mean(df$y),om=om,om0=om0,om1=om1)
}

b1L<-c(.5,2,5)
b0L<-seq(-8,0,length=20)
N<-50000
tab<-list()
ntimes<-20
pars<-expand.grid(b0=b0L,N=N,b1=b1L,nn=1:ntimes)
pars<-split(pars,1:nrow(pars))

library(parallel)
x<-mclapply(pars,ff,mc.cores=25)
df<-data.frame(do.call("rbind",x))

id<-paste(df$b0,df$b1)
L<-split(df,id)
df<-data.frame(do.call("rbind",lapply(L,colMeans)))

dump("df","")


b1L<-unique(df$b1)
par(mfrow=c(1,length(b1L)),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
pf<-function(x) {
    x<-x[order(x$prev),]
    plot(NULL,xlim=c(0,.5),ylim=c(0,1),xlab="Prevalence",ylab="IMV")
    legend("topright",bty="n",fill=c("black","red","blue"),legend=c(expression(omega),expression(omega[0]),expression(omega[1])),
                                        #title=as.character(b1)
           )
    lines(x$prev,x$om,col='black',lwd=2)
    lines(x$prev,x$om0,col='red',lwd=2)
    lines(x$prev,x$om1,col='blue',lwd=2)
}
L<-split(df,df$b1)
lapply(L,pf)
