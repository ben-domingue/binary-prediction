
ff<-function(arg) {
    for (i in 1:length(arg)) assign(names(arg)[i],arg[[i]])
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
    z<-rnorm(N)
    p<-1/(1+exp(-(b1*x+b2*z)))
    y<-rbinom(length(x),1,p)
    y2<-rbinom(length(x),1,p)
    df<-data.frame(x=x,z=z,p=p,y=y,y2=y2)
    ##
    mx<-glm(y~x,df,family="binomial")
    df$fitx<-predict(mx,type="response")
    mz<-glm(y~z,df,family="binomial")
    df$fitz<-predict(mz,type="response")
    mxz<-glm(y~x+z,df,family="binomial")
    df$fitxz<-predict(mxz,type="response")
    ##
    om<-imv(df$y2,rep(mean(df$y),nrow(df)),df$fitxz)
    om.oraclex<-imv(df$y2,df$p,df$fitx)
    om.oraclez<-imv(df$y2,df$p,df$fitz)
    om.oraclexz<-imv(df$y2,df$p,df$fitxz)
    #om.overfit<-imv(df$y,df$fit,df$p)
    ##
    r2<-1-sum((df$y2-df$fit)^2)/sum((df$y2-mean(df$y))^2)
    c(N=N,b1=b1,b2=b2,om=om,
      om.oraclex=om.oraclex,
      om.oraclez=om.oraclez,
      om.oraclexz=om.oraclexz
      )#,aic=m$aic,r2=r2)
}

b1<-rnorm(100)
b2<-.3 #runif(100)
N<-250
z<-data.frame(b1=b1,b2=b2,N=N)
L<-list()
for (i in 1:nrow(z)) L[[i]]<-list(N=z$N[i],b1=z$b1[i],b2=z$b2[i])

library(parallel)
L<-mclapply(L,ff,mc.cores=2)
x<-data.frame(do.call("rbind",L))

pf<-function(x,y,col,txt) {
    tmp<-cbind(x,y)
    tmp<-tmp[order(tmp[,1]),]
    x<-tmp[,1]
    y<-tmp[,2]
    m<-loess(y~x)
    pm<-predict(m,se=TRUE)
    lines(m$x,pm$fit,lwd=2,col=col)
    text(m$x[1],fitted(m)[1],pos=2,txt,col=cols[1],cex=.9)
    cc<-col2rgb(col)
    c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
    polygon(c(m$x,rev(m$x)),c(pm$fit+1.96*pm$se.fit,rev(pm$fit-1.96*pm$se.fit)),col=c1,border=NA)
}
plot(NULL,xlim=range(x$b1),ylim=c(-.5,.5))
pf(x$b1,x$om,col='red','')
pf(x$b1,x$om.oraclexz,col='blue','')
pf(x$b1,x$om.oraclex,col='black','')
pf(x$b1,x$om.oraclez,col='gray','')
