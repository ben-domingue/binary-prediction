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
    p<-1/(1+exp(-b*x))
    y<-rbinom(length(x),1,p)
    y2<-rbinom(length(x),1,p)
    df<-data.frame(x=x,p=p,y=y,y2=y2)
    ##
    m<-glm(y~x,df,family="binomial")
    df$fit1<-predict(m,type="response")
    m<-glm(y~x+I(x^2),df,family="binomial")
    df$fit2<-predict(m,type="response")
    ##
    om0<-imv(df$y,df$fit1,df$fit2)
    om<-imv(df$y2,df$fit1,df$fit2)
    ##
    c(N=N,b=b,om=om0,om=om)
}

L<-list()
N<-runif(10000,log10(50),log10(2500))
N<-sort(round(10^(N)))

tab<-list()
for (i in 1:length(N)) tab[[i]]<-ff(list(N=N[i],b=.5))
z<-do.call("rbind",tab)


pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/ben_figures/complexity.pdf",width=5,height=4)
par(mgp=c(2,1,0),mar=c(3,3,1,1))
plot(NULL,xlim=c(50,2500),ylim=c(-.025,.025),xlab="Sample Size",ylab="IMV")
abline(h=0,col='gray')
title(line=0.5,bquote("True model: "~sigma~"(.5*x)"))
m<-loess(z[,3]~z[,1])
lines(m$x,fitted(m),col='red',lwd=2)
m<-loess(z[,4]~z[,1])
lines(m$x,fitted(m),col='blue',lwd=2)
legend("bottomright",bty='n',fill=c("blue","red"),
       title="IMV comparing linear and quadratic fit based on",
       c("out-of-sample data","in-sample data")
       )
dev.off()
