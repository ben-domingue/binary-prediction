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

x<-read.csv("football__prediction_data.csv")
x$year<-substr(x$season,6,9)

outer<-function(x) {
    L<-split(x,x$year)
    f<-function(x) {
        p0<-mean(x$outcome)
        imv(x$outcome,rep(p0,nrow(x)),x$prob.outcome...1.)
    }
    om<-sapply(L,f)
}
par(mfrow=c(1,2),mar=c(3,3,1,1),oma=rep(.5,4),mgp=c(2,1,0))
om<-outer(x)
plot(as.numeric(names(om)),om,type='l',ylim=c(0,.3),xlab="Season end",ylab="IMV")
legend("topleft",bty='n','All')
om<-outer(x[x$country=="England",])
plot(as.numeric(names(om)),om,type='l',ylim=c(0,.3),xlab="Season end",ylab="IMV")
legend("topleft",bty='n','England')

## countries<-unique(x$country)
## plot(NULL,xlim=c(1994,2025),type='l',ylim=c(0,.5))
## countries<-c("England","France","Germany","Spain")
## cols<-colorRampPalette(c("blue", "red"))( length(countries))
## for (i in 1:length(countries)) {
##     country<-countries[i]
##     y<-x[x$country==country,]
##     om<-outer(y)
##     lines(as.numeric(names(om)),om,col=cols[i])
##     n<-length(om)
##     text(as.numeric(names(om))[n],om[n],country,pos=4,cex=.5,col=cols[i])
## }
