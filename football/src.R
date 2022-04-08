set.seed(1234)
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
    prev<-sapply(L,function(x) mean(x$outcome))
    cbind(om,prev)
}

pf<-function(om,txt) {
    xv<-as.numeric(rownames(om))
    yv<-om[,1]
    plot(xv,yv,type='b',ylim=c(0,.4),xlab="Season end",ylab="IMV",col="darkgray",cex=1.3,pch=19)
    m<-loess(yv~xv)
    tmp<-cbind(m$x,fitted(m))
    lines(tmp,col='red',lwd=2)
    ##
    legend("bottomright",bty='n',txt)
}    
pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/ben_figures/soccer.pdf",width=7,height=4)
par(mfrow=c(1,3),mar=c(3,3,1,1),oma=rep(.5,4),mgp=c(2,1,0))
##
om<-outer(x)
pf(om,"All")
##
om<-outer(x[x$country=="England",])
pf(om,"England")
##
om<-outer(x[x$country=="Netherlands",])
pf(om,"Netherlands")
##
dev.off()
