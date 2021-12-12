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


N<-1000

b0seq<-seq(-3,3,length.out=100)
b1seq<-seq(0,3,length.out=100)

sigma<-function(x) 1/(1+exp(-x))
omega<-list()
for (b0 in b0seq) {
    for (b1 in b1seq) {
        x<-rnorm(N)
        p<-sigma(b0+b1*x)
        y<-rbinom(N,1,p)
        m<-glm(y~x,family="binomial")
        x2<-rnorm(N)
        p2<-predict(m,data.frame(x=x2),type="response")
        y2<-rbinom(N,1,p2)
        omega[[paste(b0,b1)]]<-c(b0,b1,imv(y2,rep(mean(y),length(y2)),p2))
    }
}

omega<-do.call("rbind",omega)
omega<-data.frame(omega)
names(omega)<-c("b0","b1","omega")


getcol<-function(val,cols) {
    del<-abs(val-cols$pd)
    if (!all(is.na(del))) {
        index<-which.min(del)
        cols$col[index]
    } else NA
}
getcol<-Vectorize(getcol,"val")

range(omega$omega,na.rm=TRUE)->ran
cols<-colorRampPalette(c("red","blue"))(1000)
cols<-data.frame(pd=seq(ran[1],ran[2],length.out=length(cols)),col=cols)

omega$col<-getcol(omega$omega,cols)

par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=c(rep(.5,3),1))
plot(omega$b0,omega$b1,col=omega$col,pch=19,xlab="b0",ylab="b1",bty='n',cex=.65)
cols$yv<-seq(min(omega$b1),max(omega$b1),length.out=nrow(cols))
points(rep(max(omega$b0)+.2,nrow(cols)),cols$yv,col=cols$col,xpd=TRUE,cex=.25)
text(max(omega$b0)+.2,cols$yv[1],round(cols$pd[1],digits=2),pos=1,xpd=TRUE,cex=.7)
text(max(omega$b0)+.2,cols$yv[nrow(cols)],round(cols$pd[nrow(cols)],digits=2),pos=3,xpd=TRUE,cex=.7)




