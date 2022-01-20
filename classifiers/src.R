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

ff<-function(b,b0) {
    ##example
    x<-rnorm(1000)
    p<-1/(1+exp(-1*(b0+b*x)))
    y<-rbinom(length(x),1,p)
    df<-data.frame(x=x,y=y)
    m<-glm(y~x,df,family="binomial")
    ##to use the imv function, i need: 1. y=observed outcomes, p1=expected prevalence for new outcomes, p2=predicted probabiity for new outcomes. 
    p1<-mean(y) #sjoerd, you'll need to be careful here as i think this is the sticky part for your case
    ##i'm now going to create new outcomes
    x<-rnorm(1000)
    p<-1/(1+exp(-1*(b0+b*x)))
    y<-rbinom(length(x),1,p)
    p2<-predict(m,data.frame(x=x),type="response")
    imv0<-imv(y,p1,p2)
    z<-list()
    eps<-.005
    for (p in seq(.5+eps,1-eps,by=eps)) {
        p2a<-ifelse(p2>.5,p,1-p)
        z[[as.character(p)]]<-c(p,imv(y,p1,p2a))
    }
    z<-do.call("rbind",z)
    list(imv0,z)
}
L<-list()

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
b0vals<-c(0,1,2,3)
cols<-colorRampPalette(c("blue", "red"))( length(b0vals) ) ## (n)
for (b in c(.5,1,2)) {
    plot(NULL,xlim=c(.5,1),ylim=c(-.3,.6),xlab='p*',ylab="IMV")
    abline(h=0,col='gray')
    legend("topright",bty='n',title=paste("b1=",b,sep=''),fill=cols,legend=b0vals)
    for (i in 1:length(b0vals)) {
        b0<-b0vals[i]
        L<-ff(b,b0)
        z<-L[[2]]
        ii<-which.max(z[,2])
        lines(z,lwd=2,col=cols[i])
        arrows(z[ii,1],z[ii,2],0,L[[1]],col=cols[i])
    }
}
