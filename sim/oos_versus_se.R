

f<-function(N,b,k=10) {
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
    sigma<-function(x) 1/(1+exp(-x))
    x<-rnorm(N)
    p<-sigma(b*x)
    y<-rbinom(N,1,p)
    m<-glm(y~x,family="binomial")
    se<-summary(m)$coef[2,2]
    y2<-rbinom(N,1,p)
    om<-imv(y2,rep(mean(y),N),fitted(m))
    ##kfold
    df<-data.frame(x=x,y=y)
    df$gr<-sample(1:k,nrow(df),replace=TRUE)
    om.oos<-numeric()
    for (i in 1:k) {
        df0<-df[df$gr!=i,]
        m<-glm(y~x,df0,family="binomial")
        pr<-predict(m,df[df$gr==i,],"response")
        yy<-df$y[df$gr==i]
        om.oos[i]<-imv(yy,rep(mean(df0$y),length(yy)),pr)
    }    
    ##
    c(N=N,b=b,se=se,om=om,om.mean=mean(om.oos),om.sd=sd(om.oos))
}

N<-runif(250,log10(100),log10(100000))
N<-round(10^N)
out<-list()
for (b in c(.1,.5)) {
    for (i in 1:length(N)) {
        out[[paste(b,i)]]<-f(N[i],b)
    }
}
z<-data.frame(do.call("rbind",out))
L<-split(z,z$b)

pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/kfold_sd.pdf",width=7,height=6)
par(mfrow=c(length(L),2),mgp=c(2,1,0),mar=c(3,3,1,1))
cols<-c("blue","red","black")
for (i in 1:length(L)) {
    z<-L[[i]]
    z<-z[order(z$N),]
    plot(NULL,xlim=c(100,10000),ylim=c(0,.3),xlab="N",ylab="SE")
    points(z$N,z$se,col=cols[i],pch=19,cex=.5)
    legend("topright",bty='n',title=expression(beta),legend=unique(z$b))
                                        #plot(NULL,xlim=c(100,10000),ylim=c(-.1,.45),xlab="N",ylab=expression(omega))
    #points(z$N,z$om,col=cols[i])
    #plot(NULL,xlim=c(100,10000),ylim=c(-.1,.45),xlab="N",ylab=expression(omega))
    #points(z$N,z$om.mean,col=cols[i])
    plot(NULL,xlim=c(100,10000),ylim=c(0,.3),xlab="N",ylab=paste("sd omega"))
    points(z$N,z$om.sd,col=cols[i],pch=19,cex=.5)
}
dev.off()




    
