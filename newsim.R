##hold IMV constant
f<-function(b0,omega=0.01) {
    getp<-function(a) { #see eqn 7. this identies the isomorphic coin
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
    }
    a<-1/(1+exp(-b0))
    w0<-getp(a)
    w1<-omega*w0+w0
    ##
    getp.inv<-function(p) { #see eqn 7. this identies the isomorphic coin
        f<-function(a,p) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.5,upper=.999,p=p)$par
    }
    a1<-getp.inv(w1)
    x<-rnorm(10000)
    ##
    llpart<-function(b1,a1,x) {
        p<-1/(1+exp(-1*(b0+b1*x)))
        l<-p*log(p)+(1-p)*log(1-p)
        abs(log(a1)-mean(l))
    }
    ## llp<-list()
    ## for (b1 in seq(0,1,length.out=25)) llp[[as.character(b1)]]<-llpart(b1,a1=a1,x=x)
    ## plot(seq(0,1,length.out=25),unlist(llp))
    b1<-optim(0,llpart,lower=0,upper=10,a1=a1,x=x,method="Brent")$par
    ##
    p<-1/(1+exp(-1*(b0+b1*x)))
    y<-rbinom(length(x),1,p)
    r2<-cor(y,x)^2
    ##
    c(prev=a,b1=b1,r2=r2)
}
L<-list()
for (b0 in seq(0,1,length.out=25)) L[[as.character(b0)]]<-f(b0)
df1<-data.frame(do.call("rbind",L))
L<-list()
for (b0 in seq(0,1,length.out=25)) L[[as.character(b0)]]<-f(b0,omega=.05)
df2<-data.frame(do.call("rbind",L))

par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,1,0),lwd=2)
plot(df2$prev,df2$b1,type='l',col='red')
lines(df1$prev,df1$b1,col='black')
plot(df2$prev,df2$r2,type='l',col='red')
lines(df1$prev,df1$r2,col='black')
legend("topleft",title="omega",legend=c(0.01,0.03),fill=c("black","red"))
