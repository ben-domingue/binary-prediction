set.seed(8675309)
##getting the outcomes.
##we'll combine a set of tosses from even coin with tosses from heavily weighted coin
x1<-rbinom(20,1,.5)
x2<-rbinom(20,1,.95)
x<-c(x1,x2)
##a function to compute the log-likelihood
ll<-function(x,p) {
    z<-log(p)*x+log(1-p)*(1-x)
    z<-sum(z)/length(z)
    exp(z)
}    
##baseline prediction
p=.55
a0<-ll(x=x,p=p)
a0
f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
p0<-nlminb(.5,f,lower=0.001,upper=.999,a=a0)$par
p0

##enhanced prediction
p<-c(rep(.5,20),rep(.9,20))
a1<-ll(x=x,p=p)
p1<-nlminb(.5,f,lower=0.001,upper=.999,a=a1)$par
##the single-blind bet
ew<-(p1-p0)/p0







## pv<-seq(.5,.99,by=.001)
## y<-list()
## for (p in pv) {
##     for (i in 1:500) {
##         z<-rbinom(1000,1,p)
##         z<-log(p)*z+log(1-p)*(1-z)
##         z<-sum(z)/length(z)
##         y[[paste(p,i)]]<-c(p,exp(z))
##     }
## }
## y<-do.call("rbind",y)
## plot(y,pch=19,cex=.4); abline(0,1)

## z<-aggregate(y[,2],list(y[,1]),mean)
## lines(z,col='red',lwd=2)

## names(z)<-c("p","ll")
## z->coins
## save(coins,file="coins.Rdata")

## getp<-function(a) {
##     f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
##     nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
## }

    
## ## ##bets
## ## win<-list()
## ## for (p in seq(.5,.99,by=.005)) {
## ##     z<-rbinom(500000,1,p)
## ##     odds<-p/(1-p)
## ##     #house puts up $1
## ##     house<-1
## ##     me<-house*odds
## ##     w<-sum(z*(me+house))-length(z)*me
## ##     win[[as.character(p)]]<-c(p,w/length(z))
## ## }
## ## win<-do.call("rbind",win)
## ## plot(win)

## win<-function(p) {
##     bet<-function(tp,ap) (tp-ap)/ap
##     bet(p[,2],p[,1])
## }
