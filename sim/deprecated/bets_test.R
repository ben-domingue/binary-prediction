simdata<-function(N,b0,b1,b2) { 
    gen.data<-function(x,z,b0=0,b1=.5,b2=1) {
        k<-exp(b0+b1*x+b2*z)
        #k<-exp(.3+.5*x+.2*z)
        p<-k/(1+k)
        y<-rbinom(length(x),1,p)
        y
    }
    ##
    x<-rnorm(N)
    z<-rnorm(N)
    data.frame(x=x,y=gen.data(x,b0=b0,b1=b1,b2=b2,z=z),z=z)
}
ew<-function(x,p0=p.bet,p1=p.side) { #compute the E(W) metric
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(z)
        exp(z)
    }    
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    ##
    a0<-ll(x=x$y,p=p0)
    a1<-ll(x=x$y,p=p1)
    coin0<-nlminb(.5,f,lower=0.001,upper=.999,a=a0)$par
    coin1<-nlminb(.5,f,lower=0.001,upper=.999,a=a1)$par
    ##the single-blind bet
    ew<-(coin1-coin0)/coin0
    ew
}
simwin<-function(p.bet,p.side,x) { #this will tell us how much you won based on actual gambling
    #p.bet and p.side both give probability of head (x$y=1)
    bet<-ifelse(p.side<.5,0,1) #you bet on T if p.side<.5 and H otherwise
    p.bet<-ifelse(bet==0,1-p.bet,p.bet) #flip this as needed so that p.bet is the probability of the side you betted
    o.bet<-p.bet/(1-p.bet)
    h<-x$y
    w<-ifelse(h==bet,1/o.bet,-1)
    sum(w)/length(h) #to scale as E(W)
}

set.seed(10101010)
b0<-0
b1<-1
b2<-0
N<-50000


x<-simdata(N=N,b0=b0,b1=b1,b2=b2)
#ew if x is side info. note that p.bet and p.side are based on true parameters, not estimates
k<-rep(exp(b0),nrow(x))
p.bet<-k/(1+k)
k<-exp(b0+b1*x$x)
p.side<-k/(1+k)
sw1<-simwin(p.bet=p.bet,p.side=p.side,x=x) #should be .38
w1<-ew(x,p0=p.bet,p1=p.side)
c(w1,sw1)




#ew if z is side info
k<-exp(b0+b1*x$x)
p.bet<-k/(1+k)
k<-exp(b0+b1*x$x+b2*x$z)
p.side<-k/(1+k)
sw2<-simwin(p.bet,p.side,x) #should be 0.12
w2<-ew(x,p.bet,p.side)


out<-list()
b0<-0
b1<-1
for (b2 in seq(0,2.5,by=.1)) {
    simdata(N=N,b0=b0,b1=b1,b2=b2)->x
    ##win if x is side info
    simwin<-function(p.bet,p.side,x) {
        p.bet<-ifelse(p.bet<.5,1-p.bet,p.bet)
        o.bet<-p.bet/(1-p.bet)
        h<-x$y
        bet<-ifelse(p.side<.5,0,1)
        w<-ifelse(h==bet,1/o.bet,-1)
        sum(w)/length(h)
    }
    #ew if x is side info
    k<-rep(exp(b0),nrow(x))
    p.bet<-k/(1+k)
    k<-exp(b0+b1*x$x)
    p.side<-k/(1+k)
    sw1<-simwin(p.bet,p.side,x) #should be .38
    w1<-ew(x,p.bet,p.side)
    #ew if z is side info
    k<-exp(b0+b1*x$x)
    p.bet<-k/(1+k)
    k<-exp(b0+b1*x$x+b2*x$z)
    p.side<-k/(1+k)
    sw2<-simwin(p.bet,p.side,x) #should be 0.12
    w2<-ew(x,p.bet,p.side)
    ##
    out[[as.character(b2)]]<-c(b2,w1,sw1,w2,sw2)
}
tab<-do.call("rbind",out)

plot(tab[,1:2],col='black',type='l',ylim=c(0,.5))
lines(tab[,1],tab[,3],col='red')
lines(tab[,1],tab[,4],col='black',lty=2)
lines(tab[,1],tab[,5],col='red',lty=2)


