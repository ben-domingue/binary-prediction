simdata<-function(N,b0,b1) { 
    gen.data<-function(x,z,b0=0,b1=.5) {
        k<-exp(b0+b1*x)
        #k<-exp(.3+.5*x+.2*z)
        p<-k/(1+k)
        y<-rbinom(length(x),1,p)
        y
    }
    ##
    x<-rbinom(N,1,.5)
    #z<-rnorm(N)
    data.frame(x=x,y=gen.data(x,b0=b0,b1=b1))
}
ew<-function(x,p0=p.bet,p1=p.side) { #compute the E(W) metric
    ll<-function(x,p) {
        z<- log(p)*x+log(1-p)*(1-x)
        z<- sum(z)/length(z)
        z#exp(z)
    }    
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-a)#log(a))
    ##
    a0<-ll(x=x$y,p=p0)
    a1<-ll(x=x$y,p=p1)
    print(exp(c(a0,a1)))
    coin0<-nlminb(.5,f,lower=0.001,upper=.999,a=a0)$par
    coin1<-nlminb(.5,f,lower=0.001,upper=.999,a=a1)$par
    print(c(coin0,coin1))
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
N<-1000000
x<-simdata(N=N,b0=b0,b1=b1)
k<-rep(exp(b0),nrow(x))
p.bet<-k/(1+k)
k<-exp(b0+b1*x$x)
p.side<-k/(1+k)
df<-data.frame(p.bet=p.bet,p.side=p.side,x=x$x,y=x$y)
rm("x")

n<-1000000
x1<-rbinom(n,1,.5)
x2<-rbinom(n,1,.73)
x<-c(x1,x2)
sum(x==1)/length(x)



df2<-df[df$x==1,]
sw1<-simwin(p.bet=df2$p.bet,p.side=df2$p.side,x=df2[,c("x","y")]) #should be .38
w1<-ew(df2[,c("x","y")],p0=df2$p.bet,p1=df2$p.side)
c(w1,sw1)
df2<-df[df$x==0,]
sw1<-simwin(p.bet=df2$p.bet,p.side=df2$p.side,x=df2[,c("x","y")]) #should be .38
w1<-ew(df2[,c("x","y")],p0=df2$p.bet,p1=df2$p.side)
c(w1,sw1)

#ew if x is side info. note that p.bet and p.side are based on true parameters, not estimates
sw1<-simwin(p.bet=df$p.bet,p.side=df$p.side,x=df[,c("x","y")]) #should be .38
w1<-ew(df[,c("x","y")],p0=df$p.bet,p1=df$p.side)
c(w1,sw1)




x<-rbinom(10000,1,.67)
simwin(p.bet=.5,p.side=.67,x=data.frame(y=x))



f<-function(p) abs(p*log(p)+(1-p)*log(1-p)-.5*(.5*log(.5)+.5*log(.5)+.73*log(.73)+(1-.73)*log(1-.73)))
nlminb(.5,f,lower=0.001,upper=.999)

f<-function(x,p=.62) abs(p*log(p)+(1-p)*log(1-p)-x*(.5*log(.5)+.5*log(.5))-(1-x)*(.73*log(.73)+(1-.73)*log(1-.73)))
nlminb(.5,f,lower=0.001,upper=.999)

