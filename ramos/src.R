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


#############################################################
library(R.matlab)
x<-readMat("scoresExperimentSpeaker.mat")
plot(density(x$scores10TargetCalibrated),xlim=c(-20,20))
lines(density(x$scores10NonTargetCalibrated),col='red') #compare to top right panel of figure 8, doi:10.3390/e20030208

x1<-x$scores10TargetCalibrated[1,]
x2<-x$scores10NonTargetCalibrated[1,]
x<-c(x1,x2)
y<-c(rep(1,length(x1)),rep(0,length(x2)))
df<-data.frame(x=x,y=y)

l2p<-function(df) {
    My<-mean(df$y)
    lr<-exp(df$x)
    #alpha<-lr*My/(1-My)
    #alpha/(1+alpha)
    o<-My/(1-My)
    lr*o/(1+lr*o)
}
df$p<-l2p(df)
imv(df$y,rep(mean(df$y),nrow(df)),df$p)
##[1] 0.013

p0<-mean(df$y)
f<-function(df) imv(df$y,rep(p0,nrow(df)),df$p)
L<-split(df,df$y)
lapply(L,f)

#############################################################
library(R.matlab)
x<-readMat("mvk_scores_glass_fragments.mat")
plot(density(x$targetScores),xlim=c(-20,20))
lines(density(x$nonTargetScores),col='red') #compare to top row of figure 9, doi:10.3390/e20030208
