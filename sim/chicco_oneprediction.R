## getvals<-function(df,p) {
##     df$p1<-p
##     ##r2
##     r2<-1-sum((df$y2-df$p1)^2)/sum((df$y2-mean(df$y))^2)
##     r2.true<-1-sum((df$y2-df$p)^2)/sum((df$y2-mean(df$y))^2)
##     ##auc
##     library(pROC)
##     au<-pROC::auc(response=df$y2,predictor=df$p1)
##     au.true<-pROC::auc(response=df$y2,predictor=df$p)
##     ##f1
##     imv<-function(y,p1,p2) {
##         ll<-function(x,p) {
##             z<-log(p)*x+log(1-p)*(1-x)
##             z<-sum(z)/length(x)
##             exp(z)
##         }    
##         loglik1<-ll(y,p1)
##         loglik2<-ll(y,p2)
##         getcoins<-function(a) {
##             f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
##             nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
##         }
##         c1<-getcoins(loglik1)
##         c2<-getcoins(loglik2)
##         ew<-function(p1,p0) (p1-p0)/p0
##         imv<-ew(c2,c1)
##         imv
##     }
##     ew<-imv(df$y2,df$p1,mean(df$y))
##     ew.true<-imv(df$y2,mean(df$y),df$p)
##     c(r2=r2,r2.true=r2.true,
##       auc=au,auc.true=au.true,
##       imv=ew,imv.true=ew.true)
## }

## ptrue<-.5
## y<-rbinom(10000,1,ptrue)
## y2<-rbinom(10000,1,ptrue)
## df<-data.frame(y=y,y2=y2,p=ptrue)
## L<-list()
## for (p in seq(.05,.95,by=.01)) {
##     L[[as.character(p)]]<-c(p=p,getvals(df,p=p))
## }
## df<-data.frame(do.call("rbind",L))

## par(mfcol=c(3,1),mgp=c(2,1,0),mar=c(3,2,1,1),oma=rep(.1,4))
## plot(df$p,df$r2)
## lines(df$p,df$r2.true)
## plot(df$p,df$auc)
## lines(df$p,df$auc.true)
## plot(df$p,df$imv)
## lines(df$p,df$imv.true)
