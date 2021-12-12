imv<-function(pr) {
    ##
    ll<-function(x,p='pv') {
        z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
        z<-sum(z)/nrow(x)
        exp(z)
    }    
    loglik1<-ll(pr,'pv1')
    loglik2<-ll(pr,'pv2')
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


##https://www.kaggle.com/c/titanic
x<-read.csv("train.csv")
x$died<-1-x$Survived
x$oos<-rbinom(nrow(x),1,p=.1)
test<-x[x$oos==1,]

m<-glm(died~Pclass+Sex+Age+SibSp+Parch+Embarked,x[x$oos==0,],family="binomial")


##prediction relative to prevalence
test$resp<-test$died
test$pv1<-mean(x$died,na.rm=TRUE)
test$pv2<-predict(m,test,type="response")
imv(test[!is.na(test$pv2),])

##no sex
m0<-glm(died~Pclass+Age+SibSp+Parch+Embarked,x[x$oos==0,],family="binomial")
test$pv1<-predict(m0,test,type="response")
imv(test[!is.na(test$pv2),])

##no age
m0<-glm(died~Pclass+Sex+SibSp+Parch+Embarked,x[x$oos==0,],family="binomial")
test$pv1<-predict(m0,test,type="response")
imv(test[!is.na(test$pv2),])



