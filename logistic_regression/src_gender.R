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

models<-function(x,fm) {
    x$group<-sample(1:10,nrow(x),replace=TRUE)
    x.hold<-x
    beta<-om<-om00<-numeric()
    for (i in 1:10) {
        x<-x.hold
        x$oos<-ifelse(x$group==i,1,0)
        test<-x[x$oos==1,]
        m<-glm(fm,x[x$oos==0,],family="binomial")
        m0<-glm(update.formula(fm,.~.-female),x[x$oos==0,],family="binomial")
        m00<-glm(paste(all.vars(fm)[1],"~1"),x[x$oos==0,],family="binomial")
        print(m00)
        ##
        xx<-test[,all.vars(fm)]
        test<-test[rowSums(is.na(xx))==0,]
        p00<-predict(m00,test,type="response")
        p0<-predict(m0,test,type="response")
        p1<-predict(m,test,type="response")
        co<-coef(m)
        beta[i]<-co[which(names(co)=="female")]
        om[i]<-imv(test[[all.vars(fm)[1] ]],p0,p1)
        om00[i]<-imv(test[[all.vars(fm)[1] ]],p00,p0)
    }
    per.female<-mean(x.hold$female,na.rm=TRUE)
    prevalence.outcome<-mean(x.hold[[all.vars(fm)[1] ]],na.rm=TRUE)
    c(N=nrow(x.hold),per.female=per.female,prevalence.outcome=prevalence.outcome,beta=mean(beta),omega0=mean(om00),omega=mean(om))
}


data<-list()

############################################
##Titanic
##https://www.kaggle.com/c/titanic
x<-read.csv("train.csv")
x$died<-1-x$Survived
x$female<-ifelse(x$Sex=="female",1,0)
x$outcome<-x$died
data$Titanic<-x
fm.titanic<-as.formula("outcome~Pclass+female+Age+SibSp+Parch+Embarked")
m<-glm(fm.titanic,x,family="binomial")
exp(coef(m))

by(x$outcome,x$female,mean,na.rm=TRUE)

############################################
##GSS
load("df_gss.Rdata")
df$female<-ifelse(df$sex==2,1,0)
df$age<-as.numeric(df$age)
df$outcome<-df$gop
data$GSS<-df
fm.gss<-as.formula("outcome~female+age+factor(marital)+factor(race)")
summary(glm(fm.gss,df,family="binomial"))

############################################
##descriptives
f<-function(z,fm) {
    n<-nrow(z)
    mf<-mean(z$female,na.rm=TRUE)
    mo<-mean(z$outcome,na.rm=TRUE)
    m<-glm(fm,z,family="binomial")
    co<-coef(m)[2]
    c(n,mf,mo,co)
}
x1<-f(data$Titanic,fm=fm.titanic)
x2<-f(data$GSS,fm=fm.gss)
tab<-do.call("rbind",list(x1,x2))

############################################
omega<-list()
omega$Titanic<-models(x,fm.titanic)
omega$GSS<-models(df,fm.gss)
z<-data.frame(do.call("rbind",omega))

z

library(xtable)
xtable(z)





