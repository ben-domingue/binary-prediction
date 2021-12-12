df<-read.csv("spambase.data",header=FALSE)
y<-df[,ncol(df)]

z<-df[,1:57] #just the nonwords
nms<-names(z)
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
for (i in 1:ncol(z)) z[,i]<-std(z[,i])

gr<-sample(1:10,nrow(df),replace=TRUE)
xx<-data.frame(y=y,z,gr=gr)


fm<-paste(nms,collapse="+")

fm<-as.formula(paste('y~1+',fm))
fm0<-as.formula("y~1")

rs<-rowSums(is.na(xx))
xx<-xx[rs==0,]

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

om<-numeric()
for (g in 1:10) {
    print(g)
    z<-xx[xx$gr!=g,]
    m<-glm(fm,z,family="binomial")
    xx0<-xx[xx$gr==g,]
    y<-data.frame(p2=predict(m,xx0,type="response"),resp=xx0$y)
    y[,1]<-ifelse(y[,1]==1,.999,y[,1])
    y[,1]<-ifelse(y[,1]==0,1-.999,y[,1])
    m<-glm(fm0,z,family="binomial")
    y$p1<-predict(m,xx0,type="response")
    om[g]<-imv(y$resp,y$p1,y$p2)
}
mean(om)




