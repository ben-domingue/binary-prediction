set.seed(1234)
df<-read.csv("processed.cleveland.data",header=TRUE)

## 1. #3 (age)
## 2. #4 (sex)
## 3. #9 (cp)
## 4. #10 (trestbps)
## 5. #12 (chol)
## 6. #16 (fbs)
## 7. #19 (restecg)
## 8. #32 (thalach)
## 9. #38 (exang)
## 10. #40 (oldpeak)
## 11. #41 (slope)
## 12. #44 (ca)
## 13. #51 (thal)
## 14. #58 (num) (the predicted attribute)
names(df)<-c("age","sex","cp","bps","chol","fbs","ecg","thalach","exang","oldpeak","slope","ca","thal","num")

df$y<-ifelse(df$num>1,1,0)

nms<-c("age","sex","cp","bps","chol","fbs","ecg","thalach","exang","oldpeak","slope","ca","thal")

z<-df[,nms]

gr<-sample(1:10,nrow(df),replace=TRUE)
xx<-data.frame(y=df$y,z,gr=gr)

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
    z<-xx[xx$gr!=g,]
    m<-glm(fm,z,family="binomial")
    xx0<-xx[xx$gr==g,]
    y<-data.frame(p2=predict(m,xx0,type="response"),resp=xx0$y)
    m<-glm(fm0,z,family="binomial")
    y$p1<-predict(m,xx0,type="response")
    om[g]<-imv(y$resp,y$p1,y$p2)
}
mean(om)*1000
sd(om)

