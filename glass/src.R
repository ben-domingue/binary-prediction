df<-read.csv("glass.data",header=FALSE)

## 1. Id number: 1 to 214
## 2. RI: refractive index
## 3. Na: Sodium (unit measurement: weight percent in corresponding oxide, as are attributes 4-10)
## 4. Mg: Magnesium
## 5. Al: Aluminum
## 6. Si: Silicon
## 7. K: Potassium
## 8. Ca: Calcium
## 9. Ba: Barium
## 10. Fe: Iron
## 11. Type of glass: (class attribute)
## -- 1 building_windows_float_processed
## -- 2 building_windows_non_float_processed
## -- 3 vehicle_windows_float_processed
## -- 4 vehicle_windows_non_float_processed (none in this database)
## -- 5 containers
## -- 6 tableware
## -- 7 headlamps
names(df)<-c("id","ri","na","mg","al","si","k","ca","ba","fe","y")
df$id<-NULL
df$y<-ifelse(df$y %in% c(1,3),1,0)
xx<-df

xx$gr<-sample(1:10,nrow(xx),replace=TRUE)
nms<-c("ri","na","mg","al","si","k","ca","ba","fe")
fm<-as.formula(paste('y~1+',paste(nms,collapse="+")))
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
mean(om)

#> mean(om)
#[1] 0.42
