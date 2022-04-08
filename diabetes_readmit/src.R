set.seed(1234)
df<-read.csv("diabetic_data.csv",header=TRUE)
df$y<-ifelse(df$readmitted=="NO",0,1)

nms<-c("race", "gender", "age", #"weight", 
"admission_type_id",# "discharge_disposition_id", "admission_source_id", 
"time_in_hospital", #"payer_code",
                                        #"medical_specialty",
"num_lab_procedures", 
"num_procedures", "num_medications", "number_outpatient", "number_emergency", 
"number_inpatient", #"diag_1", "diag_2", "diag_3",
"number_diagnoses", 
"max_glu_serum", "A1Cresult",# "metformin", "repaglinide", "nateglinide", 
## "chlorpropamide", "glimepiride", "acetohexamide", "glipizide", 
## "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone", 
## "acarbose", "miglitol", "troglitazone", "tolazamide", "examide", 
## "citoglipton", "insulin", "glyburide.metformin", "glipizide.metformin", 
## "glimepiride.pioglitazone", "metformin.rosiglitazone", "metformin.pioglitazone", 

"change", "diabetesMed")

z<-df[,nms]
index<-grep("_id",names(z))
for (i in index) z[,i]<-as.factor(z[,i])

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

#0.19
