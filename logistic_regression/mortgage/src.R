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


##Munnell, Alicia H., Geoffrey M.B. Tootell, Lynne E. Browne and James McEneaney (1996) “Mortgage lending in Boston: Interpreting HMDA data”, American Economic Review, 25-53.
##discussed in Model Uncertainty and Robustness: A Computational Framework for Multimodel Analysis by Cristobal Young, Katherine Holsteen
#library(Ecdat)
#data(Hmda)
#df<-Hmda

load("Hdma.Rdata") #https://www.dropbox.com/s/mk50v2glx67itfr/Hdma.Rdata?dl=0
## dir debt payments to total income ratio
## hir housing expenses to income ratio
## lvr ratio of size of loan to assessed value of property
## ccs consumer credit score from 1 to 6 (a low value being a good score)
## mcs mortgage credit score from 1 to 4 (a low value being a good score)
## pbcr public bad credit record ?
## dmi denied mortgage insurance ?
## self self employed ?
## single is the applicant single ?
## uria 1989 Massachusetts unemployment rate in the applicant’s industry
## condominium is unit a condominium ? (was called comdominiom in version 0.2-9 and earlier
## versions of the package)
## black is the applicant black ?
## deny mortgage application denied ?

Hdma$deny<-ifelse(Hdma$deny=="yes",1,0)
Hdma<-Hdma[rowSums(is.na(Hdma))==0,]

m<-glm(deny~black+comdominiom+single+self+dmi+pbcr+mcs+ccs+lvr+hir+dir,Hdma,family="binomial")
m0<-glm(deny~comdominiom+single+self+dmi+pbcr+mcs+ccs+lvr+hir+dir,Hdma,family="binomial")
p0<-predict(m0,type="response")
p1<-predict(m,type="response")
imv(Hdma$deny,p0,p1)

m<-glm(deny~black+comdominiom+single+self+dmi+pbcr+mcs+ccs+lvr+hir+dir,Hdma,family="binomial")
m0<-glm(deny~black+comdominiom+single+self+dmi+pbcr+mcs+lvr+hir+dir,Hdma,family="binomial")
p0<-predict(m0,type="response")
p1<-predict(m,type="response")
imv(Hdma$deny,p0,p1)
