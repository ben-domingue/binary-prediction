df<-read.csv("merged_final.csv")

mm<-median(df$FAMILY_INCOME,na.rm=TRUE)
y<-ifelse(df$FAMILY_INCOME>mm,1,0)


nms<-c(#"Winning_Competitions",
    "Math", "AP_Classes", "Work_And_Goals", 
"Camping_Swimming", "Social_Anxiety", "Gendered_Activities", 
"Fashion_Style", "Family_Members", "Medical_Experiences", "Helping_Others", 
"Despite_Words", "Latinx_Family_Issues", "Education_Opportunity", 
"Classroom_Experiences", "Youth_Volunteering", "Reading_Writing", 
"Making_Planning", "Visual_Art", "Travel", "Leadership_Skills", 
"Seeking_Answers", "Mental_Health", "Outside_School_Programs", 
"Volunteer_Cleaning", "Work_Experiences", "Family_Death", "Motivations_Goals", 
"Psychology_Understanding", "Group_Leadership", "Sports_Experiences", 
"World_Histories", "China", "Language_Experiences", "Cooking", 
"Civic_Experiences", "Time_Management", "Sensory_Experiences", 
"Sociocultural_Diversity", "Business_Economics", "Performance_Art", 
"Computer_Science", "Photography", "School_Activities", "Humor_Storytelling", 
"Group_Assignments", "Work_Money", "Process_Words", "Boy_Scouts", 
"Video_Film", "Family_Church", "Building_Engines", "Human_Nature", 
"Music", "Life_Reflections", "Time_Cycles", "Life_Challenges", 
"Sensory_Responses", "HS_Years", "Sports_General", "School_Grades", 
"Dancing_Art", "Community_Service", "Preference_Words", "Achievement_Words", 
"Puzzles_Problems", "Chemistry_Biology", "Tutoring_Groups", "Physics", 
"New_Exepriences")

z<-df[,nms]
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
for (nm in nms) z[[nm]]<-std(z[[nm]])

gr<-sample(1:10,nrow(df),replace=TRUE)
xx<-data.frame(y=y,z,gr=gr,sat=df$RSAT_TOTAL_SCORE)

cor(xx$y,xx$sat)

fm<-paste(nms,collapse="+")
fm<-as.formula(paste('y~1+sat+',fm))
fm0<-as.formula("y~1+sat")

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
