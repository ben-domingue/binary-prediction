vars<-read.csv("GSS_vars.csv",sep="|",header=TRUE)
df<-read.csv("GSS.csv",sep="|",header=TRUE)

id<-df$Respondent.id.number
NULL->df$Respondent.id.number
df$Gss.year.for.this.respondent->year
NULL->df$Gss.year.for.this.respondent
NULL->df$Ballot.used.for.interview

nms<-vars[,1:2]
nms<-nms[!is.na(nms[,1]),]
nms[,2]->names(df)
id->df$hhidpn
year->df$year


last.nm<-vars[1,2]
for (i in 1:nrow(vars)) {
    nm<-vars[i,2]
    if (nm=='') nm<-last.nm else last.nm<-nm
    vars[i,2]<-nm
}
L<-split(vars,vars[,2])
ii<-grep("age",names(L))
L<-L[-ii]

for (nm in names(df)) {
    if (nm %in% names(L)) {
        index<-match(df[[nm]],L[[nm]][,4])
        df[[nm]]<-L[[nm]][index,3]
    }
}

    
badvals<-function(x,vals) ifelse(x %in% vals,x,NA)
df$partyid<-badvals(df$partyid,c(0:2,4:6))
df$gop<-ifelse(df$partyid<4,1,0)
df$mempolit<-badvals(df$mempolit,1:2)
df$income<-badvals(df$income,1:12)
df$mobile16<-badvals(df$mobile16,1:3)
df$same.city<-ifelse(df$mobile16==1,0,1)
df$white<-ifelse(df$race==1,1,0)
df$male<-ifelse(df$sex==1,1,0)
df$educ<-badvals(df$educ,1:20)
df$age<-badvals(df$age,1:89)
df$childs<-badvals(df$childs,1:8)
df$marital<-factor(badvals(df$marital,1:5))
df$commute<-badvals(df$commute,0:97)
df$wrkstat<-factor(badvals(df$wrkstat,0:8))

save(df,file="df_gss.Rdata")
