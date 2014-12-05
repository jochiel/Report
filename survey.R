survey<-read.table("survey.csv.csv",header=T,sep=",")
names(survey)<-c("id","rdcpid","timstmp","des","othrdes","edlvl","proflvl","muse","mexprnc","lnprfrnc","prfon1","profon2","profon3","profon4","profon5","profon6","prff1","prff2","prff3","prff4","sof1","sof2","sof3","sof4","sof5","sof6","otrsof","prsof","otrpmsof", "topc1","topc2","topc3","topc4","topc5","topc6","topc7","topc8","topc9","topc10","othtopc","idea","copmlt")
head(survey)
survey$group[survey$des==1]<-"interns"
survey$group[survey$des!=1]<-"noninterns"
plot1 <- ggplot(survey[!is.na(survey$group), ], aes(proflvl, fill = group)) + geom_bar(position = "dodge",binwidth=0.8)+scale_x_discrete(breaks=1:4,labels=c("novice","moderate","proficient","veryproficient"))+ggtitle("distribution of level of statistical analysis")+xlab("proficiency level")
survey$edu[survey$edlvl==1]<-"bachelor"
survey$edu[survey$edlvl==2]<-"masters"
survey$edu[survey$edlvl==3]<-"phd"
survey$edu[survey$edlvl==4]<-"medicaldegree"
plot2<-ggplot(survey[!is.na(survey$edu),],aes(proflvl,fill=edu))+geom_bar(position="dodge",binwidth=0.8)+scale_x_discrete(breaks=1:4,labels=c("novice","moderate","proficient","veryproficient"))+ggtitle("distribution of level of statistical anlysis basing on level of education")+xlab("proficiency level")
survey$lnxprnc[survey$lnprfrnc==1]<-"online"
survey$lnxprnc[survey$lnprfrnc==2]<-"face-to-face"
survey$lnxprnc[survey$lnprfrnc==3]<-"no prefrence"
survey$muse[survey$muse==1]<-"yes"
survey$muse[survey$muse==2]<-"no"
table1<-table(survey$muse,survey$lnprfrnc)
fisher.test(table1)
survey$mexprn[is.na(survey$mexprnc)]<-"missing"
survey$mexprn[survey$mexprnc==1]<-"veryuseful"
survey$mexprn[survey$mexprnc==2]<-"useful"
survey$mexprn[survey$mexprnc==3]<-"not useful"
plot3 <- ggplot(survey[!is.na(survey$edu), ], aes(idea, fill = edu)) + geom_bar(position = "dodge",binwidth=0.8)+scale_x_discrete(breaks=1:3,labels=c("veryuseful","useful","notuseful"))+ggtitle("project rating based on level of education")+xlab("rating")
survey$des[survey$des==1]<-"INT"
survey$des[survey$des==2]<-"R.AS"
survey$des[survey$des==3]<-"PHD"
survey$des[survey$des==4]<-"PDR"
survey$des[survey$des==5]<-"IND"
survey$des[survey$des==6]<-"SNR"
survey$des[survey$des==7]<-"STAT"
survey$des[survey$des==8]<-"others"
plot4 <- ggplot(survey[!is.na(survey$des), ], aes(idea, fill = des)) + geom_bar(position = "dodge",binwidth=0.8)+scale_x_discrete(breaks=1:3,labels=c("veryuseful","useful","not useful"))+ggtitle("project rating based on role")+xlab("rating")
table1<-table(survey$des)
barplot(table1,ylab="count",main="survey respondents",col="blue")
table2<-table(survey$edu)
barplot(table2,ylab="count",main="project rating",col="blue")
 



