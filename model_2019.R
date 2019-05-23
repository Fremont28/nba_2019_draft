library(ggplot2)
library(plyr)
library(dplyr)
library(quantreg)
library(data.table)
library(mltools)
library(caret)

#read in datasets 
coll=read.csv("np1.csv") #college (entire college career 1 to 4 years)
nba=read.csv("jelly2.csv") #nba (first three seasons in the nba 09-10 to 18-19) 

#college pct
coll$two_field=coll$X2P/coll$X2PA
coll$three_field=coll$X3P/coll$X3PA
coll$free_throw=coll$FT/coll$FTA

#college data subset 
coll1=coll[,7:26]
coll2=coll[,28:28]
coll3=coll[,32:50]
drops <- c("Weight.1")
coll3=coll3[ , !(names(coll3) %in% drops)]
coll_final=cbind(coll1,coll2,coll3)
colnames(coll_final)[21] <- "Weight"

for(i in 1:ncol(coll_final)){
  coll_final[is.na(coll_final[,i]), i] <- mean(coll_final[,i], na.rm = TRUE)
}

#subset nba data ------- 
nba1=nba[,4:8]
nba2=nba[,40:59]
nba3=nba[,88:103]
nba_fin=cbind(nba1,nba2,nba3)

#rename nba columns 
colnames(nba_fin)[5] <- "Age"
colnames(nba_fin)[6] <- "G"
colnames(nba_fin)[7] <- "MP"
colnames(nba_fin)[8] <- "MP_G"
colnames(nba_fin)[9] <- "FG"
colnames(nba_fin)[10] <- "FGA"
colnames(nba_fin)[11] <- "X2P"
colnames(nba_fin)[12] <- "X2PA"
colnames(nba_fin)[13] <- "X3P"
colnames(nba_fin)[14] <- "X3PA"
colnames(nba_fin)[15] <- "FT"
colnames(nba_fin)[16] <- "FTA"
colnames(nba_fin)[17] <- "ORB"
colnames(nba_fin)[18] <- "DRB"
colnames(nba_fin)[19] <- "TRB"
colnames(nba_fin)[20] <- "AST"
colnames(nba_fin)[21] <- "STL"
colnames(nba_fin)[22] <- "BLK"
colnames(nba_fin)[23] <- "TOV"
colnames(nba_fin)[24] <- "PF"
colnames(nba_fin)[25] <- "PTS"
colnames(nba_fin)[41] <- "VORP"

#nba pct 
nba_fin$two_field=nba_fin$X2P/nba_fin$X2PA
nba_fin$three_field=nba_fin$X3P/nba_fin$X3PA
nba_fin$free_throw=nba_fin$FT/nba_fin$FTA

#fill in missing values with column mean 
nba_fin1=nba_fin[,5:44]

for(i in 1:ncol(nba_fin1)){
  nba_fin1[is.na(nba_fin1[,i]), i] <- mean(nba_fin1[,i], na.rm = TRUE)
}

#generalized linear model (GAM) projecting three-year VORP (for 2019 draft class)
fit_first=gam(VORP~Sprint+Age+
                G+MP+MP_G+FG+FGA+X2P+
                X2PA+X3P+X3PA+FT+FTA+ORB+
                DRB+AST+STL+BLK+TOV+PF+
                PTS+Height..No.Shoes.+
                Height..With.Shoes.+Wingspan+Standing.reach+
                Vertical..Max.+Vertical..Max.Reach.+
                Vertical..No.Step.+Vertical..No.Step.Reach.+
                Weight+Body.Fat+Hand..Length.+Hand..Width.+
                Bench+Agility+two_field+three_field+free_throw,data=nba_fin1)

vorp_pred1=predict(fit_first,coll_final) 
vorp_pred1=as.data.frame(vorp_pred1)
players=subset(coll,select=c("Player","rank"))
preds1=cbind(players,vorp_pred1)

#test for data normality
qqnorm(nba_fin1$VORP)
shapiro.test(nba_fin1$VORP) 

#model evaluation 
cor.test(preds1$rank,preds1$vorp_pred1) # 0.32 
1-(fit_first$deviance/fit_first$null) #0.35 
ggplot(preds1,aes(x=rank,y=vorp_pred1))+geom_point()

#all nba players (00-01 to 18-19) ------------------
past_nba=read.csv("jelly3.csv")

nbaP1=past_nba[,8:8]
nbaP2=past_nba[,40:59]
nbaP3=past_nba[,4:5]
nbaP4=past_nba[,85:85]

nba_finX=cbind(nbaP3,nbaP4,nbaP1,nbaP2)  

colnames(nba_finX)[3] <- "VORP"
colnames(nba_finX)[4] <- "Age"
colnames(nba_finX)[5] <- "G"
colnames(nba_finX)[6] <- "MP"
colnames(nba_finX)[7] <- "MP_G"
colnames(nba_finX)[8] <- "FG"
colnames(nba_finX)[9] <- "FGA"
colnames(nba_finX)[10] <- "X2P"
colnames(nba_finX)[11] <- "X2PA"
colnames(nba_finX)[12] <- "X3P"
colnames(nba_finX)[13] <- "X3PA"
colnames(nba_finX)[14] <- "FT"
colnames(nba_finX)[15] <- "FTA"
colnames(nba_finX)[16] <- "ORB"
colnames(nba_finX)[17] <- "DRB"
colnames(nba_finX)[18] <- "TRB"
colnames(nba_finX)[19] <- "AST"
colnames(nba_finX)[20] <- "STL"
colnames(nba_finX)[21] <- "BLK"
colnames(nba_finX)[22] <- "TOV"
colnames(nba_finX)[23] <- "PF"
colnames(nba_finX)[24] <- "PTS" #state of the union?? 

#nba fg pct 
nba_finX$two_field=nba_finX$X2P/nba_finX$X2PA
nba_finX$three_field=nba_finX$X3P/nba_finX$X3PA
nba_finX$free_throw=nba_finX$FT/nba_finX$FTA

#fill in missing values 
nba_finX1=nba_finX[,3:27]

for(i in 1:ncol(nba_finX1)){
  nba_finX1[is.na(nba_finX1[,i]), i] <- mean(nba_finX1[,i], na.rm = TRUE)
}


#model for projecting 5-year VORP for 2019 draft class (using only college metrics)
gam5=gam(VORP~
           G+MP+MP_G+FG+FGA+X2P+
           X2PA+X3P+X3PA+FT+FTA+ORB+
           DRB+AST+STL+BLK+TOV+PF+
           PTS+two_field+three_field+free_throw,data=nba_finX1)

pred_gam5=predict(gam5,coll_final)
preds8=cbind(players,pred_gam5) 
write.csv(preds8,file="preds8.csv") 

cor.test(preds8$rank,preds8$pred_gam5) #0.28  
ggplot(preds8,aes(x=rank,y=pred_gam5))+geom_point()+xlab("Pre Draft Ranking")+
  ylab("Five Year Vorp")+geom_text(aes(label=ifelse(pred_gam5>4.0,as.character(Player),'')),hjust=0,vjust=0)

#residual deviance 
1-(gam5$deviance/gam5$null) #0.19




#************************************************
#More Analysis on Draft and Player trends 
#1. early vorp first five seasons  
sub3=subset(all_nba,Age_x<=24) #roughly age under/equal 24
t1=ddply(sub3,.(Team), summarize, vorp_avg=mean(VORP_x),vorp_sd=mean(VORP_x))
t1[order(-t1$vorp_avg),]
table(sub3$Team)
table(past_nba$Team)

#based on age vorp 
all_nba=read.csv("jelly4.csv")
sub1=subset(all_nba,select=c("VORP_y","Age_x","Team"))
sub1$Age_x=as.factor(sub1$Age_x) 
t2=ddply(sub3,.(Age_x), summarize, vorp_avg=mean(VORP_y))

#LAC vs. LAL draft picks 
la_la=subset(sub3,Team=="LAL" | Team=="LAC")
a=ggplot(la_la, aes(x = VORP_x))
a + geom_area(stat = "bin")
# change fill colors by sex
a + geom_area(aes(fill = Team), stat="bin",alpha=0.6) +#stat_bin(binwidth = 5)
  theme_classic()+scale_fill_manual(values=c("LAL"="gold","LAC"="#C0C0C0"))+
  ggtitle("The Clippers Draft Better Than The Lakers")+ylab("Count")+xlab("VORP")+theme(plot.title = element_text(hjust = 0.5))

subXX=subset(sub3,select=c("Player","Team","VORP_x"))
subXX$bin <- cut(subXX$VORP_x, breaks=c(-5,0,2,4,15), labels=c("<-2,-2-0","0-2","2-4",">4"))

#Miami Heat Draft Picks 
mia=subset(sub3,Team=="MIA")
mia$bin <- cut(mia$VORP_x, breaks=c(-2,0,2,4,15), labels=c("-2-0","0-2","2-4",">4"))
ggplot(mia) +  geom_beeswarm(aes(x=bin,y=VORP_x))+
  xlab("")+ylab("VORP")+ggtitle("South Beach Draft Picks")+scale_colour_manual(values=c("blue"))+
  theme(plot.title = element_text(hjust = 0.5))
