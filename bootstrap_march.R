#libraries
library(dplyr)
library(ggplot2)
library(plyr)
library(boot)
library(randomForest)

#data #source: https://cran.r-project.org/web/packages/boot/boot.pdf
march_past<-read.csv("madness_past1.csv") #331 matchups
march_current<-read.csv("madness_current1.csv") 
#subset datasets
march_past1<-subset(march_past,select=c(3,4,8,11,14,15,16,17,18,19,
                                        20,21,22,23,24,25,26,27,28,29,30,
                                        31,32,33,34,35,36,37,38,39,40,
                                        41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56))
#remove missing values
march_past2=na.omit(march_past1)

march_current1<-subset(march_current,select=c(2,3,7,10,11,12,13,14,15,16,17,18,19,
                                              20,21,22,23,24,25,26,27,28,29,30,
                                              31,33,34,35,36,37,38,39,40,                                                                                          41,42,43,44,45,46,47,48,49,50,51,52,53))
#remove missing values 
march_current2<-na.omit(march_current1)

#build a random forest model on the past march madness games
model_rf=randomForest(as.factor(win_home)~.,data=march_past2,importance=TRUE,ntree=2500)
rf_fit=predict(model_rf,march_current2,type="prob")

#bootstrap
march_func=function(dat,inds,fit_pred,x_pred)
{
  rf_b=randomForest(Win ~.,data=dat)
  pred_b=predict(rf_b,x_pred,type="prob")
}

#t0-observed value, 
#t1=A matrix with sum(R) rows each of which is a bootstrap replicate of the result
#of calling statistic.
#R-The value of R as passed to boot
rf_boot=boot(vars,march_func,R=1000,fit_pred=rf_fit,x_pred=march_current2)
rf_boot$t0
rf_boot$t
str(rf_boot$t)
df_boot=as.data.frame(rf_boot$t) #mean for each matchup
#column mean for the bootstrap
bootstrap_means=colMeans(df_boot)
bootstrap_means1=as.data.frame(bootstrap_means)
bootstrap_means1

names(bootstrap_means1)[1]="win_prob"
write.csv(bootstrap_means1,file="bs_means.csv")

#reshape with R
odd_index<-seq(1,241,2)
even_index<-seq(2,242,2)

odd_prob=data.frame(x=bootstrap_means1$win_prob[odd_index])
even_prob=data.frame(x=bootstrap_means1$win_prob[even_index])

#final bootstrap probabiltiies
final_bs=cbind(odd_prob,even_prob)
names(final_bs)[1]="team1_prob"
names(final_bs)[2]="team2_prob"

#merge rf predictions and teams
teams<-subset(march_current,select=c("team_fav","team_under"))
merge_pred<-cbind(teams,rf_fit)

#the bootstrap prediction squared error 
mean((rf_boot$t0)^2)
rf_boot$t[,1] #columns 
rf_fit-sort(rf_boot$t0)
#t0-The observed value of the statistic on the same scale as the intervals.
#t1-bootstrapped statistic
plot(rf_boot$t[,1])
plot(rf_boot)
hist(rf_boot$t[,3])
rf_boot$t0
mean(rf_boot$t[,1])
mean(rf_boot$t[,2])

#calculate bias
mean(rf_boot$t)-(rf_boot$t0) #source:https://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/ 
sd(rf_boot$t) #0.3149 

#95% CI
boot.ci(boot.out=rf_boot,type=c("norm","basic","perc","bca"))





