library(readr)
library(stringr)
library(dplyr)
library(caret)


set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)

setwd("E:/R/_cp2022/_ross/collector")
train0<-read_csv("train_issues.csv")
trainS<-train0%>%select(key,summary,assignee_id, overall_worklogs)
trainS$word1<-word(str_squish(str_replace_all(tolower(trainS$summary), "[^a-zа-я ]", "")),1)

test0<-read_csv("test_issues.csv")
testS<-test0%>%select(key,summary,assignee_id)
testS$word1<-word(str_squish(str_replace_all(tolower(testS$summary), "[^a-zа-я ]", "")),1)


#############

trainF<-trainS%>%filter(word1!="communications", word1!="communication")
trainF<-trainF[trainF$assignee_id%in%unique(testS$assignee_id),]
trainF<-trainF%>%filter(overall_worklogs<=sort(trainF$overall_worklogs,decreasing = T)[5])

all<-rbind(trainF[,-4], testS)

all$key<-as.numeric(as.factor(gsub("BAD", "BA", str_replace_all(all$key, "[-\\d]", ""))))

fr3<-as.data.frame(table(all$word1))
all$word1[all$word1%in%as.character(fr3$Var1[fr3$Freq==1])]<-"rare"
all$word1<-(as.numeric(as.factor(all$word1)))

trainM<-all[1:nrow(trainF), ]
trainM$Y<-(trainF$overall_worklogs)
trainM<-trainM%>%select (key,word1,Y)

testM<-all[(1+nrow(trainF)):nrow(all), ]
testM<-testM%>%select (key,word1)

#####1 Model
'
set.seed(123)
mod2 <- train(Y~., data =trainM,
              method = "xgbTree", 
              verbosity=0,metric="Rsquared",trControl = ctrl)

mod2
plot(mod2$results$RMSE, mod2$results$Rsquared)
max(mod2$results$Rsquared)
min(mod2$results$RMSE)

ans<-c(min(mod2$results$RMSE),max(mod2$results$Rsquared))
print (ans)

write.table(ans,            "clipboard", row.names = F, col.names = F, sep=";", dec=",")

###############
sample<-read_csv("sample_solution.csv")
sample$overall_worklogs<-(predict(mod2,test3))
summary(sample$overall_worklogs)

write_csv(sample, paste0("xgb_2809_test_prefinal",j,".csv"))

'
#############

# BLEND
tmp<-0
for (k in 1:10)
  
{
  print (k)
  
  set.seed(k)
  mod2 <- train(Y~., data =trainM,
                method = "xgbTree", 
                verbosity=0,metric="Rsquared",trControl = ctrl)
  
tmp<-tmp+predict(mod2,testM)


}
sample<-read_csv("sample_solution.csv")
sample$overall_worklogs<-tmp/k
summary(sample$overall_worklogs)

write_csv(sample, "final.csv")
