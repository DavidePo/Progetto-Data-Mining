#Importo i dati
setwd("F:/Università/Lovaglio/data mining/Progetto")
data <- read.csv('2012-18_teamBoxScore.csv')

#Trasformo in formato Date la data
data$gmDate_temp <- as.Date(data$gmDate)
data$gmDate <- data$gmDate_temp
data$gmDate_temp <- NULL
class(data$gmDate)

#Tengo solo le statistiche team e droppo le oppt
colnames(data)
match("opptAbbr", names(data))
data_solo_team=data[, 1:65]

#Divido il dataset in stagioni
summary(data_solo_team$gmDate)

stagione_12_13 <- data_solo_team[which(data_solo_team$gmDate<'2013-07-30'),]
summary(stagione_12_13$gmDate)

stagione_13_14 <- data_solo_team[which(data_solo_team$gmDate<'2014-07-30' & data_solo_team$gmDate>'2013-07-30'),]
summary(stagione_13_14$gmDate)

stagione_14_15 <- data_solo_team[which(data_solo_team$gmDate<'2015-07-30' & data_solo_team$gmDate>'2014-07-30'),]
summary(stagione_14_15$gmDate)

stagione_15_16 <- data_solo_team[which(data_solo_team$gmDate<'2016-07-30' & data_solo_team$gmDate>'2015-07-30'),]
summary(stagione_15_16$gmDate)

stagione_16_17 <- data_solo_team[which(data_solo_team$gmDate<'2017-07-30' & data_solo_team$gmDate>'2016-07-30'),]
summary(stagione_16_17$gmDate)

stagione_17_18 <- data_solo_team[which(data_solo_team$gmDate<'2018-07-30' & data_solo_team$gmDate>'2017-07-30'),]
summary(stagione_17_18$gmDate)

#Creazione nuovo dataset con le medie
match("teamPTS", colnames(data_solo_team)) #17

myfunction <- function(data_anno){
  to_return <- as.data.frame(matrix(data=0, nrow=1, ncol=ncol(data_anno)))
  colnames(to_return)=colnames(data_anno)
  for (i in 1:nrow(data_anno)){
    temp1=data_anno[1:i-1,]
    temp2=temp1[which(temp1$teamAbbr==data_anno[i, "teamAbbr"]),]
    if (dim(temp2)[1]!=0) {
      temp3=as.data.frame(t(c(as.matrix(data_anno[i, 1:16]), as.matrix(colMeans(temp2[, 17:ncol(temp2)])))))
      colnames(temp3)=colnames(data_anno)
      to_return=rbind(to_return, temp3)
      print(i)
    }
    else{
      temp3=as.data.frame(t(c(as.matrix(data_anno[i, 1:16]), rep.int(0, times=49))))
      colnames(temp3)=colnames(data_anno)
      to_return=rbind(to_return, temp3)
      print(i)
      }
  }
  return(to_return)
}

means_12_13 <- myfunction(stagione_12_13)
means_13_14 <- myfunction(stagione_13_14)
means_14_15 <- myfunction(stagione_14_15)
means_15_16 <- myfunction(stagione_15_16)
means_16_17 <- myfunction(stagione_16_17)
means_17_18 <- myfunction(stagione_17_18)

means_12_13 <- means_12_13[-1,]
means_13_14 <- means_13_14[-1,]
means_14_15 <- means_14_15[-1,]
means_15_16 <- means_15_16[-1,]
means_16_17 <- means_16_17[-1,]
means_17_18 <- means_17_18[-1,]

means_allyr <- rbind(means_12_13, means_13_14, means_14_15, means_15_16, means_16_17, means_17_18)

means_allyr[, 17:65]=sapply(means_allyr[, 17:65], function(x)(as.numeric(x)))
means_allyr[, 13:14]=sapply(means_allyr[, 13:14], function(x)(as.factor(x)))

library(car)
means_allyr$teamDayOff_nuova=car::recode(means_allyr$teamDayOff, recodes="'0'='0'; '1'='1'; '2'='2'; '3'='3';'4'='4'; else='5+'", as.factor=TRUE)
table(means_allyr$teamDayOff, means_allyr$teamDayOff_nuova)
means_allyr$teamDayOff <- means_allyr$teamDayOff_nuova
means_allyr$teamDayOff_nuova <- NULL

means_allyr[, 66:121] <- 0
colnames(means_allyr) <- c('gmDate','gmTime','seasTyp','offLNm1','offFNm1','offLNm2','offFNm2','offLNm3','offFNm3',
                                     'teamAbbr','teamConf','teamDiv','teamLoc','teamRslt','teamMin','teamDayOff','teamPTS',
                                     'teamAST','teamTO','teamSTL','teamBLK','teamPF','teamFGA','teamFGM','teamFG%','team2PA',
                                     'team2PM','team2P%','team3PA','team3PM','team3P%','teamFTA','teamFTM','teamFT%','teamORB',
                                     'teamDRB','teamTRB','teamPTS1','teamPTS2','teamPTS3','teamPTS4','teamPTS5','teamPTS6','teamPTS7',
                                     'teamPTS8','teamTREB%','teamASST%','teamTS%','teamEFG%','teamOREB%','teamDREB%','teamTO%',
                                     'teamSTL%','teamBLK%','teamBLKR','teamPPS','teamFIC','teamFIC40','teamOrtg','teamDrtg',
                                     'teamEDiff','teamPlay%','teamAR','teamAST/TO','teamSTL/TO',
                                     'opptAbbr','opptConf','opptDiv','opptLoc','opptRslt','opptMin','opptDayOff','opptPTS',
                                     'opptAST','opptTO','opptSTL','opptBLK','opptPF','opptFGA','opptFGM','opptFG%','oppt2PA',
                                     'oppt2PM','oppt2P%','oppt3PA','oppt3PM','oppt3P%','opptFTA','opptFTM','opptFT%',
                                     'opptORB','opptDRB','opptTRB','opptPTS1','opptPTS2','opptPTS3','opptPTS4','opptPTS5',
                                     'opptPTS6','opptPTS7','opptPTS8','opptTREB%','opptASST%','opptTS%','opptEFG%',
                                     'opptOREB%','opptDREB%','opptTO%','opptSTL%','opptBLK%','opptBLKR','opptPPS',
                                     'opptFIC','opptFIC40','opptOrtg','opptDrtg','opptEDiff','opptPlay%','opptAR',
                                     'opptAST/TO','opptSTL/TO')


count=1
while(count < nrow(means_allyr)){
  means_allyr[count, 66:121]=means_allyr[count+1, 10:65]
  means_allyr[count+1, 66:121]=means_allyr[count, 10:65]
  print(count)
  count=count+2
}

head(means_allyr)
tail(means_allyr)


to_remove <- which(means_allyr$teamPTS==0|means_allyr$opptPTS==0)
new_means_allyr <- means_allyr[-to_remove,]

new_means_allyr$teamLoc=as.factor(new_means_allyr$teamLoc)
new_means_allyr$opptLoc=as.factor(new_means_allyr$opptLoc)
new_means_allyr$teamRslt=as.factor(new_means_allyr$teamRslt)
new_means_allyr$opptRslt=as.factor(new_means_allyr$opptRslt)
str(new_means_allyr)

library(car)
new_means_allyr$opptDayOff_nuova=car::recode(new_means_allyr$opptDayOff, recodes="'1'='0'; '2'='1'; '3'='2'; '4'='3';
                                             '5'='4'; '6'='5+'", as.factor=TRUE)
table(new_means_allyr$opptDayOff, new_means_allyr$opptDayOff_nuova)
new_means_allyr$opptDayOff <- new_means_allyr$opptDayOff_nuova
new_means_allyr$opptDayOff_nuova <- NULL
str(new_means_allyr)

#Seleziono solo una riga per partita
library(dplyr)
names <- paste(colnames(new_means_allyr[,1:9]), collapse=", ")
names
new_means_allyr <- new_means_allyr %>% arrange(gmDate, gmTime, offLNm1, offFNm1, offLNm2, offFNm2, offLNm3, offFNm3)
new_means_allyr[1:10, 1:10]

keep <- c()
set.seed(3)
for (i in seq(from=1,to=nrow(new_means_allyr), by=2)) {
  row=sample(c(i,i+1), size = 1)
  keep[[length(keep)+1]]=row
}

allyr_1riga <- new_means_allyr[keep,]
table(allyr_1riga$teamRslt)/nrow(allyr_1riga)


#Creo score
score_index=which(allyr_1riga$gmDate>'2017-07-30')
score_index
length(score_index)
score=allyr_1riga[score_index,]
data_altro=allyr_1riga[-score_index,]

library(lubridate)
table(year(score$gmDate))
table(year(data_altro$gmDate))

#Splitto in training e validation
library(caret)
set.seed(3)
split <- createDataPartition(y=data_altro$teamRslt, p = 0.66, list = FALSE)
train <- data_altro[split,]
validation <- data_altro[-split,]

table(train$teamRslt)/nrow(train)
table(validation$teamRslt)/nrow(validation)
nrow(allyr_1riga)-nrow(train)-nrow(validation)-nrow(score)


#Togliamo le variabili che reputiamo non necessarie
colnames(train)
index <- c(1:12, 15, 23:25, 27, 30, 33, 37:45, 57:58, 61, 66:71, 79:81, 83, 86, 89, 93:101, 113:114, 117)
train_info=train[, index]
train=train[, -index]
validation_info=validation[, index]
validation=validation[, -index]
score_info=score[, index]
score=score[, -index]

#save.image("F:/Università/Lovaglio/data mining/Progetto/data_means.RData")
#load("F:/Università/Lovaglio/data mining/Progetto/data_means.RData")

#Composizione dataset
library(funModeling)
library(dplyr)
status_train=df_status(train, print_results = F)
status_train%>% arrange(type,-p_na)
status_train%>% arrange(type,-unique)


#LOGISTICO
#Missing
sapply(train, function(x)(sum(is.na(x))))

#Separation
sapply(train, function(x)(is.factor(x)))
table(train$teamRslt, train$teamLoc)
table(train$teamRslt, train$teamDayOff)
table(train$teamRslt, train$opptDayOff)

train_tarW <- train[which(train$teamRslt=='Win'), c(4:34, 36:66)]
train_tarL <- train[which(train$teamRslt=='Loss'), c(4:34, 36:66)]
summary(train_tarL)[,1:12]
summary(train_tarW)[,1:12]
summary(train_tarL)[,13:24]
summary(train_tarW)[,13:24]
summary(train_tarL)[,25:36]
summary(train_tarW)[,25:36]
summary(train_tarL)[,37:48]
summary(train_tarW)[,37:48]
summary(train_tarL)[,49:60]
summary(train_tarW)[,49:60]
summary(train_tarL)[,61:62]
summary(train_tarW)[,61:62]


#Collinearità
library(caret)

isnumeric_no_collin <- sapply(train, function(x) is.numeric(x))
R=cor(train[,isnumeric_no_collin])
correlatedPredictors = findCorrelation(R, cutoff = 0.95, names = TRUE)
correlatedPredictors
#"teamTS%', 'opptTS%', 'teamAR', 'opptAR', 'opptTO%', 'teamTO', 'teamSTL%', 'opptSTL%', 'teamBLK', 'opptBLK"
R[, c('teamTS%', 'opptTS%', 'teamAR', 'opptAR', 'opptTO%', 'teamTO', 'teamSTL%', 'opptSTL%', 'teamBLK', 'opptBLK')]

#tra TS% e EFG% togliamo EFG% perché non tiene conto dei FT
match(c("teamEFG%", "opptEFG%"), names(train)) #21 53
train_no_collin1 <- train[,-c(21, 53)]


isnumeric_no_collin1 <- sapply(train_no_collin1, function(x) is.numeric(x))
R1=cor(train_no_collin1[,isnumeric_no_collin1])
correlatedPredictors1 = findCorrelation(R1, cutoff = 0.95, names = TRUE)
correlatedPredictors1
paste(correlatedPredictors1, collapse="' ,'")
#"teamTS%' ,'opptTS%' ,'teamAR' ,'opptAR' ,'opptTO%' ,'teamTO%' ,'teamSTL%' ,'opptSTL%' ,'teamBLK' ,'opptBLK"
R1[,c('teamTS%' ,'opptTS%' ,'teamAR' ,'opptAR' ,'opptTO%' ,'teamTO%' ,'teamSTL%' ,'opptSTL%' ,'teamBLK' ,'opptBLK')]

#Togliamo PPS perché è molto correlata con tutte le variabili riguardanti punti e TS%
match(c("teamPPS", "opptPPS"), names(train_no_collin1)) #27 58
train_no_collin2 <- train_no_collin1[, -c(27, 58)]

isnumeric_no_collin2 <- sapply(train_no_collin2, function(x) is.numeric(x))
R2=cor(train_no_collin2[,isnumeric_no_collin2])
correlatedPredictors2 = findCorrelation(R2, cutoff = 0.95, names = TRUE)
correlatedPredictors2
paste(correlatedPredictors2, collapse="' ,'")
#"teamAR' ,'opptAR' ,'opptTO%' ,'teamTO%' ,'teamSTL%' ,'opptSTL%' ,'teamBLK' ,'opptBLK"
R2[,c('teamAR' ,'opptAR' ,'opptTO%' ,'teamTO%' ,'teamSTL%' ,'opptSTL%' ,'teamBLK' ,'opptBLK')]

#Togliamo AR e AST e teniamo ASST%
match(c("teamAR", "opptAR", "teamAST", "opptAST"), names(train_no_collin2)) #30 60 5 35 
train_no_collin3 <- train_no_collin2[, -c(5, 30, 35, 60)]

isnumeric_no_collin3 <- sapply(train_no_collin3, function(x) is.numeric(x))
R3=cor(train_no_collin3[,isnumeric_no_collin3])
correlatedPredictors3 = findCorrelation(R3, cutoff = 0.95, names = TRUE)
correlatedPredictors3
paste(correlatedPredictors3, collapse="' ,'")
#"opptTO%' ,'teamTO%' ,'opptSTL%' ,'teamSTL%' ,'teamBLK' ,'opptBLK"
R3[,c('opptTO%' ,'teamTO%' ,'opptSTL%' ,'teamSTL%' ,'teamBLK' ,'opptBLK')]


#Togliamo TO% perché molto correlata con TO e più "complessa" come variabile
match(c("teamTO", "opptTO", "teamSTL", "opptSTL", "teamBLK", "opptBLK", "teamBLK%", "opptBLK%"), names(train_no_collin3))
#5 33  6 34  7 35 24 52
train_no_collin4 <- train_no_collin3[, -c(5, 33, 6, 34, 7, 35, 24, 52)]

isnumeric_no_collin4 <- sapply(train_no_collin4, function(x) is.numeric(x))
R4=cor(train_no_collin4[,isnumeric_no_collin4])
correlatedPredictors4 = findCorrelation(R4, cutoff = 0.90, names = TRUE)
correlatedPredictors4

#Togliamo DREB e OREB perché ci sono DREB% e OREB%
match(c("teamDRB", "opptDRB", "teamORB", "opptORB"), names(train_no_collin4)) #12, 13, 36, 37
train_no_collin5 <- train_no_collin4[, -c(12, 13, 36, 37)]

isnumeric_no_collin5 <- sapply(train_no_collin5, function(x) is.numeric(x))
R5=cor(train_no_collin5[,isnumeric_no_collin5])
correlatedPredictors5 = findCorrelation(R5, cutoff = 0.90, names = TRUE)
correlatedPredictors5

train_no_collin <- train_no_collin5

#NearZero Variance
nearZeroVar(train_no_collin, saveMetrics = TRUE)
nearZeroVar(train, saveMetrics = TRUE)
#non ci sono problemi di NZV nè in train_no_collin né in train

#Model selection con tree per logistico
library(rpart)
set.seed(3)
tree_modsel_largest <- rpart(teamRslt ~ ., data = train_no_collin, method = "class", cp=0) 
tree_modsel_largest$cptable
min(tree_modsel_largest$cptable[, "xerror"])
#il cp ottimale è 0.0050276521
tree_modsel <- rpart(teamRslt ~ ., data = train_no_collin, method = "class", cp=0.0050276521)
vi=data.frame(tree_modsel$variable.importance)
vi
viname=rownames(vi)

logistico_completo <- glm(teamRslt ~ ., data=train_no_collin, family=binomial)
#logistico_AIC <- step(logistic_completo, direction = 'both')
logistico_AIC <-glm(teamRslt ~ teamLoc + teamPTS + `team2P%` + `team3P%` + `teamTS%` + 
  `teamOREB%` + `teamTO%` + teamDrtg + `teamPlay%` + `teamAST/TO` + 
  opptDayOff + oppt2PA + `oppt2P%` + `opptTREB%` + `opptASST%` + 
  `opptTS%` + `opptSTL%` + opptOrtg + opptDrtg + `opptAST/TO` + 
  `opptSTL/TO`, data=train_no_collin, family=binomial)

logistico_PPtree <- glm(teamRslt ~ ., data=train_no_collin[, c(viname, "teamRslt")], family=binomial)
logistico_PPtree$data
summary(logistico_PPtree)
anova(logistico_AIC, logistico_PPtree, logistico_completo, test='LRT')

library(caret)
set.seed(3)
Control=trainControl(method= "cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary, savePredictions=T)
logistic_AIC_caret=train(teamRslt ~ ., data=logistico_AIC$data, 
               method = "glm", metric='ROC',
               trControl = Control, 
               tuneLength=5, 
               trace=FALSE)

set.seed(3)
Control=trainControl(method= "cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary, savePredictions=T)
logistic_PPtree_caret=train(teamRslt ~ ., data=logistico_PPtree$data, 
                         method = "glm", metric='ROC', 
                         trControl = Control, 
                         tuneLength=5, 
                         trace=FALSE)

confusionMatrix(logistic_AIC_caret)
confusionMatrix(logistic_PPtree_caret)

#togliamo dati influenti con DFFITS
data_logAIC <- logistico_AIC$data
dffits_logistico_AIC <- dffits(logistico_AIC)
data_logAIC$dffits <- dffits_logistico_AIC
cutoff_df_AIC <- 2*sqrt(ncol(data_logAIC)/nrow(data_logAIC))
cutoff_df_AIC
data_logAIC_noInf <- data_logAIC[which(abs(data_logAIC$dffits)<cutoff_df_AIC),]

data_logPPtree <- logistico_PPtree$data
dffits_logistico_PPtree <- dffits(logistico_PPtree)
data_logPPtree$dffits <- dffits_logistico_PPtree
cutoff_df_PPtree <- 2*sqrt(ncol(data_logPPtree)/nrow(data_logPPtree))
cutoff_df_PPtree
data_logPPtree_noInf <- data_logPPtree[which(abs(data_logPPtree$dffits)<cutoff_df_PPtree),]

set.seed(3)
Control=trainControl(method= "cv", number=10, classProbs = TRUE)
logistic_AIC_noInf=train(teamRslt ~ ., data=data_logAIC_noInf[, -47], 
                         method = "glm", 
                         trControl = Control, 
                         tuneLength=5, 
                         trace=FALSE)

set.seed(3)
Control=trainControl(method= "cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary)
logistic_AIC_noInf_ROC=train(teamRslt ~ ., data=data_logAIC_noInf[, -47], 
                         method = "glm", metric='ROC',
                         trControl = Control,
                         trace=FALSE)
logistic_AIC_noInf_ROC

set.seed(3)
Control=trainControl(method= "cv", number=10, classProbs = TRUE)
logistic_PPtree_noInf=train(teamRslt ~ ., data=data_logPPtree_noInf[,-39], 
                            method = "glm", 
                            trControl = Control, 
                            tuneLength=5, 
                            trace=FALSE)

set.seed(3)
Control=trainControl(method= "cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary, savePredictions = T)
logistic_PPtree_noInf_ROC=train(teamRslt ~ ., data=data_logPPtree_noInf[,-39], metric='ROC',
                            method = "glm", 
                            trControl = Control,
                            trace=FALSE)

logistic_PPtree_noInf_ROC

confusionMatrix(logistic_AIC_caret)
confusionMatrix(logistic_PPtree_caret)
confusionMatrix(logistic_AIC_noInf)
confusionMatrix(logistic_PPtree_noInf)

#LASSO
set.seed(3)
grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
Control=trainControl(method= "cv",number=10, classProbs=TRUE)
glm_lasso=train(teamRslt~.,
                data=train, 
                method = "glmnet", 
                family ="binomial",
                trControl = Control, 
                tuneLength=5, 
                tuneGrid=grid)

confusionMatrix(glm_lasso)
plot(glm_lasso)
coef(glm_lasso$finalModel, s=glm_lasso$bestTune$lambda)


set.seed(3)
grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
Control=trainControl(method= "cv",number=10, classProbs=TRUE, summaryFunction = twoClassSummary)
glm_lasso_ROC=train(teamRslt~.,
                data=train, 
                method = "glmnet", 
                family ="binomial",
                trControl = Control, 
                tuneLength=5, 
                tuneGrid=grid,
                metric="ROC")

confusionMatrix(glm_lasso_ROC)
glm_lasso_ROC

#PLS
set.seed(3)
Control=trainControl(method= "cv",number=10, classProbs=TRUE)
pls=train(teamRslt~.,data=train, 
          method = "pls", 
          trControl = Control, 
          tuneLength=5)

confusionMatrix(pls)
plot(pls)

set.seed(3)
Control=trainControl(method= "cv",number=10, classProbs=TRUE, summaryFunction = twoClassSummary, savePredictions = T)
pls_ROC=train(teamRslt~.,data=train, 
          method = "pls", 
          trControl = Control, 
          tuneLength=5,
          metric="ROC")

confusionMatrix(pls_ROC)
pls_ROC

#Tree
library(rpart)
tree <- rpart(teamRslt~., data = train, control = rpart.control(minsplit = 2, minbucket = 1, cp=0, xval=10))
tree
n_split=data.frame(tree$frame$var)
table(n_split)
varimp=data.frame(tree$variable.importance)
tree$cptable
min(tree$cptable[, "xerror"]); which.min(tree$cptable[, "xerror"])
#cp ottimale 0.0070387129

set.seed(3)
pruned <- prune(tree, cp =0.0070387129)
n_split_pruned=data.frame(pruned$frame$var)
table(n_split_pruned)
table(n_split_pruned)[which(table(n_split_pruned)>0)]
varimp_prun=data.frame(pruned$variable.importance)
vi_splittingvar=varimp_prun[c("opptAST", "opptDrtg", "opptOrtg", "teamAST/TO", "teamDrtg",  "teamLoc", "teamOrtg"), ]
names(vi_splittingvar) <- c("opptAST", "opptDrtg", "opptOrtg", "teamAST/TO", "teamDrtg",  "teamLoc", "teamOrtg")
pruned$cptable
plot(pruned$cptable[,'nsplit'], pruned$cptable[,'rel error'], type='l', col='red', ylab="Error", xlab="Numero di split")
lines(pruned$cptable[,'nsplit'], pruned$cptable[,'xerror'], type='l', col='blue')
legend(5, 0.95, legend=c("Training", "Crossvalidation"), col=c("red", "blue"),lty=1,  cex=1)

library(pROC)
pruned_predict=predict(pruned, train)[,"Win"]
roc(train$teamRslt ~ pruned_predict)


library(rpart.plot)
rpart.plot(pruned, type = 4, extra = 101,  split.font = 0.9, ycompress=FALSE, cex=.7)
prp(pruned, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(tree$frame$var == "<leaf>", 'gray', 'white')) 

confusionMatrix(predict(pruned, train, type='class'), train$teamRslt)

#Random Forest
library(caret)
set.seed(3)
control <- trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE) 
#sqrt(ncol(train)-1) 8.06....
valori_mtry=c(3:8)
tunegrid <- expand.grid(.mtry=valori_mtry) 
rf <- train(teamRslt~., data=train, method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
plot(rf)
rf$results
rf$resample
confusionMatrix(rf)
varImp(rf)


#Naive Bayes
#serve togliere variabili collin ed è preferibile model selection,
#perciò recuperiamo il dataset usato nel logistico (model selection con tree) data_logPPtree
data_nb <- data_logPPtree[, -39] #'l'ultima var è dffits
table(data_nb$teamRslt, data_nb$teamLoc) #no 0-problem
table(data_nb$teamRslt, data_nb$teamDayOff) #no 0-problem

library(klaR)
nb_normalkernel <- NaiveBayes(teamRslt ~ ., data=data_nb,
                 usekernel=FALSE)
pred_normalkernel <- predict(nb_normalkernel, train[, colnames(data_nb[,-38])], type="class")
pred_normalkernel$class
pred_normalkernel$posterior
confusionMatrix(pred_normalkernel$class, train$teamRslt, positive = 'Win')

library(pROC)
nb_predict=predict(nb_normalkernel, train[, colnames(data_nb[,-38])])$posterior[,"Win"]
roc(train$teamRslt ~ nb_predict)

#nb_densitykernel <- NaiveBayes(teamRslt ~ ., data=data_nb,
#                              usekernel=TRUE)
#pred_densitykernel <- predict(nb_densitykernel, validation[, colnames(data_nb[,-38])], type="class")
#pred_densitykernel$class
#pred_densitykernel$posterior
#confusionMatrix(pred_densitykernel$class, validation$teamRslt, positive = 'Win')
#usiamo normal kernel perché è meno pesante e le covariate hanno distribuzioni simmetriche

#proviamo a usare dataset dopo analisi collin ma senza model selection
#nb_no_collin <- NaiveBayes(teamRslt ~ ., data=train_no_collin,
#                              usekernel=FALSE)
#pred_no_collin <- predict(nb_no_collin, validation[, colnames(train_no_collin[,-2])], type="class")
#pred_no_collin$class
#pred_no_collin$posterior
#confusionMatrix(pred_no_collin$class, validation$teamRslt, positive = 'Win')
#non sembra cambiare niente, perciò teniamo il primo che usa meno variabili


#KNN
library(rpart)
set.seed(3)
tree_modsel_largest_KNN <- rpart(teamRslt ~ ., data = train, method = "class", cp=0) 
tree_modsel_largest_KNN$cptable
min(tree_modsel_largest_KNN$cptable[, "xerror"])
which.min(tree_modsel_largest_KNN$cptable[, "xerror"])
#il cp ottimale è 0.0070387129
tree_modsel_KNN <- rpart(teamRslt ~ ., data = train, method = "class", cp=0.0070387129)
vi_modsel_KNN=data.frame(tree_modsel_KNN$variable.importance)
vi_modsel_KNN
viname_KNN=rownames(vi_modsel_KNN)
data_KNN <- train[, c(viname_KNN, "teamRslt")]

set.seed(3)
Grid_knn_prova = expand.grid(.k=seq(1,200, by=1))
control_knn_prova <- trainControl(method="cv", number=10, search="grid", 
                            summaryFunction = twoClassSummary, classProbs = TRUE)
knn_prova <- train(teamRslt ~ ., data=data_KNN, method='knn', metric="ROC", trControl=control_knn_prova, 
             preProcess = c("center","scale"), tuneGrid=Grid_knn_prova, tuneLength=10)
set.seed(3)
Grid_knn = expand.grid(.k=185)
control_knn <- trainControl(method="cv", number=10, search="grid", 
                            summaryFunction = twoClassSummary, classProbs = TRUE)
knn <- train(teamRslt ~ ., data=data_KNN, method='knn', metric="ROC", trControl=control_knn, 
             preProcess = c("center","scale"), tuneGrid=Grid_knn, tuneLength=10)

knn
confusionMatrix(knn)
#save.image("F:/Università/Lovaglio/data mining/Progetto/fino_a_knn.RData")
#load("E:/Università/Lovaglio/data mining/Progetto/fino_a_knn.RData")

#Proviamo a fare le Comp Princ per usare anche le categoriali in kNN
#Contemporaneamente otteniamo riduzione della dimensionalità e input centrati e scalati per kNN
train_pca <- train
train_pca$teamLoc_A <- ifelse(train_pca$teamLoc=='Away', 1, 0)
train_pca$teamLoc_H <- ifelse(train_pca$teamLoc=='Home', 1, 0)
train_pca$teamLoc <- NULL

train_pca$teamDayOff_1 <- ifelse(train_pca$teamDayOff=='1', 1, 0)
train_pca$teamDayOff_2 <- ifelse(train_pca$teamDayOff=='2', 1, 0)
train_pca$teamDayOff_3 <- ifelse(train_pca$teamDayOff=='3', 1, 0)
train_pca$teamDayOff_4 <- ifelse(train_pca$teamDayOff=='4', 1, 0)
train_pca$teamDayOff_5più <- ifelse(train_pca$teamDayOff=='5+', 1, 0)
train_pca$teamDayOff <- NULL

train_pca$opptDayOff_1 <- ifelse(train_pca$opptDayOff=='1', 1, 0)
train_pca$opptDayOff_2 <- ifelse(train_pca$opptDayOff=='2', 1, 0)
train_pca$opptDayOff_3 <- ifelse(train_pca$opptDayOff=='3', 1, 0)
train_pca$opptDayOff_4 <- ifelse(train_pca$opptDayOff=='4', 1, 0)
train_pca$opptDayOff_5più <- ifelse(train_pca$opptDayOff=='5+', 1, 0)
train_pca$opptDayOff <- NULL

pc_knn <- princomp(train_pca[, -match("teamRslt", colnames(train_pca))])
pc_knn$center
summary(pc_knn)
#scelta numero pc
.95^(ncol(train_pca))*100
plot(sdev_pc$`% Var cumul`[1:20], type='o', xlab='', ylab='', yaxt='none', xaxt='none') + axis(at=seq(from=1, to=20, by=2), side=1)+ axis(side=2, at=seq(from=0,to=1, by=0.2)) +mtext(side=3, '% Varianza cumulata PC', font=2, cex=1.5, line = 1)+ mtext(side=2, line=2.5, "% Varianza cumulata", font=2, cex=1.2)+
  + mtext(side=1, line=2.5, "Numero di PC", font=2, cex=1.2)
screeplot(pc_knn, type="lines", 
          main="Crime: Scree plot - matrice di var-cov")

#teniamo le prime 13 pc così arriviamo a 90% di var e rispetta criterio della var media
Y.compl <- pc_knn$scores
pc_keep <- as.data.frame(Y.compl)[,1:13]
cor_PC <- round(cor(train_pca[, -match("teamRslt", colnames(train_pca))], pc_keep), 2)
cor_PC1 <- cor_PC
cor_PC1[abs(cor_PC1) <= .4] <- "  "
cor_PC1
cor(pc_keep[, sapply(pc_keep, function(x)(is.numeric(x)))])
var(pc_keep[, sapply(pc_keep, function(x)(is.numeric(x)))])
pc_keep$teamRslt <- train$teamRslt
set.seed(3)
Grid_knn_pc_prova = expand.grid(.k=seq(1,200, by=1))
control_knn_pc <- trainControl(method="cv", number=10, search="grid", 
                            summaryFunction = twoClassSummary, classProbs = TRUE)
knn_pc_prova <- train(teamRslt ~ ., data=pc_keep, method='knn', metric="ROC", trControl=control_knn_pc, 
             preProcess = c("scale"), tuneGrid=Grid_knn_pc_prova, tuneLength=10)
set.seed(3)
Grid_knn_pc = expand.grid(.k=191)
control_knn_pc <- trainControl(method="cv", number=10, search="grid", 
                               summaryFunction = twoClassSummary, classProbs = TRUE)
knn_pc <- train(teamRslt ~ ., data=pc_keep, method='knn', metric="ROC", trControl=control_knn_pc, 
                      preProcess = c("scale"), tuneGrid=Grid_knn_pc, tuneLength=10)
confusionMatrix(knn_pc)
ls(knn_pc)
knn_pc$trainingData

results_knn <- resamples(list(knn_PPtree=knn, 
                          knn_PPpca=knn_pc))
summary(results_knn)
bwplot(results_knn)
Diffs_knn <- diff(results_knn)
summary(Diffs_knn)
bwplot(results_knn)
bwplot(results_knn ,scales = list(relation = "free"),xlim = list(c(.5,1), c(0,1)))


#RETI NEURALI
#Necessitano analisi di collinearità (usiamo train_no_collin), nzv (che abbiamo verificato non esserci) e
#serve fare model selection e scalare. Possiamo usare la mod sel fatta dall'albero e centrare e scalare
#oppure usare le PC scalate che abbiamo appena usato

#usando PCA e range
pc_keep
set.seed(3)
ctrl = trainControl(method="cv", number=10, search = "grid", 
                    summaryFunction = twoClassSummary, classProbs = TRUE)
nnet_pc_range_ROC <- train(teamRslt ~ ., data=pc_keep,
                     method = "nnet",
                     preProcess = "range", 
                     metric="ROC", trControl=ctrl,
                     trace = TRUE, # use true to see convergence
                     maxit = 100)
confusionMatrix(nnet_pc_range_ROC)
print(nnet_pc_range_ROC)
plot(nnet_pc_range_ROC)

#usando PCA e scale
pc_keep
R_pc=cor(pc_keep[, -match("teamRslt", colnames(pc_keep))])
correlatedPC = findCorrelation(R_pc, cutoff = 0.95, names = TRUE)
correlatedPC

set.seed(3)
ctrl = trainControl(method="cv", number=10, search = "grid", 
                    summaryFunction = twoClassSummary, classProbs = TRUE)
nnet_pc_scale_ROC <- train(teamRslt ~ ., data=pc_keep,
                           method = "nnet",
                           preProcess = "scale", 
                           metric="ROC", trControl=ctrl,
                           trace = TRUE, # use true to see convergence
                           maxit = 100)
confusionMatrix(nnet_pc_scale_ROC)
print(nnet_pc_scale_ROC)
print(nnet_pc_range_ROC)

#usando mod sel con tree (vanno anche centrate e controllata multicoll)
str(data_logPPtree)
findCorrelation(cor(data_logPPtree[, sapply(data_logPPtree, function(x) (is.numeric(x)))]), 
                cutoff = 0.90, names = TRUE) #no collin

set.seed(3)
ctrl = trainControl(method="cv", number=10, search = "grid", 
                    summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = T)
nnet_PPtree_scale_ROC <- train(teamRslt ~ ., data=data_logPPtree[,-39],
                           method = "nnet",
                           preProcess = c("center", "scale"), 
                           metric="ROC", trControl=ctrl,
                           trace = TRUE, # use true to see convergence
                           maxit = 200)
confusionMatrix(nnet_PPtree_scale_ROC)
print(nnet_PPtree_scale_ROC)

#uso mod sel fatto su logAIC
str(data_logAIC)
findCorrelation(cor(data_logAIC[, sapply(data_logAIC[,-39], function(x) (is.numeric(x)))]), 
                cutoff = 0.90, names = TRUE) #no collin

set.seed(17)
ctrl = trainControl(method="cv", number=10, search = "grid", 
                    summaryFunction = twoClassSummary, classProbs = TRUE)
nnet_logAIC_scale_ROC <- train(teamRslt ~ ., data=data_logAIC[,-match("dffits", colnames(data_logAIC))],
                               method = "nnet",
                               preProcess = c("center", "scale"), 
                               metric="ROC", trControl=ctrl,
                               trace = TRUE, # use true to see convergence
                               maxit = 200)
confusionMatrix(nnet_logAIC_scale_ROC)
print(nnet_logAIC_scale_ROC)

#altro ennesimo tentativo di merda
set.seed(3)
ctrl = trainControl(method="cv", number=10, search = "grid", 
                    summaryFunction = twoClassSummary, classProbs = TRUE)
nnet_ultimo <- train(teamRslt ~ ., data=train,
                               method = "nnet",
                               preProcess = c("center", "scale", "pca"), 
                               metric="ROC", trControl=ctrl,
                               trace = TRUE, # use true to see convergence
                               maxit = 100,
                               tuneGrid=expand.grid(size=c(1:8), decay = c(0.001, 0.01, 0.05 , .1, .3)))
confusionMatrix(nnet_ultimo)
print(nnet_ultimo)

#LA RETE NEURALE MIGLIORE E' nnet_PPtree_scale_ROC

#save.image("F:/Università/Lovaglio/data mining/Progetto/fino_a_nnet.RData")
#load("F:/Università/Lovaglio/data mining/Progetto/fino_a_nnet.RData")
##########
match("teamDRB", colnames(train))
match("opptDayOff", colnames(train))
match("opptDRB", colnames(train))
train_semplice <- train[, c(1:17, 35:49)]

#glm
library(caret)
findCorrelation(cor(train_semplice[, sapply(train_semplice, function(x)(is.numeric(x)))]), 
                cutoff=0.8, names = T)
set.seed(3)
glm_prova <- train(teamRslt~., data=train_semplice, method="glm", metric="ROC",
                   trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary,
                                          classProbs = T, savePredictions = T))


confusionMatrix(glm_prova)
print(glm_prova)

#tree
library(rpart)
set.seed(3)
tree_prova <- rpart(teamRslt~., data = train_semplice, 
                    control = rpart.control(minsplit = 2, minbucket = 1, cp=0, xval=10))
min(tree_prova$cptable[,"xerror"])
which.min(tree_prova$cptable[,"xerror"])
tree_prova_pruned=prune(tree_prova, cp=0.0286576169)
confusionMatrix(predict(tree_prova_pruned, validation, type="class"), validation$teamRslt, positive = "Win")
tree_prova_pruned$variable.importance
n_split_pruned_sempl <- data.frame(tree_prova_pruned$frame$var)
table(n_split_pruned_sempl)[which(table(n_split_pruned_sempl)>0)]
vi_splittingvar_sempl <- data.frame(tree_prova_pruned$variable.importance)[c("oppt2P%", "oppt3P%", "team2PA", "teamLoc"),]
names(vi_splittingvar_sempl) <- c("oppt2P%", "oppt3P%", "team2PA", "teamLoc")
vi_splittingvar_sempl

#neural nets
set.seed(3)
nnet_semplice <- train(teamRslt ~ ., data=train_semplice,
                     method = "nnet",
                     preProcess = c("center", "scale", "pca"), 
                     metric="ROC", 
                     trControl=trainControl(method="cv", number=10, search = "grid", 
                                            summaryFunction = twoClassSummary, classProbs = TRUE),
                     trace = TRUE, # use true to see convergence
                     maxit = 150,
                     tuneGrid=expand.grid(size=c(1:8), decay = c(0.001, 0.01, 0.05 , .1, .3)))
set.seed(3)
nnet_semplice_nopca <- train(teamRslt ~ ., data=train_semplice,
                       method = "nnet",
                       preProcess = c("center", "scale"), 
                       metric="ROC", 
                       trControl=trainControl(method="cv", number=10, search = "grid", 
                                              summaryFunction = twoClassSummary, classProbs = TRUE,
                                              savePredictions = T),
                       trace = TRUE, # use true to see convergence
                       maxit = 150,
                       tuneGrid=expand.grid(size=c(1:5), decay = c(0.001, 0.01, 0.05 , .1, .3, .5)))

set.seed(3)
nnet_semplice_solocenter <- train(teamRslt ~ ., data=train_semplice,
                       method = "nnet",
                       preProcess = c("center"), 
                       metric="ROC", 
                       trControl=trainControl(method="cv", number=10, search = "grid", 
                                              summaryFunction = twoClassSummary, classProbs = TRUE),
                       trace = TRUE, # use true to see convergence
                       maxit = 150,
                       tuneGrid=expand.grid(size=c(1:8), decay = c(.1, .3, .5, .75)))

set.seed(3)
nnet_semplice_PPtree <- train(teamRslt ~ ., data=train_semplice[, c("teamRslt", names(tree_prova_pruned$variable.importance))],
                             method = "nnet",
                             preProcess = c("center", "scale"), 
                             metric="ROC", 
                             trControl=trainControl(method="cv", number=10, search = "grid", 
                                                    summaryFunction = twoClassSummary, classProbs = TRUE),
                             trace = TRUE, # use true to see convergence
                             maxit = 150,
                             tuneGrid=expand.grid(size=c(1:5), decay = c(0.001, 0.01, 0.05 , .1, .3, .5)))
confusionMatrix(nnet_semplice)
confusionMatrix(nnet_semplice_nopca)
confusionMatrix(nnet_semplice_solocenter)
confusionMatrix(nnet_semplice_PPtree)
print(nnet_semplice)
print(nnet_semplice_nopca)
print(nnet_semplice_solocenter)
print(nnet_semplice_PPtree)

#random forest
library(caret)-
set.seed(3)
control <- trainControl(method="cv", number=10, search="grid", 
                        summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = T) 
sqrt(ncol(train_semplice)-1)
valori_mtry=c(5-3:5+3)
tunegrid <- expand.grid(.mtry=valori_mtry) 
rf_semplice <- train(teamRslt~., data=train_semplice, method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
plot(rf_semplice)
rf_semplice$results
rf_semplice$resample
rf_semplice$bestTune
confusionMatrix(rf_semplice)
varImp(rf_semplice)$importance

#naive bayes
table(train_semplice$teamRslt, train_semplice$teamLoc) #no 0-problem
table(train_semplice$teamRslt, train_semplice$teamDayOff) #no 0-problem

library(klaR)
nb_normalkernel_sempl <- NaiveBayes(teamRslt ~ ., data=train_semplice[, c("teamRslt", names(tree_prova_pruned$variable.importance))],
                              usekernel=FALSE)
pred_normalkernel_sempl <- predict(nb_normalkernel_sempl, 
                             validation[, names(tree_prova_pruned$variable.importance)], type="class")
#pred_normalkernel_sempl$class
#pred_normalkernel_sempl$posterior
confusionMatrix(pred_normalkernel_sempl$class, validation$teamRslt, positive = 'Win')


library(gbm)
library(plyr)
library(caret)
set.seed(3)
gbm_sempl <- train(teamRslt~., data=train_semplice, method='gbm',
                   trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE), 
                   metric="ROC")
gbm_sempl

set.seed(3)
gbm_sempl1 <- train(teamRslt~., data=train_semplice, method='gbm',
                   trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, 
                                          classProbs = TRUE, savePredictions = TRUE), 
                   metric="ROC", 
                   tuneGrid=expand.grid(n.trees=50, interaction.depth=c(4:8), shrinkage=c(0.05, 0.1), 
                  n.minobsinnode=10))

gbm_sempl1

set.seed(3)
gbm_sempl2 <- train(teamRslt~., data=train_semplice, method='gbm',
                    trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE), 
                    metric="ROC", 
                    tuneGrid=expand.grid(n.trees=100, interaction.depth=c(4:8), shrinkage=c(0.1, 0.5), 
                                         n.minobsinnode=10))

gbm_sempl2$bestTune

set.seed(3)
gbm_sempl3 <- train(teamRslt~., data=train_semplice, method='gbm',
                    trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE), 
                    metric="ROC", 
                    tuneGrid=expand.grid(n.trees=c(50, 100, 150), interaction.depth=c(4:8), shrinkage=0.1, 
                                         n.minobsinnode=10))
gbm_sempl3

gbm_finale <-train(teamRslt~., data=train_semplice, method='gbm',
                   trControl=trainControl(method="none", summaryFunction = twoClassSummary, classProbs = TRUE), 
                   metric="ROC", 
                   tuneGrid=expand.grid(n.trees=50, interaction.depth=8, shrinkage=0.1, 
                                        n.minobsinnode=10))
confusionMatrix(gbm_sempl)
confusionMatrix(gbm_sempl1)
confusionMatrix(gbm_sempl2)
confusionMatrix(gbm_sempl3)
confusionMatrix(gbm_finale)
confusionMatrix(as.factor(predict(gbm_finale, newdata = validation[,colnames(train_semplice)])), validation$teamRslt, positive='Win') 
#il migliore sembra essere sempl1

#save.image("F:/Università/Lovaglio/data mining/Progetto/fino_a_gbm_semplice.RData")
#load("F:/Università/Lovaglio/data mining/Progetto/fino_a_gbm_semplice.RData")

res_to_stack_sempl <- resamples(list(glm_prova=glm_prova, gbm_sempl1=gbm_sempl1, rf_semplice=rf_semplice))
modelCor(res_to_stack_sempl)
library(corrplot)
corrplot(modelCor(res_to_stack_sempl),method='ellipse',order='AOE')
corrplot(modelCor(res_to_stack_sempl),method='number',order='AOE')
mean(modelCor(res_to_stack_sempl))


glm_sempl_predcv <- glm_prova$pred[, c("obs", "Win", "rowIndex")]
colnames(glm_sempl_predcv) <- c("obs_glm", "PWin_glm", "rowIndex")

gbm_sempl1_predcv <- gbm_sempl1$pred[which(gbm_sempl1$pred$interaction.depth==8 & gbm_sempl1$pred$shrinkage==0.1), c("obs", "Win", "rowIndex")]
colnames(gbm_sempl1_predcv) <- c("obs_gbm", "PWin_gbm", "rowIndex")

rf_semplice_predcv <- rf_semplice$pred[which(rf_semplice$pred$mtry==4), c("obs", "Win", "rowIndex")]
colnames(rf_semplice_predcv) <- c("obs_rf", "Pwin_rf", "rowIndex")

data_stacking_temp <- merge(glm_sempl_predcv, gbm_sempl1_predcv, by="rowIndex")
data_stacking <- merge(data_stacking_temp, rf_semplice_predcv, by="rowIndex")

for (i in 1:nrow(data_stacking)){
  if (data_stacking[i,2]==data_stacking[i,4]&data_stacking[i,4]==data_stacking[i,6]){}
  else {print(i)}
}

data_stacking$rowIndex <- NULL
data_stacking$teamRslt  <- data_stacking$obs_glm
data_stacking$obs_glm <- NULL
data_stacking$obs_gbm <- NULL
data_stacking$obs_rf <- NULL
data_stacking

library(caret)
set.seed(3)
stacking_semplice <- train(teamRslt ~ ., data=data_stacking, method='glm', metric="ROC",
                           trControl=trainControl(method="cv", number=10, search="grid", 
                                                  summaryFunction = twoClassSummary, classProbs = T))
print(stacking_semplice)
confusionMatrix(stacking_semplice, positive='Win')
########

library(gbm)
library(plyr)
library(caret)
set.seed(3)
gbm_base <- train(teamRslt~., data=train, method='gbm',
                   trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE), 
                   metric="ROC", tuneLength=3)
gbm1 <- train(teamRslt~., data=train, method='gbm',
              trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE), 
              metric="ROC", tuneLength=5)


gbm_2 <- train(teamRslt~., data=train, method='gbm',
              trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE), 
              metric="ROC", tuneGrid=expand.grid(n.trees=c(50, 100, 150), interaction.depth=c(4:8), shrinkage=c(0.05, 0.1, 0.5), 
                                                 n.minobsinnode=10))
set.seed(3)
gbm_3 <- train(teamRslt~., data=train, method='gbm',
               trControl=trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, 
                                      classProbs = TRUE, savePredictions = T), 
               metric="ROC", tuneGrid=expand.grid(n.trees=c(50, 100, 150), interaction.depth=c(4:8), shrinkage=c(0.01, 0.05, 0.1), 
                                                  n.minobsinnode=10))

gbm1
gbm_2
gbm_3
confusionMatrix(gbm_3)

#save.image("F:/Università/Lovaglio/data mining/Progetto/fino_a_gbm.RData")
#load("E:/Università/Lovaglio/data mining/Progetto/fino_a_gbm.RData")

#Stacking

res_to_stack <- resamples(list(logistic_PPtree_caret=logistic_PPtree_caret, glm_lasso_ROC=glm_lasso_ROC, pls_ROC=pls_ROC, 
               rf=rf, knn=knn, nnet_PPtree_scale_ROC=nnet_PPtree_scale_ROC, gbm_3=gbm_3))
modelCor(res_to_stack)
library(corrplot)
corrplot(modelCor(res_to_stack),method='ellipse',order='AOE')
corrplot(modelCor(res_to_stack),method='number',order='AOE')
mean(modelCor(res_to_stack))


res_to_stack1 <- resamples(list(logistic_PPtree_caret=logistic_PPtree_caret, pls_ROC=pls_ROC, knn=knn, gbm_3=gbm_3))
modelCor(res_to_stack1)
library(corrplot)
corrplot(modelCor(res_to_stack1),method='ellipse',order='AOE')
corrplot(modelCor(res_to_stack1),method='number',order='AOE')
mean(modelCor(res_to_stack1))


res_to_stack2 <- resamples(list(logistic_PPtree_caret=logistic_PPtree_caret, pls_ROC=pls_ROC, gbm_3=gbm_3))
modelCor(res_to_stack2)

library(corrplot)
corrplot(modelCor(res_to_stack2),method='ellipse',order='AOE')
corrplot(modelCor(res_to_stack2),method='number',order='AOE')
(0.86+0.85+0.89)/3

#save.image("E:/Università/Lovaglio/data mining/Progetto/da_fare_stacking.RData")
#load("F:/Università/Lovaglio/data mining/Progetto/da_fare_stacking.RData")

logistic_PPtree_caret_predcv <- logistic_PPtree_caret$pred[, c("obs", "Win", "rowIndex")]
colnames(logistic_PPtree_caret_predcv) <- c("obs_glm", "PWin_glm", "rowIndex")

pls_ROC_predcv <- pls_ROC$pred[which(pls_ROC$pred$ncomp==5), c("obs", "Win", "rowIndex")]
colnames(pls_ROC_predcv) <- c("obs_pls", "PWin_pls", "rowIndex")

gbm_3_predcv <- gbm_3$pred[which(gbm_3$pred$n.trees == 100 & gbm_3$pred$interaction.depth == 5 &
                                   gbm_3$pred$shrinkage == 0.05), c("obs", "Win", "rowIndex")]
colnames(gbm_3_predcv) <- c("obs_gbm", "Pwin_gbm", "rowIndex")

data_stacking_temp_completo <- merge(logistic_PPtree_caret_predcv, pls_ROC_predcv, by="rowIndex")
data_stacking_completo <- merge(data_stacking_temp_completo, gbm_3_predcv, by="rowIndex")

for (i in 1:nrow(data_stacking_completo)){
  if (data_stacking_completo[i,2]==data_stacking_completo[i,4] & 
      data_stacking_completo[i,4]==data_stacking_completo[i,6]){}
  else {print(i)}
}

data_stacking_completo$rowIndex <- NULL
data_stacking_completo$teamRslt  <- data_stacking_completo$obs_glm
data_stacking_completo$obs_glm <- NULL
data_stacking_completo$obs_pls <- NULL
data_stacking_completo$obs_gbm <- NULL
data_stacking_completo

library(caret)
set.seed(3)
stacking_completo <- train(teamRslt ~ ., data=data_stacking_completo, method='glm', metric="ROC",
                           trControl=trainControl(method='cv', number=10, 
                                                  search="grid", summaryFunction = twoClassSummary, classProbs = T))
print(stacking_completo)
confusionMatrix(stacking_completo, positive='Win')


#ASSESSMENT
#rimangono fuori nb_normalkernel, nb_densitykernel, nb_no_collin, nb_normalkernel_sempl, pruned, 
#tree_prova_pruned perché non fatti con train
#knn_pc e  nnet_pc_scale_ROC perché dovremmo fare prediction sulle PC
#stacking completo e stacking semplice perché dovremmo fare prediction sulle previsioni dei modelli base sul validation
models_list <- list(gbm1=gbm1, gbm2=gbm_2, gbm3=gbm_3,
                    gbm_s=gbm_sempl, gbm_s1=gbm_sempl1, gbm_s2=gbm_sempl2, gbm_s3=gbm_sempl3, 
                    lasso_ROC=glm_lasso_ROC, pls=pls_ROC, log_PPtree=logistic_PPtree_caret, 
                    log_PPtree_noInf=logistic_PPtree_noInf_ROC, log_AIC=logistic_AIC_caret, 
                    log_AIC_noInf=logistic_AIC_noInf_ROC, knn=knn, 
                    nnet_AIC_sc=nnet_logAIC_scale_ROC,
                    nnet_PPtree_sc=nnet_PPtree_scale_ROC, nnet_PPcaret=nnet_ultimo,
                    glm_s=glm_prova, nnet_s_pca=nnet_semplice, nnet_s=nnet_semplice_nopca, 
                    nnet_s_PPtree=nnet_semplice_PPtree, nnet_s_solocenter=nnet_semplice_solocenter,
                    rf=rf, rf_s=rf_semplice)
names(models_list)
mylist<-list()
for(i in 1:length(models_list)){  
  probs <- predict(models_list[[i]], newdata=validation, type='prob')
  probs<-data.frame(cbind(probs, winner = validation$teamRslt))
  probs$winner_num<-ifelse(probs$winner=='Win',1,0)
  pred<-ROCR::prediction(probs$Win, probs$winner_num)
  perf<-ROCR::performance(pred,"tpr", "fpr")
  mylist[[i]] <- data.frame(fpr=unlist(perf@x.values),tpr=unlist(perf@y.values),model=names(models_list)[i]) 
}

length(mylist)
ROCS1 <-data.frame(do.call("rbind",mylist[1:8]))
ROCS2 <- data.frame(do.call("rbind",mylist[9:16]))
ROCS3 <- data.frame(do.call("rbind",mylist[17:24]))


library(ggplot2)
library(ggthemes)

windows()
ggplot(ROCS1, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs 1')

windows()
ggplot(ROCS2, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs 2')

windows()
ggplot(ROCS3, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs 3')

#save.image("F:/Università/Lovaglio/data mining/Progetto/inizio_assessment.RData")
#load("E:/Università/Lovaglio/data mining/Progetto/inizio_assessment.RData")

validation_stacking_compl <- validation
pred_glm <- predict(logistic_PPtree_caret, newdata = validation_stacking_compl, type='prob')
validation_stacking_compl$PLoss_glm <- pred_glm[,1]
validation_stacking_compl$PWin_glm <- pred_glm[,2]

pred_pls <- predict(pls_ROC, newdata = validation_stacking_compl, type='prob')
validation_stacking_compl$PLoss_pls <- pred_pls[,1]
validation_stacking_compl$PWin_pls <- pred_pls[,2]

pred_gbm <- predict(gbm_3, newdata = validation_stacking_compl, type='prob')
validation_stacking_compl$PLoss_gbm <- pred_gbm[,1]
validation_stacking_compl$Pwin_gbm <- pred_gbm[,2]

probs_st_com <- predict(stacking_completo, newdata=validation_stacking_compl, type='prob')
probs_st_com<-data.frame(cbind(probs_st_com, winner = validation_stacking_compl$teamRslt))
probs_st_com$winner_num<-ifelse(probs_st_com$winner=='Win',1,0)
pred_st_com<-ROCR::prediction(probs_st_com$Win, probs_st_com$winner_num)
perf_st_com<-ROCR::performance(pred_st_com,"tpr", "fpr")
stack_compl_ROC <- data.frame(fpr=unlist(perf_st_com@x.values),tpr=unlist(perf_st_com@y.values), model="stacking_completo")

windows()
ggplot(stack_compl_ROC, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs stacking completo')

##stacking semplice

validation_stacking_sempl <- validation
pred_glm_sempl <- predict(glm_prova, newdata = validation_stacking_sempl, type='prob')
validation_stacking_sempl$PLoss_glm <- pred_glm_sempl[,1]
validation_stacking_sempl$PWin_glm <- pred_glm_sempl[,2]

pred_gbm_sempl <- predict(gbm_sempl1, newdata = validation_stacking_sempl, type='prob')
validation_stacking_sempl$PLoss_gbm <- pred_gbm_sempl[,1]
validation_stacking_sempl$PWin_gbm <- pred_gbm_sempl[,2]

pred_rf_sempl <- predict(rf_semplice, newdata = validation_stacking_sempl, type='prob')
validation_stacking_sempl$PLoss_rf <- pred_rf_sempl[,1]
validation_stacking_sempl$Pwin_rf <- pred_rf_sempl[,2]

probs_st_sem <- predict(stacking_semplice, newdata=validation_stacking_sempl, type='prob')
probs_st_sem<-data.frame(cbind(probs_st_sem, winner = validation_stacking_sempl$teamRslt))
probs_st_sem$winner_num<-ifelse(probs_st_sem$winner=='Win',1,0)
pred_st_sem<-ROCR::prediction(probs_st_sem$Win, probs_st_sem$winner_num)
perf_st_sem<-ROCR::performance(pred_st_sem,"tpr", "fpr")
stack_sempl_ROC <- data.frame(fpr=unlist(perf_st_sem@x.values),tpr=unlist(perf_st_sem@y.values), model="stacking_semplice")

windows()
ggplot(stack_sempl_ROC, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs stacking semplice')


#validation con PCA
eigvec_pca_com <- pc_knn$loadings[1:74, 1:74]

validation_pca <- validation
validation_pca$teamLoc_A <- ifelse(validation_pca$teamLoc=='Away', 1, 0)
validation_pca$teamLoc_H <- ifelse(validation_pca$teamLoc=='Home', 1, 0)
validation_pca$teamLoc <- NULL

validation_pca$teamDayOff_1 <- ifelse(validation_pca$teamDayOff=='1', 1, 0)
validation_pca$teamDayOff_2 <- ifelse(validation_pca$teamDayOff=='2', 1, 0)
validation_pca$teamDayOff_3 <- ifelse(validation_pca$teamDayOff=='3', 1, 0)
validation_pca$teamDayOff_4 <- ifelse(validation_pca$teamDayOff=='4', 1, 0)
validation_pca$teamDayOff_5più <- ifelse(validation_pca$teamDayOff=='5+', 1, 0)
validation_pca$teamDayOff <- NULL

validation_pca$opptDayOff_1 <- ifelse(validation_pca$opptDayOff=='1', 1, 0)
validation_pca$opptDayOff_2 <- ifelse(validation_pca$opptDayOff=='2', 1, 0)
validation_pca$opptDayOff_3 <- ifelse(validation_pca$opptDayOff=='3', 1, 0)
validation_pca$opptDayOff_4 <- ifelse(validation_pca$opptDayOff=='4', 1, 0)
validation_pca$opptDayOff_5più <- ifelse(validation_pca$opptDayOff=='5+', 1, 0)
validation_pca$opptDayOff <- NULL

pc_to_validate <- (as.matrix(validation_pca[,-1])-(matrix(1, nrow=nrow(validation_pca), ncol=1)%*%
                                                     colMeans(validation_pca[,-1])))%*%eigvec_pca_com
pc_to_validate <- as.data.frame(pc_to_validate)[,1:13]
pc_to_validate$teamRslt <- validation_pca$teamRslt

models_list_pc=list(knn_pc=knn_pc, nnet_pc_scale=nnet_pc_scale_ROC)
mylist_pc<-list()
for(i in 1:length(models_list_pc)){  
  probs_pc <- predict(models_list_pc[[i]], newdata=pc_to_validate, type='prob')
  probs_pc<-data.frame(cbind(probs_pc, winner = pc_to_validate$teamRslt))
  probs_pc$winner_num<-ifelse(probs_pc$winner=='Win',1,0)
  pred_pc<-ROCR::prediction(probs_pc$Win, probs_pc$winner_num)
  perf_pc<-ROCR::performance(pred_pc,"tpr", "fpr")
  mylist_pc[[i]] <- data.frame(fpr=unlist(perf_pc@x.values),tpr=unlist(perf_pc@y.values),model=names(models_list_pc)[[i]]) 
}

ROCS_pc <-data.frame(do.call("rbind",mylist_pc))

windows()
ggplot(ROCS_pc, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs modelli con pc')

library(plyr)
validation_nb <- validation
paste(names(validation), collapse = "', '")
names(validation_nb) <- c('teamLoc', 'teamRslt', 'teamDayOff', 'teamPTS', 'teamAST', 'teamTO', 'teamSTL', 
'teamBLK', 'teamPF', 'team2PA', 'team2P.', 'team3PA', 'team3P.', 'teamFTA', 'teamFT.', 'teamORB', 
'teamDRB', 'teamTREB.', 'teamASST.', 'teamTS.', 'teamEFG.', 'teamOREB.', 'teamDREB.', 'teamTO.', 
'teamSTL.', 'teamBLK.', 'teamBLKR', 'teamPPS', 'teamOrtg', 'teamDrtg', 'teamPlay.', 'teamAR', 
'teamAST.TO', 'teamSTL.TO', 'opptDayOff', 'opptPTS', 'opptAST', 'opptTO', 'opptSTL', 'opptBLK', 
'opptPF', 'oppt2PA', 'oppt2P.', 'oppt3PA', 'oppt3P.', 'opptFTA', 'opptFT.', 'opptORB', 'opptDRB', 
'opptTREB.', 'opptASST.', 'opptTS.', 'opptEFG.', 'opptOREB.', 'opptDREB.', 'opptTO.', 'opptSTL.', 
'opptBLK.', 'opptBLKR', 'opptPPS', 'opptOrtg', 'opptDrtg', 'opptPlay.', 'opptAR', 'opptAST.TO', 
'opptSTL.TO')

predict(nb_normalkernel_sempl, newdata = validation_nb, type="prob")

models_list_nb=list(nb_norker=nb_normalkernel, nb_densker=nb_densitykernel,
                    nb_no_collin=nb_no_collin, nb_normalkernel_sempl=nb_normalkernel_sempl)
mylist_nb<-list()
library(klaR)
for(i in 1:length(models_list_nb)){  
  probs_nb <- predict(models_list_nb[[i]], newdata=validation_nb, type='prob')$posterior
  probs_nb <-data.frame(cbind(probs_nb, winner = validation_nb$teamRslt))
  probs_nb$winner <- ifelse(probs_nb$winner=='2', 'Win', 'Loss')
  probs_nb$winner <- as.factor(probs_nb$winner)
  probs_nb$winner_num<-ifelse(probs_nb$winner=='Win',1,0)
  pred_nb<-ROCR::prediction(probs_nb$Win, probs_nb$winner_num)
  perf_nb<-ROCR::performance(pred_nb,"tpr", "fpr")
  mylist_nb[[i]] <- data.frame(fpr=unlist(perf_nb@x.values),tpr=unlist(perf_nb@y.values),model=names(models_list_nb)[[i]]) 
}

ROCS_nb <-data.frame(do.call("rbind",mylist_nb))

windows()
ggplot(ROCS_nb, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs modelli nb')


models_list_tree <-list(pruned=pruned, pruned_s=tree_prova_pruned)
mylist_tree <- c()
for(i in 1:length(models_list_tree)){  
  probs_tree <- predict(models_list_tree[[i]], newdata=validation, type='prob')
  probs_tree <-data.frame(cbind(probs_tree, winner = validation$teamRslt))
  probs_tree$winner <- ifelse(probs_nb$winner=='2', 'Win', 'Loss')
  probs_tree$winner <- as.factor(probs_nb$winner)
  probs_tree$winner_num<-ifelse(probs_tree$winner=='Win',1,0)
  pred_tree<-ROCR::prediction(probs_tree$Win, probs_tree$winner_num)
  perf_tree<-ROCR::performance(pred_tree,"tpr", "fpr")
  mylist_tree[[i]] <- data.frame(fpr=unlist(perf_tree@x.values),tpr=unlist(perf_tree@y.values), model=names(models_list_tree)[[i]]) 
}

ROCS_tree <-data.frame(do.call("rbind",mylist_tree))

windows()
ggplot(ROCS_tree, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs modelli tree')

all_ROCS <- rbind(ROCS1, ROCS2, ROCS3, ROCS_pc, stack_compl_ROC, stack_sempl_ROC, ROCS_nb, ROCS_tree)

library(ggplot2)
library(ggthemes)
length(unique(all_ROCS$model))
all_ROCS1 <- all_ROCS[which(all_ROCS$model %in% unique(all_ROCS$model)[1:8]),]
all_ROCS2 <- all_ROCS[which(all_ROCS$model %in% unique(all_ROCS$model)[9:16]),]
all_ROCS3 <- all_ROCS[which(all_ROCS$model %in% unique(all_ROCS$model)[17:25]),]
all_ROCS4 <- all_ROCS[which(all_ROCS$model %in% unique(all_ROCS$model)[26:34]),]

par(mfrow=c(2,2))

ggplot(all_ROCS1, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs 1')

ggplot(all_ROCS2, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs 2')

ggplot(all_ROCS3, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs 3')

ggplot(all_ROCS4, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  xlab("False Positive rate") + ylab("True Positive Rate") + 
  theme_fivethirtyeight() + scale_color_brewer(palette='Set3') + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves using hold-out probs 4')#save.image("E:/Università/Lovaglio/data mining/Progetto/fino_a_AllRocs.RData")
#load("F:/Università/Lovaglio/data mining/Progetto/fino_a_AllRocs.RData")

#AUC

library(pROC)
names(models_list)
list_AUC1 <- c()
for (i in 1:length(models_list)) {
  prob_perAUC=predict(models_list[[i]], validation, type=c("prob"))[,"Win"]
  auc <- auc(validation$teamRslt ~ prob_perAUC)
  print(auc)
  list_AUC1[[i]] <- data.frame(model=names(models_list)[[i]], AUC=auc)
}
list_AUC1
auc_df_parziale <- data.frame(do.call("rbind", list_AUC1))

names(models_list_pc)
list_AUC2 <- c()
for (i in 1:length(models_list_pc)) {
  prob_perAUC_pc=predict(models_list_pc[[i]], pc_to_validate, type=c("prob"))[,"Win"]
  auc_pc <- auc(pc_to_validate$teamRslt ~ prob_perAUC)
  print(auc_pc)
  list_AUC2[[i]] <- data.frame(model=names(models_list_pc)[[i]], AUC=auc_pc)
}
list_AUC2
auc_df_pc <- data.frame(do.call("rbind", list_AUC2))

prob_perAUC_stsem <- predict(stacking_semplice, 
                             newdata=validation_stacking_sempl[, c("PLoss_glm", "PWin_glm", "PLoss_gbm", 
                                                                   "PWin_gbm", "PLoss_rf", "Pwin_rf")], 
                             type='prob')[,"Win"]
auc_stsem <- auc(validation_stacking_sempl$teamRslt ~ prob_perAUC_stsem) 
auc_stsem

prob_perAUC_stcom <- predict(stacking_completo, 
                             newdata=validation_stacking_compl[, c("PLoss_glm", "PWin_glm", "PLoss_pls", 
                                                                   "PWin_pls", "PLoss_gbm", "Pwin_gbm")], 
                             type='prob')[,"Win"]
auc_stcom <- auc(validation_stacking_compl$teamRslt ~ prob_perAUC_stcom) 
auc_stcom

library(klaR)
names(models_list_nb)
list_AUC3 <- c()
for (i in 1:length(models_list_nb)) {
  prob_perAUC_nb=predict(models_list_nb[[i]], validation_nb, type=c("prob"))$posterior[,"Win"]
  auc_nb <- auc(validation_nb$teamRslt ~ prob_perAUC_nb)
  print(auc_nb)
  list_AUC3[[i]] <- data.frame(model=names(models_list_nb)[[i]], AUC=auc_nb)
}
list_AUC3
auc_df_nb <- data.frame(do.call("rbind", list_AUC3))

names(models_list_tree)
list_AUC4 <- c()
for (i in 1:length(models_list_tree)) {
  prob_perAUC_tree=predict(models_list_tree[[i]], validation, type=c("prob"))[,"Win"]
  auc_tree <- auc(validation$teamRslt ~ prob_perAUC_tree)
  print(auc_tree)
  list_AUC4[[i]] <- data.frame(model=names(models_list_tree)[[i]], AUC=auc_tree)
}
list_AUC4
auc_df_tree <- data.frame(do.call("rbind", list_AUC4))


auc_df_all <- rbind(auc_df_parziale, auc_df_pc, auc_df_nb, auc_df_tree,
                    data.frame(model="stacking_completo", AUC=auc_stcom), 
                    data.frame(model="stacking_semplice", AUC=auc_stsem))
auc_df_all

library(dplyr)
auc_df_all <- auc_df_all %>% arrange(desc(AUC))
auc_df_all


#save.image("F:/Università/Lovaglio/data mining/Progetto/fino_a_dopoAUC.RData")
#load("F:/Università/Lovaglio/data mining/Progetto/fino_a_dopoAUC.RData")

library(funModeling)
list_gain <- c()
for (i in 1:length(models_list)){
  posteriors = predict(models_list[[i]], newdata = validation, type="prob")[,"Win"]
  pdf(file = NULL)
  lift <- data.frame(model=names(models_list)[[i]], 
             gain_lift(data = cbind(teamRslt=validation$teamRslt, posteriors), score = 'posteriors', target = 'teamRslt'))
  dev.off()
  list_gain[[i]] <- lift
}

length(list_gain)



for (i in 1:length(models_list_pc)){
  posteriors_pc = predict(models_list_pc[[i]], newdata = pc_to_validate, type="prob")[,"Win"]
  pdf(file = NULL)
  lift_pc <- data.frame(model=names(models_list_pc)[[i]], 
                     gain_lift(data = cbind(teamRslt=pc_to_validate$teamRslt, posteriors=posteriors_pc), 
                               score = 'posteriors', target = 'teamRslt'))
  dev.off()
  list_gain[[length(list_gain)+1]] <- lift_pc
}
length(list_gain)



posteriors_stsem = predict(stacking_semplice, newdata = validation_stacking_sempl, type="prob")[,"Win"]
pdf(file = NULL)
lift_stsem <- data.frame(model="stacking_semplice", 
                      gain_lift(data = cbind(teamRslt=validation_stacking_sempl$teamRslt, posteriors=posteriors_stsem), 
                                score = 'posteriors', target = 'teamRslt'))
dev.off()
list_gain[[length(list_gain)+1]] <- lift_stsem
length(list_gain)



posteriors_stcom = predict(stacking_completo, newdata = validation_stacking_compl, type="prob")[,"Win"]
pdf(file = NULL)
lift_stcom <- data.frame(model="stacking_completo", 
                         gain_lift(data = cbind(teamRslt=validation_stacking_compl$teamRslt, posteriors=posteriors_stcom), 
                                   score = 'posteriors', target = 'teamRslt'))
dev.off()
list_gain[[length(list_gain)+1]] <- lift_stcom
length(list_gain)


library(klaR)
for (i in 1:length(models_list_nb)){
  posteriors_nb = predict(models_list_nb[[i]], newdata = validation_nb, type="prob")$posterior[,"Win"]
  pdf(file = NULL)
  lift_nb <- data.frame(model=names(models_list_nb)[[i]], 
                        gain_lift(data = cbind(teamRslt=validation_nb$teamRslt, posteriors=posteriors_nb), 
                                  score = 'posteriors', target = 'teamRslt'))
  dev.off()
  list_gain[[length(list_gain)+1]] <- lift_nb
}
length(list_gain)



for (i in 1:length(models_list_tree)){
  posteriors_tree = predict(models_list_tree[[i]], newdata = validation, type="prob")[,"Win"]
  pdf(file = NULL)
  lift_tree <- data.frame(model=names(models_list_tree)[[i]], 
                        gain_lift(data = cbind(teamRslt=validation$teamRslt, posteriors=posteriors_tree), 
                                  score = 'posteriors', target = 'teamRslt'))
  dev.off()
  list_gain[[length(list_gain)+1]] <- lift_tree
}
length(list_gain)

all_gain <- data.frame(do.call("rbind", list_gain))
all_gain
all_gain_20 <- all_gain[which(all_gain$Population==20),]
library(dplyr)
all_gain_20 %>% arrange(-Gain)
all_gain_30 <- all_gain[which(all_gain$Population==all_gain[3,2]),] #non so perché non mi legge il 30
all_gain_30
all_gain_30 %>% arrange(-Gain)
auc_df_all %>% arrange(desc(AUC))

#save.image("F:/Università/Lovaglio/data mining/Progetto/fino_a_Gain.RData")
#load("F:/Università/Lovaglio/data mining/Progetto/fino_a_Gain.RData")

#CICLO SOGLIA PRUNED TREE

predP_tree <- predict(pruned, validation, type = "prob")
df_tree=data.frame(cbind(validation$teamRslt, predP_tree[,2], predP_tree[,1]))
colnames(df_tree)=c("Class","Prob_Event","Prob_nonEvent")
df_tree[which(df_tree$Class=='2'), 'Class'] <- 'Win'
df_tree[which(df_tree$Class=='1'), 'Class'] <- 'Loss'
df_tree=df_tree[,1:2]

library(dplyr)
thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table_tree <- data.frame(threshold = thresholds, prop_true_Event = NA,  prop_true_nonEvent = NA, true_Event = NA,  true_nonEvent = NA ,fn_Event=NA)
for (threshold in thresholds) {
  pred_tree <- ifelse(df_tree$Prob_Event > threshold, "Win", "Loss")
  pred_t_tree <- ifelse(pred_tree == df_tree$Class, TRUE, FALSE)
  
  group_tree <- data.frame(df_tree, "pred" = pred_t_tree) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_Event_tree <- filter(group_tree, Class == "Win")
  
  true_Event_tree=sum(filter(group_Event_tree, pred == TRUE)$n)
  prop_Event_tree <- sum(filter(group_Event_tree, pred == TRUE)$n) / sum(group_Event_tree$n)
  
  prop_table_tree[prop_table_tree$threshold == threshold, "prop_true_Event"] <- prop_Event_tree
  prop_table_tree[prop_table_tree$threshold == threshold, "true_Event"] <- true_Event_tree
  
  fn_Event_tree=sum(filter(group_Event_tree, pred == FALSE)$n)
  # true Event predicted as nonEvent
  prop_table_tree[prop_table_tree$threshold == threshold, "fn_Event"] <- fn_Event_tree
  
  
  group_nonEvent_tree <- filter(group_tree, Class == "Loss")
  
  true_nonEvent_tree=sum(filter(group_nonEvent_tree, pred == TRUE)$n)
  prop_nonEvent_tree <- sum(filter(group_nonEvent_tree, pred == TRUE)$n) / sum(group_nonEvent_tree$n)
  
  prop_table_tree[prop_table_tree$threshold == threshold, "prop_true_nonEvent"] <- prop_nonEvent_tree
  prop_table_tree[prop_table_tree$threshold == threshold, "true_nonEvent"] <- true_nonEvent_tree
  
}

prop_table_tree$n=nrow(validation)
prop_table_tree$fp_Event=nrow(validation)-prop_table_tree$true_nonEvent-prop_table_tree$true_Event-prop_table_tree$fn_Event
prop_table_tree$Accuracy=(prop_table_tree$true_nonEvent+prop_table_tree$true_Event)/nrow(validation)
prop_table_tree$Precision_Event=prop_table_tree$true_Event/(prop_table_tree$true_Event+prop_table_tree$fp_Event)

head(prop_table_tree)
tail(prop_table_tree)

library(Hmisc)
prop_table_tree$Precision_Event=impute(prop_table_tree$Precision_Event, 1)
tail(prop_table_tree)

prop_table_tree$F1=2*prop_table_tree$prop_true_Event*prop_table_tree$Precision_Event/(prop_table_tree$prop_true_Event+prop_table_tree$Precision_Event)
head(prop_table_tree)
tail(prop_table_tree)

prop_table_tree2 = prop_table_tree[,-c(4:8)] 
head(prop_table_tree2)

library(dplyr)
library(tidyr)

gathered_tree=prop_table_tree2 %>%
  gather(x, y, prop_true_Event:F1)

library(ggplot2)
windows()
gathered_tree %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(title='Confronto soglie pruned tree', y = "measures",
       color = "Event\nnonEvent")#+coord_cartesian(xlim = c(0.5, 0.65))

prop_table_tree2[which.max(prop_table_tree2$Accuracy),]
prop_table_tree2[which.min(abs(prop_table_tree2$prop_true_Event-prop_table_tree2$prop_true_nonEvent)),]
prop_table_tree2[which(prop_table_tree2$Accuracy==max(prop_table_tree2$Accuracy)),]
prop_table_tree2

#Previsioni su 2017-2018 e classifica
topred_index=which(new_means_allyr$gmDate>'2017-07-30')
topred=new_means_allyr[topred_index, ]
head(topred)
list_team <- unique(topred$teamAbbr)
w_l <- vector(mode='list', length=30)
for (i in 1:30){w_l[[i]] <- c(Win=0, Loss=0)}
names(w_l) <- list_team
w_l

for (i in 1:nrow(topred)){
  if (topred[i, 'teamRslt']=='Win'){
    w_l[[topred[i, 'teamAbbr']]][1]=w_l[[topred[i, 'teamAbbr']]][1]+1  
  } else {
    if (topred[i, 'teamRslt']=='Loss'){
      w_l[[topred[i, 'teamAbbr']]][2]=w_l[[topred[i, 'teamAbbr']]][2]+1  
    }
  }
}

w_l
w_l_df <- as.data.frame(t(as.data.frame(w_l)))
w_l_df$'win%' <- w_l_df$Win/(w_l_df$Win+w_l_df$Loss)
head(w_l_df)

topred$posterior=predict(pruned, topred, "prob")[,"Win"]
topred$pred_y0.5=as.factor(ifelse(topred$posterior>0.50, "Win", "Loss"))

w_l_pred0.5 <- vector(mode='list', length=30)
for (i in 1:30){w_l_pred0.5[[i]] <- c(Win=0, Loss=0)}
names(w_l_pred0.5) <- list_team
w_l_pred0.5

for (i in 1:nrow(topred)){
  if (topred[i, 'pred_y0.5']=='Win'){
    w_l_pred0.5[[topred[i, 'teamAbbr']]][1]=w_l_pred0.5[[topred[i, 'teamAbbr']]][1]+1  
  } else {
    if (topred[i, 'pred_y0.5']=='Loss'){
      w_l_pred0.5[[topred[i, 'teamAbbr']]][2]=w_l_pred0.5[[topred[i, 'teamAbbr']]][2]+1  
    }
  }
}

w_l_pred0.5
w_l_df_pred0.5 <- as.data.frame(t(as.data.frame(w_l_pred0.5)))
names(w_l_df_pred0.5) <- c('Win_0.5', 'Loss_0.5')
w_l_df_pred0.5$'0.5win%' <- w_l_df_pred0.5$Win_0.5/(w_l_df_pred0.5$Win_0.5+w_l_df_pred0.5$Loss_0.5)
tail(w_l_df_pred0.5)

topred$pred_y0.57=as.factor(ifelse(topred$posterior>0.57, "Win", "Loss"))

w_l_pred0.57 <- vector(mode='list', length=30)
for (i in 1:30){w_l_pred0.57[[i]] <- c(Win=0, Loss=0)}
names(w_l_pred0.57) <- list_team
w_l_pred0.57

for (i in 1:nrow(topred)){
  if (topred[i, 'pred_y0.57']=='Win'){
    w_l_pred0.57[[topred[i, 'teamAbbr']]][1]=w_l_pred0.57[[topred[i, 'teamAbbr']]][1]+1  
  } else {
    if (topred[i, 'pred_y0.57']=='Loss'){
      w_l_pred0.57[[topred[i, 'teamAbbr']]][2]=w_l_pred0.57[[topred[i, 'teamAbbr']]][2]+1  
    }
  }
}

w_l_pred0.57
w_l_df_pred0.57 <- as.data.frame(t(as.data.frame(w_l_pred0.57)))
names(w_l_df_pred0.57) <- c('Win_0.57', 'Loss_0.57')
w_l_df_pred0.57$'0.57win%' <- w_l_df_pred0.57$Win_0.57/(w_l_df_pred0.57$Win_0.57+w_l_df_pred0.57$Loss_0.57)
tail(w_l_df_pred0.57)

w_l_df_pred0.5==w_l_df_pred0.57

classifica <- cbind(w_l_df, w_l_df_pred0.5, w_l_df_pred0.57)
classifica$diff0.5 <- abs(classifica$`win%`-classifica$`0.5win%`)
classifica$diff0.57 <- abs(classifica$`win%`-classifica$`0.57win%`)
classifica
colMeans(classifica)[c('diff0.5', 'diff0.57')]
#####################################
#####################################
#####################################
all_gain[which(all_gain$model=='stacking_completo'),]
all_gain[which(all_gain$model=='logistic_PPtree_caret'),]

#Teniamo stacking_completo e logistic_PPtree_caret
library(caret)
results <- resamples(list(stacking_completo=stacking_completo, logistico_PPtree=logistic_PPtree_caret))
summary(results)
bwplot(results)
Diffs <- diff(results)
summary(Diffs)
bwplot(results)

library(ggplot2)
library(ggthemes)
windows()
ggplot(all_ROCS[which(all_ROCS$model=='logistic_PPtree_caret'|all_ROCS$model=='stacking_completo'),], 
       aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_line(aes(y=tpr,color=model),size=1) +  
  scale_fill_manual(values =  colorRampPalette(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)])(22)) + 
  geom_segment(aes(x=0, y=0, xend=1, yend=1), colour = 'black',lty=2) + labs(title='ROC curves dei due best model', x='FPR', y='TPR')

#CICLO SOGLIA: logistico

predP_log <- predict(logistic_PPtree_caret, validation, type = "prob")
df_log=data.frame(cbind(validation$teamRslt, predP_log[,2], predP_log[,1]))
colnames(df_log)=c("Class","Prob_Event","Prob_nonEvent")
df_log[which(df_log$Class=='2'), 'Class'] <- 'Win'
df_log[which(df_log$Class=='1'), 'Class'] <- 'Loss'
df_log=df_log[,1:2]

library(dplyr)
thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table_log <- data.frame(threshold = thresholds, prop_true_Event = NA,  prop_true_nonEvent = NA, true_Event = NA,  true_nonEvent = NA ,fn_Event=NA)
for (threshold in thresholds) {
  pred_log <- ifelse(df_log$Prob_Event > threshold, "Win", "Loss")
  pred_t_log <- ifelse(pred_log == df_log$Class, TRUE, FALSE)
  
  group_log <- data.frame(df_log, "pred" = pred_t_log) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_Event_log <- filter(group_log, Class == "Win")
  
  true_Event_log=sum(filter(group_Event_log, pred == TRUE)$n)
  prop_Event_log <- sum(filter(group_Event_log, pred == TRUE)$n) / sum(group_Event_log$n)
  
  prop_table_log[prop_table_log$threshold == threshold, "prop_true_Event"] <- prop_Event_log
  prop_table_log[prop_table_log$threshold == threshold, "true_Event"] <- true_Event_log
  
  fn_Event_log=sum(filter(group_Event_log, pred == FALSE)$n)
  # true Event predicted as nonEvent
  prop_table_log[prop_table_log$threshold == threshold, "fn_Event"] <- fn_Event_log
  
  
  group_nonEvent_log <- filter(group_log, Class == "Loss")
  
  true_nonEvent_log=sum(filter(group_nonEvent_log, pred == TRUE)$n)
  prop_nonEvent_log <- sum(filter(group_nonEvent_log, pred == TRUE)$n) / sum(group_nonEvent_log$n)
  
  prop_table_log[prop_table_log$threshold == threshold, "prop_true_nonEvent"] <- prop_nonEvent_log
  prop_table_log[prop_table_log$threshold == threshold, "true_nonEvent"] <- true_nonEvent_log
  
}

prop_table_log$n=nrow(validation)
prop_table_log$fp_Event=nrow(validation)-prop_table_log$true_nonEvent-prop_table_log$true_Event-prop_table_log$fn_Event
prop_table_log$Accuracy=(prop_table_log$true_nonEvent+prop_table_log$true_Event)/nrow(validation)
prop_table_log$Precision_Event=prop_table_log$true_Event/(prop_table_log$true_Event+prop_table_log$fp_Event)

head(prop_table_log)
tail(prop_table_log)

library(Hmisc)
prop_table_log$Precision_Event=impute(prop_table_log$Precision_Event, 1)
tail(prop_table_log)

prop_table_log$F1=2*prop_table_log$prop_true_Event*prop_table_log$Precision_Event/(prop_table_log$prop_true_Event+prop_table_log$Precision_Event)
head(prop_table_log)
tail(prop_table_log)

prop_table_log2 = prop_table_log[,-c(4:8)] 
head(prop_table_log2)

library(dplyr)
library(tidyr)

gathered_log=prop_table_log2 %>%
  gather(x, y, prop_true_Event:F1)

library(ggplot2)
windows()
gathered_log %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(title='Confronto soglie logistico', y = "measures",
       color = "Event\nnonEvent")#+coord_cartesian(xlim = c(0.4, 0.7))


#CICLO SOGLIA: stacking completo

predP_stcom <- predict(stacking_completo, validation_stacking_compl, type = "prob")
df_stcom=data.frame(cbind(validation_stacking_compl$teamRslt, predP_stcom[,2], predP_stcom[,1]))
colnames(df_stcom)=c("Class","Prob_Event","Prob_nonEvent")
df_stcom[which(df_stcom$Class=='2'), 'Class'] <- 'Win'
df_stcom[which(df_stcom$Class=='1'), 'Class'] <- 'Loss'
df_stcom=df_stcom[,1:2]

library(dplyr)
thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table_stcom <- data.frame(threshold = thresholds, prop_true_Event = NA,  prop_true_nonEvent = NA, true_Event = NA,  true_nonEvent = NA ,fn_Event=NA)
for (threshold in thresholds) {
  pred_stcom <- ifelse(df_stcom$Prob_Event > threshold, "Win", "Loss")
  pred_t_stcom <- ifelse(pred_stcom == df_stcom$Class, TRUE, FALSE)
  
  group_stcom <- data.frame(df_stcom, "pred" = pred_t_stcom) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_Event_stcom <- filter(group_stcom, Class == "Win")
  
  true_Event_stcom=sum(filter(group_Event_stcom, pred == TRUE)$n)
  prop_Event_stcom <- sum(filter(group_Event_stcom, pred == TRUE)$n) / sum(group_Event_stcom$n)
  
  prop_table_stcom[prop_table_stcom$threshold == threshold, "prop_true_Event"] <- prop_Event_stcom
  prop_table_stcom[prop_table_stcom$threshold == threshold, "true_Event"] <- true_Event_stcom
  
  fn_Event_stcom=sum(filter(group_Event_stcom, pred == FALSE)$n)
  # true Event predicted as nonEvent
  prop_table_stcom[prop_table_stcom$threshold == threshold, "fn_Event"] <- fn_Event_stcom
  
  
  group_nonEvent_stcom <- filter(group_stcom, Class == "Loss")
  
  true_nonEvent_stcom=sum(filter(group_nonEvent_stcom, pred == TRUE)$n)
  prop_nonEvent_stcom <- sum(filter(group_nonEvent_stcom, pred == TRUE)$n) / sum(group_nonEvent_stcom$n)
  
  prop_table_stcom[prop_table_stcom$threshold == threshold, "prop_true_nonEvent"] <- prop_nonEvent_stcom
  prop_table_stcom[prop_table_stcom$threshold == threshold, "true_nonEvent"] <- true_nonEvent_stcom
  
}

prop_table_stcom$n=nrow(validation_stacking_compl)
prop_table_stcom$fp_Event=nrow(validation_stacking_compl)-prop_table_stcom$true_nonEvent-prop_table_stcom$true_Event-prop_table_stcom$fn_Event
prop_table_stcom$Accuracy=(prop_table_stcom$true_nonEvent+prop_table_stcom$true_Event)/nrow(validation_stacking_compl)
prop_table_stcom$Precision_Event=prop_table_stcom$true_Event/(prop_table_stcom$true_Event+prop_table_stcom$fp_Event)


head(prop_table_stcom)
tail(prop_table_stcom)
prop_table_stcom[80:101,]
library(Hmisc)
prop_table_stcom$Precision_Event=impute(prop_table_stcom$Precision_Event, 1)

prop_table_stcom$F1=2*prop_table_stcom$prop_true_Event*prop_table_stcom$Precision_Event/(prop_table_stcom$prop_true_Event+prop_table_stcom$Precision_Event)

head(prop_table_stcom)
tail(prop_table_stcom)


prop_table_stcom2 = prop_table_stcom[,-c(4:8)] 
head(prop_table_stcom2)

library(dplyr)
library(tidyr)

gathered_stcom=prop_table_stcom2 %>%
  gather(x, y, prop_true_Event:F1)


windows()
gathered_stcom %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(title='Confronto soglie stacking completo', y = "measures",
       color = "Event\nnonEvent")#+coord_cartesian(xlim = c(0.4, 0.7))

prop_table_log2[which.min(abs(prop_table_log2$prop_true_Event-prop_table_log2$prop_true_nonEvent)),]
prop_table_stcom2[which.min(abs(prop_table_stcom2$prop_true_Event-prop_table_stcom2$prop_true_nonEvent)),]

#Teniamo il modello logistico con soglia 0.49

#save.image("G:/Università/Lovaglio/data mining/Progetto/assessment_finito.RData")
#load("G:/Università/Lovaglio/data mining/Progetto/assessment_finito.RData")

#MODELLO FINALE
# coefficienti
library(coefplot)
coefplot(logistico_PPtree, intercept=FALSE, decreasing = T)

# OR
OR=round(exp(cbind(OR=coef(logistico_PPtree), confint(logistico_PPtree))), digits = 3)
OR
library(forestmodel)
print(forest_model(log3),text_size = 5)

# fit
drop1(logistico_PPtree, test="LRT")

summary(logistico_PPtree)


#SCORE
score$posterior=predict(logistic_PPtree_caret, score, "prob")[,"Win"]
score$pred_y=as.factor(ifelse(score$posterior>0.49, "Win", "Loss"))