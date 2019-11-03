## We read the data and use str to understand their structure:

flagdata<-read.table ("flag.data", header=FALSE, sep=",")
str(flagdata)
colnames(flagdata)<-c("country", "landmass", "geoquandrant", "area", "population",
                      "language", "religion", "bars", "stripes", "colours", "red",
                      "green", "blue", "gold", "white","black", "orange", "mainhue",
                      "circles", "crosses", "saltires", "quarters", "sunstars", "crescent",
                      "triangle", "inanimate", "animate", "text","topleftcl", "botrightcl")
str(flagdata)
#Nominal values to binary columns
flagdata$mainhue_green<-as.numeric(flagdata$mainhue=="green")
flagdata$mainhue_black<-as.numeric(flagdata$mainhue=="black")
flagdata$mainhue_blue<-as.numeric(flagdata$mainhue=="blue")
flagdata$mainhue_brown<-as.numeric(flagdata$mainhue=="brown")
flagdata$mainhue_gold<-as.numeric(flagdata$mainhue=="gold")
flagdata$mainhue_orange<-as.numeric(flagdata$mainhue=="orange")
flagdata$mainhue_red<-as.numeric(flagdata$mainhue=="red")
flagdata$mainhue_white<-as.numeric(flagdata$mainhue=="white")

flagdata$topleft_green<-as.numeric(flagdata$topleftcl=="green")
flagdata$topleft_black<-as.numeric(flagdata$topleftcl=="black")
flagdata$topleft_blue<-as.numeric(flagdata$topleftcl=="blue")
flagdata$topleft_gold<-as.numeric(flagdata$topleftcl=="gold")
flagdata$topleft_orange<-as.numeric(flagdata$topleftcl=="orange")
flagdata$topleft_red<-as.numeric(flagdata$topleftcl=="red")
flagdata$topleft_white<-as.numeric(flagdata$topleftcl=="white")

flagdata$botright_green<-as.numeric(flagdata$botrightcl=="green")
flagdata$botright_black<-as.numeric(flagdata$botrightcl=="black")
flagdata$botright_blue<-as.numeric(flagdata$botrightcl=="blue")
flagdata$botright_gold<-as.numeric(flagdata$botrightcl=="gold")
flagdata$botright_orange<-as.numeric(flagdata$botrightcl=="orange")
flagdata$botright_red<-as.numeric(flagdata$botrightcl=="red")
flagdata$botright_white<-as.numeric(flagdata$botrightcl=="white")
flagdata$botright_brown<-as.numeric(flagdata$mainhue=="brown")

#Dataset for Modeling


flag<-flagdata[,c("religion", "bars", "stripes", "colours", "red",
  "green", "blue", "gold", "white","black", "orange",
  "circles", "crosses", "saltires", "quarters", "sunstars", "crescent",
  "triangle", "inanimate", "animate", "text","mainhue_green", "mainhue_black",
  "mainhue_blue", "mainhue_brown", "mainhue_gold", "mainhue_orange", "mainhue_red",
  "mainhue_white", "topleft_green", "topleft_black", "topleft_blue", "topleft_gold",
  "topleft_orange", "topleft_red", "topleft_white", "botright_green", "botright_black",
  "botright_blue", "botright_gold", "botright_orange", "botright_red", "botright_white",
  "botright_brown")]


#Conversion to factors
flag$religion<-as.factor(flag$religion)
flag$red<-as.factor(flag$red)
flag$green<-as.factor(flag$green)
flag$blue<-as.factor(flag$blue)
flag$gold<-as.factor(flag$gold)
flag$white<-as.factor(flag$white)
flag$black<-as.factor(flag$black)
flag$orange<-as.factor(flag$orange)
flag$crescent<-as.factor(flag$crescent)
flag$triangle<-as.factor(flag$triangle)
flag$inanimate<-as.factor(flag$inanimate)
flag$animate<-as.factor(flag$animate)
flag$text<-as.factor(flag$text)
flag$mainhue_green<-as.factor(flag$mainhue_green)
flag$mainhue_blue<-as.factor(flag$mainhue_blue)
flag$mainhue_white<-as.factor(flag$mainhue_white)
flag$topleft_orange<-as.factor(flag$topleft_orange)
flag$botright_blue<-as.factor(flag$botright_blue)
flag$botright_brown<-as.factor(flag$botright_brown)
flag$botright_brown<-as.factor(flag$botright_brown)
flag$mainhue_black<-as.factor(flag$mainhue_black)
flag$mainhue_brown<-as.factor(flag$mainhue_brown)
flag$topleft_green<-as.factor(flag$topleft_green)
flag$topleft_red<-as.factor(flag$topleft_red)
flag$botright_gold<-as.factor(flag$botright_gold)
flag$mainhue_gold<-as.factor(flag$mainhue_gold)
flag$topleft_black<-as.factor(flag$topleft_black)
flag$topleft_white<-as.factor(flag$topleft_white)
flag$botright_orange<-as.factor(flag$botright_orange)
flag$mainhue_orange<-as.factor(flag$mainhue_orange)
flag$topleft_blue<-as.factor(flag$topleft_blue)
flag$botright_green<-as.factor(flag$botright_green)
flag$botright_red<-as.factor(flag$botright_red)
flag$mainhue_red<-as.factor(flag$mainhue_red)
flag$topleft_gold<-as.factor(flag$topleft_gold)
flag$botright_black<-as.factor(flag$botright_black)
flag$botright_white<-as.factor(flag$botright_white)

summary(flag)
str(flag)


##BARPLOTS
library(ggplot2)

#per religion and Mainhue
label_parsed<-c(`0`="Catholic", `1`="Other Christian", 
                `2`="Muslim", `3`="Buddhist", `4`="Hindu", 
                `5`="Ethnic", `6`="Marxist", `7`="Others")
 ggplot(data=flag, aes(x=flagdata$mainhue))+
   geom_bar(aes(fill=factor(flag$religion,labels=label_parsed)), position = "dodge", alpha=0.5)+
   labs(fill="Religion", x="Mainhue", y="Frequency")

#per religion
ggplot(data=flag, aes(x=flag$religion))+
  scale_x_discrete(labels = label_parsed)+
  geom_bar(aes(fill=factor(flag$religion,labels=label_parsed)), position = "dodge", alpha=0.5)+
  labs(fill="Religion", x="Religion", y="Frequency")

#per mainhue
col = c('Black', 'Blue', 'Brown', 'Gold', 'Green', 'Orange', 'Red', 'White')
ggplot(data=flagdata, aes(x=flagdata$mainhue))+
  geom_bar(aes(fill=flagdata$mainhue),fill=col, position = "dodge", alpha=0.5)+
  labs(fill=col, x="Mainhue", y="Frequency")

#VARIABLE SELECTION LASSO
library(glmnet)
LASSO <- model.matrix(religion~., flag)[,-1]
hmda_models_lasso <- glmnet(LASSO, flag$religion, alpha=1, standardize = FALSE, family="multinomial", type.multinomial="grouped")
# Plot
plot(hmda_models_lasso, xvar = "lambda", label = TRUE)

# Cross Validation
lasso.cv <- cv.glmnet(LASSO,flag$religion, alpha=1, family="multinomial", type.multinomial="grouped")

ncol(flag)
# Cross Validation Plot
plot(lasso.cv)

coef(lasso.cv, s = "lambda.min")
coef(lasso.cv, s = "lambda.1se")

flagforclass<-flag[, c("religion","bars","stripes", "red","green","blue","gold", "white","black",
                       "circles","crosses","saltires","sunstars","crescent","triangle",
                       "animate","mainhue_blue","mainhue_brown","mainhue_orange",
                       "mainhue_white","topleft_green","topleft_gold","topleft_white",
                       "botright_black","botright_gold","botright_orange",
                       "botright_red")]

parsimonious<-flag[,c("religion","green",
                      "blue",
                      "gold",
                      "crosses",
                      "crescent",
                      "topleft_white",
                      "botright_black")]

#########################
#####CLASSIFICATION######
#########################

#Classification Tree with split of data
set.seed(3345)
library (tree)
#split of data with the respective probabilities
table(flagforclass$religion)/nrow(flagforclass)

sample.ind <- sample(c(0:7),nrow(flagforclass),
                     replace = T,
                     prob = c(0.21,0.31, 0.19, 0.04, 0.02, 0.14, 0.077, 0.02))
sample.ind
sample.ind <- sample(2,
                     nrow(flagforclass),
                     replace = T,
                     prob = c(0.7,0.3))

train <- flagforclass[sample.ind==1,]
val <- flagforclass[sample.ind==2,]
table(train$religion)/nrow(train)
table(val$religion)/nrow(val)


tree_model<-tree(religion~.,train)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty =0)


tree_pred<-predict (tree_model ,val,type ="class")
conf.matrix<-table(tree_pred, val$religion)
conf.matrix
sum(diag(conf.matrix))/sum(conf.matrix)

importance(tree_model)

#Pruning
cv.tree =cv.tree(tree_model, FUN=prune.misclass )
cv.tree #The tree with 4 terminal nodes results in the lowest
#cross-validation error rate, with 84 cross-validation errors.
pruned =prune.misclass(tree_model ,best=4)
plot(pruned )
text(pruned, pretty=0)

#Pruned Prediction
tree.pred=predict (pruned , val ,type="class")
conf.matrix<-table(tree.pred, val$religion)
sum(diag(conf.matrix))/sum(conf.matrix)

#Now 41% of the test observations are correctly classified, so the
#the pruning process produced a more interpretable tree, but also
#improved the classification accuracy.

#BAGGING
set.seed(3345)
library("ipred")
mod <- bagging(religion ~., data=flagforclass ,coob=TRUE)
print(mod)
conf<-table(predict(mod), flagforclass$religion)
sum(diag(conf))/sum(conf)

# RANDOM FOREST with 10-fold cross validation

set.seed(3345)
folds <- sample(rep(1:10, length.out = nrow(flagforclass)), size = nrow(flagforclass), replace = F)#10 fold CV
table(folds)

library(randomForest)

CV_rf <- lapply(1:10, function(x){ #10 corresponds to the number of folds defined earlier
  model <- randomForest(religion ~ ., data = flagforclass[folds != x,], ntree=500, importance=T)
  print("OMG")
  print(model)
  #print(importance(model))
  preds <- predict(model,  flagforclass[folds == x,], type="response")
  return(data.frame(preds, real = flagforclass$religion[folds == x]))
})

CV_rf <- do.call(rbind, CV_rf)
CV_rf
library(caret)
confusionMatrix(CV_rf$preds, CV_rf$real)

#####################
#####CLUSTERING######
#####################

  
flagforclustering<-flagdata[,c("religion","country", "bars", "stripes", "colours", "red",
                  "green", "blue", "gold", "white","black", "orange",
                  "circles", "crosses", "saltires", "quarters", "sunstars", "crescent",
                  "triangle", "inanimate", "animate", "text","mainhue_green", "mainhue_black",
                  "mainhue_blue", "mainhue_brown", "mainhue_gold", "mainhue_orange", "mainhue_red",
                  "mainhue_white", "topleft_green", "topleft_black", "topleft_blue", "topleft_gold",
                  "topleft_orange", "topleft_red", "topleft_white", "botright_green", "botright_black",
                  "botright_blue", "botright_gold", "botright_orange", "botright_red", "botright_white",
                  "botright_brown")]


#Conversion to factors
flagforclustering$religion<-as.factor(flagforclustering$religion)
flagforclustering$country<-as.factor(flagforclustering$country)
flagforclustering$red<-as.factor(flagforclustering$red)
flagforclustering$green<-as.factor(flagforclustering$green)
flagforclustering$blue<-as.factor(flagforclustering$blue)
flagforclustering$gold<-as.factor(flagforclustering$gold)
flagforclustering$white<-as.factor(flagforclustering$white)
flagforclustering$black<-as.factor(flagforclustering$black)
flagforclustering$orange<-as.factor(flagforclustering$orange)
flagforclustering$crescent<-as.factor(flagforclustering$crescent)
flagforclustering$triangle<-as.factor(flagforclustering$triangle)
flagforclustering$inanimate<-as.factor(flagforclustering$inanimate)
flagforclustering$animate<-as.factor(flagforclustering$animate)
flagforclustering$text<-as.factor(flagforclustering$text)
flagforclustering$mainhue_green<-as.factor(flagforclustering$mainhue_green)
flagforclustering$mainhue_blue<-as.factor(flagforclustering$mainhue_blue)
flagforclustering$mainhue_white<-as.factor(flagforclustering$mainhue_white)
flagforclustering$topleft_orange<-as.factor(flagforclustering$topleft_orange)
flagforclustering$botright_blue<-as.factor(flagforclustering$botright_blue)
flagforclustering$botright_brown<-as.factor(flagforclustering$botright_brown)
flagforclustering$botright_brown<-as.factor(flagforclustering$botright_brown)
flagforclustering$mainhue_black<-as.factor(flagforclustering$mainhue_black)
flagforclustering$mainhue_brown<-as.factor(flagforclustering$mainhue_brown)
flagforclustering$topleft_green<-as.factor(flagforclustering$topleft_green)
flagforclustering$topleft_red<-as.factor(flagforclustering$topleft_red)
flagforclustering$botright_gold<-as.factor(flagforclustering$botright_gold)
flagforclustering$mainhue_gold<-as.factor(flagforclustering$mainhue_gold)
flagforclustering$topleft_black<-as.factor(flagforclustering$topleft_black)
flagforclustering$topleft_white<-as.factor(flagforclustering$topleft_white)
flagforclustering$botright_orange<-as.factor(flagforclustering$botright_orange)
flagforclustering$mainhue_orange<-as.factor(flagforclustering$mainhue_orange)
flagforclustering$topleft_blue<-as.factor(flagforclustering$topleft_blue)
flagforclustering$botright_green<-as.factor(flagforclustering$botright_green)
flagforclustering$botright_red<-as.factor(flagforclustering$botright_red)
flagforclustering$mainhue_red<-as.factor(flagforclustering$mainhue_red)
flagforclustering$topleft_gold<-as.factor(flagforclustering$topleft_gold)
flagforclustering$botright_black<-as.factor(flagforclustering$botright_black)
flagforclustering$botright_white<-as.factor(flagforclustering$botright_white)

library(cluster)
gower_dist <- daisy(flagforclustering[,-c(1,2)], metric = "gower")
summary(gower_dist)



#PAM

sil_width <- c(NA)

for(i in 2:15){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}



# Plot sihouette width (higher is better)

plot(1:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:15, sil_width)
#13 clusters

pam_fit <- pam(gower_dist, diss = TRUE, k =13)
plot(pam_fit)
summary(pam_fit)
pam_fit$clustering

flagforclustering$clustering<-pam_fit$clustering
flagforclustering
library(magrittr)
library(dplyr)

#Summary Statistics for interpretation
pam_results <- flagforclustering %>%
  dplyr::select(-c(1,2)) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
library(ggplot2)

#t-Distributed Stochastic Neighbor Embedding (t-SNE) 
library(Rtsne)
set.seed(35)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = flagforclustering$country)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))




#per religion

flagforclustering
flagforclustering<-flagforclustering[order(flagforclustering$clustering),]

#BARPLOTS for identifying and explaining clusters
label_parsed<-c(`1`="1", 
                `2`="2", `3`="3", `4`="4", 
                `5`="5", `6`="6", `7`="7", `8`="8", `9`="9", `10`="10", `11`="11",
                `12`="12", `13`="13")



ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$bars)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="Bars"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$stripes)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="Stripes"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$colours)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="colours"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$red)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="red"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$green)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="green"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$blue)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="blue"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$gold)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="gold"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$white)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="white"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$black)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="black"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$orange)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="orange"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$circles)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="circles"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$crosses)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="crosses"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$circles)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="circles"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$crosses)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="saltires"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$quarters)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="quartes"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$sunstars)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="sunstars"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$crescent)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="crescent"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$triangle)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="triangle"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$inanimate)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="inanimate"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$animate)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="animate"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$animate)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="text")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="text"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_green)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_green"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_red)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_red"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_blue)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_blue"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_gold)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_gold"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_white)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_white"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_orange)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_orange"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_black)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_black"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_brown)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_brown"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$mainhue_red)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="mainhue_red"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_green)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_green"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_red)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_red"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_blue)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_blue"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_gold)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_gold"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_white)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_white"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_orange)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_orange"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_black)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_black"))


ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$topleft_red)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="topleft_red"))


ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_green)), alpha=0.5, position = "dodge")+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_green"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_red)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_red"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_blue)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_blue"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_gold)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_gold"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_white)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_white"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_orange)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_orange"))

ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_black)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_black"))


ggplot(data=flagforclustering, aes(x=flagforclustering$clustering))+
  geom_bar(aes(fill=factor(flagforclustering$botright_red)), alpha=0.5)+
  labs(fill=label_parsed, x="Clustering", y="Frequency")+
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))+
  guides(fill=guide_legend(title="botright_red"))











