library(nnet)
library(splitstackshape)
library(data.table)
library(yardstick)

set.seed(123)
setwd("D:\Work\Projects\bc2406\Project Files")

dt <- fread("heart_disease_uci_cleaned.csv")

dt$sex <- factor(dt$sex)
dt$cp <- factor(dt$cp)
dt$fbs <- factor(dt$fbs)
dt$restecg <- factor(dt$restecg)
dt$exang <- factor(dt$exang)
dt$slope <- factor(dt$slope)
dt$thal <- factor(dt$thal)
dt$num <- factor(dt$num)
dt[dt==''] <- NA

kmeans10 <- fread("heart_disease_uci_kmeans_10.csv")
hcluster8 <- fread("heart_disease_uci_hcluster_8.csv")


dt.clusters <- data.table(dt, kmeans10, hcluster8)
dt.clusters$kmeans10 <- factor(dt.clusters$kmeans10)
dt.clusters$hcluster8 <- factor(dt.clusters$hcluster8)


dt.clusters <- dt.clusters[!is.na(dt.clusters$thal)]
dt.clusters <- dt.clusters[!is.na(dt.clusters$slope)]
dt.clusters <- dt.clusters[!is.na(dt.clusters$fbs)]

sample <- stratified(dt.clusters, c("num"), 0.8, bothSets = TRUE)
dt.clusters_train <- sample$SAMP1
dt.clusters_test <- sample$SAMP2

summary(dt.clusters_train)

ytest2 <- ifelse(dt.clusters_test$num == 0, 0, 1)

#Without clustering
model.default <- multinom(num~age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal
                          , data=dt.clusters_train)

summary(model.default)
yhat.default <- predict(model.default, dt.clusters_test, "class")
table(dt.clusters_test$num, yhat.default, deparse.level = 2)
yhat.default2 <- ifelse(yhat.default == 0, 0, 1)

acc.default <- mean(yhat.default == dt.clusters_test$num)
acc.default

cm.default <- conf_mat(table(yhat.default, dt.clusters_test$num, deparse.level = 2))
acc.default_str = sprintf(acc.default, fmt = '%#.4f')

autoplot(cm.default, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.default_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")  

table(ytest2, yhat.default2)
acc.default2 <- mean(yhat.default2 == ytest2)
acc.default2 

cm.default2 <- conf_mat(table(yhat.default2, ytest2 ))
acc.default2_str = sprintf(acc.default2, fmt = '%#.4f')

autoplot(cm.default2, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.default2_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")  

#Kmeans 10
model.kmeans10 <- multinom(num~age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal + kmeans10 
                          , data=dt.clusters_train)

summary(model.kmeans10)
yhat.kmeans10 <- predict(model.kmeans10, dt.clusters_test, "class")
table(dt.clusters_test$num, yhat.kmeans10, deparse.level = 2)
yhat.kmeans10_2 <- ifelse(yhat.kmeans10 == 0, 0, 1)

acc.kmeans10 <- mean(yhat.kmeans10 == dt.clusters_test$num)
acc.kmeans10

cm.kmeans10 <- conf_mat(table(yhat.kmeans10, dt.clusters_test$num ))
acc.kmeans10_str = sprintf(acc.kmeans10, fmt = '%#.4f')

autoplot(cm.kmeans10, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.kmeans10_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")  



table.kmeans10_2 <- table(yhat.kmeans10_2 , ytest2)
table.kmeans10_2
acc.kmeans10_2 <- mean(yhat.kmeans10_2 == ytest2)
acc.kmeans10_2

cm.kmeans10_2 <- conf_mat(table.kmeans10_2)
acc.kmeans10_2_str = sprintf(acc.kmeans10_2, fmt = '%#.4f')

autoplot(cm.kmeans10_2, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.kmeans10_2_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")  

acc.kmeans10_2_str = sprintf(acc.kmeans10_2, fmt = '%#.4f')

table.default2.cm <- conf_mat(table.default2)
autoplot(table.default2.cm, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.kmeans10_2_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")  


#Hierarchical clustering
model.hcluster8 <- multinom(num~age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal + hcluster8 
                           , data=dt.clusters_train)

summary(model.hcluster8)
yhat.hcluster8 <- predict(model.hcluster8, dt.clusters_test, "class")
yhat.hcluster8_2 <- ifelse(yhat.hcluster8 == 0, 0, 1)

table(dt.clusters_test$num, yhat.hcluster8, deparse.level = 2)
acc.hcluster8 <- mean(yhat.hcluster8 == dt.clusters_test$num)
acc.hcluster8

cm.hcluster8 <- conf_mat(table(yhat.hcluster8, dt.clusters_test$num ))
acc.hcluster8_str = sprintf(acc.hcluster8, fmt = '%#.4f')

autoplot(cm.hcluster8, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.hcluster8_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")  

table(ytest2, yhat.hcluster8_2)
acc.hcluster8_2 <- mean(yhat.hcluster8_2 == ytest2)
acc.hcluster8_2

cm.hcluster8_2 <- conf_mat(table(yhat.hcluster8_2, ytest2 ))
acc.hcluster8_2_str = sprintf(acc.hcluster8_2, fmt = '%#.4f')

autoplot(cm.hcluster8_2, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.hcluster8_2_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")  

ytest3 <- dt.clusters_test$num
ytest3[ytest3 == 2] = 1
ytest3[ytest3 == 3] = 2
ytest3[ytest3 == 4] = 2

yhat.hcluster8_3 <- yhat.hcluster8
yhat.hcluster8_3[yhat.hcluster8_3 == 2] = 1
yhat.hcluster8_3[yhat.hcluster8_3 == 3] = 2
yhat.hcluster8_3[yhat.hcluster8_3 == 4] = 2

table(ytest3, yhat.hcluster8_3, deparse.level = 2)
acc.hcluster8_3 <- mean(yhat.hcluster8_3 == ytest3)
acc.hcluster8_3

#Binary Logistic Regression
dt.binary_train <- dt.clusters_train
dt.binary_test <- dt.clusters_test

dt.binary_train$num <- ifelse(dt.binary_train$num == 0, 0, 1)
dt.binary_train$num <- factor(dt.binary_train$num)
dt.binary_test$num <- ifelse(dt.binary_test$num == 0, 0, 1)
dt.binary_test$num <- factor(dt.binary_test$num)

model.binary <- glm(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal , family='binomial', data=dt.binary_train)
summary(model.binary)

yhat.binary <- predict(model.binary, dt.binary_test, 'response')
yhat.binary <- ifelse(yhat.binary > 0.5, 1, 0)
table(dt.binary_test$num, yhat.binary, deparse.level = 2)
acc.binary <- mean(yhat.binary == dt.binary_test$num)
acc.binary

#Remove non-significant variables
model.binary2 <- glm(num ~  cp + trestbps + chol + restecg + thalch + oldpeak + slope , family='binomial', data=dt.binary_train)
summary(model.binary2)

yhat.binary2 <- predict(model.binary2, dt.binary_test, 'response')
yhat.binary2 <- ifelse(yhat.binary2 > 0.5, 1, 0)
table(dt.binary_test$num, yhat.binary, deparse.level = 2)
acc.binary2 <- mean(yhat.binary2 == dt.binary_test$num)
acc.binary2


cm.binary <- conf_mat(table(yhat.binary, dt.binary_test$num ))
acc.binary2_str = sprintf(acc.binary2, fmt = '%#.4f')

autoplot(cm.binary, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.binary2_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")
