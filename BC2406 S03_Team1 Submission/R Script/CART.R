library(rpart)
library(rpart.plot)
library(splitstackshape)
library(data.table)
<<<<<<< HEAD

setwd("D:\Work\Projects\bc2406\Project Files")
=======
library(ggplot2)
library(yardstick)
setwd("C:/Users/wujia/OneDrive/Documents/GitHub/bc2406/Project Files")
>>>>>>> 32f353dca6826d27bb20e65e29bf1be4a52a39a7

set.seed(123)

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


sample <- stratified(dt.clusters, c("num"), 0.8, bothSets = TRUE)
dt.clusters_train <- sample$SAMP1
dt.clusters_test <- sample$SAMP2

summary(dt.clusters_train)

ytest2 <- ifelse(dt.clusters_test$num == 0, 0, 1)


#Without clustering
set.seed(123)
model.default <- rpart(num~age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal
                          , data=dt.clusters_train, method='class', control = list(cp = 0, xval = 10))

rpart.plot(model.default, nn=T)
plotcp(model.default)
model.default$cptable
print(model.default)

model.default <- prune(model.default, cp = 0.0075)

rpart.plot(model.default, nn=T)
plotcp(model.default)
model.default$cptable
print(model.default)

yhat.default <- predict(model.default, dt.clusters_test, type='class')
yhat.default2 <- ifelse(yhat.default == 0, 0, 1)

table(dt.clusters_test$num, yhat.default, deparse.level = 2)
acc.default <- mean(yhat.default == dt.clusters_test$num)
acc.default

cm.default <- conf_mat(table(yhat.default, dt.clusters_test$num ))
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
set.seed(123)
model.kmeans10 <- rpart(num~age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal + kmeans10
                       , data=dt.clusters_train, method='class', control = list(cp = 0, xval = 10))

rpart.plot(model.kmeans10, nn=T)
plotcp(model.kmeans10)
model.kmeans10$cptable
print(model.kmeans10)

model.kmeans10 <- prune(model.kmeans10, cp = 0.0092)

rpart.plot(model.kmeans10, nn=T)
print(model.kmeans10)
plotcp(model.kmeans10)
model.kmeans10$cptable

yhat.kmeans10 <- predict(model.kmeans10, dt.clusters_test, type='class')
yhat.kmeans10_2 <- ifelse(yhat.kmeans10 == 0, 0, 1)

table(dt.clusters_test$num, yhat.kmeans10, deparse.level = 2)
acc.kmeans10 <- mean(yhat.kmeans10 == dt.clusters_test$num)
acc.kmeans10

cm.kmeans10<- conf_mat(table(yhat.kmeans10, dt.clusters_test$num ))
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


table(ytest2, yhat.kmeans10_2)
acc.kmeans10_2 <- mean(yhat.kmeans10_2 == ytest2)
acc.kmeans10_2

cm.kmeans10_2 <- conf_mat(table(yhat.kmeans10_2, ytest2 ))
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


#Hierarchical clustering
set.seed(123)
model.hcluster8 <- rpart(num~age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal + hcluster8
                       , data=dt.clusters_train, method='class', control = list(cp = 0, xval = 10))

rpart.plot(model.hcluster8, nn=T)
plotcp(model.hcluster8)
model.hcluster8$cptable
print(model.hcluster8)

model.hcluster8 <- prune(model.hcluster8, cp = 0.01)

rpart.plot(model.hcluster8, nn=T)
plotcp(model.hcluster8)
model.hcluster8$cptable
print(model.hcluster8)

yhat.hcluster8 <- predict(model.hcluster8, dt.clusters_test, type='class')
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

<<<<<<< HEAD
#binary CART
=======
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

#Binary model
>>>>>>> b58ee34ea578027bbf4a162c3abf4ab64b93e116
set.seed(123)
dt.binary <- dt.clusters

dt.binary$num <- ifelse(dt.binary$num == 0, 0, 1)

sample <- sample(c(TRUE, FALSE), nrow(dt.binary), replace=TRUE, prob=c(0.85, 0.15))
dt.binary_train <- dt.binary[sample, ]
dt.binary_test <- dt.binary[!sample, ]

set.seed(123)
model.binary <- rpart(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + thal + kmeans10
                      , data=dt.binary_train, method='class', control=list(cp=0, xval=10))

rpart.plot(model.binary, nn=T)
plotcp(model.binary)
model.binary$cptable
print(model.binary)

model.binary <- prune(model.binary, cp=0.022)

rpart.plot(model.binary, nn=T)
plotcp(model.binary)
model.binary$cptable
print(model.binary)

yhat.binary <- predict(model.binary, dt.binary_test, type='class')
table(dt.binary_test$num, yhat.binary, deparse.level = 2)
acc.binary <- mean(yhat.binary == dt.binary_test$num)
acc.binary

cm.binary <- conf_mat(table(yhat.binary, dt.binary_test$num ))
acc.binary_str = sprintf(acc.binary, fmt = '%#.4f')

autoplot(cm.binary, type = "heatmap") +
  labs(caption = paste("Accuracy: ",acc.binary_str )) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + 
  ylab("Prediction") + xlab("Reference")  + 
  theme(plot.caption = element_text(size = 15)) +  
  scale_x_discrete(position = "top") + 
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 11)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=0)) +
  ggtitle("Confusion Matrix and Statistics")

