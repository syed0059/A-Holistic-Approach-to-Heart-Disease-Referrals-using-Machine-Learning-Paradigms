library(data.table)
library(caret)


setwd("D:/Work/Projects/bc2406/Project Files")

dt <- fread("heart_disease_uci.csv")

dt <- dt[,-c("id", "ca", "dataset")] #drop ca, dataset

dt[dt==''] <- NA

dt$sex <- factor(dt$sex)
dt$cp <- factor(dt$cp)
dt$fbs <- factor(dt$fbs)
dt$restecg <- factor(dt$restecg)
dt$exang <- factor(dt$exang)
dt$slope <- factor(dt$slope)
dt$thal <- factor(dt$thal)
dt$num <- factor(dt$num)

summary(dt)

#Handle missing data:
#trestbps: 59   chol: 30  fbs: 90   restecg: 2  thalch: 55    exang: 55   oldpeak: 62   slope: 309    thal: 486

dt <- dt[!is.na(dt$exang)]
dt <- dt[!is.na(dt$trestbps)]
dt <- dt[!is.na(dt$restecg)]
dt <- dt[!is.na(dt$oldpeak)]
dt <- dt[!is.na(dt$chol)]

fwrite(dt, "heart_disease_uci_cleaned.csv")

summary(dt)

dummy <- dummyVars(" ~ .", data=dt)
dt_norm <- predict(dummy, newdata=dt, na.action = na.pass)
process <- preProcess(dt_norm, method=c("range"))
dt_norm <- predict(process, dt_norm)
dt_norm[is.na(dt_norm)] <- 0

summary(dt_norm)

fwrite(dt_norm, "heart_disease_uci_norm_kmeans.csv")
