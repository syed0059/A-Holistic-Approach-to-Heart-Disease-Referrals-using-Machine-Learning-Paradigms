library(data.table)
library(ggplot2)
library(creditmodel)
library(ggcorrplot)
library(ggpmisc)

setwd("D:/Work/Projects/bc2406/Project Files")

dt <- fread("heart_disease_uci.csv")
summary(dt)

dt$restecg[dt$restecg == ''] <- NA
dt$slope[dt$slope == ''] <- 
dt$thal[dt$thal == ''] <- NA

#Factorise categorical columns
dt$sex <- factor(dt$sex)
dt$cp <- factor(dt$cp)
dt$fbs <- factor(dt$fbs)
dt$restecg <- factor(dt$restecg)
dt$exang <- factor(dt$exang)
dt$slope <- factor(dt$slope)
dt$ca <- factor(dt$ca)
dt$thal <- factor(dt$thal)
dt$num <- factor(dt$num)


dt <- dt[, -c("id", "dataset")]
summary(dt)

#Single variable charts
ggplot(data=dt, aes(age)) + 
  geom_histogram(binwidth=5, fill='light blue', color='black') +
  theme_light()

ggplot(data=dt, aes(x=sex)) + 
  geom_bar(fill=c('pink', 'light blue'), color='black') +
  theme_light()

ggplot(data=dt, aes(x=cp)) + 
  geom_bar(fill='lightblue', color='black') +
  theme_light()

ggplot(data=dt, aes(x=trestbps)) + 
  geom_density(kernel='gaussian', color='blue') +
  theme_light()

ggplot(data=dt, aes(x=trestbps)) + 
  stat_boxplot(fill='lightblue') +
  theme_light()

ggplot(data=dt, aes(x=chol)) + 
  geom_density(kernel='gaussian', color='blue') +
  theme_light()

ggplot(data=dt, aes(x=chol)) + 
  stat_boxplot(fill='lightblue') +
  theme_light()

ggplot(data=dt[!is.na(dt$fbs)], aes(x=fbs)) + 
  geom_bar(fill='light blue', color='black') +
  xlab('fbs') + 
  theme_light()

ggplot(data=dt[!is.na(dt$restecg)], aes(x=restecg)) + 
  geom_bar(fill='light blue', color='black') +
  xlab('restecg') + 
  theme_light()

ggplot(data=dt, aes(x=thalch)) + 
  geom_density(kernel='gaussian', color='blue') +
  theme_light()

ggplot(data=dt, aes(x=thalch)) + 
  stat_boxplot(fill='lightblue') +
  theme_light()

ggplot(data=dt[!is.na(dt$exang)], aes(x=exang)) + 
  geom_bar(fill='light blue', color='black') +
  theme_light()

ggplot(data=dt, aes(x=oldpeak)) + 
  geom_density(kernel='gaussian', color='blue') +
  theme_light()

ggplot(data=dt, aes(x=oldpeak)) + 
  stat_boxplot(fill='lightblue') +
  theme_light()

ggplot(data=dt[!is.na(dt$slope)], aes(x=slope)) + 
  geom_bar(fill='light blue', color='black') +
  theme_light()

ggplot(data=dt[!is.na(dt$ca)], aes(x=ca)) + 
  geom_bar(fill='light blue', color='black') +
  theme_light()

ggplot(data=dt[!is.na(dt$thal)], aes(x=thal)) + 
  geom_bar(fill='light blue', color='black') +
  theme_light()

ggplot(data=dt, aes(x=num)) + 
  geom_bar(fill='light blue', color='black') +
  theme_light()

#Relation with num
ggplot(data=dt, aes(x=num, y=age)) +
  geom_boxplot(fill='lightblue') + 
  theme_light()

ggplot(data=dt, aes(x=num, fill=sex)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()

ggplot(data=dt, aes(x=num, fill=cp)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()

ggplot(data=dt, aes(x=num, y=trestbps)) +
  geom_boxplot(fill='lightblue') + 
  theme_light()

ggplot(data=dt, aes(x=num, y=chol)) +
  geom_boxplot(fill='lightblue') + 
  theme_light()

ggplot(data=dt[!is.na(dt$fbs)], aes(x=num, fill=fbs)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()

ggplot(data=dt[!is.na(dt$restecg)], aes(x=num, fill=restecg)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()

ggplot(data=dt, aes(x=num, y=thalch)) +
  geom_boxplot(fill=('light blue')) + 
  theme_light()

ggplot(data=dt[!is.na(dt$exang)], aes(x=num, fill=exang)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()

ggplot(data=dt, aes(x=num, y=oldpeak)) +
  geom_boxplot(fill='lightblue') + 
  theme_light()

ggplot(data=dt[!is.na(dt$slope)], aes(x=num, fill=slope)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()

ggplot(data=dt[!is.na(dt$ca)], aes(x=num, fill=ca)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()

ggplot(data=dt[!is.na(dt$thal)], aes(x=num, fill=thal)) +
  geom_bar(stat='count', position='fill') + 
  ylab('proportion') + 
  theme_light()


#Correlation
dt[dt==''] <- NA
dt <- dt[!is.na(dt$exang)]
dt <- dt[!is.na(dt$trestbps)]
dt <- dt[!is.na(dt$restecg)]
dt <- dt[!is.na(dt$oldpeak)]
dt <- dt[!is.na(dt$chol)]

summary(dt)

#Categorical
cat_corr <- char_cor(dat=as.data.frame(dt))

ggcorrplot(cat_corr, hc.order=TRUE, lab=TRUE)

#num vs exang 
tab <- table(dt[, c("num", "exang")])
tab
chi <- chisq.test(tab)
chi

ggplot(data=dt, aes(x=num, fill=exang, na.rm=TRUE)) + 
  geom_bar(stat='count', position='fill') + 
  scale_fill_manual(values=c('indianred', 'light green')) + 
  ylab("Distribution of exang") + 
  theme_light()

#cp vs exang
tab <- table(dt[, c("cp", "exang")])
tab
chi <- chisq.test(tab)
chi

ggplot(data=dt[!is.na(dt$exang)], aes(x=cp, fill=exang, na.rm=TRUE)) + 
  geom_bar(stat='count', position='fill') + 
  scale_fill_manual(values=c('indianred', 'light green')) + 
  ylab("Distribution of exang") + 
  theme_light()

#Continuous 
cont_corr <- cor(dt[, c("age", "trestbps", "chol", "thalch", "oldpeak")])
ggcorrplot(cont_corr, hc.order=TRUE, lab=TRUE)


#age vs thalch
ggplot(data = dt, aes(x = age, y = thalch)) +
  stat_poly_line(se=FALSE) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point() + 
  theme_light()



