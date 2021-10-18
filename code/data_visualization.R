library(ggplot2)
library(plotly)
library(car)
library(caret)

## read the data
setwd("D:/courseware/wisconsin/stat 628/assignment 2/data")
data <- read.csv("./BodyFat.csv",encoding = 'UTF-8',head = T)

# divide the age into different groups and draw box plot
data$age_group <- cut(data$AGE,breaks = c(20,44,60,90),labels = c("young", "middle-aged", "old"))
ggplot(data = data) +
  geom_boxplot(aes(x = as.factor(age_group), y = BODYFAT,fill = as.factor(age_group)),alpha = 0.9)+labs(x = "age", y = "body fat")+
  scale_fill_manual(name = "age group",values=wes_palette(n=3, name="GrandBudapest2"))

## box plot
ggplot(data = data) +
  geom_boxplot(aes(y = WEIGHT),fill = "#F1BB7B",outlier.colour="black", outlier.shape=1,alpha = 0.8)+
  labs(y = "Weight")+ geom_text(x=0.05, y=261, label="39")

ggplot(data = data) +
  geom_boxplot(aes(y = BODYFAT),fill = "#706BA9",outlier.colour="black", outlier.shape=1,alpha = 0.8)+
  labs(y = "Bodyfat")+ geom_text(x=0.05, y=45, label="216")

ggplot(data = data) +
  geom_boxplot(aes(y = DENSITY),fill = "#706BA9",outlier.colour="black", outlier.shape=1,alpha = 0.8)+
  labs(y = "Density")+ geom_text(x=0.06, y=0.995, label="216")


## plotly
plot_ly(data, x = ~BODYFAT, y = ~ADIPOSITY, z = ~AGE) %>%
  add_markers(color = ~CHEST, colors = "YlGnBu")

## dot plot
ggplot(data = data,aes(x =ADIPOSITY ,y = BODYFAT))+
  geom_point(aes(colour = AGE, size=CHEST))+stat_smooth(linetype=1)+scale_color_gradient(low="lightblue", high="darkblue")


## linear regression
lr_final = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST, data=train)
summary(lr_final)
predictions_train = predict(lr_final, newdata = train)
sqrt(mean((train$BODYFAT - predictions_train)^2))
predictions_test = predict(lr_final, newdata = test)
sqrt(mean((test$BODYFAT - predictions_test)^2))
