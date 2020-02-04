

diabetes = read.csv(file="Diabetes.csv")
summary(diabetes)
head(diabetes)
#DATA VISUALIZATION
library("RColorBrewer")
brewer.pal(n = 2,name = "RdBu")
barplot(table(diabetes$Outcome), main="Outcome",col=cm.colors(2), xlab="Diabetes", ylab="Número de casos",las=2)

barplot(table(diabetes$Pregnancies,diabetes$Outcome), main="Relação Pregnancies vs Outcome",col=brewer.pal(n = 2, name = "RdBu"), xlab="Outcome", ylab="Nº de Pregnancies",las=2)

boxplot(Age~Outcome,data=diabetes, main="Age",col=brewer.pal(n = 2, name = "RdBu"), horizontal=TRUE)

boxplot(diabetes,las=2,col=brewer.pal(n = 9, name = "RdBu"), ylim = c(0, 300),main="Overview diabetes")
boxplot(Insulin~Outcome,data=diabetes,main="Insulin and Outcome",col=brewer.pal(n = 2, name = "RdBu"))
boxplot(BMI~Outcome,data=diabetes,main="BMI and Outcome",col=brewer.pal(n = 2, name = "RdBu"),horizontal=TRUE,ylim=c(15,60))
boxplot(Pregnancies~Outcome,data=diabetes,main="Pregnacies and Outcome",col=brewer.pal(n = 2, name = "RdBu"),horizontal=TRUE)

dim(diabetes)
library(ggplot2)
library(wesanderson)

plot(Age~Glucose,data=diabetes, col=ifelse(diabetes$Outcome==1,"red","blue"), pch=20,main="Relação Glucose e Age",xlabel="Glucose",ylabel="Age",ylim = c(20, 70),xlim=c(50,200))
legend("topleft", inset=.02, title="Outcome",c("1","0"), fill=c("red","blue"), horiz=TRUE, cex=0.8)

plot(BloodPressure~Glucose,data=diabetes, col=ifelse(diabetes$Outcome==1,"red","blue"), pch=20,main="Relação Glucose e BloodPressure",xlabel="Glucose",ylabel="BloodPressure",ylim = c(20, 120),xlim=c(50,200))
legend("topleft", inset=.02, title="Outcome",c("1","0"), fill=c("red","blue"), horiz=TRUE, cex=0.8)

plot(BMI~Glucose,data=diabetes, col=ifelse(diabetes$Outcome==1,"red","blue"), pch=20,main="Relação Glucose e BMI",xlabel="Glucose",ylabel="BMI",ylim = c(15, 70),xlim=c(35,200))
legend("topleft", inset=.02, title="Outcome",c("1","0"), fill=c("red","blue"), horiz=TRUE, cex=0.8)

#correlation matrix
library("Hmisc")
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(diabetes,type="full")

#MISSING VALUES
diabetes_Insulin <- diabetes[!(diabetes$Insulin == 0),]
boxplot(Insulin~Outcome,data=diabetes,main="Insulin and Outcome")

#SPLIT DOS DADOS
library(caret)
training.samples <-createDataPartition(diabetes$Outcome,p = 0.8, list = FALSE)
train.data  <- diabetes[training.samples, ]
test.data <- diabetes[-training.samples, ]
summary(train.data)

#Model com todas variáveis
model = glm(Outcome ~. , data = train.data,family="binomial")
summary(model)

#PREDICTION
pred=predict(model,newdata = test.data,type="response")
predicted.classes <- ifelse(pred> 0.5, "1", "0")
#ACCURACY
mean(predicted.classes == test.data$Outcome)


#Model tratado
model.2 = glm(Outcome ~Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction , data = train.data,family="binomial")
summary(model.2)

#PREDICTION
pred2=predict(model.2,newdata = test.data,type="response")
predicted.classes2 <- ifelse(pred2> 0.5, "1", "0")

#ACCURACY
mean(predicted.classes2 == test.data$Outcome)

cm=confusionMatrix(table(predict=predicted.classes2, real=test.data$Outcome))
fourfoldplot(cm$table)
