heart <- read.csv("heart-disease-dsa.csv")
attach(heart)
dim(heart)

###############Explore the response variable################
table(disease)

prop.table(table(disease))

###############Re-label the categories#####################
heart[blood.disorder == 0, "blood.disorder"] = 2

heart[blood.disorder == 2, "blood.disorder"] = 3

heart[blood.disorder == 3, "blood.disorder"] = 2

heart[chest.pain == 0, "chest.pain"] <- 2

heart[chest.pain == 1, "chest.pain"] <- 2

heart[chest.pain == 2, "chest.pain"] <- 1

heart[chest.pain == 3, "chest.pain"] <- 3

#########The association between the response and each input variable#########
##age##
boxplot(age ~ disease, col = "blue")

##sex##
gender = ifelse(sex == 1, "Male", "Female")
tab_g <- table(disease, gender)
tab_gender <- prop.table(tab_g, margin = 2)
barplot(tab_gender, col = c("green", "purple"), legend.text = c("no", "yes"))
OR_gender =(prod(diag(tab_gender))) / (tab_gender[2,1] * tab_gender[1,2])

#####chest.pain####
tab_ch <- table(disease, chest.pain)
tab_chest <- prop.table(tab_ch, margin = 2)
barplot(tab_chest, col = c("cyan", "red"), legend.text = c("no", "yes"))

#bp
boxplot(bp ~ disease, col = "orange")

#chol
boxplot(chol ~ disease, col = "yellow")

#fbs
FBS = ifelse(fbs == 1, "True", "False")
tab_FBS <- table(disease, FBS)
tab_fbs <- prop.table(tab_FBS, margin = 2)
barplot(tab_fbs, col = c("dodgerblue", "darkorange"), legend.text = c("no", "yes"))
OR_fbs =(prod(diag(tab_FBS))) / (tab_FBS[2,1] * tab_FBS[1,2])

#rest.ecg
tab_r <- table(disease, rest.ecg)
tab_rest <- prop.table(tab_r, margin = 2)
barplot(tab_rest, col = c("blueviolet", "gold"), legend.text = c("no", "yes"))

#heart.rate
boxplot(heart.rate ~ disease, col = "lawngreen")

#angina
tab_ang <- table(disease, angina)
tab_angina <- prop.table(tab_ang, margin = 2)
barplot(tab_angina, col = c("forestgreen", "coral"), legend.text = c("no", "yes"))
OR_angina =(prod(diag(tab_ang))) / (tab_ang[2,1] * tab_ang[1,2])

#st.depression
boxplot(st.depression ~ disease, col = "violet")

#vessels
tab_ves <- table(disease, vessels)
tab_vessels <- prop.table(tab_ves, margin = 2)
barplot(tab_vessels, col = c("royalblue1", "deeppink"), legend.text = c("no", "yes"))

#blood.disorder
tab_bl <- table(disease, blood.disorder)
tab_blood <- prop.table(tab_bl, margin = 2)
barplot(tab_blood, col = c("limegreen", "orangered"), legend.text = c("no", "yes"))

#Remove unnecessary data
heart_new = heart[-c(4, 5, 6)]


####KNN####
library(class)
set.seed(1101)

heart_KNN = scale(heart_new[, -10])
X = heart_KNN
Y = heart_new[, 10]

ave.tpr_KNN = numeric(0)
n_folds=5

folds <- sample(rep(1:n_folds, length.out = dim(heart_new)[1]))  

table(folds)
K = c(1,3,5,7,9,11,13,15,17)
tpr_KNN = numeric(0)
for (i in K) {
  for (j in 1:n_folds) {
    test <- which(folds == j)
    pred_KNN <- knn(train=X[-test, ], test=X[test, ], cl=Y[-test ], k=i, prob = TRUE)
    confusion.matrix_KNN = table(Y[test],pred_KNN)
    tpr_KNN= append(tpr_KNN,confusion.matrix_KNN[2,2] / sum(confusion.matrix_KNN[2,]))
  }
  ave.tpr_KNN = append(ave.tpr_KNN, mean(tpr_KNN))
}
ave.tpr_KNN
cbind(K, ave.tpr_KNN)

##########KNN with best k = 3 #########
pred_KNN_class <- knn(train= X, test= X, cl= Y, k=3, prob = TRUE)
pred_KNN_prob <- attr(pred_KNN_class, "prob")
pred_KNN_numeric <- as.numeric(pred_KNN_class) - 1 
prob_KNN_adjusted <- ifelse(pred_KNN_numeric == 1, pred_KNN_prob, 1 - pred_KNN_prob)

pred = prediction(prob_KNN_adjusted , disease ) 
roc = performance(pred , "tpr", "fpr")
auc = performance(pred , measure ="auc")
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))

confusion.matrix_KNN = table(disease, pred_KNN_class)
tpr_KNN = confusion.matrix_KNN[2,2] / sum(confusion.matrix_KNN[2,])
precision_KNN = confusion.matrix_KNN[2,2] / sum(confusion.matrix_KNN[,2])

##########Decision Tree###########

heart_factor = heart_new[rep(row.names(heart_new), times = 1),]
heart_factor$disease = ifelse(heart_factor$disease == "yes",  1, 0)

heart_factor$disease = as.factor(heart_factor$disease)
heart_factor$sex = as.factor(heart_factor$sex)
heart_factor$chest.pain = as.factor(heart_factor$chest.pain)
heart_factor$rest.ecg = as.factor(heart_factor$rest.ecg)
heart_factor$angina = as.factor(heart_factor$angina)
heart_factor$blood.disorder = as.factor(heart_factor$blood.disorder)
heart_factor$vessels = as.factor(heart_factor$vessels)

library("rpart")
library("rpart.plot")

ave.tpr_DT = numeric(0)
tpr_DT = numeric(0)

for (i in 2:10) {
  for (j in 1:n_folds) {
    test <- which(folds == j)
    fit <- rpart(disease ~ ., method="class", data=heart_factor[-test,],
                 control=rpart.control(maxdepth = i),
                 parms=list(split='information'))
    rpart.plot(fit, type=4, extra=2)
    pred_DT = predict(fit, newdata = heart_factor[test,], type = "class")
    confusion.matrix_DT = table(heart_factor[test,]$disease, pred_DT)
    tpr_DT= append(tpr_DT,confusion.matrix_DT[2,2] / sum(confusion.matrix_DT[2,]))
  }
  ave.tpr_DT = append(ave.tpr_DT, mean(tpr_DT))
}
ave.tpr_DT
cbind(c(2:10), ave.tpr_DT)

########Decision Tree with best maxdpeth = 10 with average TPR = 0.7237234#########
fit_best <- rpart(disease ~ ., method="class", data=heart_factor,
                  control=rpart.control(maxdepth = 10),
                  parms=list(split='information'))
rpart.plot(fit_best, type=4, extra=2)
pred_DT_class = predict(fit_best, newdata = heart_factor, type = "class")
pred_DT_prob = predict(fit_best, newdata = heart_factor, type = "prob")[, 2]

pred = prediction(pred_DT_prob , disease )
roc = performance(pred , "tpr", "fpr")
auc = performance(pred , measure ="auc")
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))


confusion.matrix_DT = table(heart_new$disease, pred_DT_class)
tpr_DT = confusion.matrix_DT[2,2] / sum(confusion.matrix_DT[2,])
precision_DT = confusion.matrix_DT[2,2] / sum(confusion.matrix_DT[,2])






#Logistic Regression

#Using all variables
M1<- glm(disease ~., data = heart_factor, family = binomial)
summary(M1)
library(ROCR)
prob_M1 = predict(M1, type ="response")
pred_M1 = prediction(prob_M1 , disease ) 
roc_M1 = performance(pred_M1 , "tpr", "fpr")
auc_M1 = performance(pred_M1 , measure ="auc")
auc_M1@y.values[[1]]
plot(roc_M1 , col = "red", main = paste(" Area under the curve :", round(auc_M1@y.values[[1]] ,4)))

alpha_M1 <- round (as.numeric(unlist(roc_M1@alpha.values)) ,4)
length(alpha_M1) 
fpr_M1 <- round(as.numeric(unlist(roc_M1@x.values)) ,4)
tpr_M1 <- round(as.numeric(unlist(roc_M1@y.values)) ,4)
cbind(alpha_M1, tpr_M1,fpr_M1, tpr_M1-fpr_M1)
which(tpr_M1-fpr_M1 == max(tpr_M1-fpr_M1))
#threshold = 0.5142, TPR = 0.8478

#Remove the age variable
M2<- glm(disease ~ sex + chest.pain + rest.ecg + heart.rate + angina 
         + st.depression + vessels + blood.disorder, 
         data = heart_factor, family = binomial)
summary(M2)
prob_M2 = predict(M2, type ="response")
pred_M2 = prediction(prob_M2 , disease ) 
roc_M2 = performance(pred_M2 , "tpr", "fpr")
auc_M2 = performance(pred_M2 , measure ="auc")
auc_M2@y.values[[1]]
plot(roc_M2 , col = "red", main = paste(" Area under the curve :", round(auc_M2@y.values[[1]] ,4)))

alpha_M2 <- round (as.numeric(unlist(roc_M2@alpha.values)) ,4)
length(alpha_M2) 
fpr_M2 <- round(as.numeric(unlist(roc_M2@x.values)) ,4)
tpr_M2 <- round(as.numeric(unlist(roc_M2@y.values)) ,4)
value_M2 = cbind(alpha_M2, tpr_M2,fpr_M2, tpr_M2-fpr_M2)
which(tpr_M2-fpr_M2 == max(tpr_M2-fpr_M2))
#threshold = 0.4686, TPR =  0.8696

#Remove the age and rest.ecg variable
M3<- glm(disease ~ sex + chest.pain + heart.rate + angina 
         + st.depression + vessels + blood.disorder, 
         data = heart_factor, family = binomial)
summary(M3)
prob_M3 = predict(M3, type ="response")
pred_M3 = prediction(prob_M3 , disease ) 
roc_M3 = performance(pred_M3 , "tpr", "fpr")
auc_M3 = performance(pred_M3 , measure ="auc")
auc_M3@y.values[[1]]
plot(roc_M3 , col = "red", main = paste(" Area under the curve :", round(auc_M3@y.values[[1]] ,4)))

alpha_M3 <- round (as.numeric(unlist(roc_M3@alpha.values)) ,4)
length(alpha_M3) 
fpr_M3 <- round(as.numeric(unlist(roc_M3@x.values)) ,4)
tpr_M3 <- round(as.numeric(unlist(roc_M3@y.values)) ,4)
cbind(alpha_M3, tpr_M3,fpr_M3, tpr_M3-fpr_M3)
which(tpr_M3-fpr_M3 == max(tpr_M3-fpr_M3))
#threshold = 0.4291, TPR = 0.8551

#From the above, we conclude M2 is the best LR model



probability_M2 = ifelse(prob_M2 >= value_M2[140,1], "yes", "no")
confusion.matrix_LR = table(disease, probability_M2)
tpr_LR = confusion.matrix_LR[2,2] / sum(confusion.matrix_LR[2,])
precision_LR = confusion.matrix_LR[2,2] / sum(confusion.matrix_LR[,2])
