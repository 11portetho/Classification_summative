install.packages('rpart.plot')
install.packages('randomForest')
install.packages('gbm')
install.packages('caret')
install.packages('ISLR')
install.packages('skimr')
install.packages('DataExplorer')
install.packages('ggplot2')

library(skimr)
library(DataExplorer)

df <- read.csv("https://www.louisaslett.com/Courses/MISCADA/bank_personal_loan.csv")
skimr::skim(df)
df2 <- df

nr = 0
i = 1
while (i <= 5000-nr) {
  if (df[i,"Experience"] < 0) {
    df<- df[-c(i), ]
    nr = nr + 1
  }
  else{
    i = i + 1
  }
}

table(df["Experience"]<0)

skimr::skim(df)#new mean is 20.3

for (i in 1:5000) {
 if (df2[i,"Experience"]<0) {
   df2[i,"Experience"]=20.3
 } 
}

table(df2["Experience"]<0)
df <- df2


table(df2["CreditCard"]!=1&df2["CreditCard"]!=0)

DataExplorer::plot_bar(df, ncol = 3)

DataExplorer::plot_histogram(df, ncol = 3)

DataExplorer::plot_boxplot(df, by = "Personal.Loan", ncol = 3)

table(df2["Personal.Loan"]==1)

df$Personal.Loan<-as.factor(ifelse(df$Personal.Loan==1,"Yes","No"))

set.seed(1) # set seed for reproducibility
trn_idx = sample(1:nrow(df), 0.5*nrow(df))
trn = df[trn_idx,]
tst_idx = sample(c(1:nrow(df))[-trn_idx],0.25*nrow(df))
tst = df[tst_idx,]
val = df[-cbind(trn_idx,tst_idx),]

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)
library(ISLR)

#Logistic regression
glm = glm(Personal.Loan ~ ., data = trn, family = binomial(link='logit'))

glm_tst_pred = ifelse(predict(glm, tst, "response") > 0.5, "Yes", "No")
table(predicted = glm_tst_pred, actual = tst$Personal.Loan)

#Trees
tree <- rpart(Personal.Loan ~ ., data = trn)
rpart.plot(tree)#Each node box displays the classification, the probability of each class at that node (i.e. the probability of the class conditioned on the node) and the percentage of observations used at that node.

tree_tst_pred = predict(tree, tst, type = "class")
table(predicted = tree_tst_pred, actual = tst$Personal.Loan)

#Boosting
trn_mod = trn
trn_mod$Personal.Loan = as.numeric(ifelse(trn_mod$Personal.Loan == "No", "0", "1"))

set.seed(7)
boost = gbm(Personal.Loan ~ ., data = trn_mod, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
boost

boost_tst_pred = ifelse(predict(boost, tst, n.trees = 5000, "response") > 0.5, "Yes", "No")
table(predicted = boost_tst_pred, actual = tst$Personal.Loan)

#Tuning - Boosting
cv_5 = trainControl(method = "cv", number = 5)
gbm_grid =  expand.grid(interaction.depth = 1:5, n.trees = (1:6) * 500, shrinkage = c(0.001, 0.01, 0.1), n.minobsinnode = 10)
set.seed(1)
gbm_tune = train(Personal.Loan ~ ., data = trn, method = "gbm", trControl = cv_5, verbose = FALSE, tuneGrid = gbm_grid)# This may take a few minutes
plot(gbm_tune)
table(predicted = predict(gbm_tune, tst, n.trees = gbm_tune$bestTune$n.trees), actual = tst$Personal.Loan)

gbm_tune$bestTune

#Final model - Boosting
boost_val_pred = ifelse(predict(boost, val, n.trees = 5000, "response") > 0.5, "Yes", "No")
table(predicted = boost_val_pred, actual = val$Personal.Loan)

True <- factor(c(0, 0, 1, 1))
Pred <- factor(c(0, 1, 0, 1))
Y      <- c(1112, 3, 2, 133)
df <- data.frame(True, Pred, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = True, y = Pred)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "orange") +
  theme_bw() + theme(legend.position = "none")

