# Run this file second.
library(caret)
library(randomForest)
library(ggrepel)

set.seed(03012024)

trainIndex <- createDataPartition(c3$`Parenting Gap`, p = 0.75, list = FALSE)
trainData <- c3[trainIndex, ]
testData <- c3[-trainIndex, ]

baseline.acc <- max(table(trainData$`Parenting Gap`))/dim(trainData)[1]

# Train the Random Forest model.
train2 <- trainData
names(train2) <- names(testData) <- c("Cohort", "Education", "Employment", "Census Region",
                   "Miles from LGBT Health", "Happy", "Good: Minorities", "Good: GLB",
                   "Good: Transgender", "Good: Immigrants", 
                   "Left out", "Isolated", "General Health", "30 Days: Physical Health",
                   "30 Days: Mental Health", "Out: Family", "Out: Straight Friends",
                   "Out: Co-workers", "Out: Healthcare", "Community Connectedness",
                   "Everyday discrimination", "Felt Stigma", "Internalized Homophobia", "Social Well-being",
                   "Social Support - Family", "Social Support - Friends", "Social Support - S.O.",
                   "Income", "Race", "Sexual ID", "ParentingGap", "Sexual Identity is")

rfModel <- train(ParentingGap ~ ., data = train2,
                 method = "rf", 
                 importance=T)

rfModel$finalModel
var.imp.rf <- varImp(rfModel, scale=F)
plot(var.imp.rf, top = 20)


# Accuracy on Training: 
trainacc <- (38+42)/(dim(trainData)[1])*100

((trainacc/100 - baseline.acc))/baseline.acc

# Accuracy on Test:
table(testData$ParentingGap, predict(object = rfModel, newdata = testData))
testacc <- (15+13)/(dim(testData)[1])*100 

(testacc/100 - baseline.acc)/baseline.acc

# Gathering top 10 variables for decrease in accuracy and gini
ourimp <- as.data.frame(importance(rfModel$finalModel)[,3:4])
imp.acc.10 <- ourimp |> arrange(desc(MeanDecreaseAccuracy)) |> slice_head(n = 10)
imp.gini.10 <- ourimp |> arrange(desc(MeanDecreaseGini)) |> slice_head(n = 10)
all.imps <- unique(c(rownames(imp.acc.10), rownames(imp.gini.10)))

all.15.imp.vars <- ourimp[which(rownames(ourimp) %in% all.imps), ] |> arrange(desc(MeanDecreaseAccuracy)) 
