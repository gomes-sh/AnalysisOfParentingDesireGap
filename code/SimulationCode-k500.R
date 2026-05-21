# One big file

# Import the two data files
 
# Libraries ####
library(tidyverse)
library(tidymodels)
library(mice)
library(forcats)
library(ggplot2)
library(naniar)
library(visdat)
library(caret)
library(randomForest)
library(ggrepel)
library(cluster)
library(clValid)
library(fpc)
library(gtsummary)
library(flextable)

# SET SEED
set.seed(03012024)

# DATA CLEANING AND PROCESSING ####

d.w1 <- firstdata

# Setting up variable to identify sexual orientation combined with gender
newvar <- rep(NA, dim(d.w1)[1])
newvar <- ifelse(d.w1$W1SEX == 1 & d.w1$W1Q29 == 2, "Lesbian woman", newvar)
newvar <- ifelse(d.w1$W1SEX == 1 & d.w1$W1Q29 == 6, "Lesbian woman", newvar)
newvar <- ifelse(d.w1$W1SEX == 1 & d.w1$W1Q29 == 4, "Bisexual woman", newvar)
newvar <- ifelse(d.w1$W1SEX == 1 & d.w1$W1Q29 == 5, "Bisexual woman", newvar)
newvar <- ifelse(d.w1$W1SEX == 1 & d.w1$W1Q29 == 7, "Bisexual woman", newvar)
newvar <- ifelse(d.w1$W1SEX == 2 & d.w1$W1Q29 == 3, "Gay man", newvar)
newvar <- ifelse(d.w1$W1SEX == 2 & d.w1$W1Q29 == 4, "Bisexual man", newvar)
newvar <- ifelse(d.w1$W1SEX == 2 & d.w1$W1Q29 == 5, "Bisexual man", newvar)
newvar <- ifelse(d.w1$W1SEX == 2 & d.w1$W1Q29 == 7, "Bisexual man", newvar)
d.w1$SexualID <- newvar

d1 <- subset(d.w1, 
             select = c("STUDYID", "W1Q169", "W1Q172", "W1RACE", "SexualID"))
d <- Data_37166_0003

# Pulling out variables of interest for future use.
vars.of.interest <- which(names(d) %in% c("STUDYID", "W2COHORT", "GEDUC1", "GEMPLOYMENT2010", "GCENREG", 
                                          "GMILESAWAY", "W2Q03", "W2Q03", "W2Q06", "W2Q06", "W2Q19A", "W2Q19B", "W2Q19C", "W2Q19D", 
                                          "W2Q03", "W2Q08", "W2Q09", "W2Q10", "W2Q11", "W2Q12", "W2Q13", "W2Q14", "W2Q15", "W2Q16", "W2Q17", 
                                          "W2Q24", "W2Q25", "W2Q30", "W2Q31", "W2Q32", "W2Q33", "W2Q38", "W2Q42", "W2Q43B",
                                          "W2Q47", "W2Q48", "W2Q49", "W2Q50", "W2Q51", "W2Q52", "W2Q54", "W2Q56", "W2Q57", 
                                          "W2Q77", "W2Q78", "W2Q79", "W2Q106A", "W2Q106B", "W2Q106D", "W2Q106C", "W2Q108", 
                                          "W2Q109", "W2Q110", "W2Q149", "W2Q150", "W1Q169", "W2CONNECTEDNESS_I", "W2EVERYDAY_I", 
                                          "W2FELTSTIGMA_I", "W2INTERNALIZED_I", "W2SOCIALWB_I", "W2SOCSUPPORT_FAM_I", "W2SOCSUPPORT_FR_I", 
                                          "W2SOCSUPPORT_SO_I"))

d2 <- subset(x = d, select = vars.of.interest)

# Merging datasets.
d.fin <- merge(d2, d1, by="STUDYID")
d2 <- d.fin

# Remove records with planned missing in target variable
d2 <- subset(x = d2, subset = W2Q149 != 7)
# Remove records of those who already have children
d2 <- subset(x = d2, subset = W1Q169 == 2)
d2 <- d2[ , -c(which(names(d2) %in% c("W1Q169")))]
# Exclude Cohort 3
d2 <- subset(x = d2, subset = W2COHORT != 3)
d2 <- d2[ , -c(which(names(d2) %in% c("STUDYID")))]

# Constructing target variable by subtracting desire from likelihood
d2$ParentingGap <- as.factor(d2$W2Q150 - d2$W2Q149)
levels(d2$ParentingGap) <- c("Much higher desire", "Somewhat higher desire", "Slightly higher desire", "Equal", 
                             "Slightly higher likelihood", "Somewhat higher likelihood", "Much higher likelihood")
d2 <- d2 |> subset(select=-c(W2Q150, W2Q149))


# VARIABLE REMOVAL
# Due to either irrelevancy or high missing data.

# Removing connectedness and other remaining variables that are no longer necessary.
# Connectedness: W2Q30 - W2Q36. 
# Feltstigma: W2Q108 - W2Q110 
# SocialWB: W2Q04 - W2Q18

remove.these <- c("W2Q30", "W2Q31", "W2Q32", "W2Q33", "W2Q34", "W2Q35", "W2Q36")
remove.these <- c(remove.these, c("W2Q108", "W2Q109", "W2Q110"))
remove.these <- c(remove.these, c("W2Q04", "W2Q05", "W2Q06", "W2Q07", "W2Q08", "W2Q09", "W2Q10", "W2Q11", "W2Q12", "W2Q13", "W2Q14", "W2Q15", "W2Q16", "W2Q17", "W2Q18"))

d2 <- d2[, -(which(names(d2) %in% remove.these))]

# Large missingness: W2Q38, 42, 43B, 47, 48, 49, 50, 51, 52, 54.
# All these pertain to "current partner" and will be removed for the combined-relationship-status analysis
which.pertain.to.relationships <- which(names(d2) %in% c("W2Q38", "W2Q42", "W2Q43B", "W2Q47", "W2Q48", "W2Q49", "W2Q50", "W2Q51", "W2Q52", "W2Q54"))

d2 <- d2[,-which.pertain.to.relationships]


# VARIABLE REFORMATTING. SETTING TO FACTORS, COLLAPSING, ECT.

# Central vs Insig identity
c2 <- d2
c2$CentralInsig <- rep("InsigYes", dim(c2)[1]) # 24 4-6, 25 1-3
c2$CentralInsig <- ifelse(c2$W2Q24 %in% c(1:3) & c2$W2Q25  %in% c(4:6), "CentralYes", c2$CentralInsig)
c2$CentralInsig <- ifelse(c2$W2Q24 %in% c(4:6) & c2$W2Q25 %in% c(4:6), "Both", c2$CentralInsig)
c2$CentralInsig <- ifelse(c2$W2Q24 %in% c(1:3) & c2$W2Q25 %in% c(1:3), "Neither", c2$CentralInsig)
d2 <- c2[,!(names(c2) %in% c("W2Q24", "W2Q25"))]


# Setting non-numeric variables to factors.

is.numeric <- which(names(d2) %in% c("GMILESAWAY", "W2Q38", "W2Q78", "W2Q79", "W2CONNECTEDNESS_I",
                                     "W2EVERYDAY_I", "W2FELTSTIGMA_I","W2INTERNALIZED_I",
                                     "W2SOCIALWB_I", "W2SOCSUPPORT_FAM_I", "W2SOCSUPPORT_FR_I",
                                     "W2SOCSUPPORT_SO_I"))

yes.numeric <- names(d2)[is.numeric]
is.a.factor <- which(!(names(d2) %in% yes.numeric))

for(i in c(is.a.factor)){ # These are all ordered factors!
  d2[,i] <- as.factor(d2[,i])
}
d3 <- d2

# Collapsing factor values according to similar levels codebook.
d3$GEMPLOYMENT2010 <- fct_collapse(d3$GEMPLOYMENT2010,
                                   "1" = c(1, 2),
                                   "2" = c(3, 5),
                                   "3" = c(4, 6))

d3$W2Q77 <- fct_collapse(d3$W2Q77,
                         "1" = c(1,2),
                         "2" = 3,
                         "3" = 4, 
                         "4" = 5)

d3$W2Q106A <- fct_collapse(d3$W2Q106A,
                           "1" = 1,
                           "2" = 2,
                           "3" = c(3,4))

d3$W2Q106B <- fct_collapse(d3$W2Q106B,
                           "1" = 1,
                           "2" = 2,
                           "3" = c(3,4))

d3$W2Q106C <- fct_collapse(d3$W2Q106C,
                           "1" = 1,
                           "2" = 2,
                           "3" = c(3,4))

d3$W2Q106D <- fct_collapse(d3$W2Q106D,
                           "1" = 1,
                           "2" = 2,
                           "3" = c(3,4))

d3$W1Q172 <- fct_collapse(d3$W1Q172,
                          "1" = c(1:4),
                          "2" = c(5:7),
                          "3" = 8,
                          "4" = c(9,10),
                          "5" = c(11, 12))

d3$W1RACE <- fct_collapse(d3$W1RACE,
                          "6" = 6,
                          "2" = 2,
                          "3" = 3,
                          other_level = "7")


# Remove records with ParentingGap == "Equal"
d3 <- subset(x = d3, subset = ParentingGap != "Equal")

# collapse 'Much' and 'Somewhat' ParentingGap levels
d2 <- d3
d2$ParentingGap <- fct_collapse(d2$ParentingGap, 
                                "Higher Desire" = c("Much higher desire", "Somewhat higher desire", "Slightly higher desire"),
                                "Higher Likelihood" = c("Somewhat higher likelihood", "Much higher likelihood", "Slightly higher likelihood"))

d2$ParentingGap <- fct_drop(d2$ParentingGap)


# DEALING WITH MISSING VALUES 

# Code missing values
planned.missing.7 <- c("W2Q42", "W2Q43B", "W2Q48", "W2Q49", "W2Q50",
                       "W2Q51", "W2Q52", "W2Q54")
planned.missing.97 <- c("W2Q38", "W2Q47")

d2.2 <- d2|> mutate(across(which(names(d2) %in% planned.missing.7),
                           ~ ifelse(.x == 7, NA, .x)) )
d2.2 <- d2.2|> mutate(across(which(names(d2) %in% planned.missing.97),
                             ~ ifelse(.x == 97, NA, .x)) )

c3 <- d2.2

mis.count <- c3 |> is.na() |>
  apply(2, sum) 
mis.vars <- which(names(c2) %in% names(which(mis.count > 0)))
# vis_miss(c3[,mis.vars], cluster = T, sort_miss = T, show_perc = T,
#          show_perc_col = F)

# Using multiple imputation to fill in missing data within dataset.
m01 <- mice(data = c3, m = 1, method = "pmm")
c3 <- complete(m01)

names(c3) <- c("Cohort", "Education", "Employment", "Census Region",
               "Miles from LGBT Health", "Happy", "Good: Minorities", "Good: GLB",
               "Good: Transgender", "Good: Immigrants", 
               "Left out", "Isolated", "General Health", "30 Days: Physical Health",
               "30 Days: Mental Health", "Out: Family", "Out: Straight Friends",
               "Out: Co-workers", "Out: Healthcare", "Community Connectedness",
               "Everyday discrimination", "Felt Stigma", "Internalized Homophobia", "Social Well-being",
               "Social Support - Family", "Social Support - Friends", "Social Support - S.O.",
               "Income", "Race", "Sexual ID", "Parenting Gap", "Sexual Identity is")

c3.num <- c3[,is.numeric]
c3.cat <- c3[,-is.numeric]
c3.num2 <- c3[, c(is.numeric,which(names(c3) == "Parenting Gap"))]

savefortables <- c3

# names(c3[,-is.numeric])

# Run data summary tables now ####

c3[ , is.numeric] <- scale(c3[ , is.numeric])

trainIndex <- createDataPartition(c3$`Parenting Gap`, p = 0.75, list = FALSE)
trainData <- c3[trainIndex, ]
testData <- c3[-trainIndex, ]

# Save train and test data sets
#write.csv(x = trainData, file = "SET LOCATION/trainData_2.csv")
#write.csv(x = testData, file = "SET LOCATION/testData_2.csv")

baseline.acc <- max(table(trainData$`Parenting Gap`))/dim(trainData)[1]

# START THE SIMULATION ####

# Set simulation repetitions
reps <- 500
trainAcc.sim <- testAcc.sim <- topVars <- list()
Sys.time()
for(i in 1:reps){
  print(i)
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
  
  # Model performance
  # rfModel$finalModel
  
  trainAcc.sim[[i]] <- (rfModel$finalModel$confusion[1,1] + 
                 rfModel$finalModel$confusion[2,2])/dim(trainData)[1]
  
  # var.imp.rf <- varImp(rfModel, scale=F)
  
  # Accuracy on Test:
  testAcc.table <- table(testData$ParentingGap, predict(object = rfModel, newdata = testData))
  testAcc.sim[[i]] <- (testAcc.table[1,1]+testAcc.table[2,2])/(dim(testData)[1])
  
  # Gathering top 10 variables for decrease in accuracy and gini
  ourimp <- as.data.frame(importance(rfModel$finalModel)[,3:4])
  imp.acc.10 <- ourimp |> arrange(desc(MeanDecreaseAccuracy)) |> slice_head(n = 10)
  imp.gini.10 <- ourimp |> arrange(desc(MeanDecreaseGini)) |> slice_head(n = 10)
  all.imps <- unique(c(rownames(imp.acc.10), rownames(imp.gini.10)))
  topVars[[i]] <- ourimp[which(rownames(ourimp) %in% all.imps), ] |> arrange(desc(MeanDecreaseAccuracy)) 
  
}
Sys.time()

### END SIMULATION ####

#save(trainAcc.sim, testAcc.sim, topVars, file = "INSERT LOCATION/SimK500_2.RData")
# load(file = "INSERT LOCATION/SimK500.RData")

mean(unlist(trainAcc.sim))
mean(unlist(testAcc.sim))
TopVars.sim <- vector()
for(i in 1:reps){
  TopVars.sim <- c(TopVars.sim, rownames(topVars[[i]]))
}

TopVars.sim.df <- as.data.frame(TopVars.sim)
table(TopVars.sim)
head(TopVars.sim.df)
TopVars.sim.freq <- TopVars.sim.df |>
  group_by(TopVars.sim) |>
  mutate(pct = n()/reps) |>
  unique() 
head(TopVars.sim.freq)
TopVars.sim.freq <- TopVars.sim.freq |> 
  mutate(sd = sqrt((pct*(1-pct))/reps)) |>
  mutate(Lower = pct - 2*sd, Upper = pct + 2*sd) |>
  arrange(desc(pct))
TopVars.sim.freq[1:10,]

TopVars.sim.freq$TopVars.sim <- factor(TopVars.sim.freq$TopVars.sim, 
                                          levels = unique(TopVars.sim.freq$TopVars.sim))
levels(TopVars.sim.freq$TopVars.sim)

ggplot(data = TopVars.sim.freq, 
       aes(x=TopVars.sim,
           y = pct)) +
  geom_bar(stat="identity", 
            fill="skyblue", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_errorbar( aes(x=TopVars.sim, ymin=Lower, ymax=Upper), 
                 width=0.4, colour="orange", alpha=0.9, size=1.1) + 
  xlab("Variable") + ylab("Percent Appearance")

# Table for overall mean dec in accuracy, gini

final.table.results <- data.frame(Variable = unique(TopVars.sim.freq$TopVars.sim),
                                  MeanAcc = NA, SdAcc = NA, 
                                  MeanGini = NA, SdGini = NA)

for(i in 1:length(unique(TopVars.sim.freq$TopVars.sim))){ # For each variable
  accs <- ginis <- rep(NA, reps)
  for(j in 1:reps){ # For each list item
    if(unique(TopVars.sim.freq$TopVars.sim)[i] %in% rownames(topVars[[j]])){
      accs[j] <- topVars[[j]][ which(unique(TopVars.sim.freq$TopVars.sim)[i] == rownames(topVars[[j]])), 1] # Save the metric
      ginis[j] <- topVars[[j]][ which(unique(TopVars.sim.freq$TopVars.sim)[i] == rownames(topVars[[j]])), 2] # Save the metric
    } # If the variable is in the top most imp.
  }
  final.table.results$MeanAcc[i] <- mean(na.omit(accs))
  final.table.results$SdAcc[i] <- sd(na.omit(accs))
  final.table.results$MeanGini[i] <- mean(na.omit(ginis))
  final.table.results$SdGini[i] <- sd(na.omit(ginis))
}

final.table.results |> arrange(desc(MeanAcc))
names(TopVars.sim.freq)[1] <- "Variable"
both.dfs <- merge(TopVars.sim.freq, final.table.results, by = "Variable")
dim(both.dfs)

both.dfs <- both.dfs |> mutate(WtMeanAcc = MeanAcc * pct,
                   WtMeanGini = MeanGini * pct)
both.dfs <- both.dfs |> select(Variable, pct, MeanAcc, SdAcc, WtMeanAcc, MeanGini, SdGini, WtMeanGini) |>
  arrange(desc(WtMeanAcc))

head(both.dfs)
both.dfs[,2:8] <- round(both.dfs[,2:8], 2)

both.dfs$Variable <- factor(both.dfs$Variable, 
                            levels = unique(both.dfs$Variable))

ggplot(data = both.dfs, aes(x = Variable, y = WtMeanAcc)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ylab("Weighted Mean Decrease in Accuracy")

ggplot(data = both.dfs, aes(x = MeanAcc, y = MeanGini)) +
  geom_point(aes(col = pct), size = 3)

both.dfs2 <- both.dfs |> filter(pct > 0.05)

# Summary of top 15 variable importance
#flextable(both.dfs2) %>% 
  #theme_vanilla() |>
  #save_as_docx( path = "INSERT LOCATION/SummTable_wtacc_k500_fin.docx")

# CLUSTER ANALYSIS ####

# trainData <- read.csv(file = "INSERT LOCATION/trainData.csv")
# testData <- read.csv(file = "INSERT LOCATION/testData.csv")

names(trainData)

cluster.d <- both.dfs |> filter(WtMeanAcc >= 1) |> select(Variable)
# c.d <- c(Cohort, Community Connectedness, Internalized Homophobia, Out: Healthcare, 
# Social Support - Family, Sexual Identity is, Income, Census Region,
# Felt Stigma, Out: Co-workers, Education, Sexual ID,
# Miles from LGBT Health, 30 Days: Mental Health)

clus.train <- trainData[,c(1, 20, 23, 19, 25, 32, 28, 4, 22, 18, 2, 30, 5, 15, 31)]
clus.test <- testData[,c(1, 20, 23, 19, 25, 32, 28, 4, 22, 18, 2, 30, 5, 15, 31)]
names(trainData)
for(i in c(1, 4, 6:8, 10:12)){
  clus.train[,i] <- as.factor(clus.train[,i])
  clus.test[,i] <- as.factor(clus.test[,i])
}

# hclust to find profiles.
gower.dist <- daisy(clus.train, metric = "gower")
hclust.01 <- hclust(gower.dist, method = "complete")

maxclusters <- 10

# Evaluating cluster metrics for first 10 clusters 
clust.metrics <- data.frame(k = 2:maxclusters,
                            dunns = rep(0, maxclusters-1),
                            avg.sil = rep(NA, maxclusters-1),
                            b.w.ratio = rep(NA, maxclusters-1))

for(i in 2:maxclusters){
  results <- cluster.stats(d = gower.dist, clustering = cutree(tree = hclust.01, k = i))
  clust.metrics$dunns[i-1] <- results$dunn
  clust.metrics$avg.sil[i-1] <- results$avg.silwidth
  clust.metrics$b.w.ratio[i-1] <- results$average.between/(results$average.between+results$average.within)
}

metrics2 <- pivot_longer(data = clust.metrics, cols = !k, names_to = "Metric", values_to = "Value")

ggplot(data = metrics2, aes(x = k, y = Value, col = Metric)) + 
  geom_point() + geom_line() + 
  geom_line(lwd = 1) + ggtitle("Cluster Evaluation Metrics") + 
  xlab("Number of Clusters") + ylab("Metric Value")

# Table with exact numbers for the 3 metrics.
flextable(round(clust.metrics, 3)) |> theme_vanilla()


metrics2 <- pivot_longer(data = clust.metrics, cols = !k, names_to = "Metric", values_to = "Value")

# Cutting the hierarchical tree to 4 clusters.
d.desc <- clus.train
d.desc$Cluster <- as.factor(cutree(tree = hclust.01, k = 3))
d.desc$ParentingGap <- trainData$Parenting.Gap

tbl_summary(d.desc,
                     statistic = list(all_continuous() ~ "{mean} ({sd})"),
                     by = Cluster) |>
  add_p(list(all_categorical() ~ "chisq.test", all_continuous() ~ "kruskal.test")) |>
  as_gt() 
