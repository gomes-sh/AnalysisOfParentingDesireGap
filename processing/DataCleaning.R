# Run this file first.
library(tidyverse)
library(tidymodels)
library(mice)
library(forcats)
library(ggplot2)
library(naniar)
library(visdat)


# DATA CLEANING AND PROCESSING.

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
mis.vars <- which(names(c3) %in% names(which(mis.count > 0)))

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

# Variable description now.  In TablesGraphs.R

c3[ , is.numeric] <- scale(c3[ , is.numeric])
