# Code for generating tables and graphs.
# Run this after all other code has been run.
library(gtsummary)
library(gt)
library(flextable)


# Summaries of data after data cleaning.
t.cat.sm <- tbl_summary(c3.cat)
t.cat.sm |> 
  as_gt() |> 
  gt::gtsave(filename = "C:/Users/larosec/OneDrive - Eastern Connecticut State University/Research/Generations Study/catfinsm.docx") 

t.cat <- tbl_summary(c3.cat,
                     by = 'Parenting Gap')  |>
  add_p(list(all_categorical() ~ "chisq.test"))
t.cat |> 
  as_gt() |> 
  gt::gtsave(filename = "C:/Users/larosec/OneDrive - Eastern Connecticut State University/Research/Generations Study/catfin.docx") 

t.num <- tbl_summary(c3.num2,
                     statistic = list(all_continuous() ~ "{mean} ({sd})"),
                     by = 'Parenting Gap') |>
  add_p(list(all_continuous() ~ "kruskal.test"))
t.num |> 
  as_gt() |> 
  gt::gtsave(filename = "C:/Users/larosec/OneDrive - Eastern Connecticut State University/Research/Generations Study/catnum.docx") 


# Summary of top 15 variable importance
flextable(cbind(rownames(all.15.imp.vars), all.15.imp.vars)) %>% 
  theme_vanilla() |>
  save_as_docx( path = "C:/Users/larosec/OneDrive - Eastern Connecticut State University/Research/Generations Study/impvars.docx")

# Table with exact numbers for the 3 metrics.
flextable(round(clust.metrics, 3)) |> theme_vanilla() |>
  save_as_docx( path = "C:/Users/larosec/OneDrive - Eastern Connecticut State University/Research/Generations Study/clustermetricsfin.docx")


# Final table with p-values for the top 10 variables that were selected by gini index and accuracy.
t.all <- tbl_summary(d.desc, percent = "column",
                     by = Cluster,
                     statistic = list(all_continuous() ~ "{mean} ({sd})")) |>
  add_p(list(all_continuous() ~ "kruskal.test", all_categorical() ~ "chisq.test"))