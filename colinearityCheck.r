library(tidyr)
library(dplyr)
library(Hmisc)
library(caret)

setwd("~/Dropbox/GSU/Feedback Comp/PERSUADEnlpToolData")

df <- read.csv("PERSUADE_Combined_Results.csv")

plot(df$Word.Count, df$basic_ntokens)
cor.test(df$Word.Count, df$basic_ntokens)

df %>%
  filter(df$Word.Count > 2000) %>%
  select(filename)

corMatrix <- df %>%
  select(-c(content, vmod_iobj_deps_NN_struct, xsubj_per_cl,
            X, filename)) %>%
  cor(use="complete.obs")

corMatrix <- round(corMatrix, 2) 



findCorrelation(x=corMatrix, cutoff =(0.90), verbose=TRUE)

corMatrix[1316,1328]

colnames(df)[1316]
colnames(df)[1328]

newlist <- corMatrix[, highCorr]

which(newlist >0.9, arr.ind = T)

#Discourse Effectiveness
dfEffectiveness <- read.csv("")