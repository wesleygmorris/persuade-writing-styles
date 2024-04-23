library(tidyr)
library(dplyr)
library(psych)
setwd("~/Dropbox/GSU/Feedback Comp/PERSUADEnlpToolData")

options(scipen = 999)
df <- read.csv("TAASSCresults.csv")
webfiles <- c("1518.txt", "1727.txt", "3280.txt", "7734.txt", "14843.txt",
              "18716.txt", "19427.txt", "19857.txt", "20653.txt", "21231.txt",
              "24614.txt")

dfWeb <- df %>%
  filter(filename %in% webfiles)

webMeans <- describe(dfWeb) %>%
  select(mean) %>%
  rename("webMean" = "mean")

allMeans <- describe(df) %>%
  select(mean) %>%
  rename("allMean" = "mean")



twoMeans <- cbind(webMeans, allMeans)
twoMeans <- twoMeans[-X,]
twoMeans
str(twoMeans)
cor.test(describe(dfWeb)$mean, describe(df)$mean)
