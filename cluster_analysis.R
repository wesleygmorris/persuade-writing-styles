citation()

rm(list = ls())
library(factoextra)
library(cluster)
data <- "..\\data\\"
setwd("~/vanderbilt/projects/writing_styles_cluster")
###LOAD
index_data <- read.csv('.\\data\\index_data.csv')
set.seed(42)
head(index_data)
df <- as.data.frame(scale(index_data[,5:55]))
head(df)

###EGA
library(EGAnet); library(psychTools)
library(ggplot2)

ega <- EGA(df, plot.EGA=TRUE, algorithm="louvain", resolution = 0.7)

###HIERARCHICAL CLUSTERING
#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(df, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#compute distance matrix
d <- dist(df, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=4)

#find number of observations in each cluster
table(groups)

#append cluster labels to original data
final_data <- cbind(index_data, cluster = groups)

#display first six rows of final data
head(final_data)

###KMEANS CLUSTERING
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, iter.max = 20)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(df, nc = 20)
fviz_nbclust(df, kmeans, method = "wss", title='jfkld')
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)


#plot AIC values for different numbers of clusters
kmeansAIC = function(fit){
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + 2*m*k)
}

aicplot <- function(data, nc=15, seed=42){
  aiclist = c()
  for (i in 1:nc){
    set.seed(seed)
    aiclist[i] = kmeansAIC(kmeans(data, centers = i, iter.max=40))}
  plot(1:nc, aiclist, type='b', xlab="Number of groups",
       ylab="AIC")}
aicplot(df, nc=10)

# Plot average silhouette width
silplot <- function(data, nc=15, seed=42){
  sillist = c()
  for (i in 2:nc){
    set.seed(seed)
    clusters = kmeans(data, centers = i, iter.max=40)
    sil = silhouette(clusters$cluster, dist(df))
    sillist[i] = summary(sil)$avg.width
  }
  plot(1:nc, sillist, type='b', xlab="Number of groups",
       ylab="Average Silhouette Width")}

silplot(df, nc=10)

fviz_nbclust(df, kmeans, method = "silhouette", iter.max=20)

  #Define kmeans_clusters
library(cluster)
set.seed(42)
kmeans_clusters <- kmeans(df, centers = 5, iter.max = 40)
kmeans_clusters
sil <- silhouette(kmeans_clusters$cluster, dist(df))
fviz_silhouette(sil)
summary(sil)$avg.width



kmeansAIC(kmeans_clusters)

#Silhouette chart
set.seed(42)
kmeans_clusters <- kmeans(df, centers = 4, iter.max=40)
sil <- silhouette(kmeans_clusters$cluster, dist(df))
fviz_silhouette(sil)
sil

kmeansAIC = function(fit){
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + 2*m*k)
}

kmeansAIC(kmeans_clusters)

#plot results of final k-means model
set.seed(42)
kmeans_clusters <- kmeans(df, centers = 4, iter.max=40)
fviz_cluster(kmeans_clusters, data = df)

#Visualize Plots
library(fpc)
plotcluster(df, kmeans_clusters$cluster)
clusplot(df, kmeans_clusters$cluster, color=TRUE, shade=TRUE)

##Bind clusters to original dataframe
# Four Clusters
df_clustered <- cbind(df, kmeans_clusters$cluster)
names(df_clustered)[names(df_clustered) == "kmeans_clusters$cluster"] <- "cluster"
df_clustered$cluster <- as.factor(df_clustered$cluster)
df_clustered

df_clustered_ids <- cbind(index_data$essay_id_comp, df_clustered)


write.csv(df_clustered_ids, file='combined_results_four_clusters.csv', row.names=FALSE)

# Five Clusters
df_clustered <- cbind(df, kmeans_clusters$cluster)
names(df_clustered)[names(df_clustered) == "kmeans_clusters$cluster"] <- "cluster"
df_clustered$cluster <- as.factor(df_clustered$cluster)
df_clustered

df_clustered_ids <- cbind(index_data$essay_id_comp, df_clustered)


write.csv(df_clustered_ids, file='combined_results_five_clusters.csv', row.names=FALSE)
###MANOVA
## Four Clusters
#convert dependent variables to numeric
df_four_clusters = read.csv('.\\data\\combined_results_four_clusters.csv', row.names = 'index_data.essay_id_comp')
table(df_four_clusters$cluster)
colnames(df_four_clusters)
table(df_four_clusters$cluster)
ggplot(df_four_clusters, aes(cluster)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)
# Number of cars in each class:
four_dat <- sapply(df_four_clusters[,1:51], as.numeric )
four_dat

## Five Clusters
df_five_clusters = read.csv('combined_results_five_clusters.csv', row.names = 'index_data.essay_id_comp')
colnames(df_five_clusters)

five_dat <- sapply(df_five_clusters[,1:51], as.numeric )
five_dat
df_clustered


## Run manovas
library(MASS)
dependent.vars <- cbind(df_four_clusters$Arousal, df_four_clusters$Positive_EmoLex)

four_cluster_man <- manova(four_dat ~ cluster, data=df_four_clusters)
summary(four_cluster_man)

four_cluster_lda <- lda(df_four_clusters$cluster ~ four_dat)
four_cluster_lda

plot(four_cluster_lda)
library(car)
four_cluster_aov <- aov(four_cluster_man)
summary(four_cluster_aov)
library(effectsize)
effect <- eta_squared(four_cluster_aov)
write.csv(effect, "table.csv")

five_cluster_man <- manova(five_dat ~ cluster, data=df_five_clusters)
summary(five_cluster_man)
five_cluster_man$coefficients



sum <- manovaAnova$coefficients
sum
write.csv(sum, "table.csv")


#Check data using SVM and Logistic Regression
#Prepare data
## Find center essays
install.packages('lsa')
library(tidyverse)
library(lsa)

df_four_clusters_1 <- df_four_clusters %>%
  filter(cluster==1)

centroid_1 <- as.vector(colMeans(df_four_clusters_1))

df_four_clusters_1$distance <- 

library(caTools)
#Split Data
set.seed(42)
df_four_clusters$cluster = as.factor(df_four_clusters$cluster)
split = sample.split(df_four_clusters$cluster, SplitRatio = 0.8)
training_set = subset(df_four_clusters, split == TRUE)
test_set = subset(df_four_clusters, split == FALSE)

#SVM
library(e1071)
svmfit = svm(cluster ~ ., data = training_set, kernel = "radial")
#Use SVM to predict outcomes on test set
predR = predict(svmfit, newdata=test_set[1:51])
#Assign actual values
actR = test_set[,52]
#Build confusion matrix
cmR= table(test_set[,52], predR)
cmR
write.csv(cmR, "table.csv")
#Get values for precision, accuracy, F1
n = sum(cmR) # number of instances
nc = nrow(cmR) # number of classes
diag = diag(cmR) # number of correctly classified instances per class 
rowsums = apply(cmR, 1, sum) # number of instances per class
colsums = apply(cmR, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


accuracy=sum(diag)/n
precision = diag/colsums
recall = diag/rowsums
f1 = 2*precision*recall/(precision + recall)
classReport <- data.frame(precision,recall,f1)
write.csv(classReport, "table.csv")
accuracy
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
classReport <- data.frame(macroPrecision, macroRecall, macroF1)
write.csv(classReport, "table.csv")
classReport
#10-fold cross-validation
set.seed(42)
cv = tune(svm, cluster ~ ., data = df_four_clusters, kernel = "radial")
summary(cv)
cv$best.model

#Logistic Regression
library(nnet)
model <- multinom(cluster ~.,family=binomial(link='logit'),data=training_set)
sum <- summary(model)
sum
#predicted values
predR = predict(model, newdata=test_set[1:51])
#Assign actual values
actR = test_set[,52]
#Build confusion matrix
cmR= table(actR, predR)
cmR
write.csv(cmR, "table.csv")
#Get values for precision, accuracy, F1
n = sum(cmR) # number of instances
nc = nrow(cmR) # number of classes
diag = diag(cmR) # number of correctly classified instances per class 
rowsums = apply(cmR, 1, sum) # number of instances per class
colsums = apply(cmR, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

accuracy=sum(diag)/n
precision = diag/colsums
recall = diag/rowsums
f1 = 2*precision*recall/(precision + recall)
classReport <- data.frame(precision,recall,f1)
write.csv(classReport, "table.csv")
classReport
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
classReport <- data.frame(macroPrecision, macroRecall, macroF1)
write.csv(classReport, "table.csv")
classReport

#10-fold cross-validation
set.seed(42)
cv = tune(multinom, cluster ~ ., data = df_four_clusters)#, kernel = "radial")
summary(cv)
cv$best.model

#Aggregate Data
library(dplyr)
library(reshape2)

table(df_clustered$cluster)

df_melt <- melt(df_clustered, id.vars = 'cluster')


df_melt %>%
  ggplot(aes(x=cluster,y=value))+
  geom_bar(aes(fill=cluster), stat='summary', fun='mean')+
  facet_wrap(~variable,scales = "free_y")+
  theme(axis.title = element_text(size=2))

agg_data <- aggregate(df_melt$value, list(df_melt$cluster, df_melt$variable), FUN='mean')
agg_data
reshape_data <- reshape(agg_data, idvar='Group.2', timevar = 'Group.1', direction='wide')
reshape_data
write.csv(reshape_data, "table.csv")

#Make it wide
wideDf = reshape(data=df_melt, idvar = "variable", timevar="cluster", direction="wide")
write.csv(wideDf, "table.csv")

df_clustered
df_clustered[5:56]

colnames(df_clustered)
hist(df_clustered$fic_av_lemma_freq_type)

