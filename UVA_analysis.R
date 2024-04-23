rm(list=ls()) 

install.packages('moments')

library(EGAnet); library(psychTools); library(ggplot2)
library(moments)

# Read data and take indices
df <- read.csv('C:\\Users\\morriwg1\\OneDrive - Vanderbilt\\Documents\\vanderbilt\\projects\\writing_styles_cluster\\data\\PERSUADE_Combined_Results.csv')
indices <- df[,4:1334]
ncol(indices)
head(df$X)
# Remove columns with mostly zeros
cols_to_keep <- colSums(indices != 0) > (nrow(indices)*0.5)
indices <- indices[,cols_to_keep]
ncol(indices)

cols_to_keep <- abs(skewness(indices)) < 2
indices <- indices[,cols_to_keep]
ncol(indices)

cols_to_keep <- abs(kurtosis(indices)) < 3
indices <- indices[,cols_to_keep]
ncol(indices)

# Apply UVA
persuade_uva <- UVA(indices, cut.off=0.25)
ncol(persuade_uva$reduced_data)
persuade_uva <- UVA(persuade_uva$reduced_data, cut.off=0.25)
ncol(persuade_uva$reduced_data)

indices <- persuade_uva$reduced_data
colnames(indices)

EGA(indices)

#Define kmeans_clusters
library(cluster)
set.seed(42)
kmeans_clusters <- kmeans(indices, centers = 4, iter.max = 40)
kmeans_clusters
sil <- silhouette(kmeans_clusters$cluster, dist(indices))
fviz_silhouette(sil)
summary(sil)$avg.width
kmeansAIC(kmeans_clusters)

wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, iter.max = 20)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(indices, nc = 20)
install.packages('fpc')

library(fpc)
plotcluster(indices, kmeans_clusters$cluster)
clusplot(indices, kmeans_clusters$cluster, color=TRUE, shade=TRUE)


df_clustered <- cbind(indices, kmeans_clusters$cluster)
names(df_clustered)[names(df_clustered) == "kmeans_clusters$cluster"] <- "cluster"


df_four_clusters <- as.data.frame(df_clustered)
df_four_clusters
table(df_four_clusters$V75)
colnames(df_four_clusters)
table(df_four_clusters$cluster)
ggplot(df_four_clusters, aes(cluster)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)

