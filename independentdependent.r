library(tidyverse)
library(ggplot2)


# Load the rstudioapi package.
library(rstudioapi)
# Get the name of the directory in which the current file is located.
cur_dir = dirname(getSourceEditorContext()$path)
setwd(cur_dir)

#################################
##Merge Dataframes to get Tasks##
#################################
df <- read.csv('..\\data\\combined_results_four_clusters.csv') %>%
    rename_at("index_data.essay_id_comp", ~"essay_id_comp")
colnames(df)

original_df <- read.csv('..\\data\\persuade_corpus_corrected.csv') %>%
    select(c(essay_id, task, prompt_name))
unique(original_df$prompt_name)



key <- read.csv('.\\data\\key_list.csv') %>%
    select(c(essay_id, essay_id_comp))
nrow(key)

tasks <- merge(original_df, key, by='essay_id', how='inner')
tasks <- tasks[!duplicated(tasks), ]
nrow(tasks)

df <- merge(tasks, df, by='essay_id_comp')
nrow(df)
head(df)

#################
##Analyze Tasks##
#################

task_df <- df %>% select(c(cluster, task))

ind_df <- task_df %>% filter(task=='Independent')
dep_df <- task_df %>% filter(task=='Text dependent')

t(table(ind_df$cluster))
t(table(dep_df$cluster))

chisq.test(t(rbind(table(ind_df$cluster), table(dep_df$cluster))))
profiles = c('Structural', 'Academic', 'Narrative', 'Conversational')

ggplot(data=task_df, aes(x=cluster, fill=task)) +
    geom_bar(position='dodge') +
    labs(title='Prevalence of Task Type in Each Cluster', x='profile')

