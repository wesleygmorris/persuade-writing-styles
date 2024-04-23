library(EGAnet)
library(tidyverse)

setwd('C:\\Users\\morriwg1\\OneDrive - Vanderbilt\\Documents\\vanderbilt\\projects\\writing_styles_cluster\\data\\')
# main_df <- read.csv('persuade_corpus_corrected.csv')

## Merge the NLP results on the key list to get the essay scores
all_results <- read.csv('PERSUADE_Combined_Results.csv') %>%
    select(-c("X","nwords" ))

key_list <- read.csv('key_list.csv') %>%
    select(c("filename", "essay_id_comp", "holistic_score_adjudicated"))

df <- merge(key_list, all_results, on='filename')


## Take only high-scoring essays
df <- df %>%
    filter(holistic_score_adjudicated>4) %>%
    select(-c("filename", "essay_id_comp", "holistic_score_adjudicated"))

## UVA can't handle sd = 0. find columns with standard deviations of zero and drop them
length(colnames(df))
df <- df %>%
    select(-c(row.names(data.frame(sapply(df, sd)) %>% filter(sapply.df..sd. == 0))))
length(colnames(df))

# Perform Unique Variable Analysis
df_uva <- UVA(data = df, cut.off = 0.2)#, key = as.character(colnames(df)))
## Print results

ncol(df_uva$reduced_data)
ncol(df)ra
