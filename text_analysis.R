# package names to load
packages <- c('dplyr', 'rvest', 'stringr', 'curl',
              'tidyr', 'purrr', 'lubridate', 'pbapply',
              'tibble', 'broom')

# packages used for text analysis
text_packages <- c('tm', 'knitr', 'magrittr', 'tidytext', 'ggplot2')

# packages used for word cloud
word_cloud_packages <- c('wordcloud', 'RColorBrewer')

# combining all packages into one list
total_packages <- c(packages, text_packages, word_cloud_packages)

# enable installation of packages into session
lapply(total_packages, require, character.only = TRUE)

# Read-in jobs data
jobs_data <- readRDS("Text_Analytics/data_repository_revised.rds") %>%
  dplyr::select(-date_scraped) %>%
  distinct() 

# Convert jobs data into working list
jobs_corpus_prep <- jobs_data %>%
  dplyr::rename(., qualifications = qualifcations) %>%
  distinct() %>%
  group_by(job_title, company) %>%
  nest() %>%
  ungroup()

############ SETTING UP META DATA AT HEADER & DOCUMENT LEVEL ############

# Converting description data into single character vectors to retain metadata
jobs_corpus_prep_2 <- jobs_corpus_prep %>%
  dplyr::select(data) %>%
  mutate(data = pblapply(data, function(x){
    
    df <- x %>%
      unnest() %>%
      distinct() %>%
      rowid_to_column() %>%
      pivot_wider(names_from = rowid, values_from = qualifications) %>%
      paste(., collapse = ' ')
    
    return(df)
    
  })) %>%
  unlist()

# Converting jobs data list into tm corpus
jobs_corpus <- VCorpus(VectorSource(jobs_corpus_prep_2))

# Adding header-level meta data
meta(jobs_corpus, tag = "strings", type = "corpus") <- "First Edition"

############ DOCUMENT TEXT MATRIX ANALYSIS ############

# reviewing document term matrix of corpus
dtm <- data.frame(unclass(inspect(DocumentTermMatrix(jobs_corpus))), 
                   check.names = FALSE, stringsAsFactors = FALSE)

# Inspect first 15 lines of first doc
writeLines(head(strwrap(jobs_corpus[[1]]), 15)) 

# Function to apply prelim transformations
pre_cleanse <- function(x){
  
  transf <- tm_map(x, removePunctuation)
  
  transf <- tm_map(transf, removeNumbers)
  
  transf <- tm_map(transf, removeWords, stopwords("english"))
  
  # Transform to lower case (need to wrap in content_transformer)
  transf <- tm_map(transf, content_transformer(tolower))
  
  transf <- tm_map(transf, stripWhitespace)
  
  return(transf)
}

# apply pre_cleanse function
jobs_corpus_cleansed <- pre_cleanse(jobs_corpus)

# Inspect first 15 lines of first doc
writeLines(head(strwrap(jobs_corpus_cleansed[[1]]), 15)) 

# writing document term matrix
dtm <- DocumentTermMatrix(jobs_corpus_cleansed)

# converting to df
dtm_df <- data.frame(unclass(inspect(DocumentTermMatrix(jobs_corpus_cleansed))), 
                  check.names = FALSE, stringsAsFactors = FALSE)

# sum all columns(words) to get frequency
words_frequency <- colSums(as.matrix(dtm)) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  dplyr::rename(., Term = rowname, Freq = .) %>%
  arrange(desc(Freq))

# convert corpus into a tfidf weighted dtm
dtm_tfidf <- DocumentTermMatrix(jobs_corpus_cleansed, 
                                       control = list(weighting = weightTfIdf)) 

# convert document term matrix into df using tidy format
df_dtm <- tidy(dtm)

# take the product of tf and idf and create new column labeled "tf_idf". Graph it. 
bind_tf_idf(df_dtm, term = term, document = document, n = count) %>% 
  arrange(desc(tf_idf)) %>%
  filter(document %in% 1:10) %>%
  mutate(word = factor(term, levels = rev(unique(term))),
         posting = factor(document, levels = 1:20)) %>%  
  group_by(document) %>% 
  top_n(6, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words by Job Posting",
       x = "Words", y = "tf-idf") +
  facet_wrap(~posting, ncol = 2, scales = "free") +
  coord_flip()

############ AMPLIFYING TERM DOCUMENT ANALYSIS ############

# convert corpus into plain text document
jobs_corpus_cleansed_2 <- tm_map(jobs_corpus_cleansed, PlainTextDocument)

# create term document matrix
jobs_corpus_cleansed_df <- TermDocumentMatrix(jobs_corpus_cleansed_2) %>%
  # convert TermDocumentMatrix object to a matrix object
  as.matrix(.) %>%
  # convert matrix to a data frame
  data.frame(.) %>%
  rename_all(., function(x){
    
    names(x)[1:length(x)] <- paste("job", 1:length(x), sep="_")
    
  })

# Determine frequency percentage of words per term doc matrix
jobs_corpus_cleansed_df_2 <- jobs_corpus_cleansed_df %>%
  rownames_to_column(.) %>%
  group_by(rowname) %>%
  nest() %>%
  ungroup() %>%
  mutate(data = pblapply(data, function(x){
    
    df <- x %>%
      unnest() %>%
      mutate_all(., function(x){ifelse(x >= 1, 1, 0)}) %>%
      mutate(row_sum = rowSums(.)) %>%
      mutate(n_jobs = ncol(.)-1) %>%
      mutate(percentage = round(row_sum / n_jobs, digits = 2)) %>%
      dplyr::select(percentage)
    
    return(df)
    
  })) %>%
  unnest() %>%
  arrange(desc(percentage))

# Common words to filter in term doc matrix
tools <- c("aws","c_plus_plus","cassandra","excel","hadoop","hive","java","kafka","mapreduce",
           "mahout","matlab","nosql","oracle", "perl","pig","python", 
           "r_program", "ruby","sas","scala","spark", "spss","stata","sql", "sqlserver")

# Common words to filter in term doc matrix
techniques <- c("classification", "clustering", "decision_tree", "dimension_reduction",
                "feature_engineering", "feature_selection", "forecasting", "logistic_regression",
                "machine_learning", "nlp", "neural_network", "optimization",
                "parallel_processing", "predictive", "propensity_modeling",
                "random_forest", "regression", "segmentation",
                "supervised_learning", "svm", "unsupervised_learning", "text_mining")

# Common words to filter in term doc matrix
other <- c("communication_skills", "computer_science", "experimental_design", "external_data",
           "scalable","statistics","visualization")

# Filter term document matrix to only include listing of common words above
jobs_corpus_cleansed_df_3 <- subset(jobs_corpus_cleansed_df_2, 
                                    rowname %in% tools | rowname %in% techniques | rowname %in% other)

# remove unnecessary word strings
remove(tools, techniques, other)

# set plot background to white
par(bg='white') 

# color palette options at http://www.sthda.com/sthda/RDoc/images/rcolorbrewer.png
wordcloud(words=jobs_corpus_cleansed_df_3$rowname, freq=jobs_corpus_cleansed_df_3$percentage, 
          scale=c(4, .5), # range of the word size
          random.order=FALSE, # plot words in decreasing freq value
          random.color=FALSE, # word color based upon freq value
          rot.per= 0, # proportion of words with 90 degree rotation
          colors=brewer.pal(n=8, name="Dark2")) # n=number of colors to use; name= name of color palette

  


