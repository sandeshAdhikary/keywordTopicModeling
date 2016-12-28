library(tm)
library(magrittr)
library(dplyr)
library(RTextTools)
# topicmodel package requires the GSL package for Ubuntu: sudo apt-get install gsl-bin libgsl-dev
library(topicmodels) #needed for LDA
library(cluster) #needed for silhouette plots
library(LDAvis) #needed for LDA visualizations


setwd("~/repos/keywordTopicAnalysisBlog")

# Loading keywords and saving as a single vector
keywords <- read.csv('keywords.csv', stringsAsFactors = F) %>%
  rename(keywords = Natural.Search.Keyword) %>%
  select(keywords) %>%
  filter(!keywords %in% c("::unspecified::","::empty::"))

keywords <- keywords$keywords

# Creating the corpus
corpus <- Corpus(VectorSource(keywords)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords,stopwords('english'))

# Creating a Document Term Matrix
dtm <- DocumentTermMatrix(corpus)

rowTotals <- apply(dtm, 1, sum)
empty.rows <- dtm[rowTotals == 0,]$dimnames[1][[1]]
dtm <- dtm[rowTotals>0,]
corpus <- corpus[-as.numeric(empty.rows)]



########### K-means ##################

weightedTIDA <- weightTfIdf(dtm) %>% 
  as.matrix()

# normalizing using euclidean distance. Need to experiment with other normalizations
norm_eucl <- function(m)
  m/apply(m,1,function(x) sum(x^2)^0.5)

weightedTIDA_norm <- norm_eucl(weightedTIDA)

set.seed(5)
k <- 5
kmeansResult <- kmeans(weightedTIDA_norm,k)

# Elbow method check
cost_df <- data.frame()

#run kmeans for all clusters up to 100
for(i in 1:100){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=weightedTIDA_norm, centers=i, iter.max=100)

  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))

}

names(cost_df) <- c("cluster", "cost")

plot(cost_df)

# Silhouette plots
diss <- dist(weightedTIDA_norm)^2

sk <- silhouette(kmeansResult$cluster, diss)
pdf("sillhouette.pdf")
plot(sk)
dev.off()