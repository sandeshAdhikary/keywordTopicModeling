library(tm)
library(magrittr)
library(dplyr)
library(RTextTools)
# topicmodel package requires the GSL package for Ubuntu: sudo apt-get install gsl-bin libgsl-dev
library(topicmodels) #needed for LDA
library(cluster) #needed for silhouette plots
library(LDAvis) #needed for LDA visualizations
library(SnowballC) #needed for stemming


setwd("~/repos/keywordTopicAnalysisBlog")
# 
# Loading keywords and saving as a single vector
# keywords <- read.csv('keywords.csv', stringsAsFactors = F) %>%
#   rename(keywords = Natural.Search.Keyword) %>%
#   select(keywords) %>%
#   mutate(keywords = tolower(keywords)) %>%
#   filter(!keywords %in% c("::unspecified::","::empty::"))
# 


# Adwords keyword list
#Loading keywords and saving as a single vector
keywords <- read.csv('adwords_keywordlist.csv', stringsAsFactors = F) %>%
  rename(keywords = keywords) %>%
  select(keywords) %>%
  mutate(keywords = tolower(keywords)) %>%
  filter(!keywords %in% c("::unspecified::","::empty::"))


# keywords <- read.csv('trp-keywords.csv', stringsAsFactors = F) %>%
#   mutate(keywords = tolower(keywords)) %>%
#   filter(!keywords %in% c("::unspecified::","::empty::"))
# 
# removeBrandedKeywords <- function(keywords,brandedTerms){
#   gsub(brandedTerms,keywords,"")
# }
# 
# brandedTerms = c("theroomplace","roomplace","room place","roomplac","harlem","place room","trp")
# 
# keywords <- removeBrandedKeywords(keywords,brandedTerms) # NEED TO FIGURE OUT HOW TO HAVE MULTIPLE TERMS IN PATTERN

# tokenizing on spaces  
spacesplit <- function(string){
  strsplit(string," ")
}

doc.list <- sapply(keywords,spacesplit)
# doc.list <- sapply(doc.list,wordStem) # Stemming: Collapsing words into their root word


############# LDA Vis #################

# data(reviews, package = "LDAvisData")

library(tm)
stop_words <- stopwords("SMART")


# compute the table of terms:
term.table <- table(unlist(doc.list), exclude = "") # Used exclude to get rid of empty strings. Probably because of keywords ending in spaces

term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

library(lda)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop


theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency,
                   R = 15)

library(servr)
serVis(json, out.dir = 'vis', open.browser = TRUE)



