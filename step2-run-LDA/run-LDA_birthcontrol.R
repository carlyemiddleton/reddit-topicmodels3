##Create the document-term matrix
library(tm)
text <- readLines('/home/cemidd02/lda-topic-model/LARC/complete-reproduction-of-the-data/step1-create-the-corpora/vcorpus_infile_rbirthcontrol.txt')
Vcorpus <- VCorpus(VectorSource(text))
text_dtm <- DocumentTermMatrix(Vcorpus,
                               control = list(tolower=TRUE,             #change all words to lowercase
                                              removePunctuation = TRUE, #remove punctuation
                                              removeNumbers= TRUE,      #remove numbers
                                              stopwords = TRUE,         #remove stopwords
                                              minWordLength = 3,        #remove words having less than 3 letters
                                              stemming = TRUE))         #convert 'happiness' and 'happily' both to 'happy'

##Reduce the vocabulary
library(slam)
summary(col_sums(text_dtm))
term_tfidf <- tapply(text_dtm$v/row_sums(text_dtm)[text_dtm$i], text_dtm$j, mean)*log2(nDocs(text_dtm)/col_sums(text_dtm > 0))
summary(term_tfidf)
text_dtm <- text_dtm[,term_tfidf >= 0.01]   
text_dtm <- text_dtm[row_sums(text_dtm) > 0,]
summary(col_sums(text_dtm))


##Calculate the perplexity for different choices of K
library(topicmodels)
k_list <- c(2:15)
n_folds <- 10 #10-fold cross validation
perplexity_mat <- data.frame(k = 1:max(k_list), train_perplexity = NA, test_perplexity=NA)
full_data  <- text_dtm
set.seed(1234)
(folds_i <- sample(rep(1:n_folds, length.out = nrow(full_data))))
for(k in k_list){
  cv_tmp <- matrix(NA, nrow=2, ncol=n_folds)
  for(fold in 1:n_folds){
    test_i <- which(folds_i == fold)
    train_set <- full_data[-test_i, ]
    test_set <- full_data[test_i, ]
    text_lda <- LDA(train_set,  k = k, method = "VEM", control=list(seed=1234))
    cv_tmp[1,fold] <- perplexity(text_lda, newdata = train_set)
    cv_tmp[2,fold] <- perplexity(text_lda, newdata = test_set)
    print(fold)
  }
  perplexity_mat$train_perplexity[k] <- apply(cv_tmp, 1, mean)[1]
  perplexity_mat$test_perplexity[k] <- apply(cv_tmp, 1, mean)[2]  
  print(k)
}

save(perplexity_mat, file='perplexity_mat_birthcontrol.Rdata')






