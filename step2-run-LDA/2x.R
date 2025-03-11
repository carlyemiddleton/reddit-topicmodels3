##Create the document-term matrix
library(tm)
text <- readLines('/home/cemidd02/lda-topic-model/LARC/complete-reproduction-of-the-data/step1-create-the-corpora/vcorpus_infile_rTwoXChromosomes.txt')
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


library(topicmodels)

##Compute the LDA model with K=8
set.seed(1234)
text_lda <- LDA(text_dtm,  k = 8, method = "VEM", control=list(seed=1234))
as.matrix(terms(text_lda,10))
save(text_lda, file='text_lda8_twoxchromosomes.RData')






