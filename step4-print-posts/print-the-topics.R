k <- 8
load(file='../step2-run-LDA/text_lda8_twoxchromosomes.RData')
word.assignments <- data.frame(doc = text_lda@wordassignments$i, 
                               termnum = text_lda@wordassignments$j, 
                               topic = text_lda@wordassignments$v)


##Create the document-term matrix
library(tm)
text <- readLines('../step1-create-the-corpora/vcorpus_infile_rtwoxchromosomes.txt')
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
#text_dtm <- text_dtm[row_sums(text_dtm) > 0,]

key.table <- data.frame(keepinds=which(row_sums(text_dtm) > 0), newinds=1:length(which(row_sums(text_dtm) > 0)))

# posterior(text_lda)$topics is what is used to assign each document a topic. its rows sum to 1.
#the names of the below vector are document id's, and its values are the probability that the document is topic 1
#print some representative documents for each topic
library(officer)

##Assign 1:30 to CM
## 31:60 to ME
## 61:90 to JG

for(i in 1:8){
  print(i)
  oldinds <- as.numeric(names(sort(posterior(text_lda)$topics[,i], decreasing=T)[sort(posterior(text_lda)$topics[,i], decreasing=T) >= .5]))
  newinds <- key.table[key.table$keepinds%in%oldinds,]$newinds
  #print(oldinds)
  set.seed(1234)
  sample <- sample(oldinds, length(oldinds), replace=F)
  print(sample)
  doc_1 <- read_docx()
  for(j in sample[1:30]){ #change this line for each person's sample
    doc_1 <- body_add_par(doc_1, substr(text[j],1,10000))
    doc_1 <- body_add_par(doc_1, '\n ')
  }
  print(doc_1, target = (paste0('CM_TwoX_topic',i,'.docx')))
  df <- word.assignments[word.assignments$doc%in%newinds,] #check the topic assignments for each term in a document
  print(pie(table(df$topic),main=paste0('Post set ',i)))
}




