library(dplyr)
k <- 8
load(file='../step2-run-LDA/text_lda8_twoxchromosomes.RData')
#the 10 most likely terms for each topic
library(topicmodels)
as.matrix(terms(text_lda,10))

word.assignments <- data.frame(doc = text_lda@wordassignments$i, 
                               termnum = text_lda@wordassignments$j, 
                               topic = text_lda@wordassignments$v)
metadata <- read.csv('../step1-create-the-corpora/metadata_TwoXChromosomes.csv',header=F)
metadata <- metadata[-c(87, 1540, 1541, 1542, 1834, 2047, 2129),] #these 7 documents got removed when the vocabulary was reduced
names(metadata) <- c('id', 'timestamp', 'permalink')
timestamp.assignments <- data.frame(doc = 1:dim(metadata)[1], timestamp=metadata$timestamp)
data <- merge(word.assignments, timestamp.assignments, by='doc', all=T)

library(anytime)
data$date <- anydate(data$timestamp)
library(lubridate)
data$month <- floor_date(data$date, unit = 'month')
for(i in 1:k){
  wordcount <- aggregate(data$topic, by=list(data$doc), FUN = function(x){sum(x==i)})
  colnames(wordcount) <- c('doc', 'Topicwordcount')
  data <- merge(data, wordcount,all=T)
  colnames(data) <- c(colnames(data)[1:length(colnames(data))-1], paste('Topicwordcount',i, sep=''))
}

poisson.dataset <- unique(data[,-c(2:3)])
doclengths <- aggregate(data$topic, by=list(data$doc), FUN = function(x){length(x)})
names(doclengths) <- c('doc','doclength')
poisson.dataset <- merge(poisson.dataset, doclengths, by='doc', all=T)
#save(poisson.dataset, file='poisson.dataset.RData')


###########################
## run the spline models ##
###########################

#1651449600 = May 2, 2022 midnight
#1656028800 = June 24, 2022 midnight
#1671840000 = December 24, 2022 midnight
event.knots <- c(1651449600, 1656028800, 1671840000)
quantile.knots <- c(quantile(poisson.dataset$timestamp)[2], 
                    quantile(poisson.dataset$timestamp)[3], 
                    quantile(poisson.dataset$timestamp)[4])
#poisson.dataset$time2 <- ifelse(poisson.dataset$timestamp >= 1651449600 & poisson.dataset$timestamp < 1671840000, 1, 0)
#poisson.dataset$time3 <- ifelse(poisson.dataset$timestamp >= 1671840000, 1, 0)

library(splines)
for(topic in 1:k){
  #topic <- 1
  fit <- glm(get(paste0('Topicwordcount',topic)) ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
  print(summary(fit))
}


NDATA <- NULL
library(jtools)
#time3 and timestamp*time3 are not significant --> remove them
fit <- glm(Topicwordcount1 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '1'
NDATA <- rbind(NDATA, ndata)
fit <- glm(Topicwordcount2 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '2'
NDATA <- rbind(NDATA, ndata)
fit <- glm(Topicwordcount3 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '3'
NDATA <- rbind(NDATA, ndata)
fit <- glm(Topicwordcount4 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '4'
NDATA <- rbind(NDATA, ndata)
fit <- glm(Topicwordcount5 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '5'
NDATA <- rbind(NDATA, ndata)
fit <- glm(Topicwordcount6 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '6'
NDATA <- rbind(NDATA, ndata)
fit <- glm(Topicwordcount7 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '7'
NDATA <- rbind(NDATA, ndata)
fit <- glm(Topicwordcount8 ~ bs(timestamp, knots=quantile.knots) + offset(log(doclength)), family=poisson(link=log), data=poisson.dataset)
#effect_plot(fit, pred = timestamp, interval = TRUE, int.type = 'confidence')

ndata <- with(poisson.dataset, data.frame(timestamp = seq(min(timestamp), max(timestamp),length = 100),
                                          doclength=rep(1, 100)))
ndata$linkfit <- predict(fit, newdata = ndata, type = 'link')
ndata$responsefit <- exp(ndata$linkfit)
ndata$linkse <- predict(fit, newdata = ndata, type = 'link',se.fit = TRUE)$se.fit
ndata$linkLB <- ndata$linkfit - 1.96*ndata$linkse
ndata$linkUB <- ndata$linkfit + 1.96*ndata$linkse
ndata$responseLB <- exp(ndata$linkLB)
ndata$responseUB <- exp(ndata$linkUB)
ndata$topic <- '8'
NDATA <- rbind(NDATA, ndata)


##Get the numbers of documents in each time category
topics <- topics(text_lda,1)
names(topics) <- sort(unique(word.assignments$doc))
table(topics(text_lda,1)) #numbers of documents in each topic
#Topic 1
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==1)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision
#Topic 2
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==2)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision
#Topic 3
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==3)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision
#Topic 4
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==4)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision
#Topic 5
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==5)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision
#Topic 6
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==6)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision
#Topic 7
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==7)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision
#Topic 8
df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics[which(topics(text_lda,1)==8)])),]
(dim(df[df$timestamp < 1651449600,][1]))#before leak
(dim(df[df$timestamp < 1663977600 & df$timestamp >= 1651449600,][1]))#leak till 3mo after decision
(dim(df[df$timestamp >= 1663977600,][1]))#beyond 3mo after decision


#######################
## stacked bar chart ##
#######################
data_wide <- poisson.dataset[,4:12]
library(tidyr)
data_long <- pivot_longer(data_wide, cols=paste0('Topicwordcount',1:k), 
                          names_to = 'topic', values_to = 'Topicwordcount')
data_long$month <- as.character(data_long$month)
data_long$topic <- substr(data_long$topic, 15,18)
data_long$Topicwordcount <- data_long$Topicwordcount/1000
library(ggplot2)
png('stacked-bar-chart_2x.png', width = 1420, height = 402)
ggplot(data_long, aes(x=month, y=Topicwordcount, fill=topic)) + 
  geom_bar(stat = "identity") + theme_bw() + scale_y_continuous(breaks=seq(0,14,by=2),limits=c(0,14)) +
  labs(x = 'Month', y = 'Number of Words (Thousands)', fill='Topic',
       title = 'Number of Words Posted on r/TwoXChromosomes by Month and Topic') + 
  scale_fill_manual(values=c('#FDAF91','#ABCD72','#ADE2D0','#FF95A8','#8A4198','#FAE48B','#9C9EDE','skyblue'),
                    labels = c('1: Contraception Advice','2: Pregancy and\n    Contraception Access',
                               '3: Roe v. Wade','4: LARC Insertion','5: Breast Implants','6: LARC Removal',
                               '7: Periods and Bleeding','8: Male Partners'),
                    name ='Topic') + 
  scale_x_discrete(breaks= sort(unique(data_long$month))[seq(2,25, by=2)],
                   labels = c('July 2021','September 2021','November 2021','January 2022','March 2022','May 2022',
                              'July 2022','September 2022','November 2022','January 2023','March 2023','May 2023')) + 
                   #labels = c('June 2021','July 2021','August 2021','September 2021','October 2021','November 2021','December 2021','January 2022','February 2022','March 2022','April 2022','May 2022',
                    #          'June 2022','July 2022','August 2022','September 2022','October 2022','November 2022','December 2022','January 2023','February 2023','March 2023','April 2023','May 2023','June 2023')) + 
  theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1,size=15),
        axis.title = element_text(size=18), plot.title = element_text(size=20),
        legend.title = element_text(size=18), legend.text=element_text(size=15))+ 
  geom_vline(xintercept = "2022-05-01", col='#7C878E', lty=2) + #the leak 
  geom_vline(xintercept = "2022-06-01", col='#7C878E',lty = 2) + #the decision
  ggplot2::annotate("text", x="2022-05-01", y=14, label= 'bold("Leak            ")', col='#7C878E',parse=T, size=5) +
  ggplot2::annotate("text", x="2022-07-01", y=14, label= 'bold("Decision   ")', col='#7C878E',parse=T,size=5)
dev.off()


NDATA$topic <- factor(NDATA$topic, levels=c('1','2','3','4','5','6','7','8'))
png('poissonregression_plot_2x.png', width=863,height=465)
ggplot(NDATA, aes(group=topic, color=topic,x = timestamp,y=responsefit)) +  geom_line() + 
  geom_ribbon(aes(x = timestamp,ymin=responseLB, ymax=responseUB, fill=topic), alpha=.1,show.legend = F) +theme_bw() + 
  labs(y='Predicted Proportion of Words (95% CI)',x='Posting Date',color='Topic', 
       title ='Predicted Proportions of Words by Topic and Posting Date', subtitle = '  r/TwoXChromosomes') +
  geom_vline(xintercept = 1651449600, col='#7C878E', lty=2) + #the leak 
  geom_vline(xintercept = 1656028800, col='#7C878E',lty = 2) + #the decision
  scale_fill_manual(values=c('#FDAF91','#ABCD72','#ADE2D0','#FF95A8','#8A4198','#FAE48B','#9C9EDE','skyblue')) +
  scale_color_manual(values=c('#FDAF91','#ABCD72','#ADE2D0','#FF95A8','#8A4198','#FAE48B','#9C9EDE','skyblue'),
                     labels = c('1: Contraception Advice','2: Pregancy and\n    Contraception Access',
                                '3: Roe v. Wade','4: LARC Insertion','5: Breast Implants','6: LARC Removal',
                                '7: Periods and Bleeding','8: Male Partners')) +  
  ggplot2::annotate("text", x=1651449600-3000000, y=.3, label= 'bold("Leak")', col='#7C878E',parse=T,size=5) +
  ggplot2::annotate("text", x=1656028800+7000000, y=.3, label= 'bold("Decision            ")', col='#7C878E',parse=T,size=5) +
  scale_x_continuous(breaks = c(1625097600,1630454400,1635724800,1640995200,1646092800,1651363200,
                                1656633600,1661990400,1667260800,1672531200,1677628800,1682899200), 
                     labels = c('July 2021','September 2021','November 2021','January 2022','March 2022','May 2022',
                                'July 2022','September 2022','November 2022','January 2023','March 2023','May 2023')) + 
  theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1), axis.title = element_text(size=15), 
        plot.title = element_text(size=20),plot.subtitle = element_text(size=20),
        legend.title = element_text(size=18), legend.text=element_text(size=15)) + 
  guides(colour = guide_legend(override.aes = list(linewidth = 3))) + 
  scale_y_continuous(breaks = c(seq(0,.30, by=.05)), limits = c(0, .30))
dev.off()

##########################################
## mississippi/supreme court pie charts ##
##########################################
##Create the document-term matrix
library(tm)
text <- readLines('../step1-create-the-corpora/vcorpus_infile_rTwoXChromosomes.txt')
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
key.table <- data.frame(keepinds=which(row_sums(text_dtm) > 0), newinds=1:length(which(row_sums(text_dtm) > 0)))

#pie chart for supreme court/mississippi documents only
supreme.court <- c(3,83,169,170,180,221,392,394,411,428,460,489,572,637,855,864,871,881,904,906,907,909,917,919,924,930,937,942,954,983,987,1014,1054,1113,
                   1169,1178,1181,1183,1186,1189,1190,1209,1233,1249,1267,1283,1287,1343,1349,1349,1353,1362,1375,1391,1403,1418,1419,1569,1592,1598,1610,1628,
                   1745,1746,1760,1870,1921,2244,2261)
roe <- c(169,170,174,202,328,384,386,392,393,398,411,448,453,489,572,621,704,856,858,859,861,862,863,864,871,872,874,
         878,886,889,894,895,896,898,899,900,901,904,906,907,909,912,915,918,919,920,921,923,924,930,934,937,939,940,942,945,946,948,950,952,954,955,957,
         961,962,964,969,974,977,980,982,983,987,992,1004,1013,1014,1018,1030,1036,1037,1046,1063,1072,1105,1118,1134,1154,1166,1167,1169,1170,
         1171,1177,1180,1181,1183,1184,1187,1191,1197,1199,1200,1210,1213,1214,1216,1217,1224,1230,1232,1240,1242,1248,1250,1255,
         1275,1278,1287,1289,1293,1307,1308,1310,1318,1343,1345,1349,1350,1351,1353,1362,1363,1365,1371,1375,1391,1418,1429,
         1472,1491,1516,1534,1551,1559,1569,1578,1610,1628,1684,1745,1746,1750,1760,1810,1821,1925,2006,2040,2064,
         2142,2176,2227,2244,2271,2297,2380)
rvw <- c(392,885,913,922,923,929,940,1018,1171,1186,1212,1226,1280,1289,1312,1349,1391,
         1477,1572,1597,1760,1810,2008,2049,2227,2245)
dobbs.dataset <- poisson.dataset[which(key.table$keepinds %in% union(union(supreme.court, roe),rvw)),]
#pie(sort(apply(dobbs.dataset[,c(-1,-2,-3,-4,-13)], 2, sum)))

#convert to posts instead of words
dobbs.docs.dataset <- dobbs.dataset[,1:4]
for(i in 1:k){
  df <- dobbs.dataset[dobbs.dataset$doc %in% as.numeric(names(topics(text_lda,1)[which(topics(text_lda,1)==i)])),]
  summary <- df %>%group_by(date) %>%
    summarize(Ndocs=n()) 
  names(summary)[2] <- paste0("Topic",i,"docs")
  dobbs.docs.dataset <- merge(dobbs.docs.dataset, summary, by='date',all=T)
}
dobbs.docs.dataset$doc <- NULL
dobbs.docs.dataset <- unique(dobbs.docs.dataset)
dobbs.docs.dataset2 <- dobbs.docs.dataset %>%group_by(date) %>%
  summarize(Timestamp=min(timestamp))
dobbs.docs.dataset <- merge(dobbs.docs.dataset, dobbs.docs.dataset2, by='date',all=T)
dobbs.docs.dataset$timestamp <- dobbs.docs.dataset$doc <- NULL
dobbs.docs.dataset <- unique(dobbs.docs.dataset)
dobbs.dataset <- dobbs.docs.dataset
names(dobbs.dataset)[length(names(dobbs.dataset))] <- 'timestamp'
dobbs.dataset[is.na(dobbs.dataset)] <- 0


dobbs.dataset_long <- pivot_longer(dobbs.dataset[,c(-1,-2,-11)], cols=paste0('Topic',1:k,'docs'), 
                                   names_to = 'topic', values_to = 'doccount')
dobbs.dataset_long$topic <- substr(dobbs.dataset_long$topic, 6,6)
dobbs.dataset_long$doccount <- as.numeric(dobbs.dataset_long$doccount)
df <- aggregate(doccount ~ topic, data = dobbs.dataset_long, FUN = sum)
png('pie_2x.png', width = 541, height = 382)
ggplot(df, aes(x = "", y = doccount, fill = topic)) +
  geom_col(color = "black") +
  #geom_text(aes(label = topic),position = position_stack(vjust = .5),size = 7) + 
  theme_void() + #theme(legend.position = "none") +
  coord_polar(theta = "y") + labs(title='Proportions of Posts in Subset of Posts\nContaining "supreme court," "roe," or "rvw"\nby Topic', subtitle = 'r/TwoXChromosomes') + 
  theme(plot.title = element_text(size=25),plot.subtitle = element_text(size=18),
        legend.title = element_text(size=20), legend.text=element_text(size=18)) + 
  scale_fill_manual(values=c('#FDAF91','#ABCD72','#ADE2D0','#FF95A8','#8A4198','#FAE48B','#9C9EDE','skyblue'),
                    labels = c('1: Contraception Advice','2: Pregancy and\n    Contraception Access',
                               '3: Roe v. Wade','4: LARC Insertion','5: Breast Implants','6: LARC Removal',
                               '7: Periods and Bleeding','8: Male Partners'),
                    name ='Topic') 
dev.off()  


timestamp.df <- data.frame(timestamp=union(metadata$timestamp[roe], metadata$timestamp[supreme.court]),
                           vresponsefit=rep(1, length(union(metadata$timestamp[roe], metadata$timestamp[supreme.court]))))
png('violin_2x.png', width=663,height=284)
ggplot(timestamp.df, aes(x=timestamp, y=vresponsefit)) + 
  geom_violin() + geom_boxplot(width=.15) + theme_bw() + 
  labs(y='',x='Posting Date',color='Topic', 
       title ='Posting Dates of Subset of Posts Containing "supreme court," "roe," or "rvw"', subtitle = '  r/TwoXChromosomes') +
  scale_x_continuous(breaks = c(1625097600,1630454400,1635724800,1640995200,1646092800,1651363200,
                                1656633600,1661990400,1667260800,1672531200,1677628800,1682899200), 
                     labels = c('July 2021','September 2021','November 2021','January 2022','March 2022','May 2022',
                                'July 2022','September 2022','November 2022','January 2023','March 2023','May 2023')) +
  coord_cartesian(xlim = c(1622505600,1688169600), expand = 0) +
  theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1), axis.title = element_text(size=15), 
        plot.title = element_text(size=18),plot.subtitle = element_text(size=18),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) + 
  geom_vline(xintercept = 1651449600, col='#7C878E', lty=2) + #the leak 
  geom_vline(xintercept = 1656028800, col='#7C878E',lty = 2)  #the decision
dev.off()



##Redefine Poisson Dataset
###############################################################################################################################
rm(list=ls())
k <- 8
load(file='../step2-run-LDA/text_lda8_twoxchromosomes.RData')
#the 10 most likely terms for each topic
library(topicmodels)
as.matrix(terms(text_lda,10))

word.assignments <- data.frame(doc = text_lda@wordassignments$i, 
                               termnum = text_lda@wordassignments$j, 
                               topic = text_lda@wordassignments$v)
metadata <- read.csv('../step1-create-the-corpora/metadata_TwoXChromosomes.csv',header=F)
metadata <- metadata[-c(87, 1540, 1541, 1542, 1834, 2047, 2129),] #these 7 documents got removed when the vocabulary was reduced
names(metadata) <- c('id', 'timestamp', 'permalink')
timestamp.assignments <- data.frame(doc = 1:dim(metadata)[1], timestamp=metadata$timestamp)
data <- merge(word.assignments, timestamp.assignments, by='doc', all=T)

library(anytime)
data$date <- anydate(data$timestamp)
library(lubridate)
data$month <- floor_date(data$date, unit = 'month')
for(i in 1:k){
  wordcount <- aggregate(data$topic, by=list(data$doc), FUN = function(x){sum(x==i)})
  colnames(wordcount) <- c('doc', 'Topicwordcount')
  data <- merge(data, wordcount,all=T)
  colnames(data) <- c(colnames(data)[1:length(colnames(data))-1], paste('Topicwordcount',i, sep=''))
}

poisson.dataset <- unique(data[,-c(2:3)])
doclengths <- aggregate(data$topic, by=list(data$doc), FUN = function(x){length(x)})
names(doclengths) <- c('doc','doclength')
poisson.dataset <- merge(poisson.dataset, doclengths, by='doc', all=T)
#save(poisson.dataset, file='poisson.dataset.RData')

poisson.docs.dataset <- poisson.dataset[,1:4]
for(i in 1:k){
  df <- poisson.dataset[poisson.dataset$doc %in% as.numeric(names(topics(text_lda,1)[which(topics(text_lda,1)==i)])),]
  summary <- df %>%group_by(date) %>%
    summarize(Ndocs=n()) 
  names(summary)[2] <- paste0("Topic",i,"docs")
  poisson.docs.dataset <- merge(poisson.docs.dataset, summary, by='date',all=T)
}
poisson.docs.dataset$doc <- NULL
poisson.docs.dataset <- unique(poisson.docs.dataset)
poisson.docs.dataset2 <- poisson.docs.dataset %>%group_by(date) %>%
  summarize(Timestamp=min(timestamp))
poisson.docs.dataset <- merge(poisson.docs.dataset, poisson.docs.dataset2, by='date',all=T)
poisson.docs.dataset$timestamp <- poisson.docs.dataset$doc <- NULL
poisson.docs.dataset <- unique(poisson.docs.dataset)
poisson.dataset <- poisson.docs.dataset
names(poisson.dataset)[length(names(poisson.dataset))] <- 'timestamp'
poisson.dataset[is.na(poisson.dataset)] <- 0


########################################
## piecewise Poisson regression model ##
########################################
#time 0 (reference category) = before leak
#time 1 = leak until 3mo post decision
#time 2 = after 3mo post decision
poisson.dataset$time2 <- ifelse(poisson.dataset$timestamp >= 1651449600 & poisson.dataset$timestamp < 1663977600, 1, 0)
poisson.dataset$time3 <- ifelse(poisson.dataset$timestamp >= 1663977600, 1, 0)

fit <- glm(Topic1docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

fit <- glm(Topic2docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

fit <- glm(Topic3docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

fit <- glm(Topic4docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

fit <- glm(Topic5docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

fit <- glm(Topic6docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

fit <- glm(Topic7docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

fit <- glm(Topic8docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

overall.docs <- apply(poisson.dataset[,3:10], 1, sum)
fit <- glm(overall.docs ~ time2 + time3 #+ offset(log(doclength))
           , family=poisson(link=log), data=poisson.dataset)
(as.data.frame(summary(fit)$coef))           #coefficient estimates
View(data.frame(RR = exp(summary(fit)$coef[,1]),
                RR.lb  = exp(summary(fit)$coef[,1] - qnorm(1-.025)*summary(fit)$coef[,2]),
                RR.ub = exp(summary(fit)$coef[,1] + qnorm(1-.025)*summary(fit)$coef[,2]),
                p.val = 2*pnorm(abs(summary(fit)$coef[,1]/summary(fit)$coef[,2]), lower.tail=F)
))        #relative risks

poisson.dataset_long <- pivot_longer(poisson.dataset[,c(-1,-2,-11)], cols=paste0('Topic',1:k,'docs'), 
                                     names_to = 'topic', values_to = 'doccount')
poisson.dataset_long$topic <- substr(poisson.dataset_long$topic, 6,6)
poisson.dataset_long$doccount <- as.numeric(poisson.dataset_long$doccount)
df <- aggregate(doccount ~ topic, data = poisson.dataset_long, FUN = sum)
png('pie_2x_overall.png', width = 541, height = 382)
ggplot(df, aes(x = "", y = doccount, fill = topic)) +
  geom_col(color = "black") +
  #geom_text(aes(label = topic),position = position_stack(vjust = .5),size = 7) + 
  theme_void() + #theme(legend.position = "none") +
  coord_polar(theta = "y") + labs(title='Proportions of Posts by Topic', subtitle = 'r/TwoXChromosomes') +
  theme(plot.title = element_text(size=25),plot.subtitle = element_text(size=18),
        legend.title = element_text(size=20), legend.text=element_text(size=18)) + 
  scale_fill_manual(values=c('#FDAF91','#ABCD72','#ADE2D0','#FF95A8','#8A4198','#FAE48B','#9C9EDE','skyblue'),
                    labels = c('1: Contraception Advice','2: Pregancy and\n    Contraception Access',
                               '3: Roe v. Wade','4: LARC Insertion','5: Breast Implants','6: LARC Removal',
                               '7: Periods and Bleeding','8: Male Partners'),
                    name ='Topic') 
dev.off()















