load(file='../step2-run-LDA/perplexity_mat_twoxchromosomes.RData')
perplexity_mat_TwoX <- perplexity_mat
perplexity_mat_TwoX$subreddit <- 'r/TwoXChromosomes'
load(file='../step2-run-LDA/perplexity_mat_birthcontrol.RData')
perplexity_mat_birthcontrol <- perplexity_mat
perplexity_mat_birthcontrol$subreddit <- 'r/birthcontrol'
perplexity_mat <- rbind(perplexity_mat_birthcontrol, perplexity_mat_TwoX)
library(tidyr)
perplexity_mat_long <- pivot_longer(perplexity_mat, cols=c('train_perplexity', 'test_perplexity'),
                                    names_to='perplexity.type', values_to='Perplexity')


library(ggplot2)
png('train-test-perplexity.png', width = 717, height = 428)
ggplot(data=perplexity_mat_long[perplexity_mat_long$k != 1,], aes(x=k, y=Perplexity, col=subreddit, lty=perplexity.type)) + 
  geom_point() + geom_line() +  
  theme_bw() + labs(x='Number of Topics (K)',y='Perplexity') +
  scale_color_manual(name='Subreddit', values = c('#FF95A8','skyblue')) +
  scale_linetype_manual(name='Type of Perplexity',values=c(2,1),
                        labels=c('Test Perplexity','Training Perplexity')) + 
  ggtitle('LDA Model Perplexity vs. Number of Topics') + 
  scale_x_continuous(breaks=2:15)
dev.off()             


load(file='../step2-run-LDA/perplexity_mat_birthcontrol.RData')
percent.changes <- NULL
per <- perplexity_mat$test_perplexity
for(k in 3:15){
  percent.changes <- c(percent.changes, (per[k]-per[k-1])/per[k-1] )
} 
pc.df <- data.frame(index=3:15, percent.change = percent.changes, subreddit = 'r/birthcontrol')  
load(file='../step2-run-LDA/perplexity_mat_twoxchromosomes.RData')
percent.changes <- NULL
per <- perplexity_mat$test_perplexity
for(k in 3:15){
  percent.changes <- c(percent.changes, (per[k]-per[k-1])/per[k-1] )
} 
pc.df <- rbind(pc.df, data.frame(index=3:15, percent.change = percent.changes, subreddit = 'r/TwoXChromosomes'),
               by='index', all=T ) 
library(Hmisc)
pc.df <- pc.df[pc.df$percent.change %nin% c('index','TRUE'),]
pc.df$percent.change <- as.numeric(pc.df$percent.change)
pc.df$index <- as.numeric(pc.df$index)

# Custom Y-axis labels 
labels <- function(x) {
  paste0(c(2:15)[x-1], " vs.", c(2:15)[x])
}
png('percent-changes.png', width = 717, height = 428)
ggplot(data=pc.df, aes(x=index, y=percent.change*100, col=subreddit)) + 
  geom_point() + geom_line() + geom_hline(yintercept = -1, col='grey',lty=2) + 
  theme_bw() + labs(x='Number of Topics (K)',y='Percent Change in Test Perplexity') +
  scale_color_manual(name='Subreddit', values = c('#FF95A8','skyblue')) + 
  ggtitle('Percent Change in Test Perplexity vs. Number of Topics') + 
  scale_x_continuous(breaks=3:15, label = labels(2:14)) +
  scale_y_continuous(breaks=-10:0)+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
  geom_point(aes(x=7,y=-3.496718e-03*100), color='black', size=2) +
  geom_text(aes(x=7,y=-3.496718e-03*100, label = "Choice:  K = 6"), colour = "black", 
            size=4, hjust = -0.1, vjust = -0.2, check_overlap = TRUE ) + 
  geom_point(aes(x=9,y=-9.032594e-03*100), color='black', size=2) +
  geom_text(aes(x=9,y=-9.032594e-03*100, label = "Choice:  K = 8"), colour = "black", 
            size=4, hjust = -0.1, vjust = -0.2, check_overlap = TRUE )
dev.off()                        


#load(file='../step2-run-LDA/perplexity_mat_twoxchromosomes.RData')
#percent.changes <- NULL
#per <- perplexity_mat$test_perplexity
#for(k in 3:15){
#  percent.changes <- c(percent.changes, (per[k]-per[k-1])/per[k-1] )
#}
#plot(percent.changes, type='b')
#abline(h=-.01, col='blue')
#abline(h=-.02, col='black')
#legend(10, -.06, legend=c("-1%", "-2%"),
#       col=c("blue", "black"), lty=1, cex=0.8)



