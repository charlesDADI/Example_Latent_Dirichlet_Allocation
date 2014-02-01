install.packages("ggplot2")
install.packages("ltm")
library(ggplot2)
library(reshape2)
library(ltm)

library(tm.plugin.webmining)
library(lda)
data(cora.documents)
data(cora.vocab)
theme_set(theme_bw())  
set.seed(8675309)


source("/home/cerveau2charles/github/Sentiment_mining/cleaning_text_function.R")

Y_news1=WebCorpus( YahooNewsSource("Eurusd") )
Y_news2=WebCorpus(YahooNewsSource("Europe"))
Y_news3=WebCorpus(YahooNewsSource("Federer"))

Y_news=c(as.character(Y_news1),as.character(Y_news2),as.character(Y_news3))
Y_treated=Pretraitement(as.character(Y_news),langue="english",remove_stop_word=c("up","down"))

doc=lexicalize(Y_treated)
#lda_test=lda.collapsed.gibbs.sampler(doc$documents,K=2,vocab=doc$vocab,num.iterations=100,alpha=c(1,2),eta=c(0.5,3))


K <- 5 ## Num clusters
result <- lda.collapsed.gibbs.sampler(doc$documents,
                                       K,  ## Num clusters
                                       vocab=doc$vocab,
                                       500,  ## Num iterations
                                      0.5,
                                       0.5,
                                     compute.log.likelihood=TRUE) 

 ## Get the top words in the cluster
 top.words <- top.topic.words(result$topics, 5, by.score=TRUE)

 ## Number of documents to display
 N<-10
 topic.proportions <- t(result$document_sums) / colSums(result$document_sums)

topic.proportions <-
   topic.proportions[sample(1:dim(topic.proportions)[1], N),]
 topic.proportions[is.na(topic.proportions)] <-  1 / K

 colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")

 topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                    document=factor(1:N)),
                              variable.name="topic",
                             id.vars = "document")  
dev.new()
 qplot(topic, value, fill=document, ylab="proportion",
       data=topic.proportions.df, geom="bar") +
   opts(axis.text.x = theme_text(angle=90, hjust=1)) +  
   coord_flip() +
   facet_wrap(~ document, ncol=5)




