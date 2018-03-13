
library(ggplot2); library(NLP); library(tm)
#creating the corpus from the documents contained in folder "Topic mining pause"
docspause<- Corpus(DirSource("C:/Users/Aditya Rai/Desktop/capstone/Topic mining pause"))

# Convert all the alphabets to lower case in the "docspause" corpus
docspause = tm_map(docspause, content_transformer(tolower))
writeLines(as.character(docspause))
#remove punctuation
docspause <- tm_map(docspause, removePunctuation)
#Strip digits
docspause <- tm_map(docspause, removeNumbers)
#remove stopwords
docspause <- tm_map(docspause, removeWords, stopwords("en"))
myStopwords3 = c("romp","just","thank","like","make","month","roll","will","abl","now","son","get")
docspause = tm_map(docspause, removeWords, myStopwords3)
#remove whitespace
docspause <- tm_map(docspause, stripWhitespace)
#steming the document
docspause <- tm_map(docspause,stemDocument)

#Create document-term matrix
dtmpause <- DocumentTermMatrix(docspause)

inspect(dtmpause[3:5,1000:1005])
# mining the corpus
freqpause = colSums(as.matrix(dtmpause))
length(freqpause)
ordpause = order(freqpause, decreasing = TRUE)
freqpause[head(ordpause)]; freqpause[tail(ordpause)]
dtmrpause <-DocumentTermMatrix(docspause, control=list(wordLengths=c(4, 20), bounds = list(global = c(1,15))))

freqpause2 = colSums(as.matrix(dtmrpause)); length(freqpause2)
ordrpause = order(freqpause2, decreasing = TRUE)
freqpause2[head(ordrpause,25)]; freqpause2[tail(ordrpause)]
freqpause2[ordrpause>quantile(ordrpause, probs = .5)]
str(ordpause)
findFreqTerms(dtmrpause, lowfreq = 60)

findAssocs(dtmrpause, "preschool", 0.85)
wfpause=data.frame(term=names(freqpause2),occurrences=freqpause2)
ppause <- ggplot(subset(wfpause, freqpause2>60), aes(term, occurrences))
ppause <- ppause + geom_bar(stat="identity", color = "blue", fill = "red")
ppause <- ppause + theme(axis.text.x=element_text(angle=45, hjust=1))
ppause

#wordcloud
library(wordcloud); library(RColorBrewer)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqpause2),freqpause2, min.freq=60,colors=brewer.pal(6,"Dark2"))

#Topic modeling: In this step a given number of topics (in this case 6) are found out based on
#the occurence of words in the corpus
library(topicmodels)
burnin <- 400
iter <- 2000
thin <- 500
seed <-list(2003,5,63,01,765)
nstart <- 5
k <- 4
best = TRUE
ldaOutpause <-LDA(dtmrpause,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaTopics = as.matrix(topics(ldaOutpause))
ldaOutpause.terms <- as.matrix(terms(ldaOutpause,6))


