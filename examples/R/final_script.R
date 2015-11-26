
"""
Com a exemple farem servir 4 start-ups que estan oferint continuament varis 
descomptes i cupons per atraure els seus clients. 
    - Meru Cabs
    - Ola Cabs
    - TaxiForSure 
    - Uber India
"""

"""
Primer ens cal autentificar-nos a Twitter per poder tenir accés als tweets que 
ens interessi. Per això anteriorment caldrà haver crear una aplicació a twitter
"""
#==============
#AUTHENTICATION
#==============

#install.packages('base64enc')
library(devtools)
#install_github("twitteR", username="geoffjentry")

require(twitteR)
api_key <- "DRoNBqwlZz2XzcJy2oybBHBUk"
api_secret <- "mw3WFrzLVFLMA5LeWOmsY6yB5EQrSeaUfFwcaP2Alw5v0unAxI"
access_token <- "1585120602-ov5qyNRY3WMrvWPuGnqcYjeeLzb8FUwsjlWefZw"
access_token_secret <- "2XjXnXziOT0LEpblWY9zG5mxbgKkALf4xbjKoZhKG1mUA"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
rm(api_key)
rm(api_secret)
rm(access_token)
rm(access_token_secret)



#==========================
#GET THE TWEETS AS A CORPUS
#==========================

Meru_tweets = searchTwitter("MeruCabs", n=2000, lang="en")
Ola_tweets = searchTwitter("OlaCabs", n=2000, lang="en")
TaxiForSure_tweets = searchTwitter("TaxiForSure", n=2000, lang="en")
Uber_tweets = searchTwitter("Uber_Delhi", n=2000, lang="en")



#===================
#CLEANING THE CORPUS
#===================
head(Meru_tweets)
"""
Veiem que el corpus és molt sorollós, per tant l'hem de netejar. Ho farem fent servir les següents funcions
"""

MeruTweets = sapply(Meru_tweets, function(x) x$getText())
OlaTweets = sapply(Ola_tweets, function(x) x$getText())
TaxiForSureTweets = sapply(TaxiForSure_tweets, function(x) x$getText())
UberTweets = sapply(Uber_tweets, function(x) x$getText())

catch.error = function(x)
{
    y = NA
    catch_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(catch_error, "error"))
    y = tolower(x)
    return(y)
}

cleanTweets<- function(tweet){
    tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
    tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
    tweet = gsub("#\\w+", " ", tweet)
    tweet = gsub("@\\w+", " ", tweet)
    tweet = gsub("[[:punct:]]", " ", tweet)
    tweet = gsub("[[:digit:]]", " ", tweet)
    tweet = gsub("[ \t]{2,}", " ", tweet)
    tweet = gsub("^\\s+|\\s+$", "", tweet)
    tweet = catch.error(tweet)
    tweet
}

cleanTweetsAndRemoveNAs <- function(Tweets) {
    TweetsCleaned = sapply(Tweets, cleanTweets)
    TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
    names(TweetsCleaned) = NULL
    TweetsCleaned = unique(TweetsCleaned)
    TweetsCleaned
}
MeruTweetsCleaned = cleanTweetsAndRemoveNAs(MeruTweets)
OlaTweetsCleaned = cleanTweetsAndRemoveNAs(OlaTweets)
TaxiForSureTweetsCleaned = cleanTweetsAndRemoveNAs(TaxiForSureTweets)
UberTweetsCleaned = cleanTweetsAndRemoveNAs(UberTweets)
"""
Ara que ja tenim el nostre corpus net, podem anar al nostre objectiu: Una estimació dels sentiments
"""

#================================================
#VERSION A. ESTIMATING SENTIMENT: Naive Algorithm
#================================================
"""
Hi han molts recursos disponibles per a fer aquesta estimació de sentiments. 
Val la pena comentar que no tots els tweets han de representar sentiments. 
Alguns tweets poden ser simplements d'informació, mentre que d'altres poden 
ser respostes de clients. 
"""
"""
En aquest cas farem servir un algoritme Naive, que dóna una puntuació basada 
en el nº de vegades que hi ha una paraula positiva o negativa en una frase (
en el nostre cas, en un tweet)
"""
"""
Farem servir una llibreria que té unes 68000 paraules positives i negatives en anglès.
"""
opinion.lexicon.pos = scan('MachineLearning/Mastering_Social_Media_Mining_with_R_Code/Chapter2/code/opinion-lexicon-English/positive-words.txt',what='character', comment.char=';')
opinion.lexicon.neg = scan('MachineLearning/Mastering_Social_Media_Mining_with_R_Code/Chapter2/code/opinion-lexicon-English/negative-words.txt',what='character', comment.char=';')
"""
Afegim algunes paraules customitzades
"""
words.positive= c(opinion.lexicon.pos,'upgrade')
words.negative = c(opinion.lexicon.neg,'wait','waiting', 'wtf', 'cancellation')
"""
Fem una funció que calculi la puntuació basat en un algoritme bàsic de Machine Learning
"""
getSentimentScore = function(sentences, words.positive, words.negative, .progress='none') {
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, words.positive, words.negative) {
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        sentence = tolower(sentence)
        words = unlist(str_split(sentence, '\\s+'))
        pos.matches = !is.na(match(words, words.positive))
        neg.matches = !is.na(match(words, words.negative))
        score = sum(pos.matches) - sum(neg.matches)
        return(score)
    }, 
    words.positive, words.negative, .progress=.progress )
    return(data.frame(text=sentences, score=scores))
}

MeruResult = getSentimentScore(MeruTweetsCleaned, words.positive, words.negative)
OlaResult = getSentimentScore(OlaTweetsCleaned, words.positive, words.negative)
TaxiForSureResult = getSentimentScore(TaxiForSureTweetsCleaned, words.positive, words.negative) 
UberResult = getSentimentScore(UberTweetsCleaned, words.positive, words.negative)

"""
Podem anar a veure els histogrames
"""
hist(MeruResult$score)
hist(OlaResult$score)
hist(TaxiForSureResult$score)
hist(UberResult$score)

"""
Podem anar a veure les mitjanes
"""
mean(MeruResult$score)
mean(OlaResult$score)
mean(TaxiForSureResult$score)
mean(UberResult$score)


#===============================
#VERSION B. ESTIMATING SENTIMENT
#===============================
"""
La segona versió serà una mica més complexe: en comptes d'utilitzar un simple matching, 
farem servir el què se'n diu Naive Bayes per decidir quina emoció hi ha en cada tweet.
"""
library(sentiment)
MeruTweetsClassEmo = classify_emotion(MeruTweetsCleaned, algorithm="bayes", prior=1.0)
OlaTweetsClassEmo = classify_emotion(OlaTweetsCleaned, algorithm="bayes", prior=1.0)
TaxiForSureTweetsClassEmo = classify_emotion(TaxiForSureTweetsCleaned, algorithm="bayes", prior=1.0)
UberTweetsClassEmo = classify_emotion(UberTweetsCleaned, algorithm="bayes", prior=1.0)

MeruEmotion = MeruTweetsClassEmo[,7]
OlaEmotion = OlaTweetsClassEmo[,7]
TaxiForSureEmotion = TaxiForSureTweetsClassEmo[,7]
UberEmotion = UberTweetsClassEmo[,7]

MeruEmotion[is.na(MeruEmotion)] = "unknown"
OlaEmotion[is.na(OlaEmotion)] = "unknown"
TaxiForSureEmotion[is.na(TaxiForSureEmotion)] = "unknown"
UberEmotion[is.na(UberEmotion)] = "unknown"

head(MeruEmotion, 20)
head(OlaEmotion, 20)
head(TaxiForSureEmotion, 20)
head(UberEmotion, 20)


#POLARITY
#========
"""
A més a més, podem classificar també cada tweet en dos grups: Positiu i Negatiu. 
"""
MeruTweetsClassPol = classify_polarity(MeruTweetsCleaned, algorithm="bayes")
OlaTweetsClassPol = classify_polarity(OlaTweetsCleaned, algorithm="bayes")
TaxiForSureTweetsClassPol = classify_polarity(TaxiForSureTweetsCleaned, algorithm="bayes")
UberTweetsClassPol = classify_polarity(UberTweetsCleaned, algorithm="bayes")

head(MeruTweetsClassPol, 20)
head(OlaTweetsClassPol, 20)
head(TaxiForSureTweetsClassPol, 20)
head(UberTweetsClassPol, 20)

MeruPol = MeruTweetsClassPol[,4]
OlaPol = OlaTweetsClassPol[,4]
TaxiForSurePol = TaxiForSureTweetsClassPol[,4]
UberPol = UberTweetsClassPol[,4]

MeruSentimentDataFrame = data.frame(text=MeruTweetsCleaned, emotion=MeruEmotion, polarity=MeruPol, stringsAsFactors=FALSE)
OlaSentimentDataFrame = data.frame(text=OlaTweetsCleaned, emotion=OlaEmotion, polarity=OlaPol, stringsAsFactors=FALSE)
TaxiForSureSentimentDataFrame = data.frame(text=TaxiForSureTweetsCleaned, emotion=TaxiForSureEmotion, polarity=TaxiForSurePol, stringsAsFactors=FALSE)
UberSentimentDataFrame = data.frame(text=UberTweetsCleaned, emotion=UberEmotion, polarity=UberPol, stringsAsFactors=FALSE)

MeruSentimentDataFrame = within(MeruSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
OlaSentimentDataFrame = within(OlaSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
TaxiForSureSentimentDataFrame = within(TaxiForSureSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
UberSentimentDataFrame = within(UberSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


require(ggplot2)

plotSentiments1<- function (sentiment_dataframe,title) {
    library(ggplot2)
    ggplot(sentiment_dataframe, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') +
    xlab('Emotion Categories')
}

plotSentiments1(MeruSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about MeruCabs')
plotSentiments1(OlaSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about OlaCabs')
plotSentiments1(TaxiForSureSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about TaxiForSure')
plotSentiments1(UberSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about UberIndia')


plotSentiments2 <- function (sentiment_dataframe,title) {
    library(ggplot2)
    ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') +
    xlab('Polarity Categories')
}

plotSentiments2(MeruSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about MeruCabs')
plotSentiments2(OlaSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about OlaCabs')
plotSentiments2(TaxiForSureSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about TaxiForSure')
plotSentiments2(UberSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about UberIndia')


#==========
#WORD CLOUD
#==========

removeCustomeWords <- function (TweetsCleaned) {
    for(i in 1:length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
    TweetsCleaned[i] = removeWords(TweetsCleaned[i], c(stopwords("english"), "care", "guys", "can", "dis", "didn", "guy" ,"booked", "plz"))
        TweetsCleaned[i]
    }, error=function(cond) {
        TweetsCleaned[i]
    }, warning=function(cond) {
        TweetsCleaned[i]
    })
 }
 return(TweetsCleaned)
}

getWordCloud <- function (sentiment_dataframe, TweetsCleaned, Emotion) {
    emos = levels(factor(sentiment_dataframe$emotion))
    n_emos = length(emos)
    emo.docs = rep("", n_emos)
    TweetsCleaned = removeCustomeWords(TweetsCleaned)
    for (i in 1:n_emos){
        emo.docs[i] = paste(TweetsCleaned[Emotion ==
        emos[i]], collapse=" ")
    }
    corpus = Corpus(VectorSource(emo.docs))
    tdm = TermDocumentMatrix(corpus)
    tdm = as.matrix(tdm)
    colnames(tdm) = emos
    require(wordcloud)
    suppressWarnings(comparison.cloud(tdm, colors = brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order = FALSE, title.size = 1.5))
}

getWordCloud(MeruSentimentDataFrame, MeruTweetsCleaned, MeruEmotion)
getWordCloud(OlaSentimentDataFrame, OlaTweetsCleaned, OlaEmotion)
getWordCloud(TaxiForSureSentimentDataFrame, TaxiForSureTweetsCleaned, TaxiForSureEmotion)
getWordCloud(UberSentimentDataFrame, UberTweetsCleaned, UberEmotion)









