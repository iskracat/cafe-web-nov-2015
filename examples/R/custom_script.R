install.packages(c("devtools", "rjson", "bit64", "httr"))
# #RESTART R session!
install.packages('base64enc')
library(devtools)
install_github("twitteR", username="geoffjentry")

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

Meru_tweets = searchTwitter("MeruCabs", n=2000, lang="en")
Ola_tweets = searchTwitter("OlaCabs", n=2000, lang="en")
TaxiForSure_tweets = searchTwitter("TaxiForSure", n=2000, lang="en")
Uber_tweets = searchTwitter("Uber_Delhi", n=2000, lang="en")

MeruTweets = sapply(Meru_tweets, function(x) x$getText())
OlaTweets = sapply(Ola_tweets, function(x) x$getText())
TaxiForSureTweets = sapply(TaxiForSure_tweets, function(x) x$getText())
UberTweets = sapply(Uber_tweets, function(x) x$getText())

catch.error = function(x)
{
    # let us create a missing value for test purpose
    y = NA
    # Try to catch that error (NA) we just created
    catch_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(catch_error, "error"))
    y = tolower(x)
    # check result if error exists, otherwise the function works fine.
    return(y)
}
cleanTweets<- function(tweet){
    # Clean the tweet for sentiment analysis
    # remove html links, which are not required for sentiment analysis
    tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
    # First we will remove retweet entities from the stored tweets (text)
    tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
    # Then remove all "#Hashtag"
    tweet = gsub("#\\w+", " ", tweet)
    # Then remove all "@people"
    tweet = gsub("@\\w+", " ", tweet)
    # Then remove all the punctuation
    tweet = gsub("[[:punct:]]", " ", tweet)
    # Then remove numbers, we need only text for analytics
    tweet = gsub("[[:digit:]]", " ", tweet)
    # finally, we remove unnecessary spaces (white spaces, tabs etc)
    tweet = gsub("[ \t]{2,}", " ", tweet)
    tweet = gsub("^\\s+|\\s+$", "", tweet)
    # if anything else, you feel, should be removed, you can. For example "slang words" etc using the above function and methods.
    # Next we'll convert all the word in lower case.This makes uniform pattern.
    tweet = catch.error(tweet)
    tweet
}
cleanTweetsAndRemoveNAs <- function(Tweets) {
    TweetsCleaned = sapply(Tweets, cleanTweets)
    # Remove the "NA" tweets from this tweet list
    TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
    names(TweetsCleaned) = NULL
    # Remove the repetitive tweets from this tweet list
    TweetsCleaned = unique(TweetsCleaned)
    TweetsCleaned
}

MeruTweetsCleaned = cleanTweetsAndRemoveNAs(MeruTweets)
OlaTweetsCleaned = cleanTweetsAndRemoveNAs(OlaTweets)
TaxiForSureTweetsCleaned = cleanTweetsAndRemoveNAs(TaxiForSureTweets)
UberTweetsCleaned = cleanTweetsAndRemoveNAs(UberTweets)


#=======================
#A. ESTIMATING SENTIMENT
#=======================

#Get positive and negative words; posar el path que toqui
opinion.lexicon.pos = scan('MachineLearning/Mastering_Social_Media_Mining_with_R_Code/Chapter2/code/opinion-lexicon-English/positive-words.txt',what='character', comment.char=';')
opinion.lexicon.neg = scan('MachineLearning/Mastering_Social_Media_Mining_with_R_Code/Chapter2/code/opinion-lexicon-English/negative-words.txt',what='character', comment.char=';')

#We'll add a few industry-specific and/or especially emphatic terms based on our requirements:
pos.words = c(opinion.lexicon.pos,'upgrade')
neg.words = c(opinion.lexicon.neg,'wait','waiting', 'wtf', 'cancellation')

words.positive = pos.words
words.negative = neg.words
#  create a function, score.sentiment(), which computes the raw sentiment based on the simple matching algorithm:
getSentimentScore = function(sentences, words.positive, words.negative, .progress='none') {
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, words.positive, words.negative) {
        # Let first remove the Digit, Punctuation character and Control characters:
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        # Then lets convert all to lower sentence case:
        sentence = tolower(sentence)
        # Now lets split each sentence by the space delimiter
        words = unlist(str_split(sentence, '\\s+'))
        # Get the boolean match of each words with the positive & negative opinion-lexicon
        pos.matches = !is.na(match(words, words.positive))
        neg.matches = !is.na(match(words, words.negative))
        # Now get the score as total positive sentiment minus the total negatives
        score = sum(pos.matches) - sum(neg.matches)
        return(score)
    }, 
    words.positive, words.negative, .progress=.progress )
    # Return a data frame with respective sentence and the score
    return(data.frame(text=sentences, score=scores))
}

# we apply the preceding function to the corpus of tweets collected and cleaned so far:

MeruResult = getSentimentScore(MeruTweetsCleaned, words.positive, words.negative)
OlaResult = getSentimentScore(OlaTweetsCleaned, words.positive, words.negative)
TaxiForSureResult = getSentimentScore(TaxiForSureTweetsCleaned, words.positive, words.negative) 
UberResult = getSentimentScore(UberTweetsCleaned, words.positive, words.negative)


#Anem a representar els histogrames
hist(MeruResult$score)
hist(OlaResult$score)
hist(TaxiForSureResult$score)
hist(UberResult$score)

#Anem a calcular la mitjana
mean(MeruResult$score)
mean(OlaResult$score)
mean(TaxiForSureResult$score)
mean(UberResult$score)


#=======================
#B. ESTIMATING SENTIMENT
#=======================

#Let's now move one step further. Instead of using simple matching of opinion
#lexicon, we'll use something called Naive Bayes to decide on the emotion present
#in any tweet. 
install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")
require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz") 
require(sentiment)
ls("package:sentiment")

library(sentiment)
# classify_emotion function returns an object of class data frame 
# with seven columns (anger, disgust, fear, joy, sadness, surprise, 
# best_fit) and one row for each document:

MeruTweetsClassEmo = classify_emotion(MeruTweetsCleaned, algorithm="bayes", prior=1.0)
OlaTweetsClassEmo = classify_emotion(OlaTweetsCleaned, algorithm="bayes", prior=1.0)
TaxiForSureTweetsClassEmo = classify_emotion(TaxiForSureTweetsCleaned, algorithm="bayes", prior=1.0)
UberTweetsClassEmo = classify_emotion(UberTweetsCleaned, algorithm="bayes", prior=1.0)


#The sentiment package was built to use a trained dataset of emotion words (nearly
#1,500 words). The function classify_emotion() generates results belonging to one
#of the following six emotions: anger, disgust, fear, joy, sadness, and surprise. When
#the system is not able to classify the overall emotion as any of the six, NA is returned:


#Let's substitute these NA values with the word unknown to make further
#analysis easier:
# we will fetch emotion category best_fit for our analysis purposes.

MeruEmotion = MeruTweetsClassEmo[,7]
OlaEmotion = OlaTweetsClassEmo[,7]
TaxiForSureEmotion = TaxiForSureTweetsClassEmo[,7]
UberEmotion = UberTweetsClassEmo[,7]

MeruEmotion[is.na(MeruEmotion)] = "unknown"
OlaEmotion[is.na(OlaEmotion)] = "unknown"
TaxiForSureEmotion[is.na(TaxiForSureEmotion)] = "unknown"
UberEmotion[is.na(UberEmotion)] = "unknown"

#The best-fit emotions present in these tweets are as follows:
head(MeruEmotion, 20)
head(OlaEmotion, 20)
head(TaxiForSureEmotion, 20)
head(UberEmotion, 20)


#Further, we'll use another function, classify_polarity(), provided by the
#sentiment package, to classify the tweets into two classes, pos (positive sentiment)
#and neg (negative sentiment). The idea is to compute the log likelihood of a
#tweet, assuming it belongs to either of the two classes. Once these likelihoods are
#calculated, a ratio of the pos-likelihood to neg-likelihood is calculated, and, based on
#this ratio, the tweets are classified as belonging to a particular class. It's important
#to note that if this ratio turns out to be 1, then the overall sentiment of the tweet is
#assumed to be "neutral". The code is as follows:

MeruTweetsClassPol = classify_polarity(MeruTweetsCleaned, algorithm="bayes")
OlaTweetsClassPol = classify_polarity(OlaTweetsCleaned, algorithm="bayes")
TaxiForSureTweetsClassPol = classify_polarity(TaxiForSureTweetsCleaned, algorithm="bayes")
UberTweetsClassPol = classify_polarity(UberTweetsCleaned, algorithm="bayes")

#Lets see thw results
head(MeruTweetsClassPol, 20)
head(OlaTweetsClassPol, 20)
head(TaxiForSureTweetsClassPol, 20)
head(UberTweetsClassPol, 20)



#We'll now generate consolidated results from the two functions in a data frame 
#for each cab service for plotting purposes:

# we will fetch polarity category best_fit for our analysis purposes,
MeruPol = MeruTweetsClassPol[,4]
OlaPol = OlaTweetsClassPol[,4]
TaxiForSurePol = TaxiForSureTweetsClassPol[,4]
UberPol = UberTweetsClassPol[,4]
# Let us now create a data frame with the above results
MeruSentimentDataFrame = data.frame(text=MeruTweetsCleaned, emotion=MeruEmotion, polarity=MeruPol, stringsAsFactors=FALSE)
OlaSentimentDataFrame = data.frame(text=OlaTweetsCleaned, emotion=OlaEmotion, polarity=OlaPol, stringsAsFactors=FALSE)
TaxiForSureSentimentDataFrame = data.frame(text=TaxiForSureTweetsCleaned, emotion=TaxiForSureEmotion, polarity=TaxiForSurePol, stringsAsFactors=FALSE)
UberSentimentDataFrame = data.frame(text=UberTweetsCleaned, emotion=UberEmotion, polarity=UberPol, stringsAsFactors=FALSE)


# rearrange data inside the frame by sorting it
MeruSentimentDataFrame = within(MeruSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
OlaSentimentDataFrame = within(OlaSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
TaxiForSureSentimentDataFrame = within(TaxiForSureSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
UberSentimentDataFrame = within(UberSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

install.packages('ggplot2', dep = TRUE) 
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



# Similarly we will plot distribution of polarity in the tweets
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

#It's a basic human trait to inform others about what's wrong rather than informing
#them if there was something right. We tend to tweet/report if something bad
#happens rather reporting/tweeting if an experience was rather good. Hence, the
#negative tweets are expected to be larger than the positive tweets in general. Still,
#over a period of time (a week in our case), the ratio of the two easily reflects the
#overall market share versus the level of customer satisfaction for each service
#provider.



#Next, we try to get a sense of the overall content of the tweets by
#using the word clouds

#instalem alguns packets necessaris, per fer el word cloud
for (package in c('tm', 'wordcloud', 'tau')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

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




