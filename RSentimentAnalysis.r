# install devtools for install packages from github
install.packages(c("devtools", "rjson", "bit64", "httr"))
# load package
library(devtools)
#install twitterR from github
install_github("geoffjentry/twitteR")
#load package
library(twitteR)
#set the api of twitter
consumer_key    <- 'set your..'
consumer_secret <- 'set your..'
access_token    <- 'set your..'
access_secret   <- 'set your..'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
#get the 30000 (sometimes you need to write in different ways the tag (e.g #alitalia or just alitalia) number of tweets) most recent tweets mentioning @alitalia
alitalia.tweets=searchTwitter("alitalia",n=30000)
#load positive words list
list.pos=scan('../data/positive-words.txt', what='character', comment.char=';')
#load negative words list
list.neg=scan('../data/negative-words.txt', what='character', comment.char=';')
#laply package unifies all the tweets with a consistent naming convention
install.packages("plyr")
library(plyr)
alitalia.text=laply(alitalia.tweets,function(t) t$getText())

# alogrithm for calculate the score for each tweet
score.sentiment = function(sentences, list.pos, list.neg, .progress='none')
{
install.packages("stringr")

    require(plyr)
    require(stringr)
     
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, list.pos, list.neg) {
         
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence = tolower(sentence)
 
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
 
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, list.pos)
        neg.matches = match(words, list.neg)
     
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
 
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
 
        return(score)
    }, list.pos, list.neg, .progress=.progress )
 
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

#calculate the score for tweets related to alitalia
alitalia.scores=score.sentiment(alitalia.text,list.pos,list.neg, .progress='text')
#add two column for identify the brand of airline
alitalia.scores$airline='Alitalia'
alitalia.scores$code='AI'
#histogram of alitalia scores
install.packages("ggplot2")
library("ggplot2")
qplot(alitalia.scores$score)
#other airline brands
#the same procedure that we made for alitalia
#ryanair
ryanair.tweets=searchTwitter("@ryanair",n=30000)
ryanair.text=laply(ryanair.tweets,function(t) t$getText())
ryanair.scores=score.sentiment(ryanair.text,list.pos,list.neg, .progress='text')
ryanair.scores$airline='Ryanair'
ryanair.scores$code='RY'
qplot(ryanair.scores$score)
#easyjet
easyjet.tweets=searchTwitter("@easyjet",n=30000)
easyjet.text=laply(easyjet.tweets,function(t) t$getText())
easyjet.scores=score.sentiment(easyjet.text,list.pos,list.neg, .progress='text')
easyjet.scores$airline='Easyjet'
easyjet.scores$code='EJ'
qplot(easyjet.scores$score)
#turkish airlines
turkishairlines.tweets=searchTwitter("turkishairlines",n=30000)
turkishairlines.text=laply(turkishairlines.tweets,function(t) t$getText())
turkishairlines.scores=score.sentiment(turkishairlines.text,list.pos,list.neg, .progress='text')
turkishairlines.scores$airline='TurkishAirlines'
turkishairlines.scores$code='TA'
qplot(turkishairlines.scores$score)
#lufthansa
lufthansa.tweets=searchTwitter("lufthansa",n=30000)
lufthansa.text=laply(lufthansa.tweets,function(t) t$getText())
lufthansa.scores=score.sentiment(lufthansa.text,list.pos,list.neg, .progress='text')
lufthansa.scores$airline='Lufthansa'
lufthansa.scores$code='LF'
qplot(lufthansa.scores$score)
#airfrance
airfrance.tweets=searchTwitter("airfrance",n=30000)
airfrance.text=laply(airfrance.tweets,function(t) t$getText())
airfrance.scores=score.sentiment(airfrance.text,list.pos,list.neg, .progress='text')
airfrance.scores$airline='Airfrance'
airfrance.scores$code='AF'
qplot(airfrance.scores$score)

#combine all the result into a single all.scores
all.scores=rbind(alitalia.scores,ryanair.scores,easyjet.scores,turkishairlines.scores,lufthansa.scores,airfrance.scores)
#ggplot2 implements "grammar of graphics" for build plots in layers
ggplot(data=all.scores) +
geom_bar(mapping=aes(x=score,fill=airline),bindwidth=1)+
facet_grid(airline~.)+
theme_bw()+scale_fill_brewer()