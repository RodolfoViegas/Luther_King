# Analise de twittes
setwd("/home/rodolfoviegas/R_Scripts/Luther_Nuvem/")
# bibliotecas
install.packages("twitteR")
install.packages ("rtweet")
library(rtweet)


library(tm)
library(wordcloud)
library(RColorBrewer)
library(twitteR)


# Chaves de autenticação

consumer_key <- "ARWEMkx1S5O121fR2Rq55otBB"
consumer_secret <- "IHOrVvvUWLtHvB22dcKaVMxKbpfDcZafyvWe0cdPpQQrgBJggm"
access_token <- "1176572720001236992-qpxQcXUbwl0CcQSanWAdwdRsDYwvnv"
accesse_secret <- "PKtzdxaH7hT5J7VCGhLZASRFEi63fJghNNA329iJJqGCN"

token <- create_token(app= "TuiteLula",
                     consumer_key,
                     consumer_secret,
                     access_token,
                     access_secret)
auth_setup_default()
rstat_tweets<-search_tweets("#stf", n=500,lang = "pt")

tweets<- paste(rstat_tweets$full_text, collapse = " ")
tweets


tweets_S <- VectorSource(tweets)
corpus <- Corpus(tweets_S)
inspect(corpus)


corpus <- tm_map(corpus, content_transformer(tolower))
# Remoção de putuação
corpus <- tm_map(corpus,removePunctuation)
# Remoção de espaços em branco a mais
corpus <- tm_map(corpus,stripWhitespace)
# Remoção de palavras ruído (stopwords)
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))



removeURL <- function(x){gsub("http[^[:space:]]*","",x)}
corpus<-tm_map(corpus,removeURL)

# remove qualquer coisa que nao seja letras em portugues e espaço
removeNumPunct<-function(x){gsub("[^[:alpha:][:space:]]*","",x)}
corpus<-tm_map(corpus,removeNumPunct)


# Cria matriz

dtm<-TermDocumentMatrix(corpus)
dtm<-as.matrix(dtm)

# Frequencia ordenada
fre<-sort(rowSums(dtm),decreasing = T)
fre


wordcloud(words = corpus, min.freq = 3, max.words = Inf,
          random.order = F, rot.per = 0.15,
          colors = brewer.pal(8,"Dark2"),scale = c(8,.2))

