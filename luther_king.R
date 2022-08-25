####################################################

# Nuvem de Palavras do discurso de Martin Luther King Jr.

############################################

# Bibliotecas usadas

install.packages("tm")
install.packages("wordcloud")
install.packages("readr")


#importação
library(tm)
library(wordcloud)
library(readr)


# função para ler o texto e convertê-lo em string

discurso<- read_file("C:\\Users\\aluno\\Desktop\\livros\\machine learning\\mestrado_eng_prod\\discurso_luther_king.txt")

discurso
class(discurso)

# retirada dos caratectes especiais de quebra de linha com a função gsub

discurso2 <-gsub("\\r\\n\\r\\n"," ",discurso)
discurso2

# Agora é necessário converter o vetor em um objeto "corpus"
# assim o pacote tm pode trabalhá-lo

vs <- VectorSource(discurso2)
corpus <- Corpus(vs)

inspect(corpus)


##########################

# Processamento dos caracteres

###############################


# Tudo para minúsculo

corpus <- tm_map(corpus, content_transformer(tolower))

# Remoção de putuação

corpus <- tm_map(corpus,removePunctuation)

# Remoção de espaços em branco a mais

corpus <- tm_map(corpus,stripWhitespace)

# Retirada números

corpus <- tm_map(corpus, removeNumbers)


# Remoção de palavras ruído (stopwords)

corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))


inspect(corpus)
 




##############################3

# Verificação das frequências das palavras no texto

################

# Converte para matriz
tdm <- as.matrix(TermDocumentMatrix(corpus))

# ordenando as frequências da matriz

fre <- sort(rowSums(tdm), decreasing = T)


# escolhe subconjunto de dados de palavras com frequência
# maior que 2

aux <- subset(fre,fre>3)


# plotagem do gráfico de barras das frequência

barplot(aux, las=2, col=rainbow(10))




########################################

### Gerando a Nuvem de Palavras

#######################################



wordcloud(words = corpus, min.freq = 5, max.words = 100,
          random.order = F, rot.per = 0.25,
          colors = brewer.pal(8,"Dark2"))





