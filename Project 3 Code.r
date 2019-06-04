#Part a


setwd("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document")
getwd()
Text <- VCorpus(DirSource(".", ignore.case=TRUE,mode="text"))
Text
inspect(Text)
str(Text)


Chapter1 <- Text[[1]]
Chapter2 <- Text[[2]]
Chapter3 <- Text[[3]]
Chapter4 <- Text[[4]]
Chapter5 <- Text[[5]]
Chapter6 <- Text[[6]]
Chapter7 <- Text[[7]]
Chapter8 <- Text[[8]]
Chapter9 <- Text[[9]]
Chapter10 <- Text[[10]]


hc <- hclust(docs1, method="ward.D2")
ggdendrogram(hc, rotate = FALSE, size = 2)


Chapter1
Chapter2
Chapter3
Chapter4
Chapter5
Chapter6
Chapter7
Chapter8
Chapter9
Chapter10


Textdtm <- DocumentTermMatrix(Text)
Textdtm


Texttdm <- TermDocumentMatrix(Text)
Texttdm
inspect(Texttdm[1:50,1:10])


Chapter1tf <- termFreq(Chapter1)
Chapter2tf <- termFreq(Chapter2)
Chapter3tf <- termFreq(Chapter3)
Chapter4tf <- termFreq(Chapter4)
Chapter5tf <- termFreq(Chapter5)
Chapter6tf <- termFreq(Chapter6)
Chapter7tf <- termFreq(Chapter7)
Chapter8tf <- termFreq(Chapter8)
Chapter9tf <- termFreq(Chapter9)
Chapter10tf <- termFreq(Chapter10)


Chapter1tf
Chapter2tf
Chapter3tf
Chapter4tf
Chapter5tf
Chapter6tf
Chapter7tf
Chapter8tf
Chapter9tf
Chapter10tf


Chapter1df <- as.data.frame(Chapter1tf)
Chapter2df <- as.data.frame(Chapter2tf)
Chapter3df <- as.data.frame(Chapter3tf)
Chapter4df <- as.data.frame(Chapter4tf)
Chapter5df <- as.data.frame(Chapter5tf)
Chapter6df <- as.data.frame(Chapter6tf)
Chapter7df <- as.data.frame(Chapter7tf)
Chapter8df <- as.data.frame(Chapter8tf)
Chapter9df <- as.data.frame(Chapter9tf)
Chapter10df <- as.data.frame(Chapter10tf)




Chapter1df
Chapter2df
Chapter3df
Chapter4df
Chapter5df
Chapter6df
Chapter7df
Chapter8df
Chapter9df
Chapter10df


Textlow <- tm_map(Text, content_transformer(tolower))


RemoveNumPunc <- function(x)
  gsub("[^[:alpha:][:space:]]*", "", x)


Textcl <- tm_map(Textlow,content_transformer(RemoveNumPunc))


StopWords <- c(stopwords('english'))
TextStop <- tm_map(Textcl,removeWords,StopWords)
inspect(TextStop[1:10])


Chapter1StopWords <- c(stopwords('english'))
Chapter1Stop <- tm_map(Chapter1cl,removeWords,StopWords)


Textdm2 <- TermDocumentMatrix(TextStop,control= list(wordlengths=c(1,Inf)))
Textdm2
FreqTerms <- findFreqTerms(Textdtm2,lowfreq = 4)
StateAssoc <- findAssocs(Textdm2,"states",0.5)


TermFrequency <- rowSums(as.matrix(Textdm2))
TermFrequencySub <- subset(TermFrequency,TermFrequency >= 6)
TermFrequencydf <- as.data.frame(names(TermFrequency),freq=TermFrequency)


Textdm2


sparsetdm2 <- removeSparseTerms(Textdm2, sparse = 0.75)
inspect(sparsetdm2)
sparsetdm2


fit <- hclust(distMatrix, method= "ward.D2") 


Chapter1M <-  as.matrix(Textdm2)
word.freq <- sort(rowSums(Chatper1M),decreasing=T)
word.freq
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(words=names(word.freq),freq=word.freq,min.freq=0,random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#Part b


#Read the text from the text file
text = readLines("DrJekyllAndMrHyde.txt")


#Use collapse paste to store the text as one string and then split at period(.) and then a single space.
str = paste(text,collapse=" ")
splitstr = strsplit(str, ". ", fixed = TRUE)
str_new = list()


#Remove extra spaces
i = 1
for(s in splitstr){
  s = gsub("^\\s+|\\s+$", "", s)
  str_new[[i]] = s
  i = i + 1
}
str2 = str_new[[1]]


str2 = gsub("  ", " ", str2)


lengthSents = list()
#get length of words for each sentence
for (s in 1:length(str2)) {
  lengthSents[s] = sapply(strsplit(str2[s], " "), length)
}




#Function to sort sentences by number of words in decreasing order
sortnumlist = function(x) {
  n = length(x)
  for (k in n:2) {
    i = 1
    while (i < k) {
      if (x[[i]] < x[[i+1]]) {
        tmp = x[[i+1]]
        x[[i+1]] = x[[i]]
        x[[i]] = tmp
      }
      i = i+1
    }
  }
  x
}


#Store list of sorted sentences by words and get the first 10.
wordlist = sortnumlist(lengthSents)
wordlist = unique(wordlist)
wordlist = wordlist[1:10]


#Match word count to sentences stored in str2.
Sentences = list()
wordlist_final = list()
k = 1
for (t in 1:length(wordlist)) { 
  for (s in 1:length(str2)) {
    if (sapply(strsplit(str2[s], " "), length) == wordlist[t]) {
      Sentences[k] = str2[s]
      wordlist_final[k] = wordlist[t]
      k = k + 1
    }
  }
}


Sentences = Sentences[1:10]
wordlist_final = wordlist_final[1:10]


#Display the result
print(paste0("The 10 longest sentences: "))
Sentences


print(paste0("Word count of the 10 longest sentences: "))
wordlist_final






#Part c word cloud, dendrogram and longest word
text1 <- readLines("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 1.txt")
docs1 <- Corpus(VectorSource(text1))
inspect(docs1)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs1 <- tm_map(docs1, toSpace, "/")
docs1 <- tm_map(docs1, toSpace, "@")
docs1 <- tm_map(docs1, toSpace, " “ ")
docs1 <- tm_map(docs1, toSpace, "\\|")
# Convert the text to lower case
docs1 <- tm_map(docs1, content_transformer(tolower))
# Remove numbers
docs1 <- tm_map(docs1, removeNumbers)
# Remove english common stopwords
docs1 <- tm_map(docs1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs1 <- tm_map(docs1, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs1 <- tm_map(docs1, removePunctuation)
# Eliminate extra white spaces
docs1 <- tm_map(docs1, stripWhitespace)
dtm1 <- TermDocumentMatrix(docs1)
m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
noquote(d1)
head(d1, 10)
set.seed(1234)
wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)


a <- data.frame(names=d1$word,chr=apply(d1,2,nchar)[,1])
head(a)
a[which.max(a$chr),]


text2 <- readLines(file.choose())
docs2 <- Corpus(VectorSource(text2))
inspect(docs2)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs2 <- tm_map(docs2, toSpace, "/")
docs2 <- tm_map(docs2, toSpace, "@")
docs2 <- tm_map(docs2, toSpace, " “ ")
docs2 <- tm_map(docs2, toSpace, "\\|")
# Convert the text to lower case
docs2 <- tm_map(docs2, content_transformer(tolower))
# Remove numbers
docs2 <- tm_map(docs2, removeNumbers)
# Remove english common stopwords
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs2 <- tm_map(docs2, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs2 <- tm_map(docs2, removePunctuation)
# Eliminate extra white spaces
docs2 <- tm_map(docs2, stripWhitespace)
dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
noquote(d2)
head(d2, 10)
set.seed(1234)
wordcloud(words = d2$word, freq = d2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)
b <- data.frame(names=d2$word,chr=apply(d2,2,nchar)[,1])
b[which.max(b$chr),]








text3 <- readLines("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 3.txt")
docs3 <- Corpus(VectorSource(text3))
inspect(docs3)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs3 <- tm_map(docs3, toSpace, "/")
docs3 <- tm_map(docs3, toSpace, "@")
docs3 <- tm_map(docs3, toSpace, " “ ")
docs3 <- tm_map(docs3, toSpace, "\\|")
# Convert the text to lower case
docs3 <- tm_map(docs3, content_transformer(tolower))
# Remove numbers
docs3 <- tm_map(docs3, removeNumbers)
# Remove english common stopwords
docs3 <- tm_map(docs3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs3 <- tm_map(docs3, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs3 <- tm_map(docs3, removePunctuation)
# Eliminate extra white spaces
docs3 <- tm_map(docs3, stripWhitespace)
dtm3 <- TermDocumentMatrix(docs3)
m3 <- as.matrix(dtm3)
v3 <- sort(rowSums(m3),decreasing=TRUE)
d3 <- data.frame(word = names(v3),freq=v3)
noquote(d3)
head(d3, 10)
set.seed(1234)
wordcloud(words = d3$word, freq = d3$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)
c <- data.frame(names=d3$word,chr=apply(d3,2,nchar)[,1])
c[which.max(c$chr),]




text4 <- readLines("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 4.txt")
docs4 <- Corpus(VectorSource(text4))
inspect(docs4)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs4 <- tm_map(docs4, toSpace, "/")
docs4 <- tm_map(docs4, toSpace, "@")
docs4 <- tm_map(docs4, toSpace, " “ ")
docs4 <- tm_map(docs4, toSpace, "\\|")
# Convert the text to lower case
docs4 <- tm_map(docs4, content_transformer(tolower))
# Remove numbers
docs4 <- tm_map(docs4, removeNumbers)
# Remove english common stopwords
docs4 <- tm_map(docs4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs4 <- tm_map(docs4, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs4 <- tm_map(docs4, removePunctuation)
# Eliminate extra white spaces
docs4 <- tm_map(docs4, stripWhitespace)
dtm4 <- TermDocumentMatrix(docs4)
m4 <- as.matrix(dtm4)
v4 <- sort(rowSums(m4),decreasing=TRUE)
d4 <- data.frame(word = names(v4),freq=v4)
noquote(d4)
head(d4, 10)
set.seed(1234)
wordcloud(words = d4$word, freq = d4$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)
d <- data.frame(names=d4$word,chr=apply(d4,2,nchar)[,1])
d[which.max(d$chr),]




text5 <- readLines("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 5.txt")
docs5 <- Corpus(VectorSource(text5))
inspect(docs5)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs5 <- tm_map(docs5, toSpace, "/")
docs5 <- tm_map(docs5, toSpace, "@")
docs5 <- tm_map(docs5, toSpace, " “ ")
docs5 <- tm_map(docs5, toSpace, "\\|")
# Convert the text to lower case
docs5 <- tm_map(docs5, content_transformer(tolower))
# Remove numbers
docs5 <- tm_map(docs5, removeNumbers)
# Remove english common stopwords
docs5 <- tm_map(docs5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs5 <- tm_map(docs5, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs5 <- tm_map(docs5, removePunctuation)
# Eliminate extra white spaces
docs5 <- tm_map(docs5, stripWhitespace)
dtm5 <- TermDocumentMatrix(docs5)
m5 <- as.matrix(dtm5)
v5 <- sort(rowSums(m5),decreasing=TRUE)
d5 <- data.frame(word = names(v5),freq=v5)
noquote(d5)
head(d5, 10)
set.seed(1235)
wordcloud(words = d5$word, freq = d5$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)
e <- data.frame(names=d5$word,chr=apply(d5,2,nchar)[,1])
e[which.max(e$chr),]


text6 <- readLines("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 6.txt")
docs6 <- Corpus(VectorSource(text6))
inspect(docs6)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs6 <- tm_map(docs6, toSpace, "/")
docs6 <- tm_map(docs6, toSpace, "@")
docs6 <- tm_map(docs6, toSpace, " “ ")
docs6 <- tm_map(docs6, toSpace, "\\|")
# Convert the text to lower case
docs6 <- tm_map(docs6, content_transformer(tolower))
# Remove numbers
docs6 <- tm_map(docs6, removeNumbers)
# Remove english common stopwords
docs6 <- tm_map(docs6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs6 <- tm_map(docs6, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs6 <- tm_map(docs6, removePunctuation)
# Eliminate extra white spaces
docs6 <- tm_map(docs6, stripWhitespace)
dtm6 <- TermDocumentMatrix(docs6)
m6 <- as.matrix(dtm6)
v6 <- sort(rowSums(m6),decreasing=TRUE)
d6 <- data.frame(word = names(v6),freq=v6)
noquote(d6)
head(d6, 10)
set.seed(1236)
wordcloud(words = d6$word, freq = d6$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.36, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)
f <- data.frame(names=d6$word,chr=apply(d6,2,nchar)[,1])
f[which.max(f$chr),]




text7 <- readLines(file.choose())
docs7 <- Corpus(VectorSource(text7))
inspect(docs7)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs7 <- tm_map(docs7, toSpace, "/")
docs7 <- tm_map(docs7, toSpace, "@")
docs7 <- tm_map(docs7, toSpace, " “ ")
docs7 <- tm_map(docs7, toSpace, "\\|")
# Convert the text to lower case
docs7 <- tm_map(docs7, content_transformer(tolower))
# Remove numbers
docs7 <- tm_map(docs7, removeNumbers)
# Remove english common stopwords
docs7 <- tm_map(docs7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs7 <- tm_map(docs7, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs7 <- tm_map(docs7, removePunctuation)
# Eliminate extra white spaces
docs7 <- tm_map(docs7, stripWhitespace)
dtm7 <- TermDocumentMatrix(docs7)
m7 <- as.matrix(dtm7)
v7 <- sort(rowSums(m7),decreasing=TRUE)
d7 <- data.frame(word = names(v7),freq=v7)
noquote(d7)
head(d7, 10)
set.seed(1237)
wordcloud(words = d7$word, freq = d7$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.37, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)
g <- data.frame(names=d7$word,chr=apply(d7,2,nchar)[,1])
g[which.max(g$chr),]


text8 <- readLines("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 8.txt")
docs8 <- Corpus(VectorSource(text8))
inspect(docs8)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs8 <- tm_map(docs8, toSpace, "/")
docs8 <- tm_map(docs8, toSpace, "@")
docs8 <- tm_map(docs8, toSpace, " “ ")
docs8 <- tm_map(docs8, toSpace, "\\|")
# Convert the text to lower case
docs8 <- tm_map(docs8, content_transformer(tolower))
# Remove numbers
docs8 <- tm_map(docs8, removeNumbers)
# Remove english common stopwords
docs8 <- tm_map(docs8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs8 <- tm_map(docs8, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs8 <- tm_map(docs8, removePunctuation)
# Eliminate extra white spaces
docs8 <- tm_map(docs8, stripWhitespace)
dtm8 <- TermDocumentMatrix(docs8)
m8 <- as.matrix(dtm8)
v8 <- sort(rowSums(m8),decreasing=TRUE)
d8 <- data.frame(word = names(v8),freq=v8)
noquote(d8)
head(d8, 10)
set.seed(1238)
wordcloud(words = d8$word, freq = d8$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.38, 
          colors=brewer.pal(8, "Dark2"))
options(max.print=999999)
h <- data.frame(names=d8$word,chr=apply(d8,2,nchar)[,1])
h[which.max(h$chr),]




text9 <- readLines("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 9.txt")
docs9 <- Corpus(VectorSource(text9))
inspect(docs9)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs9 <- tm_map(docs9, toSpace, "/")
docs9 <- tm_map(docs9, toSpace, "@")
docs9 <- tm_map(docs9, toSpace, " “ ")
docs9 <- tm_map(docs9, toSpace, "\\|")
# Convert the text to lower case
docs9 <- tm_map(docs9, content_transformer(tolower))
# Remove numbers
docs9 <- tm_map(docs9, removeNumbers)
# Remove english common stopwords
docs9 <- tm_map(docs9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs9 <- tm_map(docs9, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs9 <- tm_map(docs9, removePunctuation)
# Eliminate extra white spaces
docs9 <- tm_map(docs9, stripWhitespace)
dtm9 <- TermDocumentMatrix(docs9)
m9 <- as.matrix(dtm9)
v9 <- sort(rowSums(m9),decreasing=TRUE)
d9 <- data.frame(word = names(v9),freq=v9)
noquote(d9)
head(d9, 10)
set.seed(1239)
wordcloud(words = d9$word, freq = d9$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.39, 
          colors=brewer.pal(9, "Dark2"))
options(max.print=999999)
i <- data.frame(names=d9$word,chr=apply(d9,2,nchar)[,1])
i[which.max(i$chr),]




text10 <- readLines(file.choose())
docs10 <- Corpus(VectorSource(text2))
inspect(docs10)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs10 <- tm_map(docs10, toSpace, "/")
docs10 <- tm_map(docs10, toSpace, "@")
docs10 <- tm_map(docs10, toSpace, " “ ")
docs10 <- tm_map(docs10, toSpace, "\\|")
# Convert the text to lower case
docs10 <- tm_map(docs10, content_transformer(tolower))
# Remove numbers
docs10 <- tm_map(docs10, removeNumbers)
# Remove english common stopwords
docs10 <- tm_map(docs10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs10 <- tm_map(docs10, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs10 <- tm_map(docs10, removePunctuation)
# Eliminate extra white spaces
docs10 <- tm_map(docs10, stripWhitespace)
dtm10 <- TermDocumentMatrix(docs10)
m10 <- as.matrix(dtm10)
v10 <- sort(rowSums(m10),decreasing=TRUE)
d10 <- data.frame(word = names(v10),freq=v10)
noquote(d10)
head(d10, 10)
set.seed(12310)
wordcloud(words = d10$word, freq = d10$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.310, 
          colors=brewer.pal(10, "Dark2"))
options(max.print=999999)
j <- data.frame(names=d10$word,chr=apply(d10,2,nchar)[,1])
j[which.max(j$chr),]




Chapter1tokens <- tokens(Chapter1)


#Part d


#Use collapse paste to store the text as one string and then split at period(.).
str = paste(Chp1,collapse=" ")
splitstr = strsplit(str, ". ", fixed = TRUE)


str_new = list()


#Remove extra spaces
i = 1
for(s in splitstr){
  s = gsub("^\\s+|\\s+$", "", s)
  str_new[[i]] = s
  i = i + 1
}
str2 = str_new[[1]]


str2 = gsub("  ", " ", str2)


lengthSents = list()
#get length of words for each sentence
for (s in 1:length(str2)) {
  lengthSents[s] = sapply(strsplit(str2[s], " "), length)
}


#Function to sort sentences by number of words in decreasing order
sortnumlist = function(x) {
  n = length(x)
  for (k in n:2) {
    i = 1
    while (i < k) {
      if (x[[i]] < x[[i+1]]) {
        tmp = x[[i+1]]
        x[[i+1]] = x[[i]]
        x[[i]] = tmp
      }
      i = i+1
    }
  }
  x
}


#Store list of sorted sentences by words.
wordlist = sortnumlist(lengthSents)
wordlist = unique(wordlist)


longestSentence = list()


k = 1


  for (s in 1:length(str2)) {
    if (sapply(strsplit(str2[s], " "), length) == wordlist[1]) {
      longestSentence[k] = str2[s]
      print(wordlist[1])
      k = k + 1
    }
  }


#Shortest sentence with more than 1 word
for (s in length(wordlist):1) {
  if (wordlist[s] > 1) {
    shortestSentence = wordlist[s]
    break;
  }
}


# Part e 
df<-read.delim("C:/Intro to Big Data/Project 3/DrJekyllAndMrHyde.txt")
df2<-paste(unlist(df),collapse='')        
df2<-gsub(".*STORY OF THE DOOR\\s*|SEARCH FOR MR. HYDE.*","",df2)
df3<-noquote(strsplit(df2," "))
inWordnet<-function(w,pos        =c("ADJECTIVE","ADVERB","NOUN","VERB")){        
  for(x        in pos)        {
  filter<-getTermFilter("ExactMatchFilter",w,TRUE)
  terms<-getIndexTerms(x,5,filter)
  if(!is.null(terms)){
    return(x)
  }
}
  return("None")
}
for(i        in        df3){
  for(j in i){
    if( nchar(j) > 4)
    {
    sink("C:/Intro to Big Data/Project 3/output.txt",        append        =        TRUE)        
    cat(j)
    cat("        -        ")
    cat(inWordnet(j))
    cat("\n")
    sink()
  }
  }        
}


#part f


ItaRi.tfl<-read.tfl("C:\Users\aneri\OneDrive\Documents\RProject3\Projec3\Text Document.txt")        
ItaUltra.tfl<-read.tfl("C:\Users\aneri\OneDrive\Documents\RProject3\Projec3\Text Document.txt")        
ItaRi2.tfl<-read.tfl("C:\Users\aneri\OneDrive\Documents\RProject3\Projec3\Text Document.txt")        
        
ItaRi.spc<-tfl2spc(ItaRi.tfl)        
ItaUltra.spc<-tfl2spc(ItaUltra.tfl)
ItaRi2.spc<-        tfl2spc(ItaRi2.tfl)
summary(ItaRi.spc)


N(ItaRi.spc)
V(ItaRi.spc)
Vm(ItaRi.spc, 1) / N(ItaRi.spc)
plot(ItaRi.spc)
plot(ItaRi.spc, log="x")
 with(ItaRi.spc, plot(m, Vm, main="Frequency Spectrum"




#part g
textlines = readLines(file.choose())
text = scan("C://Users//aneri//OneDrive//Documents//RProject3//Projec3//Text Document//Chapter 1.txt", quote=NULL, what="x")
head(text)
counts = as.data.frame(xtabs(~text))
#installed tau
chapter1.str = paste(text, collapse=" ")
# this does the counting, lowercasing everything first
chapter1.counts = textcnt(chapter1.str, n=1, method="string", tolower=T)
# chapter1.counts is a vector with names on the entries.
# Here is how you access entries:
names(chapter1.counts)
chapter1.counts.df = data.frame(word = names(chapter1.counts), count = c(chapter1.counts))


chapter1.counts.df[chapter1.counts.df$word == "chapter1",]
#tm library
chapter1 <- Corpus(VectorSource(textlines))


# normalization of the text:
chapter1 <- tm_map(chapter1, tolower) #lowercase
chapter1 <- tm_map(chapter1, removePunctuation, preserve_intra_word_dashes = FALSE) # remove punctuation
chapter1 <- tm_map(chapter1, removeWords, stopwords("english")) # remove stopwords
chapter1 <- tm_map(chapter1, stemDocument) # reduce word forms to stems
chapter1.tdm.1 <- TermDocumentMatrix(chapter1[1])
findFreqTerms(oz.tdm.1, 100)
chapter1.tdm.2 <- TermDocumentMatrix(chapter1[2])
findFreqTerms(chapter1.tdm.2, 50)


# count how often the term appears in each of the documents in the collection
tdm = TermDocumentMatrix(chapter1)
chapter1.str = paste(text, collapse = " ")
chapter1.corpus = Corpus(VectorSource(chapter1.str))
chapter1.corpus = tm_map(chapter1.corpus, tolower)
chapter1.corpus = tm_map(chapter1.corpus, removePunctuation, preserve_intra_word_dashes = FALSE)
cleaned.chapter1.str = as.character(chapter1.corpus)[1]
chapter1.words = strsplit(cleaned.chapter1.str, " ", fixed = T)[[1]]
a=chapter1.words[nchar(chapter1.words)>6]
chapter1.bigrams = vapply(ngrams(a, 2), paste, "", collapse = " ")
chapter1.Trigrams = vapply(ngrams(a, 3), paste, "", collapse = " ")
chapter1.bigrams
chapter1.Trigrams


#part h


df<-read.delim ("/Users/ishaterdal/Dropbox/BigData/Project3/DrJekyllAndMrHyde.txt")
df2<-paste(unlist(df),collapse='')  
df2<-gsub(".*STORY OF THE DOOR\\s*|SEARCH FOR MR. HYDE.*","",df2)
df2 <- stri_replace_all(df2, "", regex = "<.*?>")
df2 <- stri_trim(df2)
df2 <- stri_trans_tolower(df2)
tc <- create_tcorpus(df2)
sw <- stopwords("english")
tokens_remove(tokschap1, sw)
text <- corpus(df2)
dtm <- dfm(text, remove = sw, remove_punct=TRUE)
tc <- create_tcorpus(df2)
hits <- tc$search_features('"loathing gentleman"~5')
kwic <- tc$kwic(hits, ntokens = 3)
head(kwic$kwic, 3)
chap1_df <- tibble(text=df2)
tidy <- chap1_df %>% unnest_tokens(word, text)
data("stop_words")
tidy <- tidy %>% anti_join(stop_words)
common <- tidy %>% count(word, sort=TRUE)