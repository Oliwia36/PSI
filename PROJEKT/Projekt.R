#' ---
#' title: "Sentyment a popularność tekstów"
#' author: "Oliwia Turkowska, Anastasiia Mashchenko"
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      
#'     highlight: kate      
#'     toc: true            
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false 
#' ---



knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

#' Wymagane pakiety

library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(SentimentAnalysis)

#' #Wczytanie danych
data <- read.csv("data.csv", stringsAsFactors = FALSE, encoding = "UTF-8") #kolumny: text, likes, comments

corpus <- VCorpus(VectorSource(data$text))

#' Przetwarzanie i oczyszczanie tekstu

corpus <- VCorpus(VectorSource(data$text))

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuń zbędne znaki lub pozostałości url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Czyste teksty do analizy sentymentu
data$new_text <- sapply(corpus, as.character)

#' Analiza sentymentu
sentiment <- analyzeSentiment(data$new_text)
data$sentiment <- sentiment$SentimentQDAP

data <- data %>%
  mutate(sentiment_kat = case_when(
    sentiment > 0 ~ "pozytywny",
    sentiment < 0 ~"negatywny",
    TRUE          ~"neutralny"
  ))

#' Rozkład sentymentu
ggplot(data, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "white") + 
  theme_minimal() +
  labs(title = "Rozkład sentymentu QDAP", x = "Sentyment", y = "Liczba tekstów")

#' Sentyment a popularność

# Polubienia
ggplot(data, aes(x = sentiment, y = likes)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Sentyment a liczba polubień", x = "Sentyment", y = "Polubienia")

# Komentarze
ggplot(data, aes(x = sentiment, y = comments)) +
  geom_point(alpha = 0.6, color = "green") + 
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Sentyment a liczba komentarzy", x = "Sentyment", y = "Komentarze")

#' Korelacja
likes_correlation <- cor(data$sentiment, data$likes, use = "complete.obs")
comments_correlation<- cor(data$sentiment, data$comments, use= "complete.obs")

#' Wyniki

cat("Średni sentyment:", round(mean(data$sentiment, na.rm= TRUE), 3), "\n")
cat("Korelacja z polubieniami:", round(likes_correlation, 3), "\n")
cat("Korelacja z komentarzami:", round(comments_correlation, 3), "\n")

