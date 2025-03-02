text_2021 <- readLines(file.choose(), encoding = "UTF-8") 
text_2024 <- readLines(file.choose(), encoding = "UTF-8") 

install.packages("qdap")
library(qdap)

frequent_terms_2021 <- freq_terms(text_2021)
frequent_terms_2024 <- freq_terms(text_2024)
frequent_terms_2021 <- freq_terms(text_2021, stopwords=Top200Words)
frequent_terms_2024 <- freq_terms(text_2024, stopwords=Top200Words)

plot(frequent_terms_2021)
plot(frequent_terms_2024)

install.packages("wordcloud")
library(wordcloud)

wordcloud(frequent_terms_2021$WORD,frequent_terms_2021$FREQ)
wordcloud(frequent_terms_2024$WORD,frequent_terms_2024$FREQ)

install.packages("RColorBrewer")
library(RColorBrewer)


wordcloud(frequent_terms_2021$WORD, frequent_terms_2021$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues"))
wordcloud(frequent_terms_2024$WORD, frequent_terms_2024$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))

#Na podstawie analizy chmur słów, można stwierdzić, że przemówienia były do siebie podobne pod względem tematów poruszanych w obu przemowach.
