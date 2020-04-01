library(wordcloud2)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(yaml)
library(base64enc)

figPath = system.file("examples/4.png",package = "wordcloud2")
wordcloud2(df_word, figPath = figPath ,size=1)



