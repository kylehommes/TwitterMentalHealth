# Libraries needed to run the app
library(shiny)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(tm)
library(twitteR)
library(tidyr)
library(factoextra)
library(gridExtra)
library(RColorBrewer)
library(maps)
library(tidytext)
library(reshape2)

# Set up the environment for the app
load("Twitter.RData")

# Choices for the Input Selection
Hashtag = list("#MentalHealthAwareness" = "m",
               "#Anxiety" = "a","#Suicide" = "s","#Depression" = "d",
               "#PTSD" = "p","All Hashtags" = "t")

Sent_choice = list("Postive/Negative Sentiment Segments Plot" = 
                     "bing_seg_plot","Most Frequent Sentiment Words Plot" = "bing_plot", 
                   "Positive/Negative Sentiment by Hashtag" = "bing_hashtag_plot", 
                   "Sentiment Groups Most Frequent Words Plot" = "nrc_plot",
                   "Sentiment Groups by Hashtag" = "nrc_hashtag_plot")

Topics_choice = list("LDA Topics" = "lda_plot", 
                     "LSA Topics" = "lsa_plot", "Cluster Topics" = "cluster_plot",
                     "Bigrams" = "bigram_plot", "Trigrams" = "trigram_plot", 
                     "4-grams" = "fourgram_plot")

