Sent_choice = list("Postive/Negative Sentiment Segments Plot" = 
                     "bing_seg_plot","Most Frequent Sentiment Words Plot" = "bing_plot", 
                   "Positive/Negative Sentiment by Hashtag" = "bing_hashtag_plot", 
                   "Sentiment Groups Most Frequent Words Plot" = "nrc_plot",
                   "Sentiment Groups by Hashtag" = "nrc_hashtag_plot")

tidytweet %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
  max.words = 100,scale=c(3,.15))

tweet_search <- totaltweet %>% dplyr::mutate(rowIndex=
  as.numeric(row.names(.))) %>%
  dplyr::select(retweetCount,rowIndex,hashtag)
tweet_search$text <- tottxt
docList <- as.list(tweet_search$text)
N.docs <- length(docList)

QrySearch <- memoise(function(queryTerm) {
  tweet_search <- totaltweet %>% dplyr::mutate(rowIndex=
                                                 as.numeric(row.names(.))) %>%
    dplyr::select(retweetCount,rowIndex,hashtag)
  tweet_search$text <- tottxt
  docList <- as.list(tweet_search$text)
  N.docs <- length(docList)
  my.docs <- VectorSource(c(docList, queryTerm))
  my.corpus <- VCorpus(my.docs) %>% 
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)
  term.doc.matrix.stm <- TermDocumentMatrix(my.corpus,
                                            control=list(weighting=function(x) weightSMART(x,spec="ltc"),
                                                         wordLengths=c(1,Inf)))
  term.doc.matrix <- tidy(term.doc.matrix.stm) %>% 
    dplyr::group_by(document) %>% 
    dplyr::mutate(vtrLen=sqrt(sum(count^2))) %>% 
    dplyr::mutate(count=count/vtrLen) %>% 
    ungroup() %>% 
    dplyr::select(term:count)
  docMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document<N.docs+1)
  qryMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document>=N.docs+1)
  searchRes <- docMatrix %>% 
    inner_join(qryMatrix,by=c("term"="term"),
               suffix=c(".doc",".query")) %>% 
    dplyr::mutate(termScore=round(count.doc*count.query,4)) %>% 
    dplyr::group_by(document.query,document.doc) %>% 
    dplyr::summarise(Score=sum(termScore)) %>% 
    filter(row_number(desc(Score))<=10) %>% 
    dplyr::arrange(desc(Score)) %>% 
    left_join(tweet_search,by=c("document.doc"="rowIndex")) %>% 
    ungroup() %>% 
    rename(Result=text) %>% 
    dplyr::select(Result,Score,retweetCount,hashtag) %>% 
    data.frame()
  return(searchRes)
})

tweet_search <- totaltweet %>% dplyr::mutate(rowIndex=
                                               as.numeric(row.names(.))) %>%
  dplyr::select(retweetCount,rowIndex,hashtag)
tweet_search$text <- tottxt
docList <- as.list(tweet_search$text)
N.docs <- length(docList)
QrySearch <- memoise(function(queryTerm) {
  tweet_search <- totaltweet %>% dplyr::mutate(rowIndex=
                                                 as.numeric(row.names(.))) %>%
    dplyr::select(retweetCount,rowIndex,hashtag)
  tweet_search$text <- tottxt
  docList <- as.list(tweet_search$text)
  N.docs <- length(docList)
  my.docs <- VectorSource(c(docList, queryTerm))
  my.corpus <- VCorpus(my.docs) %>% 
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)
  term.doc.matrix.stm <- TermDocumentMatrix(my.corpus,
                                            control=list(weighting=function(x) weightSMART(x,spec="ltc"),
                                                         wordLengths=c(1,Inf)))
  term.doc.matrix <- tidy(term.doc.matrix.stm) %>% 
    dplyr::group_by(document) %>% 
    dplyr::mutate(vtrLen=sqrt(sum(count^2))) %>% 
    dplyr::mutate(count=count/vtrLen) %>% 
    ungroup() %>% 
    dplyr::select(term:count)
  docMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document<N.docs+1)
  qryMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document>=N.docs+1)
  searchRes <- docMatrix %>% 
    inner_join(qryMatrix,by=c("term"="term"),
               suffix=c(".doc",".query")) %>% 
    dplyr::mutate(termScore=round(count.doc*count.query,4)) %>% 
    dplyr::group_by(document.query,document.doc) %>% 
    dplyr::summarise(Score=sum(termScore)) %>% 
    filter(row_number(desc(Score))<=10) %>% 
    dplyr::arrange(desc(Score)) %>% 
    left_join(tweet_search,by=c("document.doc"="rowIndex")) %>% 
    ungroup() %>% 
    rename(Result=text) %>% 
    dplyr::select(Result,Score,retweetCount,hashtag) %>% 
    data.frame()
  return(searchRes)
})