QrySearch <- function(queryTerm) {
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
}