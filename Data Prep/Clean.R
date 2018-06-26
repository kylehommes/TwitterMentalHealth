# Each dataset of tweets was turned into a dataframe to help with 
# Preparation of of the data
mhatweet <- twListToDF(mentalhealthawarenesstweets)
anxtweet <- twListToDF(anxietytweets)
deptweet <- twListToDF(depressiontweets)
ptsdtweet <- twListToDF(ptsdtweets)
suitweet <- twListToDF(suicidetweets)

# The sets of tweets, broken down by hashtag, were combined and labeled
# with the hashtag they belong to for indentification in later analysis
totaltweet <- bind_rows(mhatweet %>% mutate(hashtag = "MHA"),
  anxtweet %>% mutate(hashtag = "Anxiety"), deptweet %>%
  mutate(hashtag = "Depression"), ptsdtweet %>% 
  mutate(hashtag = "PTSD"), suitweet %>% 
  mutate(hashtag = "Suicide"))

# Cleaned the tweets for tidy analysis with code adapted from 
# https://www.tidytextmining.com
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidytweet <- totaltweet %>%
  group_by(hashtag) %>% 
  dplyr::mutate(linenumber = row_number()) %>%
  ungroup() %>% filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", 
  pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

# Cleaning process for TDM creating using tm package
# Stemming/Lemmitization was not performed for better word presentation
# in wordclouds and to keep the size of the files smaller.
# After the data was cleaned and turned into Term Document Matrices
# and Document Term Matrices, the frequency for each word was 
# calcuated by finding the rowsums on the tdms and made into data frames
ctrl <- list(removePunctuation= list(preserve_intra_word_dashes = T), 
  tolower= T, stopwords= c(stopwords(kind = "en"), removeNumbers= T))
mhatxt <- sapply(mentalhealthawarenesstweets, 
  function(x) x$getText())
mhatxt <- iconv(mhatxt, "latin1", "ASCII", sub = "")
mhatxt <- gsub("http(s?)([^ ]*)", " ", mhatxt, ignore.case = T)
mhatxt <- gsub("&amp", "and", mhatxt)
mhacorpus <- VCorpus(VectorSource(mhatxt))
mhatdm <- TermDocumentMatrix(x= mhacorpus, control= ctrl)
mhadtm <- DocumentTermMatrix(x= mhacorpus, control= ctrl)
mhafreq <- sort(rowSums(as.matrix(mhatdm)), decreasing = T)
mhafreq.df <- data.frame(word = names(mhafreq), 
  freq = mhafreq, stringsAsFactors = F, row.names = NULL)
stxt <- sapply(suicidetweets, 
  function(x) x$getText())
stxt <- iconv(stxt, "latin1", "ASCII", sub = "")
stxt <- gsub("http(s?)([^ ]*)", " ", stxt, ignore.case = T)
stxt <- gsub("&amp", "and", stxt)
scorpus <- VCorpus(VectorSource(stxt))
stdm <- TermDocumentMatrix(x= scorpus, control= ctrl)
sdtm <- DocumentTermMatrix(x= scorpus, control= ctrl)
sfreq <- sort(rowSums(as.matrix(stdm)), decreasing = T)
sfreq.df <- data.frame(word = names(sfreq), 
  freq = sfreq, stringsAsFactors = F, row.names = NULL)
atxt <- sapply(anxietytweets, 
  function(x) x$getText())
atxt <- iconv(atxt, "latin1", "ASCII", sub = "")
atxt <- gsub("http(s?)([^ ]*)", " ", atxt, ignore.case = T)
atxt <- gsub("&amp", "and", atxt)
acorpus <- VCorpus(VectorSource(atxt))
atdm <- TermDocumentMatrix(x= acorpus, control= ctrl)
adtm <- DocumentTermMatrix(x= acorpus, control= ctrl)
afreq <- sort(rowSums(as.matrix(atdm)), decreasing = T)
afreq.df <- data.frame(word = names(afreq), 
  freq = afreq, stringsAsFactors = F, row.names = NULL)
dtxt <- sapply(depressiontweets, 
  function(x) x$getText())
dtxt <- iconv(dtxt, "latin1", "ASCII", sub = "")
dtxt <- gsub("http(s?)([^ ]*)", " ", dtxt, ignore.case = T)
dtxt <- gsub("&amp", "and", dtxt)
dcorpus <- VCorpus(VectorSource(dtxt))
dtdm <- TermDocumentMatrix(x= dcorpus, control= ctrl)
ddtm <- DocumentTermMatrix(x= dcorpus, control= ctrl)
dfreq <- sort(rowSums(as.matrix(dtdm)), decreasing = T)
dfreq.df <- data.frame(word = names(dfreq), 
  freq = dfreq, stringsAsFactors = F, row.names = NULL)
ptxt <- sapply(ptsdtweets, 
  function(x) x$getText())
ptxt <- iconv(ptxt, "latin1", "ASCII", sub = "")
ptxt <- gsub("http(s?)([^ ]*)", " ", ptxt, ignore.case = T)
ptxt <- gsub("&amp", "and", ptxt)
pcorpus <- VCorpus(VectorSource(ptxt))
ptdm <- TermDocumentMatrix(x= pcorpus, control= ctrl)
pdtm <- DocumentTermMatrix(x= pcorpus, control= ctrl)
pfreq <- sort(rowSums(as.matrix(ptdm)), decreasing = T)
pfreq.df <- data.frame(word = names(pfreq), 
  freq = pfreq, stringsAsFactors = F, row.names = NULL)
tottxt <- totaltweet$text
tottxt <- iconv(tottxt, "latin1", "ASCII", sub = "")
tottxt <- gsub("http(s?)([^ ]*)", " ", tottxt, ignore.case = T)
tottxt <- gsub("&amp", "and", tottxt)
totcorpus <- VCorpus(VectorSource(tottxt))
totcorpus <- tm_map(totcorpus, PlainTextDocument)
tottdm <- TermDocumentMatrix(x= totcorpus, control= ctrl)
totdtm <- DocumentTermMatrix(x= totcorpus, control= ctrl)
totfreq <- sort(rowSums(as.matrix(tottdm)), decreasing = T)
totfreq.df <- data.frame(word = names(totfreq), 
  freq = totfreq, stringsAsFactors = F, row.names = NULL)