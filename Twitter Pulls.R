mentalhealthawarenesstweets <- 
  searchTwitter("#mentalhealthawareness", 
  n = 20000, since = "2018-5-1")
suicidetweets <- searchTwitter("#suicide", 
  n = 20000, since = "2018-5-1")
ptsdtweets <- searchTwitter("#ptsd", 
  n = 20000, since = "2018-5-1")
depressiontweets <- searchTwitter("#depression", 
  n = 20000, since = "2018-5-1")
anxietytweets <- searchTwitter("#anxiety", 
  n = 20000, since = "2018-5-1")

mhatweet <- twListToDF(mentalhealthawarenesstweets)
anxtweet <- twListToDF(anxietytweets)
deptweet <- twListToDF(depressiontweets)
ptsdtweet <- twListToDF(ptsdtweets)
suitweet <- twListToDF(suicidetweets)
totaltweet <- bind_rows(mhatweet %>% mutate(hashtag = "MHA"),
  anxtweet %>% mutate(hashtag = "Anxiety"), deptweet %>%
  mutate(hashtag = "Depression"), ptsdtweet %>% 
  mutate(hashtag = "PTSD"), suitweet %>% 
  mutate(hashtag = "Suicide"))

write.table(totaltweet,"~/Desktop/totaltweet.txt",sep="\t",row.names=FALSE)
write.table(mhatweet,"~/Desktop/mhatweet.txt",sep="\t",row.names=FALSE)
write.table(anxtweet,"~/Desktop/anxtweet.txt",sep="\t",row.names=FALSE)
write.table(ptsdtweet,"~/Desktop/ptsdtweet.txt",sep="\t",row.names=FALSE)
write.table(deptweet,"~/Desktop/deptweet.txt",sep="\t",row.names=FALSE)
write.table(suitweet,"~/Desktop/suitweet.txt",sep="\t",row.names=FALSE)
