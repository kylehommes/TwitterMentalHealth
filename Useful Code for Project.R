tweet_sentence <- data_frame(text = tottxt) %>% 
  unnest_tokens(sentence, text, token = "sentences")

write.csv(location, "~/Desktop/location.csv")

location <- read.csv("~/Desktop/location.csv")
location <- location[,-1]

location <- bind_rows(location, xxx)

write.csv(totaltweet, "~/Desktop/totaltweet.csv")
write.csv(mhatweet, "~/Desktop/mhatweet.csv")
write.csv(ptsdtweet, "~/Desktop/ptsdtweet.csv")
write.csv(suitweet, "~/Desktop/suitweet.csv")
write.csv(deptweet, "~/Desktop/deptweet.csv")
write.csv(anxtweet, "~/Desktop/anxtweet.csv")