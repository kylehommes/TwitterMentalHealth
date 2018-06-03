tweet_sentence <- data_frame(text = tottxt) %>% 
  unnest_tokens(sentence, text, token = "sentences")

write.csv(location, "~/Desktop/location.csv")

location <- read.csv("~/Desktop/location.csv")
location <- location[,-1]

location <- bind_rows(location, userlocation4)

write.csv(totaltweet, "~/Desktop/totaltweet.csv")
write.csv(mhatweet, "~/Desktop/mhatweet.csv")
write.csv(ptsdtweet, "~/Desktop/ptsdtweet.csv")
write.csv(suitweet, "~/Desktop/suitweet.csv")
write.csv(deptweet, "~/Desktop/deptweet.csv")
write.csv(anxtweet, "~/Desktop/anxtweet.csv")

write.csv(mhafreq.df, "~/Desktop/mhafreq.csv")
write.csv(afreq.df, "~/Desktop/afreq.csv")
write.csv(pfreq.df, "~/Desktop/pfreq.csv")
write.csv(sfreq.df, "~/Desktop/sfreq.csv")
write.csv(dfreq.df, "~/Desktop/dfreq.csv")
write.csv(totfreq.df, "~/Desktop/totfreq.csv")

mhafreq_table <- read.csv("~/Desktop/mhafreq.csv")

rsconnect::deployApp("~/Desktop/Twitter Mental Health/Twitter_viz/")
