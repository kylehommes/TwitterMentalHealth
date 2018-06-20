tweet_sentence <- data_frame(text = tottxt) %>% 
  unnest_tokens(sentence, text, token = "sentences")

write.csv(location, "~/Desktop/location.csv")

write.csv(location_a, "~/Desktop/anxlocation.csv")

write.csv(location_s, "~/Desktop/suilocation.csv")

write.csv(location_d, "~/Desktop/deplocation.csv")

write.csv(location_p, "~/Desktop/ptsdlocation.csv")

location <- read.csv("~/Desktop/location.csv")
location <- location[,-1]

location <- bind_rows(location, userlocation8)

location_a <- userlocation_a
location_a <- bind_rows(location_a, userlocation2_a)

location_s <- userlocation_s
location_s <- bind_rows(location_s, userlocation1_s)

location_d <- userlocation_d
location_d <- bind_rows(location_d, userlocation1_d)

location_p <- userlocation_p
location_p <- bind_rows(location_p, userlocation1_p)

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



loc_plot + geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = location,aes(x=lon,y=lat),color="RED",
  size=.25) + labs(x = "Longitude", y = "Latitude", 
  title = "Map of Location for #mentalhealthawarness Tweets")

loc_plot + geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = location_a,aes(x=lon,y=lat),color="RED",
  size=.25) + labs(x = "Longitude", y = "Latitude", 
  title = "Map of Location for #anxiety Tweets")

loc_plot + geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = location_s,aes(x=lon,y=lat),color="RED",
  size=.25) + labs(x = "Longitude", y = "Latitude", 
  title = "Map of Location for #suicide Tweets")

loc_plot + geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = location_d,aes(x=lon,y=lat),color="RED",
  size=.25) + labs(x = "Longitude", y = "Latitude", 
  title = "Map of Location for #depression Tweets")

loc_plot + geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = location_p,aes(x=lon,y=lat),color="RED",
  size=.25) + labs(x = "Longitude", y = "Latitude", 
  title = "Map of Location for #ptsd Tweets")

worldMap <- map_data("world")
loc_plot <- ggplot(worldMap)
loc_plot + geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = location,aes(x=lon,y=lat),color="RED",
  size=.25) + labs(x = "Longitude", y = "Latitude", 
  title = "Map of Location for #mentalhealthawarness Tweets")

