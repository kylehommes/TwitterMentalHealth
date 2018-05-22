devtools::install_github("dkahle/ggmap")
register_google(key = google.key)
mhtweets.df <- twListToDF(mentalhealthawarenesstweets)
userinfo <- lookupUsers(mhtweets.df$screenName)
userframe <- twListToDF(userinfo)
userframe.dt <- data.table(userframe)
userframe.dt$location <-
  userframe.dt$location[!userframe.dt$location %in% ""]
location <- geocode(userframe.dt$location[!userframe.dt$location
  %in% ""])

register_google(key = google.key)
mhtweets.df <- twListToDF(mentalhealthawarenesstweets)
userinfo <- lookupUsers(mhtweets.df$screenName)
userframe <- twListToDF(userinfo)
userframe.dt <- data.table(userframe)
userframe.dt$location <-
  userframe.dt$location[!userframe.dt$location %in% ""]
userlocation <- geocode(userframe.dt$location[1:2000])
userlocation1 <- geocode(userframe.dt$location[2001:4000])
userlocation2 <- geocode(userframe.dt$location[4001:6000])
userlocation3 <- geocode(userframe.dt$location[6001:8000])
userlocation4 <- geocode(userframe.dt$location[8001:10000])
userlocation5 <- geocode(userframe.dt$location[10001:12000])
userlocation6 <- geocode(userframe.dt$location[12001:14000])
userlocation7 <- geocode(userframe.dt$location[14001:16000])
userlocation8 <- geocode(userframe.dt$location[16001:17066])
location <- bind_rows(userlocation,userlocation1)