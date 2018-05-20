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