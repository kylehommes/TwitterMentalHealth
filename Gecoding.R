# Install the ggmap package from github
devtools::install_github("dkahle/ggmap")
# Register API using the kiey from Google.
register_google(key = google.key)
# Setup the Twitter API using the keys.
setup_twitter_oauth(consumer_key = consumer.key, 
  consumer_secret = consumer.secret, access_token = access.token,
  access_secret = access.secret)
# Create dataframe from the tweets.
mhtweets.df <- twListToDF(mentalhealthawarenesstweets)
# Pull out the user info
userinfo <- lookupUsers(mhtweets.df$screenName)
# Create a dataframe with the user info
userframe <- twListToDF(userinfo)
# Turn the dataframe into a data table in order to remove NAs
userframe.dt <- data.table(userframe)
userframe.dt$location <-
  userframe.dt$location[!userframe.dt$location %in% ""]
# Geocode the user location to get longitude and latitude.
# Geocoding is broken into parts of the data frame becuase of the 
# Per diem usage alotment on the Google geocoding API
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

register_google(key = google.key)
suitweets.df <- twListToDF(suicidetweets)
userinfo_s <- lookupUsers(suitweets.df$screenName)
userframe_s <- twListToDF(userinfo_s)
userframe.dt_s <- data.table(userframe)
userframe.dt_s$location <-
  userframe.dt_s$location[!userframe.dt_s$location %in% ""]
userlocation_s <- geocode(userframe.dt_s$location[1:2000])
userlocation1_s <- geocode(userframe.dt_s$location[2001:4000])
userlocation2_s <- geocode(userframe.dt_s$location[4001:6000])
userlocation3 <- geocode(userframe.dt$location[6001:8000])
userlocation4 <- geocode(userframe.dt$location[8001:10000])
userlocation5 <- geocode(userframe.dt$location[10001:12000])
userlocation6 <- geocode(userframe.dt$location[12001:14000])
userlocation7 <- geocode(userframe.dt$location[14001:16000])
userlocation8 <- geocode(userframe.dt$location[16001:17066])
location <- bind_rows(userlocation,userlocation1)

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
location_s <- bind_rows(userlocation_s,userlocation1_s)

register_google(key = google.key)
anxtweets.df <- twListToDF(anxietytweets)
userinfo_a <- lookupUsers(anxtweets.df$screenName)
userframe_a <- twListToDF(userinfo_a)
userframe.dt_a <- data.table(userframe_a)
userframe.dt_a$location <-
  userframe.dt_a$location[!userframe.dt_a$location %in% ""]
userlocation_a <- geocode(userframe.dt_a$location[1:500])
userlocation1_a <- geocode(userframe.dt_a$location[2001:4000])
userlocation2 <- geocode(userframe.dt$location[4001:6000])
userlocation3 <- geocode(userframe.dt$location[6001:8000])
userlocation4 <- geocode(userframe.dt$location[8001:10000])
userlocation5 <- geocode(userframe.dt$location[10001:12000])
userlocation6 <- geocode(userframe.dt$location[12001:14000])
userlocation7 <- geocode(userframe.dt$location[14001:16000])
userlocation8 <- geocode(userframe.dt$location[16001:17066])
location <- bind_rows(userlocation,userlocation1)