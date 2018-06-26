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
location <- geocode(userframe.dt$location)

# Repeat steps for all hashtags
suitweets.df <- twListToDF(suicidetweets)
userinfo_s <- lookupUsers(suitweets.df$screenName)
userframe_s <- twListToDF(userinfo_s)
userframe.dt_s <- data.table(userframe)
userframe.dt_s$location <-
  userframe.dt_s$location[!userframe.dt_s$location %in% ""]
location_s <- geocode(userframe.dt_s$location)

deptweets.df <- twListToDF(depressiontweets)
userinfo_d <- lookupUsers(deptweets.df$screenName)
userframe_d <- twListToDF(userinfo_d)
userframe.dt_d <- data.table(userframe_d)
userframe.dt_d$location <-
  userframe.dt_d$location[!userframe.dt_d$location %in% ""]
location_d <- geocode(userframe.dt_d$location)

anxtweets.df <- twListToDF(anxietytweets)
userinfo_a <- lookupUsers(anxtweets.df$screenName)
userframe_a <- twListToDF(userinfo_a)
userframe.dt_a <- data.table(userframe_a)
userframe.dt_a$location <-
  userframe.dt_a$location[!userframe.dt_a$location %in% ""]
userlocation_a <- geocode(userframe.dt_a$location)

ptsdtweets.df <- twListToDF(ptsdtweets)
userinfo_p <- lookupUsers(ptsdtweets.df$screenName)
userframe_p <- twListToDF(userinfo_p)
userframe.dt_p <- data.table(userframe_a)
userframe.dt_p$location <-
  userframe.dt_p$location[!userframe.dt_p$location %in% ""]
location_p <- geocode(userframe.dt_p$location[1:2500])