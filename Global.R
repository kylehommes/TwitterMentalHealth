# Libraries needed for the app

# Load the data
location <- read.csv("location.csv")
m <- read.csv("mhafreq.csv")
a <- read.csv("~/Desktop/afreq.csv")
s <- read.csv("~/Desktop/sfreq.csv")
d <- read.csv("dfreq.csv")
p <- read.csv("pfreq.csv")
t <- read.csv("totfreq.csv")
mhatweet <- read.csv("mhatweet.csv")
anxtweet <- read.csv("anxtweet.csv")
suitweet <- read.csv("mhatweet.csv")
ptsdtweet <- read.csv("anxtweet.csv")
deptweet <- read.csv("mhatweet.csv")
totaltweet <- read.csv("anxtweet.csv")

# Choices for the Input Selection
Hashtag = list("#MentalHealthAwareness" = "m",
  "#Anxiety" = "a","#Suicide" = "s","#Depression" = "d",
  "#PTSD" = "p","All Hashtags" = "t")

