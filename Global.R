Hashtag = list("#MentalHealthAwareness" = "mha",
  "#Anxiety" = "anx","#Suicide" = "sui","#Depression" = "dep",
  "#PTSD" = "ptsd","All Hashtags" = "tot")

dofunc <- function(x) {
  if (x == "mha"){
  a = mhafreq.df$word
  b = mhafreq.df$freq
  }
  else if (x == "anx"){
  a = afreq.df$word
  b = afreq.df$freq
  }
  else if (x == "sui"){
  a = sfreq.df$word
  b = sfreq.df$freq
  }
  else if (x == "dep"){
  a = dfreq.df$word
  b = dfreq.df$freq
  }
  else if (x == "ptsd"){
  a = pfreq.df$word
  b = pfreq.df$freq
  }
  else if (x == "tot"){
  a = totfreq.df$word
  b = totfreq.df$freq
  }
  else print("Unknown Hashtag")
}
