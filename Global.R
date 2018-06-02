Hashtag = list("#MentalHealthAwareness" = "m",
  "#Anxiety" = "a","#Suicide" = "s","#Depression" = "d",
  "#PTSD" = "p","All Hashtags" = "t")

OtherHashtag = list("#MentalHealthAwareness" = "mm",
                "#Anxiety" = "aa","#Suicide" = "ss","#Depression" = "dd",
                "#PTSD" = "pp","All Hashtags" = "tt")

dofunc <- function(x) {
  if (x == 1){
  return(m)
  }
  else if (x == 2){
  return(a)
  }
  else if (x == 3){
  return(s)
  }
  else if (x == 4){
  return(d)
  }
  else if (x == 5){
  return(p)
  }
  else if (x == 6){
  return(t)
  }
  else print("Unknown Hashtag")
}

dofunc2 <- function(x) {
  if (x == 1){
    return(mm)
  }
  else if (x == 2){
    return(aa)
  }
  else if (x == 3){
    return(ss)
  }
  else if (x == 4){
    return(dd)
  }
  else if (x == 5){
    return(pp)
  }
  else if (x == 6){
    return(tt)
  }
  else print("Unknown Hashtag")
}

m <- mhafreq.df
a <- afreq.df
s <- sfreq.df
t <- totfreq.df
d <- dfreq.df
p <- pfreq.df