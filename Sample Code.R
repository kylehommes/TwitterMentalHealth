set.seed(1234)
totaltweet.sample <- sample_n(totaltweet, 1000)
tottxt.sample <- totaltweet.sample$text
tottxt.sample <- iconv(tottxt.sample, "latin1", "ASCII", sub = "")
tottxt.sample <- gsub("http(s?)([^ ]*)", " ", 
  tottxt.sample, ignore.case = T)
tottxt.sample <- gsub("&amp", "and", tottxt.sample)
totcorpus.sample <- VCorpus(VectorSource(tottxt.sample))
totcorpus.sample <- tm_map(totcorpus.sample, PlainTextDocument)
tottdm.sample <- TermDocumentMatrix(x= totcorpus.sample, 
  control= ctrl)
tottdm.sample.mat <- as.matrix(tottdm.sample)
tottdm.sample.lsa <- lw_bintf(tottdm.sample.mat) *
  gw_idf(tottdm.sample.mat)
lsaSpace.sample <- lsa(tottdm.sample.lsa)
tottdm.sample.dist <- dist(t(as.textmatrix(lsaSpace.sample)))
fit <- cmdscale(tottdm.sample.dist, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[,1], y = fit$points[,2])
ggplot(data = points) +
  geom_point(aes(x=x,y=y,color = totaltweet.sample$hashtag)) + 
  geom_text(data = points, aes(x=x,y=y-0.2,
  label = row.names(totaltweet.sample)))
fit.3 <- cmdscale(tottdm.sample.dist, eig = TRUE, k = 3)
scatterplot3d(fit.3$points[, 1], fit.3$points[, 2], 
  fit.3$points.3[, 3], color = "blue", pch = 16, 
  main = "Semantic Space Scaled to 3D", xlab = "x", 
  ylab = "y", zlab = "z", type = "h")