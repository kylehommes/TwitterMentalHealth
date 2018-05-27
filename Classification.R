set.seed(1234)
totaltweet_random <- sample_frac(totaltweet, 1L)
tottxt_ran <- totaltweet_random$text
tottxt_ran <- iconv(tottxt_ran, "latin1", "ASCII", sub = "")
tottxt_ran <- gsub("http(s?)([^ ]*)", " ", tottxt_ran, ignore.case = T)
tottxt_ran <- gsub("&amp", "and", tottxt_ran)
totcorpus_ran <- VCorpus(VectorSource(tottxt_ran))
totcorpus_ran <- tm_map(totcorpus_ran, PlainTextDocument)
totdtm_ran <- DocumentTermMatrix(x= totcorpus_ran, control= ctrl)
totdtm_ran <- removeSparseTerms(totdtm_ran, sparse = .99)
train <- totdtm_ran[1:1000,]
train.labels <- totaltweet_random$isRetweet[1:1000]
test <- totdtm_ran[1001:1500,]
test.labels <- totaltweet_random$isRetweet[1001:1500]
train <- as.data.frame(as.matrix(train))
test <- as.data.frame(as.matrix(test))
train.labels <- as.factor(train.labels)
test.labels <- as.factor(test.labels)
nb_model <- train(train, train.labels, method = "nb")
nb_pred <- predict(nb_model, test)
table(nb_pred, test.labels)
confusionMatrix(table(nb_pred, test.labels))
nb_cm <- confusionMatrix(table(nb_pred, test.labels))
svm_model <- train(train, train.labels, method = "svmLinear")
svm_pred <- predict(svm_model, test)
table(svm_pred, test.labels)
confusionMatrix(table(svm_pred, test.labels))
svm_cm <- confusionMatrix(table(svm_pred, test.labels))
fourfoldplot(svm_cm$table, color = c("#99CC99", "#CC6666"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
fourfoldplot(nb_cm$table, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")