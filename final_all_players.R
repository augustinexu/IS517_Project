library(MASS)
require(ISLR)
require(caret)
require(e1071)
library(gbm)

game_details = read_csv('2019-2020_ANNO_games_details.csv')
game_details = subset(game_details, select = -c(COMMENT))
game_details$WIN = as.factor(game_details$WIN)
head(game_details)
summary(game_details)

#point guards 
point_guards = subset(game_details, game_details$START_POSITION == 'PG')
point_guards = subset(point_guards, select = -c(START_POSITION, GAME_ID, TEAM_ID, TEAM_ABBREVIATION, 
                                                TEAM_CITY, PLAYER_ID, PLAYER_NAME))
point_guards$WIN = as.factor(point_guards$WIN)
point_guards = na.omit(point_guards)
train <- sample(nrow(point_guards) * 0.7)
train_set <- point_guards[train, ]
test_set <- point_guards[-train, ]

log.fit <- glm(WIN ~ ., data = train_set, family = "binomial")
log.probability <- predict(log.fit, newdata = test_set, type = "response")
log.probability2 <- predict(log.fit, newdata = train_set, type = "response")
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
log.prediction2 <- ifelse(log.probability2 > 0.5, 1, 0)
table(test_set$WIN, log.prediction)
pg_log_train_error = mean(log.prediction2 != train_set$WIN)
pg_log_test_error = mean(log.prediction != test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
pg_svm_train_error = 1 - postResample(predict(svm.fit2, train_set), train_set$WIN)[1]
pg_svm_test_error = 1 - postResample(predict(svm.fit2, test_set), test_set$WIN)[1]

rf.fit <- randomForest(WIN ~ ., data = train_set, mtry = 5, importance=TRUE, ntrees=25)
rf.probability <- predict(rf.fit, newdata = test_set)
rf.prediction <- ifelse(rf.probability > 0.5, 1, 0)
pg_rf_train_error = 1 - postResample(predict(rf.fit, train_set), train_set$WIN)[1]
pg_rf_test_error = 1 - postResample(predict(rf.fit, test_set), test_set$WIN)[1]
pg_rf_errors <- c(pg_rf_train_error, pg_rf_test_error)
print(pg_rf_errors)

pg_train_errors <- c(pg_log_train_error, pg_svm_train_error, pg_rf_train_error)
print(pg_train_errors)
pg_test_errors <- c(pg_log_test_error, pg_svm_test_error, pg_rf_test_error)
print(pg_test_errors)

#shooting guards
shooting_guards = subset(game_details, game_details$START_POSITION == 'SG')
shooting_guards = subset(shooting_guards, select = -c(START_POSITION, GAME_ID, TEAM_ID, TEAM_ABBREVIATION, 
                                                TEAM_CITY, PLAYER_ID, PLAYER_NAME))
shooting_guards$WIN = as.factor(shooting_guards$WIN)
shooting_guards = na.omit(shooting_guards)
train <- sample(nrow(shooting_guards) * 0.7)
train_set <- shooting_guards[train, ]
test_set <- shooting_guards[-train, ]

log.fit <- glm(WIN ~ ., data = train_set, family = "binomial")
log.probability <- predict(log.fit, newdata = test_set, type = "response")
log.probability2 <- predict(log.fit, newdata = train_set, type = "response")
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
log.prediction2 <- ifelse(log.probability2 > 0.5, 1, 0)
table(test_set$WIN, log.prediction)
sg_log_train_error = mean(log.prediction2 != train_set$WIN)
sg_log_test_error = mean(log.prediction != test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
sg_svm_train_error = 1 - postResample(predict(svm.fit2, train_set), train_set$WIN)[1]
sg_svm_test_error = 1 - postResample(predict(svm.fit2, test_set), test_set$WIN)[1]

rf.fit <- randomForest(WIN ~ ., data = train_set, mtry = 5, importance=TRUE, ntrees=25)
rf.probability <- predict(rf.fit, newdata = test_set)
rf.prediction <- ifelse(rf.probability > 0.5, 1, 0)
sg_rf_train_error = 1 - postResample(predict(rf.fit, train_set), train_set$WIN)[1]
sg_rf_test_error = 1 - postResample(predict(rf.fit, test_set), test_set$WIN)[1]

sg_train_errors <- c(sg_log_train_error, sg_svm_train_error, sg_rf_train_error)
print(sg_train_errors)
sg_test_errors <- c(sg_log_test_error, sg_svm_test_error, sg_rf_test_error)
print(sg_test_errors)

#small forwards
small_forwards = subset(game_details, game_details$START_POSITION == 'SF')
small_forwards = subset(small_forwards, select = -c(START_POSITION, GAME_ID, TEAM_ID, TEAM_ABBREVIATION, 
                                                      TEAM_CITY, PLAYER_ID, PLAYER_NAME))
small_forwards$WIN = as.factor(small_forwards$WIN)
small_forwards = na.omit(small_forwards)
train <- sample(nrow(small_forwards) * 0.7)
train_set <- small_forwards[train, ]
test_set <- small_forwards[-train, ]

log.fit <- glm(WIN ~ ., data = train_set, family = "binomial")
log.probability <- predict(log.fit, newdata = test_set, type = "response")
log.probability2 <- predict(log.fit, newdata = train_set, type = "response")
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
log.prediction2 <- ifelse(log.probability2 > 0.5, 1, 0)
table(test_set$WIN, log.prediction)
sf_log_train_error = mean(log.prediction2 != train_set$WIN)
sf_log_test_error = mean(log.prediction != test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
sf_svm_train_error = 1 - postResample(predict(svm.fit2, train_set), train_set$WIN)[1]
sf_svm_test_error = 1 - postResample(predict(svm.fit2, test_set), test_set$WIN)[1]

rf.fit <- randomForest(WIN ~ ., data = train_set, mtry = 5, importance=TRUE, ntrees=25)
rf.probability <- predict(rf.fit, newdata = test_set)
rf.prediction <- ifelse(rf.probability > 0.5, 1, 0)
sf_rf_train_error = 1 - postResample(predict(rf.fit, train_set), train_set$WIN)[1]
sf_rf_test_error = 1 - postResample(predict(rf.fit, test_set), test_set$WIN)[1]

sf_train_errors <- c(sf_log_train_error, sf_svm_train_error, sf_rf_train_error)
print(sf_train_errors)
sf_test_errors <- c(sf_log_test_error, sf_svm_test_error, sf_rf_test_error)
print(sf_test_errors)

#power forwards
power_forwards = subset(game_details, game_details$START_POSITION == 'PF')
power_forwards = subset(power_forwards, select = -c(START_POSITION, GAME_ID, TEAM_ID, TEAM_ABBREVIATION, 
                                                    TEAM_CITY, PLAYER_ID, PLAYER_NAME))
power_forwards$WIN = as.factor(power_forwards$WIN)
power_forwards = na.omit(power_forwards)
train <- sample(nrow(power_forwards) * 0.7)
train_set <- power_forwards[train, ]
test_set <- power_forwards[-train, ]

log.fit <- glm(WIN ~ ., data = train_set, family = "binomial")
log.probability <- predict(log.fit, newdata = test_set, type = "response")
log.probability2 <- predict(log.fit, newdata = train_set, type = "response")
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
log.prediction2 <- ifelse(log.probability2 > 0.5, 1, 0)
table(test_set$WIN, log.prediction)
pf_log_train_error = mean(log.prediction2 != train_set$WIN)
pf_log_test_error = mean(log.prediction != test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
pf_svm_train_error = 1 - postResample(predict(svm.fit2, train_set), train_set$WIN)[1]
pf_svm_test_error = 1 - postResample(predict(svm.fit2, test_set), test_set$WIN)[1]

rf.fit <- randomForest(WIN ~ ., data = train_set, mtry = 5, importance=TRUE, ntrees=25)
rf.probability <- predict(rf.fit, newdata = test_set)
rf.prediction <- ifelse(rf.probability > 0.5, 1, 0)
pf_rf_train_error = 1 - postResample(predict(rf.fit, train_set), train_set$WIN)[1]
pf_rf_test_error = 1 - postResample(predict(rf.fit, test_set), test_set$WIN)[1]

pf_train_errors <- c(pf_log_train_error, pf_svm_train_error, pf_rf_train_error)
print(pf_train_errors)
pf_test_errors <- c(pf_log_test_error, pf_svm_test_error, pf_rf_test_error)
print(pf_test_errors)

#centers
centers = subset(game_details, game_details$START_POSITION == 'C')
centers = subset(centers, select = -c(START_POSITION, GAME_ID, TEAM_ID, TEAM_ABBREVIATION, 
                                                    TEAM_CITY, PLAYER_ID, PLAYER_NAME))
centers$WIN = as.factor(centers$WIN)
centers = na.omit(centers)
train <- sample(nrow(centers) * 0.7)
train_set <- centers[train, ]
test_set <- centers[-train, ]

log.fit <- glm(WIN ~ ., data = train_set, family = "binomial")
log.probability <- predict(log.fit, newdata = test_set, type = "response")
log.probability2 <- predict(log.fit, newdata = train_set, type = "response")
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
log.prediction2 <- ifelse(log.probability2 > 0.5, 1, 0)
table(test_set$WIN, log.prediction)
c_log_train_error = mean(log.prediction2 != train_set$WIN)
c_log_test_error = mean(log.prediction != test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
c_svm_train_error = 1 - postResample(predict(svm.fit2, train_set), train_set$WIN)[1]
c_svm_test_error = 1 - postResample(predict(svm.fit2, test_set), test_set$WIN)[1]

rf.fit <- randomForest(WIN ~ ., data = train_set, mtry = 5, importance=TRUE, ntrees=25)
rf.probability <- predict(rf.fit, newdata = test_set)
rf.prediction <- ifelse(rf.probability > 0.5, 1, 0)
c_rf_train_error = 1 - postResample(predict(rf.fit, train_set), train_set$WIN)[1]
c_rf_test_error = 1 - postResample(predict(rf.fit, test_set), test_set$WIN)[1]

c_train_errors <- c(c_log_train_error, c_svm_train_error, c_rf_train_error)
print(c_train_errors)
c_test_errors <- c(c_log_test_error, c_svm_test_error, c_rf_test_error)
print(c_test_errors)

#model results

log_train_errors = c(pg_log_train_error, sg_log_train_error, sf_log_train_error, pf_log_train_error, 
                     c_log_train_error)
log_test_errors = c(pg_log_test_error, sg_log_test_error, sf_log_test_error, pf_log_test_error, 
                    c_log_test_error)
print(log_train_errors)
print(log_test_errors)

svm_train_errors = c(pg_svm_train_error, sg_svm_train_error, sf_svm_train_error, pf_svm_train_error,
                     c_svm_train_error)
svm_test_errors = c(pg_svm_test_error, sg_svm_test_error, sf_svm_test_error, pf_svm_test_error,
                    c_svm_test_error)
print(svm_train_errors)
print(svm_test_errors)

rf_train_errors = c(pg_rf_train_error, sg_rf_train_error, sf_rf_train_error, pf_rf_train_error,
                    c_rf_train_error)
rf_test_errors = c(pg_rf_test_error, sg_rf_test_error, sf_rf_test_error, pf_rf_test_error, 
                   c_rf_test_error)
print(rf_train_errors)
print(rf_test_errors)



