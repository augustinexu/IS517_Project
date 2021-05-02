library(MASS)
require(ISLR)
require(caret)
require(e1071)
library(gbm)

game_details = read_csv('2019-2020_ANNO_games_details.csv')
game_details = subset(game_details, select = -c(COMMENT))
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
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
table(test_set$WIN, log.prediction)

svm.fit <- svm(WIN ~ ., data = train_set, kernel = 'linear', cost = 0.01)
postResample(predict(svm.fit, train_set), train_set$WIN)
postResample(predict(svm.fit, test_set), test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
postResample(predict(svm.fit2, train_set), train_set$WIN)
postResample(predict(svm.fit2, test_set), test_set$WIN)

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
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
table(test_set$WIN, log.prediction)

svm.fit <- svm(WIN ~ ., data = train_set, kernel = 'linear', cost = 0.01)
postResample(predict(svm.fit, train_set), train_set$WIN)
postResample(predict(svm.fit, test_set), test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
postResample(predict(svm.fit2, train_set), train_set$WIN)
postResample(predict(svm.fit2, test_set), test_set$WIN)

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
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
table(test_set$WIN, log.prediction)

svm.fit <- svm(WIN ~ ., data = train_set, kernel = 'linear', cost = 0.01)
postResample(predict(svm.fit, train_set), train_set$WIN)
postResample(predict(svm.fit, test_set), test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
postResample(predict(svm.fit2, train_set), train_set$WIN)
postResample(predict(svm.fit2, test_set), test_set$WIN)

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
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
table(test_set$WIN, log.prediction)

svm.fit <- svm(WIN ~ ., data = train_set, kernel = 'linear', cost = 0.01)
postResample(predict(svm.fit, train_set), train_set$WIN)
postResample(predict(svm.fit, test_set), test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
postResample(predict(svm.fit2, train_set), train_set$WIN)
postResample(predict(svm.fit2, test_set), test_set$WIN)

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
log.prediction <- ifelse(log.probability > 0.5, 1, 0)
table(test_set$WIN, log.prediction)

svm.fit <- svm(WIN ~ ., data = train_set, kernel = 'linear', cost = 0.01)
postResample(predict(svm.fit, train_set), train_set$WIN)
postResample(predict(svm.fit, test_set), test_set$WIN)

svm.fit2 <- svm(WIN ~ ., data = train_set, kernel = 'radial', cost = 0.01)
postResample(predict(svm.fit2, train_set), train_set$WIN)
postResample(predict(svm.fit2, test_set), test_set$WIN)

