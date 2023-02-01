# 세탁복 수요 예측 모델 ####
Sys.setlocale(category = "LC_ALL", locale = "korean")
Sys.setlocale(category = "LC_ALL", locale = "de")
# 필요 라이브러리 ####
#install.packages("xlsx")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("doParallel")
library(xlsx)
library(dplyr)
library(tidyverse)
library(readxl)
library(caret)
library(doParallel)

# 김해 훈련용 데이터
kimhae_train_data <- read_csv("data/kimhae_train_data.csv", locale = locale(encoding = "EUC-KR")) %>% 
  select(공장규모, 종업원수, 중분류항목명, 공장별세탁량) %>% na.omit()


# 데이터 형태 정의

# 중분류코드
work_code2 <- read_excel("raw data/work of kind_code.xlsx", 
                         col_types = c("skip", "skip", "text", "text", 
                                       "skip", "skip", "skip", "skip", 
                                       "skip", "skip"), skip = 2) %>% filter(!is.na(코드))
kimhae_train_data$중분류항목명 <- factor(x = kimhae_train_data$중분류항목명, levels = work_code2$항목명)


# 공장규모가중치(소기업, 중기업)
kimhae_train_data$공장규모 <- as.factor(kimhae_train_data$공장규모)

# 데이터 분할
set.seed(825)
trainindex <- sample(1:nrow(kimhae_train_data), size = nrow(kimhae_train_data)*0.7)

traindata <- kimhae_train_data[trainindex,]
testdata <- kimhae_train_data[-trainindex,]

# 모델 튜닝
fitControl <- trainControl(method = "boot", n = 25)

# 멀티코어 등록
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

# 모델생성
lm_m <- lm(공장별세탁량 ~ ., data = traindata)

xgbLinear_m <- train(공장별세탁량 ~ ., data = traindata, 
                        method = "xgbLinear",
                        seeds = 825,
                        trControl = fitControl)

xgbTree_m <- train(공장별세탁량 ~ ., data = traindata, 
                    method = "xgbTree",
                    seeds = 825,
                    trControl = fitControl)

xgbDART_m <- train(공장별세탁량 ~ ., data = traindata,
                        method = "xgbDART",
                        seeds = 825,
                        trControl = fitControl)


#재표본 결과 모델비교
resamps <- resamples(list(xgbLinear_m,
                          xgbTree_m,
                          xgbDART_m))
summary(resamps)

#적합값
postResample(xgbLinear_m$finalModel, obs = traindata$공장별세탁량)

# 평가용 데이터 예측
xgbLinear_pre <- predict(xgbLinear_m, newdata = testdata)
xgbDART_pre <- predict(xgbDART_m, newdata = testdata)
xgbTree_pre <- predict(xgbTree_m, newdata = testdata)

#성능 측정
xgbLinear <- postResample(pred = xgbLinear_pre, obs = testdata$공장별세탁량)
xgbTree <- postResample(pred = xgbTree_pre, obs = testdata$공장별세탁량)
xgbDART <- postResample(pred = xgbDART_pre, obs = testdata$공장별세탁량)

rbind(xgbLinear, xgbTree, xgbDART)

# 최종 모델
summary(xgbDART)

# 대전시 데이터 예측
dj_factory <- select_local("대전광역시") %>% 
  select(위도, 경도, 공장규모, 종업원수, 중분류항목명) %>%
  filter(!(is.na(공장규모)|is.na(종업원수)|is.na(중분류항목명))) %>% filter(공장규모 != "중견기업")

dj_pre <- predict(xgbDART_m, newdata = dj_factory %>% select(c(-위도, -경도)))
dj_pre_result <- bind_cols(dj_factory, "예상세탁량" = dj_pre)

write.csv(dj_pre_result, "result_data/dj_pre_result.csv")
