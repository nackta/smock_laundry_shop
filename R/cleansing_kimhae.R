### 김해시 공장데이터와 김해시 세탁량 데이터 결합 ###
Sys.setlocale(category = "LC_ALL", locale = "korean")
# 훈련용 데이터 전처리
# 필요 라이브러리 ####
#install.packages("xlsx") #.xlsx용 # java설치 필요
#install.packages("dplyr")
#install.packages("tidyverse")
filename <- "./R/select_local.R"
eval(parse(filename, encoding="UTF-8"))
library(xlsx) 
library(dplyr)
library(tidyverse)
library(readxl)
library(geosphere)

# 가야세탁소와 거리 변수 추가
laundry_lonlat <- c(128.83217949362316,35.21534343937442) #가야세탁소 좌표입력
kimhae_factory <- select_local("김해시")
kimhae_factory <- bind_cols(kimhae_factory, "세탁소와거리" = distGeo(kimhae_factory[,c("경도","위도")], laundry_lonlat))

# 가야세탁소 세탁 현황
kimhae_laundry <- read_csv("./raw data/factory data/kimhae laundry/kimhae laundry.csv", locale = locale(encoding = "EUC-KR"))
# 업체명 문자열 처리
kimhae_laundry$수주업체명 <- kimhae_laundry$수주업체명 %>% str_to_upper() %>% str_remove("[:punct:].*[:punct:]")

# 매핑파일 생성
kimhae_match <- read_csv("raw data/factory data/kimhae laundry/kimhae match.csv", 
                         col_types = cols(...1 = col_skip()), 
                         locale = locale(encoding = "EUC-KR")) %>% rename("수주업체명" = x, "factory" = ...3) %>% select(c(수주업체명,factory))

# 공장별 세탁수량
kimhae_laundry_mm <- kimhae_laundry %>% 
  select(-접수일자) %>%
  group_by(수주업체명) %>%
  summarise(공장별세탁량 = sum(세탁수량)) %>%
  filter(수주업체명 != "개인") %>% 
  left_join(kimhae_match, by="수주업체명") %>%
  filter(!is.na(.data$factory)) 

kimhae_train_data <- kimhae_factory %>% 
  left_join(kimhae_laundry_mm, by=c("회사명"="factory")) %>%
  filter(!is.na(.data$위도)) %>%
  mutate(공장별세탁량 = case_when(
    is.na(공장별세탁량) ~ 0,
    TRUE ~ 공장별세탁량))
  
write.csv(kimhae_train_data, "./data/kimhae_train_data.csv", row.names = F)
