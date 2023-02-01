Sys.setlocale(category = "LC_ALL", locale = "korean")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_351")

library(xlsx) 
library(dplyr)
library(tidyverse)
library(readxl)
library(geosphere)

### 근로환경조사 전처리 ####
# AS06:지역, KQ04:직업분류, Q25_5:분진노출정도, Q25_6:유기용제 노출정도, Q25_7:화학제품노출정도
kwcs <- read_csv("raw data/5th_KWCS/KWCS5th(190924).csv") %>%
  select(AS06, KQ04, Q25_5, Q25_6, Q25_7)
cleansed_kwcs <- kwcs %>% rename(지역 = AS06, 직업분류 = KQ04, 분진노출정도 = Q25_5, 유기용제노출정도 = Q25_6, 화학제품노출정도 = Q25_7)

workkind <- c("관리자", "전문가", "기술공 및 준전문가", "사무 종사자", "서비스 종사자",
              "판매 종사자", "농림어업 숙련 종사자", "기능원 및 관련 기능 종사자", "장치기계 조작 및 조립 종사자", "단순노무 종사자",
              "군인", "무응답", "거절")
workcode <- data.frame(직업분류 = 1:13, 직업항목 = workkind)

cl_kwcs <- cleansed_kwcs %>% mutate(직업항목 = workcode[match(cleansed_kwcs$직업분류, workcode$직업분류),2])

write.csv(cl_kwcs, "data/cleansed_kwcs_data.csv", row.names = F)


### 9차 산업보건 실태조사 전처리 ####
siltae <- read_csv("raw data/Survey on kosha/9th_siltae_public_new_label.csv", locale = locale(encoding = "EUC-KR")) %>%
  select(SQ5, Q1_2_1, Q10_2_2) %>%
  rename(소재지 = SQ5, 업종명 = Q1_2_1, 화학물질노출근무자수 = Q10_2_2)
siltae[is.na(siltae$화학물질노출근무자수),3] <- 0
write.csv(siltae, "data/cleansed_siltae_data.csv", row.names = F)


# 전국 산업체 데이터 결합, 지오코딩, 공장규모가중치 추가, 필요변수 선택, 소분류 항목 추가 ###

# 전국산업체데이터
Gangwon <- readxl::read_xlsx("./raw data/factory data/강원도.xlsx") %>% select(-"순번")
GSN <- readxl::read_xlsx("./raw data/factory data/경상남도.xlsx") %>% select(-"순번")
GSB <- readxl::read_xlsx("./raw data/factory data/경상북도.xlsx") %>% select(-"순번")
Gwangju <- readxl::read_xlsx("./raw data/factory data/광주광역시.xlsx") %>% select(-"순번")
Daegu <- readxl::read_xlsx("./raw data/factory data/대구광역시.xlsx") %>% select(-"순번")
Daejeon <- readxl::read_xlsx("./raw data/factory data/대전광역시.xlsx") %>% select(-"순번")
Busan <- readxl::read_xlsx("./raw data/factory data/부산광역시.xlsx") %>% select(-"순번")
Seoul <- readxl::read_xlsx("./raw data/factory data/서울특별시.xlsx") %>% select(-"순번")
Sejong <- readxl::read_xlsx("./raw data/factory data/세종특별자치시.xlsx") %>% select(-"순번")
Ulsan <- readxl::read_xlsx("./raw data/factory data/울산광역시.xlsx") %>% select(-"순번")
Incheon <- readxl::read_xlsx("./raw data/factory data/인천광역시.xlsx") %>% select(-"순번")
JLN <- readxl::read_xlsx("./raw data/factory data/전라남도.xlsx") %>% select(-"순번")
JLB <- readxl::read_xlsx("./raw data/factory data/전라북도.xlsx") %>% select(-"순번")
Jeju <- readxl::read_xlsx("./raw data/factory data/제주특별자치도.xlsx") %>% select(-"순번")
CCN <- readxl::read_xlsx("./raw data/factory data/충청남도.xlsx") %>% select(-"순번")
CCB <- readxl::read_xlsx("./raw data/factory data/충청북도.xlsx") %>% select(-"순번")
GG <- readxl::read_xlsx("./raw data/factory data/경기도.xlsx") %>% select(-"순번")

# 171701 obs. 20variables
factory_data <- bind_rows(Gangwon = Gangwon, GSB = GSB, GSN = GSN, Gwangju = Gwangju, 
                          Daegu = Daegu, Daejeon = Daejeon, Busan = Busan, Seoul = Seoul, 
                          Sejong = Sejong, Ulsan = Ulsan, Incheon = Incheon, JLB = JLB, 
                          JLN = JLN, Jeju = Jeju, CCN = CCN, CCB = CCB, GG = GG, .id = "groups") %>% 
  rename("도로명주소" = "공장주소(도로명)", "지번주소" = "공장주소(지번)", "종업원수" = "종업원수(총인원)") %>%
  filter(!(is.na(도로명주소) & is.na(지번주소))) # 도로명주소와 지번주소 둘 다 NA인 경우 제거(3건)

# 위경도 변환 서비스 사용을 위한 데이터 설정
# 정규표현식을 사용하여 표준데이터셋 점검 서비스를 이용 가능하도록 주소형식 변환
factory_data$도로명주소 <- str_remove_all(factory_data$도로명주소, "외 [:digit:]필지")
factory_data$지번주소 <- str_remove_all(factory_data$지번주소, "외 [:digit:]필지")
factory_data$도로명주소 <- str_remove_all(factory_data$도로명주소, "번지.*")
factory_data$지번주소 <- str_remove_all(factory_data$지번주소, "번지.*")

# 표준데이터셋 점검 서비스 이용을 위한 데이터 설정
factory_address <- factory_data %>% 
  select("도로명주소", "지번주소") %>%
  mutate("위도" = 0, "경도" = 0)

#메모리 제한으로 인해 데이터 분할하여 저장
factory_address1 <- factory_address[1:25000,]
factory_address2 <- factory_address[25001:50000,]
factory_address3 <- factory_address[50001:75000,]
factory_address4 <- factory_address[75001:100000,]
factory_address5 <- factory_address[100001:125000,]
factory_address6 <- factory_address[125001:150000,]
factory_address7 <- factory_address[-1:-150000,]


# 메모리 부족 시 R session 초기화 후 다음 줄부터 실행
write.xlsx2(as.data.frame(factory_address1), "./raw data/factory data/lonlat data/factory_address1.xlsx", row.names = F)
write.xlsx2(as.data.frame(factory_address2), "./raw data/factory data/lonlat data/factory_address2.xlsx", row.names = F)
write.xlsx2(as.data.frame(factory_address3), "./raw data/factory data/lonlat data/factory_address3.xlsx", row.names = F)
write.xlsx2(as.data.frame(factory_address4), "./raw data/factory data/lonlat data/factory_address4.xlsx", row.names = F)
write.xlsx2(as.data.frame(factory_address5), "./raw data/factory data/lonlat data/factory_address5.xlsx", row.names = F)
write.xlsx2(as.data.frame(factory_address6), "./raw data/factory data/lonlat data/factory_address6.xlsx", row.names = F)
write.xlsx2(as.data.frame(factory_address7), "./raw data/factory data/lonlat data/factory_address7.xlsx", row.names = F)
# 이후 표준데이터셋 점검 서비스를 이용해 위경도 데이터 추출


### 전국산업체위경도 read ###
factory_lonlat1 <- readxl::read_xls("./raw data/factory data/lonlat data/lonlat/factory_address1.xls")
factory_lonlat2 <- readxl::read_xls("./raw data/factory data/lonlat data/lonlat/factory_address2.xls")
factory_lonlat3 <- readxl::read_xls("./raw data/factory data/lonlat data/lonlat/factory_address3.xls")
factory_lonlat4 <- readxl::read_xls("./raw data/factory data/lonlat data/lonlat/factory_address4.xls")
factory_lonlat5 <- readxl::read_xls("./raw data/factory data/lonlat data/lonlat/factory_address5.xls")
factory_lonlat6 <- readxl::read_xls("./raw data/factory data/lonlat data/lonlat/factory_address6.xls")
factory_lonlat7 <- readxl::read_xls("./raw data/factory data/lonlat data/lonlat/factory_address7.xls")
# 위경도가 추출안된 데이터 9971건, 추후에 지역별 분석시 GeocodeXr프로그램으로 지오코딩.
factory_lonlat <- bind_rows(factory_lonlat1, factory_lonlat2, factory_lonlat3, factory_lonlat4, factory_lonlat5, factory_lonlat6, factory_lonlat7) %>% select(위도, 경도)

# 공장규모에 따른 가중치 생성, 필요변수 선택 ####
# 공장규모별 가중치 생성, 소기업>중기업>중견기업>대기업
w_factory <- function(data){
  data %>% mutate(공장규모가중치 = if_else(공장규모 == "소기업", 3,
                                        if_else(공장규모 == "중기업", 2,
                                                    if_else(공장규모 == "중견기업", 1, 0)))) %>% return()
}

cleansed_factory <- bind_cols(factory_data, factory_lonlat) %>%
  w_factory() %>%
  select("회사명", "산업단지명", "대표업종", "생산품", "원자재", "groups", "공장규모", "공장규모가중치", "종업원수", "도로명주소", "지번주소" ,"위도", "경도")
colnames(cleansed_factory)


### 업종코드 항목명 추가 ###
#표준산업분류, 소분류코드
work_code_s <- read_excel("raw data/work of kind_code.xlsx", 
                         col_types = c("skip", "skip", "skip", "skip", 
                                       "text", "text", "skip", "skip", 
                                       "skip", "skip"), skip = 2) %>% filter(!is.na(코드)) %>% rename("소분류" = "코드")
# 소분류 변수 매칭
cleansed_factory_s <- cleansed_factory %>% mutate(소분류 = str_sub(대표업종, 1, 3)) %>%
  left_join(work_code_s, by = "소분류") %>%
  rename("소분류항목명" = "항목명")


#표준산업분류, 중분류코드
work_code_m <- read_excel("raw data/work of kind_code.xlsx", 
                         col_types = c("skip", "skip", "text", "text", 
                                       "skip", "skip", "skip", "skip", 
                                       "skip", "skip"), skip = 2) %>% filter(!is.na(코드)) %>% rename("중분류" = "코드")
# 중분류 변수 매칭
cleansed_factory_m <- cleansed_factory_s %>% mutate(중분류 = str_sub(대표업종, 1, 2)) %>%
  left_join(work_code_m, by = "중분류") %>%
  rename("중분류항목명" = "항목명")

# 1차 정제데이터 저장
write.csv(cleansed_factory_m, "data/cleansed_factory.csv", row.names = F)