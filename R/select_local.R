Sys.setlocale(category = "LC_ALL", locale = "korean")
library(tidyverse)
select_local <- function(local){
  cleansed_factory <- read_csv("./data/cleansed_factory.csv", locale = locale(encoding = "EUC-KR"))
  local_factory <- cleansed_factory %>% filter((str_detect(도로명주소, local) | str_detect(지번주소, local)))
  yes_lonlat <- local_factory %>% filter(위도 != 0)
  return(yes_lonlat)
}
