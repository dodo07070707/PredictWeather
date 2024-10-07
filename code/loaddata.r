library(readxl)
library(dplyr)

combined_data <- data.frame()

for (year in 2014:2023) {
  # 파일 이름 생성
  file_name <- paste0("data/weather_", year, ".xlsx")
  
  # 파일 경로 생성
  file_path <- file.path(getwd(), file_name)

  if (file.exists(file_path)) {
    # 엑셀 파일 읽기
    data <- read_excel(file_path)
    
    # NA 값을 0으로 변환
    data[is.na(data)] <- 0

    # Datetime 전처리
    data <- data %>% mutate(across(everything(), as.character))
    
    # 데이터 프레임에 데이터 병합
    combined_data <- bind_rows(combined_data, data)
  }
}

#갯수 출력
num_rows <- nrow(combined_data)
print(summary(combined_data))
cat("Total Number of Weather(Recently 10 Years):",num_rows,"개","\n")
