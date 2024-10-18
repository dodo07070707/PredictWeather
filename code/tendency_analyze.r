library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(httpgd)

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
    data <- data %>% mutate(across(everything(), as.character)) %>% separate(일시, into = c("날짜", "시각"), sep = " ")
    
    # 데이터 프레임에 데이터 병합
    combined_data <- bind_rows(combined_data, data)
  }
}

# 시각이 00:00일 때 이전 24시간 동안 1시간 단위 변화량 평균 계산
calculate_hourly_changes <- function(data) {
  # 결과를 저장할 데이터 프레임
  changes_summary <- data.frame(날짜 = character(), 풍속변화량평균 = numeric(), 습도변화량평균 = numeric(), 기압변화량평균 = numeric())
  
  # "시각"이 00:00인 행을 찾음
  zero_time_indices <- which(data$시각 == "00:00")
  
  # 각 "00:00" 인덱스에 대해 이전 24시간 데이터의 변화량 계산
  for (idx in zero_time_indices) {
    if (idx > 24) {
      previous_data <- data[(idx - 24):(idx - 1), ]
      
      # 1시간 단위 변화량 계산 (차분)
      wind_diff <- diff(as.numeric(previous_data$풍속))  # 풍속의 1시간 단위 변화량
      humidity_diff <- diff(as.numeric(previous_data$습도))  # 습도의 1시간 단위 변화량
      pressure_diff <- diff(as.numeric(previous_data$현지기압))  # 현지기압의 1시간 단위 변화량
      
      # 1시간 단위 변화량의 평균 계산
      wind_avg <- mean(wind_diff, na.rm = TRUE)
      humidity_avg <- mean(humidity_diff, na.rm = TRUE)
      pressure_avg <- mean(pressure_diff, na.rm = TRUE)
      
      # 해당 날짜와 변화량 평균을 저장
      changes_summary <- rbind(changes_summary, data.frame(
        날짜 = data$날짜[idx],
        풍속변화량평균 = wind_avg,
        습도변화량평균 = humidity_avg,
        기압변화량평균 = pressure_avg
      ))
    }
  }
  
  return(changes_summary)
}

# 풍속, 습도, 현지기압 1시간 단위 변화량의 평균을 계산
changes_summary <- calculate_hourly_changes(combined_data)

# 강수량이 있는 날짜의 전날을 필터링하는 함수
filter_by_precipitation <- function(data, original_data) {
  # 강수량이 0이 아닌 날짜 찾기
  precipitation_dates <- unique(original_data$날짜[as.numeric(original_data$강수량) > 0])
  
  # 전날에 해당하는 날짜를 찾기
  previous_days <- as.character(as.Date(precipitation_dates) - 1)
  
  # 전날의 날짜가 실제로 강수량이 0인지 확인 후 필터링
  filtered_data <- data[data$날짜 %in% previous_days & !(previous_days %in% original_data$날짜[as.numeric(original_data$강수량) > 0]), ]
  
  return(filtered_data)
}

# 강수량이 있는 날짜의 전날의 변화량 평균만 필터링
filtered_changes_summary <- filter_by_precipitation(changes_summary, combined_data)

# 결과 출력
#print(filtered_changes_summary)
#print(paste("length :",nrow(filtered_changes_summary)))

# 풍속변화량평균의 평균 계산
average_wind_change <- mean(filtered_changes_summary$풍속변화량평균, na.rm = TRUE)

# 습도변화량평균의 평균 계산
average_humidity_change <- mean(filtered_changes_summary$습도변화량평균, na.rm = TRUE)

# 기압변화량평균의 평균 계산
average_pressure_change <- mean(filtered_changes_summary$기압변화량평균, na.rm = TRUE)

# 결과 출력
cat("풍속변화량평균의 평균:", average_wind_change, "\n")
cat("습도변화량평균의 평균:", average_humidity_change, "\n")
cat("기압변화량평균의 평균:", average_pressure_change, "\n")

