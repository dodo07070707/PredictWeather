library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(httpgd)
library(stringr)

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
    data <- data %>%
  mutate(across(everything(), as.character)) %>%
  separate(일시, into = c("날짜", "시각"), sep = " ", fill = "right", extra = "drop")

    
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
  
  # 전날의 날짜 중 실제 데이터에 존재하는 날짜를 필터링
  filtered_data <- data[data$날짜 %in% previous_days, ]
  
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

# 기존 데이터는 그대로 유지한 상태에서 2024년 데이터는 별도로 불러오기
data_2024 <- data.frame()

# 2024년 데이터 불러오기
file_name_2024 <- paste0("data/weather_", 2024, ".xlsx")
file_path_2024 <- file.path(getwd(), file_name_2024)

if (file.exists(file_path_2024)) {
  data_2024 <- read_excel(file_path_2024)
  
  # NA 값을 0으로 변환
  data_2024[is.na(data_2024)] <- 0

  # Datetime 전처리 - separate에서 결측값 처리
  data_2024 <- data_2024 %>%
    mutate(across(everything(), as.character)) %>%
    separate(일시, into = c("날짜", "시각"), sep = " ", fill = "right", extra = "drop")

  # 시각에서 "0:00"을 "00:00"으로 변경
  data_2024$시각 <- str_replace(data_2024$시각, "^0:00$", "00:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^1:00$", "01:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^2:00$", "02:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^3:00$", "03:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^4:00$", "04:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^5:00$", "05:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^6:00$", "06:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^7:00$", "07:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^8:00$", "08:00")
  data_2024$시각 <- str_replace(data_2024$시각, "^9:00$", "09:00")

  colnames(data_2024) <- c("지점", "지점명", "날짜", "시각", "기온", "강수량", "풍속", "습도", "현지기압")

}


unique_times <- unique(data_2024$시각)

# 2024년 데이터의 시각 변화량 계산 (기존 함수 재사용)
changes_summary_2024 <- calculate_hourly_changes(data_2024)

# 유사한 날씨 패턴 찾기 함수 정의 (기존 함수 재사용)
find_similar_weather <- function(data, target_wind, target_humidity, target_pressure, tolerance = 0) {
  print(target_wind)
  print(target_humidity)
  print(target_pressure)
  similar_dates <- data %>%
    filter(
      abs(풍속변화량평균 - target_wind) <= tolerance,
      abs(습도변화량평균 - target_humidity) <= tolerance,
      abs(기압변화량평균 - target_pressure) <= tolerance
    )
  print(similar_dates)
  return(similar_dates)
}

# 비 예측 평가 함수
evaluate_predictions <- function(similar_dates, original_data) {
  correct_predictions <- 0
  incorrect_predictions <- 0
  total_predictions <- 0
  
  # 유사한 날씨 패턴이 없을 경우 NA 반환
  if (nrow(similar_dates) == 0) {
    return(NA)
  }

  # 2024년에 비가 온 날짜를 저장할 벡터 초기화
  rainy_days_2024 <- unique(original_data$날짜[as.numeric(original_data$강수량) > 0])
  
  # 비가 오지 않는 날짜를 저장할 벡터 초기화
  non_rainy_days_2024 <- unique(original_data$날짜[as.numeric(original_data$강수량) == 0])

  # 2024년의 모든 날짜 가져오기
  all_dates_2024 <- unique(original_data$날짜)

  # 예측 평가
  for (date in unique(original_data$날짜)){
    total_predictions <- total_predictions + 1  # 예측 총 개수 증가
    next_day <- as.Date(date) + 1  # 다음 날
    # 다음날에 비가 올 경우
    if (next_day %in% rainy_days_2024) {
      # 비가 온다고 예측했을경우
      if (next_day %in% similar_dates$날짜) {
        correct_predictions <- correct_predictions + 1  # 예측이 맞음
      }
      # 비가 안온다고 예측했을경우
      else{
        incorrect_predictions <- incorrect_predictions + 1 # 예측이 틀림
      }
    }
    # 다음날에 비가 안 올 경우
    else {
      # 비가 온다고 예측했을경우
      if (next_day %in% similar_dates$날짜) {
        incorrect_predictions <- incorrect_predictions + 1  # 예측이 틀림
      }
      # 비가 안온다고 예측했을경우
      else{
        correct_predictions <- correct_predictions + 1 # 예측이 맞음
      }
    }
  }

  print(paste("총 일수", total_predictions))
  print(paste("실제로 비가 온 일수", length(rainy_days_2024)))
  print(paste("비가 온다고 예측한 일수", nrow(similar_dates)))
  print(paste("예측이 맞은 일수", correct_predictions))
  print(paste("예측이 틀린 일수", incorrect_predictions))

  accuracy <- ifelse(total_predictions > 0, (correct_predictions / total_predictions), NA)
  
  return(accuracy)
}



# 2024년 데이터에서 과거 데이터와 유사한 날씨 패턴 찾기
similar_weather_2024 <- find_similar_weather(changes_summary_2024, average_wind_change, average_humidity_change, average_pressure_change)

# 유사한 패턴을 가진 날씨의 다음날 비가 올 확률 평가
accuracy <- evaluate_predictions(similar_weather_2024, data_2024)

# 결과 출력
cat("총 분석일 수 (2014~2023) : ",nrow(combined_data), "일","\n")
cat("최종 강수 예측 정확도:", ifelse(is.na(accuracy), "데이터 부족", paste0(accuracy * 100, "%")), "\n")

#print(changes_summary_2024)