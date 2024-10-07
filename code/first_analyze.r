library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

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

plot(as.numeric(combined_data$습도), 
     as.numeric(combined_data$강수량), 
     main = "강수량과 습도의 관계", 
     xlab = "습도 (%)", 
     ylab = "강수량 (mm)", 
     col = "blue", 
     pch = 19,
     xlim = c(0, 100))

# 그래프에 회귀선 추가
abline(lm(as.numeric(combined_data$강수량) ~ as.numeric(combined_data$습도)), col = "red", lwd = 2)

#갯수 출력
#num_rows <- nrow(combined_data)
#print(tail(combined_data, n = 10))
#cat("Total Number of Weather(Recently 10 Years):",num_rows,"개","\n")
