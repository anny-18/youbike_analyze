library(tidyverse)

#垂直合併資料
files <- list.files(path = "dataset", pattern = "\\.csv$", full.names = TRUE)
#部分資料集未包含車型,因此將分開處理
col_names_7 <- c("start_time", "start_station", "end_time", "end_station", "duration", "bike_type", "ride_date")
col_names_6 <- c("start_time", "start_station", "end_time", "end_station", "duration", "ride_date")

#建立空tibble以儲存資料
all_data <- tibble()

#將資料存入all_data
for(file in files[1:6]){
  
  #取得欄位數量,判斷此資料集是否包含車型資料
  first_line <- readLines(file, n=1)
  column_count <- str_count(first_line, ",")+1
  
  if(column_count==6){  #若無車型資料(column_count=6),則直接存入data
    data <- read_csv(file, col_names = col_names_6, col_type = cols(duration=col_character())) #除了duration以字串方式匯入,其他自行判斷資料型態
  }
  else if(column_count==7){ #若有車型資料(column_count=7),則刪除車型資料後, 再存入data
    data <- read_csv(file, col_names = col_names_7, col_type = cols(duration=col_character())) #除了duration以字串方式匯入,其他自行判斷資料型態
    data <- select(data, -bike_type)
  }
  else{
    cat("error:", file, "\n")
  }
  #將data寫入all_data
  all_data <-bind_rows(all_data,data)
  
}
#存成rds檔
saveRDS(all_data, "all_data_conbined.rds")


