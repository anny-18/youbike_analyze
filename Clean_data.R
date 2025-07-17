library(tidyverse)
library(stringdist)
library(jsonlite)

#讀入資料
all_data <- readRDS("all_data_conbined.rds")

#新增一欄位is_excluded,標記異常資料
all_data <- mutate(all_data, is_excluded = 0)

#檢視欄位之資料型態
glimpse(all_data)

#將duration由字串轉換為時間
all_data <- mutate(all_data, duration = period_to_seconds(hms(duration)))

#檢查是否有空字串或NULL
filter(all_data, is.na(start_time))
filter(all_data, is.na(end_time))
filter(all_data, is.na(start_station)|start_station=="") #7筆為NA(NULL)
#標記start_station為NULL的資料
all_data <- mutate(all_data, is_excluded=ifelse(start_station==""|is.na(start_station), 1, is_excluded))
filter(all_data, is.na(end_station)|end_station=="") #28筆為NA(NULL)
#標記end_station為NULL的資料
all_data <- mutate(all_data, is_excluded=ifelse(end_station==""|is.na(end_station), 1, is_excluded))
filter(all_data, is.na(duration)) 
filter(all_data, is.na(ride_date))

#檢查站點個數是否合理
length(unique(all_data$start_station))
length(unique(all_data$end_station))

#取得站點資料建立正確名稱清單
#台北市
station_data_taipei <- fromJSON("https://tcgbusfs.blob.core.windows.net/dotapp/youbike/v2/youbike_immediate.json")
station_data_taipei <- station_data_taipei %>%
  select(sno, sna, sarea, ar, latitude, longitude, Quantity) %>%
  rename(station_id=sno, station_name=sna, district=sarea, address=ar, capacity=Quantity) %>% 
  mutate(district=paste0("台北市",district))
#新北市
station_data_newtaipei <-{
  page <-0
  size <-100
  data <- list()
  repeat{
    url<-paste0("https://data.ntpc.gov.tw/api/datasets/010e5b15-3823-4b20-b401-b1cf000550c5/json?page=", page, "&size=", size)
    data_page <-fromJSON(url)
    if(length(data_page) == 0) break
    data[[page+1]] <- data_page
    page <- page+1
  }
  bind_rows(data)
  
}
station_data_newtaipei <- station_data_newtaipei %>%
  select(sno, sna, sarea, ar, lat, lng) %>%
  rename(station_id=sno, station_name=sna, district=sarea, address=ar, latitude=lat, longitude=lng) %>% 
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    district=paste0("新北市",district)
  )

station_data <- bind_rows(station_data_taipei, station_data_newtaipei)

#去除station_name前的多餘字串
station_data <- station_data %>%
  mutate(station_name = str_remove(station_name, "^YouBike2.0_"))

#交叉比對, 把在all_data裡卻不在station_data裡的資料拉出來修正(start_station)
not_in_station_data <- anti_join(all_data, station_data, by = c("start_station" = "station_name"))
not_in_station_data_distinct <- not_in_station_data %>%
  filter(is_excluded==0) %>% 
  distinct(start_station) %>% pull(start_station)

#建立correct_names清單
correct_names <- station_data %>%
  distinct(station_name) %>%
  pull(station_name)

#比對出類似名稱找出最接近站名，並修正
#修正not_in_station_data_distinct[1]
string_diffs <- stringdist(not_in_station_data_distinct[1], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diff=string_diffs) %>% 
  filter(string_diff <=3) %>% arrange(string_diff)
#需修改站名
not_in_station_data_distinct[1]
#修改後站名(人工確認similar_names，選擇最接近的站名)
correct_name <- "瑠公公園"
#修正站名
wrong_name <- not_in_station_data_distinct[1]
#修正前
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)


#修正not_in_station_data_distinct[2]
string_diffs <- stringdist(not_in_station_data_distinct[2], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[2]
#修改後站名
correct_name <- "永安藝文館_表演36房"
#修正站名
wrong_name <- not_in_station_data_distinct[2]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)



#修正not_in_station_data_distinct[3]
string_diffs <- stringdist("僑安地下停車場(2號出口)東南", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[3]
#修改後站名
correct_name <- "僑安地下停車場(2號出口)東南側"
#修正站名
wrong_name <- not_in_station_data_distinct[3]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[4]
string_diffs <- stringdist(not_in_station_data_distinct[4], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[4]
#修改後站名
correct_name <- "捷運科技大樓站(台北教育大學)"
#修正站名
wrong_name <- not_in_station_data_distinct[4]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[5]
string_diffs <- stringdist(not_in_station_data_distinct[5], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[5]
#修改後站名
correct_name <- "捷運北投站(1號出口)(北投路側)"
#修正站名
wrong_name <- not_in_station_data_distinct[5]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[6]
string_diffs <- stringdist(not_in_station_data_distinct[6], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[6]
#修改後站名
correct_name <- "崇仰公園(公舘路255巷)"
#修正站名
wrong_name <- not_in_station_data_distinct[6]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[7]
string_diffs <- stringdist(not_in_station_data_distinct[7], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[7]
#修改後站名
correct_name <- "公舘承德路口"
#修正站名
wrong_name <- not_in_station_data_distinct[7]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[8]
string_diffs <- stringdist(not_in_station_data_distinct[8], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[8]
#修改後站名
correct_name <- "捷運石牌站(2號出口)(西安街一段)"
#修正站名
wrong_name <- not_in_station_data_distinct[8]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[9]
string_diffs <- stringdist(not_in_station_data_distinct[9], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[9]
#修改後站名
correct_name <- "水源路11_1號旁"
#修正站名
wrong_name <- not_in_station_data_distinct[9]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[10]
string_diffs <- stringdist(not_in_station_data_distinct[10], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[10]
#修改後站名
correct_name <- "中央北路四段540巷口"
#修正站名
wrong_name <- not_in_station_data_distinct[10]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[11]
string_diffs <- stringdist(not_in_station_data_distinct[11], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[11]
#修改後站名
correct_name <- "銘傳大學(中山北路五段280巷口)"
#修正站名
wrong_name <- not_in_station_data_distinct[11]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[12]
string_diffs <- stringdist(not_in_station_data_distinct[12], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
not_in_station_data_distinct[12]
#修改後站名
correct_name <- "臺北市立大學(忠誠路二段207巷)"
#修正站名
wrong_name <- not_in_station_data_distinct[12]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[13]
not_in_station_data_distinct[13]
string_diffs <- stringdist("天母東路8巷/忠誠路二段154巷", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "天母東路8巷/忠誠路二段154巷口"
#修正站名
wrong_name <- not_in_station_data_distinct[13]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[14]
not_in_station_data_distinct[14]
string_diffs <- stringdist(not_in_station_data_distinct[14], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "劍潭海外青年活動中心(志清大樓)"
#修正站名
wrong_name <- not_in_station_data_distinct[14]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[15]
not_in_station_data_distinct[15]
string_diffs <- stringdist(not_in_station_data_distinct[15], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "捷運萬芳醫院站(興隆路三段115巷)"
#修正站名
wrong_name <- not_in_station_data_distinct[15]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[16]
not_in_station_data_distinct[16]
string_diffs <- stringdist(not_in_station_data_distinct[16], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "重慶北路四段190巷口(通河西街側)"
#修正站名
wrong_name <- not_in_station_data_distinct[16]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[17]
not_in_station_data_distinct[17]
string_diffs <- stringdist(not_in_station_data_distinct[17], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "捷運善導寺站(3號出口)(忠孝東路側)"
#修正站名
wrong_name <- not_in_station_data_distinct[17]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[18]
not_in_station_data_distinct[18]
string_diffs <- stringdist("中國醫藥大學附設醫院臺北", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "中國醫藥大學附設醫院臺北分院"
#修正站名
wrong_name <- not_in_station_data_distinct[18]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[19]
not_in_station_data_distinct[19]
string_diffs <- stringdist(not_in_station_data_distinct[19], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "糖廍文化園區"
#修正站名
wrong_name <- not_in_station_data_distinct[19]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[20]
not_in_station_data_distinct[20]
string_diffs <- stringdist(not_in_station_data_distinct[20], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "新生高架停車場(林森北路107巷口)"
#修正站名
wrong_name <- not_in_station_data_distinct[20]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[21]
#2025年5月『內政部營建署』場站更名為『國土管理署』
not_in_station_data_distinct[21]
string_diffs <- stringdist("國土管理署", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "國土管理署"
#修正站名
wrong_name <- not_in_station_data_distinct[21]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[22]
#2025年5月『中山公民會館』場站更名為『中山北路二段96巷口』
not_in_station_data_distinct[22]
string_diffs <- stringdist("中山北路二段96巷口", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "中山北路二段96巷口"
#修正站名
wrong_name <- not_in_station_data_distinct[22]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)


#修正not_in_station_data_distinct[23]
#2025年『中原民生路口』場站更名為『吉林路236巷口』
not_in_station_data_distinct[23]
string_diffs <- stringdist("吉林路236巷口", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "吉林路236巷口"
#修正站名
wrong_name <- not_in_station_data_distinct[23]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[24]
#修正為成美長壽橋(潭美街)
not_in_station_data_distinct[24]
string_diffs <- stringdist(not_in_station_data_distinct[24], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "成美長壽橋(潭美街)"
#修正站名
wrong_name <- not_in_station_data_distinct[24]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[25]
not_in_station_data_distinct[25]
string_diffs <- stringdist(not_in_station_data_distinct[25], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "民善新湖二路口(家樂福內湖店)"
#修正站名
wrong_name <- not_in_station_data_distinct[25]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[26]
not_in_station_data_distinct[26]
string_diffs <- stringdist(not_in_station_data_distinct[26], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "忠孝東路四段223巷口(市民大道側)"
#修正站名
wrong_name <- not_in_station_data_distinct[26]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[27]
#3樓客服中心應非可租借站點,因此排除
not_in_station_data_distinct[27]
all_data <- mutate(all_data, is_excluded=if_else(!is.na(start_station) & start_station=="3樓客服中心", 1 ,is_excluded))
all_data <- mutate(all_data, is_excluded=if_else(!is.na(end_station) & end_station=="3樓客服中心", 1 ,is_excluded))
result<- filter(all_data, start_station==not_in_station_data_distinct[27])

#修正not_in_station_data_distinct[28]
#蘆洲維修中心不算站點
not_in_station_data_distinct[28]
all_data <- mutate(all_data, is_excluded=if_else(!is.na(start_station) & start_station=="蘆洲維修中心", 1 ,is_excluded))
all_data <- mutate(all_data, is_excluded=if_else(!is.na(end_station) & end_station=="蘆洲維修中心", 1 ,is_excluded))
result<- filter(all_data, start_station==not_in_station_data_distinct[28])

#修正not_in_station_data_distinct[29]
#濱江第二放置場不算站點
not_in_station_data_distinct[29]
all_data <- mutate(all_data, is_excluded=if_else(!is.na(start_station) & start_station==not_in_station_data_distinct[29], 1, is_excluded))
all_data <- mutate(all_data, is_excluded=if_else(!is.na(end_station) & end_station==not_in_station_data_distinct[29], 1, is_excluded))
result <- filter(all_data, start_station==not_in_station_data_distinct[29])

#修正not_in_station_data_distinct[30]
#信義服務中心, 非站點, 排除
not_in_station_data_distinct[30]
all_data <- mutate(all_data, is_excluded=if_else(!is.na(start_station) & start_station=="信義服務中心", 1 ,is_excluded))
all_data <- mutate(all_data, is_excluded=if_else(!is.na(end_station) & end_station=="信義服務中心", 1 ,is_excluded))
result <- filter(all_data, start_station==not_in_station_data_distinct[30])

#修正not_in_station_data_distinct[31]
not_in_station_data_distinct[31]
string_diffs <- stringdist(not_in_station_data_distinct[31], correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "明美公園(南京東路六段451巷口)"
#修正站名
wrong_name <- not_in_station_data_distinct[31]
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)
target_rows_start <- which(all_data$start_station == wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$start_station[target_rows_start] <- correct_name
all_data$end_station[target_rows_end] <- correct_name
#確認修正成功
filter(all_data, start_station==wrong_name)
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[32]
#內湖服務中心非站點, 排除
not_in_station_data_distinct[32]
all_data <- mutate(all_data, is_excluded=if_else(!is.na(start_station) & start_station=="內湖服務中心", 1 ,is_excluded))
all_data <- mutate(all_data, is_excluded=if_else(!is.na(end_station) & end_station=="內湖服務中心", 1 ,is_excluded))
result <- filter(all_data, start_station==not_in_station_data_distinct[32])

#交叉比對, 確認start_station裡資料皆修正成功
not_in_station_data <- anti_join(all_data, station_data, by = c("start_station" = "station_name"))
not_in_station_data_distinct <- not_in_station_data %>%
  filter(is_excluded==0) %>% 
  distinct(start_station) %>% pull(start_station)

#獲取桃園站點的資料
taoyuan_station_data <- fromJSON("taoyuan_station_json.json")
taoyuan_station_data <- taoyuan_station_data %>% select(sna) %>% pull(sna)

#將含有桃園站點資料排除
all_data <- all_data %>% 
  mutate(is_excluded=if_else(end_station %in% taoyuan_station_data, 1, is_excluded)) 

##交叉比對, 把沒有在station_data裡的資料拉出來修正(end_station)
not_in_station_data <- anti_join(all_data, station_data, by = c("end_station" = "station_name"))
not_in_station_data_distinct <- not_in_station_data %>%
  filter(is_excluded==0) %>% 
  distinct(end_station) %>% pull(end_station)

#先排除有亂碼的站點(會導致程式無法)
not_in_station_data_distinct <- iconv(not_in_station_data_distinct, from = "", to = "UTF-8", sub = NA)
not_in_station_data_distinct <- not_in_station_data_distinct[!is.na(not_in_station_data_distinct)]
not_in_station_data_distinct <- sub("\\\\.*", "", not_in_station_data_distinct)

#修正錯誤
corrections <- data.frame(
  # before:錯誤名稱, after:正確的名稱，或 1 表示排除，空值表示跳過
  before = not_in_station_data_distinct,
  after = c("華江一華江五路口(雙江翠社區)", "啟文央北二路口(中央公園社區)", "環河西新月一街口(新月天地社區)",
            "環河西華江六路口(翠亨村社區)", "1", "1", "中正路145巷口", "渡船頭平面停車場", "瓦磘溝(福真里)",
            "1", "瓦磘截流站", "十四張央北二路口(波爾多社區)", "華江二華江一路口(帝國花園廣場社區)",
            "新北市藝文中心(文化路二段124巷)", "1", "金城峯廷街口", "十四張啓文路口", "1",
            "浮洲合宜住宅(合安一合宜路口)", "華江五華江一路口(帝國花園廣場社區)", "汐止國民運動中心游泳池(忠孝東路)",
            "1", "1", "1", "永翠藝文街口(柏克萊公園社區)", "中央慶利街口(風華綠中央社區)", "1", "1", "1",
            "1", "1", "新春街125巷(樂活绽社區)", "輕軌濱海義山站(西南側)", "1", "1", "1", "1", "1", "1", "1",
            "1", "1", "三鶯分區區務辦公室", "1", "新市二路三段/濱海路二段202巷口", "1", "1", "1", "1", "1", "1",
            "1", "1", "1", "臺北大學公共事務大樓(法商大道)", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1")    
)

for (i in seq_len(nrow(corrections))) {
  wrong <- corrections$before[i]
  fix <- corrections$after[i]
  
  target_rows <- which(all_data$end_station == wrong)
  
  if (length(target_rows) > 0) {
    if (fix == "1") {  # 要排除的情況
      all_data$is_excluded[target_rows] <- 1
      cat("✅ 已排除", length(target_rows), "筆資料：", wrong, "\n")
    } else if (nchar(fix) > 0) {  # 要修正名稱
      all_data$end_station[target_rows] <- fix
      cat("🔧 已修正", length(target_rows), "筆資料：", wrong, "→", fix, "\n")
    } else {  # fix 為空的情況
      cat("⏩ 跳過：", wrong, "\n")
    }
  } else {
    cat("❌ 找不到資料：", wrong, "\n")
  }
}

#將含有亂數的站點修正
not_in_station_data <- anti_join(all_data, station_data, by = c("end_station" = "station_name"))
not_in_station_data_distinct <- not_in_station_data %>%
  filter(is_excluded==0) %>% 
  distinct(end_station) %>% pull(end_station)

#修正not_in_station_data_distinct[1]
not_in_station_data_distinct[1]
string_diffs <- stringdist("河西新月一街口(新月天地社區)", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "華江一華江二路口(江匯Life社區)"
#修正站名
wrong_name <- not_in_station_data_distinct[1]
filter(all_data, end_station==wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$end_station[target_rows_end] <- correct_name
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[2]
not_in_station_data_distinct[2]
string_diffs <- stringdist("新北市政府板橋分局沙崙派出", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "新北市政府板橋分局沙崙派出所"
#修正站名
wrong_name <- not_in_station_data_distinct[2]
filter(all_data, end_station==wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$end_station[target_rows_end] <- correct_name
filter(all_data, end_station==wrong_name)

#修正not_in_station_data_distinct[3]
not_in_station_data_distinct[3]
string_diffs <- stringdist("捷運海山站(3號出口)轉乘停車", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "捷運海山站(3號出口)轉乘停車場"
#修正站名
wrong_name <- not_in_station_data_distinct[3]
filter(all_data, end_station==wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$end_station[target_rows_end] <- correct_name
filter(all_data, end_station==wrong_name)


#修正not_in_station_data_distinct[4]
not_in_station_data_distinct[4]
string_diffs <- stringdist("後埔國小(實踐路/重慶路155巷", correct_names, method="lv")
similar_names <- data.frame(name=correct_names, string_diffs=string_diffs) %>% 
  filter(string_diffs <=3) %>% arrange(string_diffs)
#修改後站名
correct_name <- "後埔國小(實踐路/重慶路155巷口)"
#修正站名
wrong_name <- not_in_station_data_distinct[4]
filter(all_data, end_station==wrong_name)
target_rows_end <- which(all_data$end_station == wrong_name)
all_data$end_station[target_rows_end] <- correct_name
filter(all_data, end_station==wrong_name)

result <- filter(all_data, end_station=="View(station_data_newtaipei)")

#檢查站點個數是否合理
length(unique(all_data$start_station))
length(unique(all_data$end_station))

#是否有非7月到12月的資料
result <- all_data %>% 
  filter(!month(ride_date) %in% 7:12)

#將騎乘時間大於6小時之資料排除於分析
all_data <- all_data %>% mutate(is_excluded = if_else(duration>21600|duration<60, 1, is_excluded))

#計算is_excluded為0極為1的筆數
all_data %>%
  count(is_excluded)

#部分站點（如福壽公園、後港公園等）在臺北與新北均有同名站點，且原始資料無法進一步辨識其所屬縣市，為避免地區分析誤判，故排除此類站點之資料（共 75,229 筆，占總資料約 0.79%）。
excluded_station <- station_data %>% count(station_name) %>% filter(n > 1) %>% pull(station_name)
all_data <- all_data %>% 
  mutate(is_excluded = if_else(start_station %in% excluded_station | end_station %in% excluded_station, 1, is_excluded))

saveRDS(all_data, file = "all_data_cleaned.rds")
saveRDS(station_data, file = "station_data.rds")