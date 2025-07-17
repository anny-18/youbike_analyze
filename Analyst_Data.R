library(tidyverse)
library(lubridate)
library(geosphere) 
library(ggrepel)
library(scales)
library(ggnewscale)
library(readr)


all_data <- readRDS("all_data_cleaned.rds")
station_data <- readRDS("station_data.rds")

#計算0點~6點之騎乘資料佔比(約0.03因此併入晚間時段, 不另外分析)
night_time_number <- all_data %>% 
  filter(is_excluded==0)  %>% 
  filter(hour(start_time) < 6) %>% 
  count()
all_number <- all_data %>% filter(is_excluded==0)  %>% count()
night_time_number/all_number

#新增平假日及各時段欄位
#新增平假日
all_data <- all_data %>% 
  mutate(
    weekday_type = case_when(
      is.na(start_time) ~ NA,
      wday(start_time, week_start =1) %in% 1:5 ~ "Weekday",
      wday(start_time, week_start =1) %in% 6:7 ~ "Weekend",
      TRUE ~ "error")
    )
#新增時段
all_data <- all_data %>% 
  mutate(
    time_period = case_when(
      is.na(start_time) ~ NA, 
      weekday_type=="Weekday" ~ case_when(
        hour(start_time) %in% 6:8 ~ "CommuteAM",
        hour(start_time) %in% 9:15 ~ "DayTime", 
        hour(start_time) %in% 16:18 ~ "CommutePM",
        (hour(start_time) %in% 19:23)|(hour(start_time) %in% 0:5) ~ "NightTime",
        TRUE ~ "weekday_error"),
      weekday_type=="Weekend" ~
        case_when(hour(start_time) %in% 6:12 ~ "Morning",
                  hour(start_time) %in% 13:18 ~ "Afternoon",
                  (hour(start_time) %in% 19:23)|(hour(start_time) %in% 0:5) ~ "Night",
                  TRUE ~ "weekend_error"),
      TRUE ~ "error")
  )

#確認是否有error之資料
result <- all_data %>% 
  filter(
    weekday_type=="error" | 
      time_period %in% c("weekday_error", "weekend_error", "error")
    )

#建立借出及歸還資料表(borrow_df / return_df) & 排除非台北市資料
# 先合併，把站名對應的區域資料加進all_data
all_data_with_str_area <- all_data %>% 
  filter(is_excluded==0) %>% 
  left_join(station_data %>% select(station_name, district, capacity), 
            by = c("start_station" = "station_name"))

borrow_df  <- all_data_with_str_area %>% 
  filter(str_detect(district, "台北市"))  %>%
  group_by(start_station, weekday_type, time_period) %>%
  summarise(borrow_count = n(), .groups = "drop") %>%
  rename(station = start_station)


all_data_with_end_area<- all_data %>% 
  filter(is_excluded==0) %>% 
  left_join(station_data %>% select(station_name, district, capacity), 
            by = c("end_station" = "station_name"))

return_df <- all_data_with_end_area %>% 
  filter(str_detect(district, "台北市"))  %>%
  group_by(end_station, weekday_type, time_period) %>% 
  summarise(return_count=n(), .groups = "drop") %>% 
  rename(station = end_station)

#合併borrow_df和return_df & 加入站點區域及經緯度資料
demand_gap <- full_join(borrow_df, return_df, by=c("station", "weekday_type", "time_period")) %>% 
  replace_na(list(borrow_count = 0, return_count = 0)) %>% 
  left_join(station_data %>% select(station_name, district, latitude, longitude, capacity), 
            by=c("station" = "station_name"))
demand_gap <- demand_gap %>%
  mutate(district = str_remove(district, "^台北市"))

#新增平均每日借出及歸還欄位
n_weekdays <- all_data %>%
  filter(is_excluded == 0 & weekday_type == "Weekday") %>%
  summarise(n_weekdays = n_distinct(ride_date)) %>%
  pull(n_weekdays)
n_weekends <- all_data %>%
  filter(is_excluded == 0 & weekday_type == "Weekend") %>%
  summarise(n_weekends = n_distinct(ride_date)) %>%
  pull(n_weekends)

demand_gap <- demand_gap %>% 
  mutate(
    borrow_per_day = case_when(
      weekday_type=="Weekday" ~ round(borrow_count/n_weekdays, 2),
      weekday_type=="Weekend" ~ round(borrow_count/n_weekends, 2)
    ),
    return_per_day = case_when(
      weekday_type=="Weekday" ~ round(return_count/n_weekdays, 2),
      weekday_type=="Weekend" ~ round(return_count/n_weekends, 2)
    )
  )

#新增差值及佔比欄位
demand_gap <- demand_gap %>% 
  mutate(
    avg_diff_per_day = round(borrow_per_day-return_per_day, 2), 
    avg_imbalance_pct = round(avg_diff_per_day/(borrow_per_day+return_per_day), 2)
  )

# demand_gap <- demand_gap %>% 
#   mutate(
#     #正數: 需補充, 負數: 需還車
#     imbalance_number = 
#       case_when(
#         avg_diff_per_day>0 ~ avg_diff_per_day, #還需補車數量
#         avg_diff_per_day<0 & avg_diff_per_day+(capacity-3)>=0 ~ 0, #確保有3台空位以上, 無須調整車數
#         avg_diff_per_day<0 & avg_diff_per_day+(capacity-3)<0 ~ avg_diff_per_day+capacity, #多出avg_diff_per_day+capacity台, 需取車
#         avg_diff_per_day==0 ~ 0
#       )
#   )

#重新排列順序
demand_gap <- demand_gap %>%
  select(station, weekday_type, time_period, avg_diff_per_day, capacity, borrow_count, return_count, borrow_per_day, return_per_day, avg_imbalance_pct, district, latitude, longitude)

#找出每日平均差值超出容量的前100個站點分析
top_100_station_name <- demand_gap %>% 
  group_by(station) %>% 
  summarise(total_diff_per_day = sum(abs(avg_diff_per_day), na.rm=TRUE)) %>% 
  arrange(desc(abs(total_diff_per_day))) %>% 
  slice_head(n=100) %>% 
  pull(station)

top_100_station_data <- demand_gap %>% 
  filter(station %in% top_100_station_name)

top_100_station_data <- top_100_station_data %>%
  mutate(time_period = factor(time_period, levels = c(
    "CommuteAM", "DayTime", "CommutePM", "NightTime",  # 平日
    "Morning", "Afternoon", "Night"                    # 假日
  )))

#氣泡+時間分段
ggplot(top_100_station_data, aes(x = longitude, y = latitude)) +
  geom_point(aes(
    size = abs(avg_diff_per_day),        # 氣泡大小：差值絕對值
    color = district,                    # 氣泡顏色：行政區
    shape = avg_diff_per_day > 0         # 形狀：TRUE(正差) / FALSE(負差)
  ), alpha = 0.7) +
  facet_wrap(~ time_period, nrow = 2) +
  scale_size_continuous(range = c(1, 8)) +
  scale_color_viridis_d(option = "turbo", name = "行政區")+
  scale_shape_manual(
    values = c(`TRUE` = 1, `FALSE` = 16),    # 16實心圓, 1空心圓，你也可以換其他形狀
    breaks = c(FALSE, TRUE),
    labels = c(`TRUE` = "需補充", `FALSE` = "需運走"),
    name = "差值方向"
  ) +
  labs(
    title = "各時段YouBike站點供需不平衡地圖",
    size = "日均差值（絕對值）",
    x = "經度", y = "緯度"
  ) +
  theme_minimal()
ggsave("氣泡圖.png", width = 14, height = 8, dpi = 300, bg="white")

#將站點分為短時間不平衡及整體不平衡
total_diff_data <- demand_gap %>% 
  filter(station %in% top_100_station_name, ) %>% 
  group_by(station) %>% 
  summarise(total_diff_per_day = sum(avg_diff_per_day, na.rm=TRUE)) 
#短時間不平衡名單
short_term_imbalanced_stations <- total_diff_data %>% 
  filter(abs(total_diff_per_day)<10) %>% 
  pull(station)
#整體不平衡名單
daily_imbalanced_stations <- total_diff_data %>% 
  filter(!station %in% short_term_imbalanced_stations) %>% 
  pull(station)

#短時間不平衡：平日_早上通勤時間後調度名單, 正數:保留1-3台/負數:維持1-3個空位
availability_watchlist_weekday_morning <- demand_gap %>% 
  filter(station %in% short_term_imbalanced_stations, time_period %in% c("NightTime", "CommuteAM")) %>%
  group_by(station, district) %>% 
  summarise(total_diff=sum(avg_diff_per_day, na.rm=TRUE),
            .groups="drop") %>% 
  filter(abs(total_diff)>10) %>% 
  mutate(recommend_action = 
           case_when(
             total_diff>0 ~ "補充",
             total_diff<0 ~ "取車"
           )) %>% 
  select(station, district, recommend_action) %>% 
  arrange(district, recommend_action, station)


#短時間不平衡：平日_晚間通勤時間後調度名單, 正數:保留1-3台/負數:維持1-3個空位
availability_watchlist_weekday_evening <- demand_gap %>% 
  filter(station %in% short_term_imbalanced_stations, time_period %in% c("Daytime", "CommutePM")) %>%
  group_by(station, district) %>% 
  summarise(total_diff=sum(avg_diff_per_day, na.rm=TRUE),
            .groups="drop") %>% 
  filter(abs(total_diff)>10) %>% 
  mutate(recommend_action = 
           case_when(
             total_diff>0 ~ "補充",
             total_diff<0 ~ "取車"
           )) %>% 
  select(station, district, recommend_action) %>% 
  arrange(district, recommend_action, station)

#短時間不平衡：假日_中午調度名單
availability_watchlist_weekend_noon <- demand_gap %>% 
  filter(station %in% short_term_imbalanced_stations, time_period %in% c("Night", "Morning")) %>%
  group_by(station, district) %>% 
  summarise(tottal_diff=sum(avg_diff_per_day, na.rm=TRUE),
            .groups="drop") %>% 
  filter(abs(tottal_diff)>10) %>% 
  mutate(recommend_action = 
           case_when(
             tottal_diff>0 ~ "補充",
             tottal_diff<0 ~ "取車"
           )) %>% 
  select(station, district, recommend_action) %>% 
  arrange(district, recommend_action, station)

#短時間不平衡：假日_夜間調度名單
availability_watchlist_weekend_night <- demand_gap %>% 
  filter(station %in% short_term_imbalanced_stations, time_period=="Afternoon", abs(avg_diff_per_day)>10) %>% 
  mutate(recommend_action = 
           case_when(
             avg_diff_per_day>0 ~ "補充",
             avg_diff_per_day<0 ~ "取車"
           )) %>% 
  select(station, district, recommend_action) %>% 
  arrange(district, recommend_action, station)

#整日不平衡：平日_早上通勤時段後調度名單
dispatch_plan_weekday_morning <- demand_gap %>% 
  filter(station %in% daily_imbalanced_stations, time_period %in% c("NightTime", "CommuteAM")) %>%
  group_by(station, district) %>% 
  summarise(total_diff = sum(avg_diff_per_day, na.rm=TRUE), capacity=min(capacity), latitude=min(latitude), longitude=min(longitude), .groups="drop") %>% 
  mutate(
    dispatch_number=ceiling(total_diff),
    recommend_action=(
      case_when(
        total_diff >0 ~ "補充",
        total_diff <0 ~ "取車"
        )
    ),
    min_bike_required = ceiling(capacity*1/3)
  ) %>% 
  select(station, district, recommend_action, dispatch_number, min_bike_required, latitude, longitude) %>% 
  arrange(district, recommend_action, station)
  
#整日不平衡：平日_晚上通勤時段後調度名單
dispatch_plan_weekday_evening <- demand_gap %>% 
  filter(station %in% daily_imbalanced_stations, time_period %in% c("Daytime", "CommutePM")) %>%
  group_by(station, district) %>% 
  summarise(total_diff = sum(avg_diff_per_day, na.rm=TRUE), capacity=min(capacity), latitude=min(latitude), longitude=min(longitude), .groups="drop") %>% 
  mutate(
    dispatch_number=ceiling(total_diff),
    recommend_action=(
      case_when(
        total_diff >0 ~ "補充",
        total_diff <0 ~ "取車"
      )
    ),
    min_bike_required = ceiling(capacity*1/3)
  ) %>% 
  select(station, district, recommend_action, dispatch_number, min_bike_required, latitude, longitude) %>% 
  arrange(district, recommend_action, station)

#整日不平衡：假日_中午通勤時段後調度名單
dispatch_plan_weekend_noon <- demand_gap %>% 
  filter(station %in% daily_imbalanced_stations, time_period %in% c("Night", "Morning")) %>%
  group_by(station, district) %>% 
  summarise(total_diff = sum(avg_diff_per_day, na.rm=TRUE), capacity=min(capacity), latitude=min(latitude), longitude=min(longitude), .groups="drop") %>% 
  mutate(
    dispatch_number=ceiling(total_diff),
    recommend_action=(
      case_when(
        total_diff >0 ~ "補充",
        total_diff <0 ~ "取車"
      )
    ),
    min_bike_required = ceiling(capacity*1/3)
  ) %>% 
  select(station, district, recommend_action, dispatch_number, min_bike_required, latitude, longitude) %>% 
  arrange(district, recommend_action, station)

#整日不平衡：假日_晚間通勤時段後調度名單
dispatch_plan_weekend_night <- demand_gap %>% 
  filter(station %in% daily_imbalanced_stations, time_period =="Afternoon") %>%
  mutate(
    dispatch_number=ceiling(avg_diff_per_day),
    recommend_action=(
      case_when(
        avg_imbalance_pct >0 ~ "補充",
        avg_imbalance_pct <0 ~ "取車"
      )
    ),
    min_bike_required = ceiling(capacity*1/3)
  ) %>% 
  select(station, district, recommend_action, dispatch_number, min_bike_required, latitude, longitude) %>% 
  arrange(district, recommend_action, station)

#針對整天不平衡
#整天不平衡:平日_早上調度配對
#需補車站點
stations_need_bike <- dispatch_plan_weekday_morning %>% 
  filter(dispatch_number > 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(need=dispatch_number)
#需取車站點
stations_with_extra <- dispatch_plan_weekday_morning %>% 
  filter(dispatch_number < 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(extra=dispatch_number)


# 設定最大配對距離（單位：公尺）
max_distance <- 2000

# 初始化
routes_one_to_one <- data.frame()
used_extras <- c()

# 依序處理每個補車站
for (i in 1:nrow(stations_need_bike)) {
  need_row <- stations_need_bike[i, ]
  
  # 找出尚未被配對的取車站
  available_extras <- stations_with_extra %>% 
    filter(!station %in% used_extras)
  
  if (nrow(available_extras) == 0) break  # 如果沒有取車站可配對，結束
  
  # 計算該補車站與所有尚未配對取車站的距離
  distances <- distHaversine(
    matrix(c(need_row$longitude, need_row$latitude), ncol = 2),
    matrix(c(available_extras$longitude, available_extras$latitude), ncol = 2)
  )
  # 加入距離欄位，並篩選距離小於指定值的站點
  available_extras <- available_extras %>%
    mutate(distance = distances) %>%
    filter(distance <= max_distance)
  
  if (nrow(available_extras) == 0) next  # 若都太遠則跳過
  
  # 選擇距離最近的一個取車站
  closest_extra <- available_extras %>% slice_min(order_by = distance, n = 1)
  used_extras <- c(used_extras, closest_extra$station)  # 記錄已被配對的取車站
  
  # 建立配對紀錄
  routes_one_to_one <- bind_rows(
    routes_one_to_one,
    data.frame(
      station_need = need_row$station,
      longitude_need = need_row$longitude,
      latitude_need = need_row$latitude,
      need = need_row$need,
      station_extra = closest_extra$station,
      longitude_extra = closest_extra$longitude,
      latitude_extra = closest_extra$latitude,
      extra = closest_extra$extra,
      district_extra = closest_extra$district,
      distance = closest_extra$distance
    )
  )
}

# 整理結果表
dispatch_plan_weekday_morning_match_list <- routes_one_to_one %>% 
  select(station_need, need, longitude_need, latitude_need, station_extra, extra, longitude_extra, latitude_extra, district_extra, distance) %>% 
  arrange(district_extra, extra)

#整天不平衡:平日_晚間調度配對
#需補車站點
stations_need_bike <- dispatch_plan_weekday_evening %>% 
  filter(dispatch_number > 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(need=dispatch_number)
#需取車站點
stations_with_extra <- dispatch_plan_weekday_evening %>% 
  filter(dispatch_number < 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(extra=dispatch_number)

# 初始化
routes_one_to_one <- data.frame()
used_extras <- c()

# 依序處理每個補車站
for (i in 1:nrow(stations_need_bike)) {
  need_row <- stations_need_bike[i, ]
  
  # 找出尚未被配對的取車站
  available_extras <- stations_with_extra %>% 
    filter(!station %in% used_extras)
  
  if (nrow(available_extras) == 0) break  # 如果沒有取車站可配對，結束
  
  # 計算該補車站與所有尚未配對取車站的距離
  distances <- distHaversine(
    matrix(c(need_row$longitude, need_row$latitude), ncol = 2),
    matrix(c(available_extras$longitude, available_extras$latitude), ncol = 2)
  )
  # 加入距離欄位，並篩選距離小於指定值的站點
  available_extras <- available_extras %>%
    mutate(distance = distances) %>%
    filter(distance <= max_distance)
  
  if (nrow(available_extras) == 0) next  # 若都太遠則跳過
  
  # 選擇距離最近的一個取車站
  closest_extra <- available_extras %>% slice_min(order_by = distance, n = 1)
  used_extras <- c(used_extras, closest_extra$station)  # 記錄已被配對的取車站
  
  # 建立配對紀錄
  routes_one_to_one <- bind_rows(
    routes_one_to_one,
    data.frame(
      station_need = need_row$station,
      longitude_need = need_row$longitude,
      latitude_need = need_row$latitude,
      need = need_row$need,
      station_extra = closest_extra$station,
      longitude_extra = closest_extra$longitude,
      latitude_extra = closest_extra$latitude,
      extra = closest_extra$extra,
      district_extra = closest_extra$district,
      distance = closest_extra$distance
    )
  )
}

# 整理結果表
dispatch_plan_weekday_evening_match_list <- routes_one_to_one %>% 
  select(station_need, need, longitude_need, latitude_need, station_extra, extra, longitude_extra, latitude_extra, district_extra, distance) %>% 
  arrange(district_extra, extra)

#整天不平衡:假日_中午調度配對
#需補車站點
stations_need_bike <- dispatch_plan_weekend_noon %>% 
  filter(dispatch_number > 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(need=dispatch_number)
#需取車站點
stations_with_extra <- dispatch_plan_weekend_noon %>% 
  filter(dispatch_number < 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(extra=dispatch_number)

# 初始化
routes_one_to_one <- data.frame()
used_extras <- c()

# 依序處理每個補車站
for (i in 1:nrow(stations_need_bike)) {
  need_row <- stations_need_bike[i, ]
  
  # 找出尚未被配對的取車站
  available_extras <- stations_with_extra %>% 
    filter(!station %in% used_extras)
  
  if (nrow(available_extras) == 0) break  # 如果沒有取車站可配對，結束
  
  # 計算該補車站與所有尚未配對取車站的距離
  distances <- distHaversine(
    matrix(c(need_row$longitude, need_row$latitude), ncol = 2),
    matrix(c(available_extras$longitude, available_extras$latitude), ncol = 2)
  )
  # 加入距離欄位，並篩選距離小於指定值的站點
  available_extras <- available_extras %>%
    mutate(distance = distances) %>%
    filter(distance <= max_distance)
  
  if (nrow(available_extras) == 0) next  # 若都太遠則跳過
  
  # 選擇距離最近的一個取車站
  closest_extra <- available_extras %>% slice_min(order_by = distance, n = 1)
  used_extras <- c(used_extras, closest_extra$station)  # 記錄已被配對的取車站
  
  # 建立配對紀錄
  routes_one_to_one <- bind_rows(
    routes_one_to_one,
    data.frame(
      station_need = need_row$station,
      longitude_need = need_row$longitude,
      latitude_need = need_row$latitude,
      need = need_row$need,
      station_extra = closest_extra$station,
      longitude_extra = closest_extra$longitude,
      latitude_extra = closest_extra$latitude,
      extra = closest_extra$extra,
      district_extra = closest_extra$district,
      distance = closest_extra$distance
    )
  )
}

# 整理結果表
dispatch_plan_weekend_noon_match_list <- routes_one_to_one %>% 
  select(station_need, need, longitude_need, latitude_need, station_extra, extra, longitude_extra, latitude_extra, district_extra, distance) %>% 
  arrange(district_extra, extra)

#整天不平衡:假日_晚間調度配對
#需補車站點
stations_need_bike <- dispatch_plan_weekend_night %>% 
  filter(dispatch_number > 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(need=dispatch_number)
#需取車站點
stations_with_extra <- dispatch_plan_weekend_night %>% 
  filter(dispatch_number < 0) %>% 
  select(station, longitude, latitude, dispatch_number, district) %>% 
  rename(extra=dispatch_number)

# 初始化
routes_one_to_one <- data.frame()
used_extras <- c()

# 依序處理每個補車站
for (i in 1:nrow(stations_need_bike)) {
  need_row <- stations_need_bike[i, ]
  
  # 找出尚未被配對的取車站
  available_extras <- stations_with_extra %>% 
    filter(!station %in% used_extras)
  
  if (nrow(available_extras) == 0) break  # 如果沒有取車站可配對，結束
  
  # 計算該補車站與所有尚未配對取車站的距離
  distances <- distHaversine(
    matrix(c(need_row$longitude, need_row$latitude), ncol = 2),
    matrix(c(available_extras$longitude, available_extras$latitude), ncol = 2)
  )
  # 加入距離欄位，並篩選距離小於指定值的站點
  available_extras <- available_extras %>%
    mutate(distance = distances) %>%
    filter(distance <= max_distance)
  
  if (nrow(available_extras) == 0) next  # 若都太遠則跳過
  
  # 選擇距離最近的一個取車站
  closest_extra <- available_extras %>% slice_min(order_by = distance, n = 1)
  used_extras <- c(used_extras, closest_extra$station)  # 記錄已被配對的取車站
  
  # 建立配對紀錄
  routes_one_to_one <- bind_rows(
    routes_one_to_one,
    data.frame(
      station_need = need_row$station,
      longitude_need = need_row$longitude,
      latitude_need = need_row$latitude,
      need = need_row$need,
      station_extra = closest_extra$station,
      longitude_extra = closest_extra$longitude,
      latitude_extra = closest_extra$latitude,
      extra = closest_extra$extra,
      district_extra = closest_extra$district,
      distance = closest_extra$distance
    )
  )
}

# 整理結果表
dispatch_plan_weekend_night_match_list <- routes_one_to_one %>% 
  select(station_need, need, longitude_need, latitude_need, station_extra, extra, longitude_extra, latitude_extra, district_extra, distance) %>% 
  arrange(district_extra, extra)


#製作調度配對圖
#建立一組不相似顏色列表
color_list<- c(
  "red", "blue", "green", "orange", "purple", "brown", "pink", "cyan",
  "black", "darkgreen", "darkblue", "goldenrod", "magenta", "gray20",
  "violet", "turquoise", "salmon", "darkred", "navy", "coral", "deeppink",
  "darkorange", "tomato", "maroon", "steelblue", "seagreen", "hotpink",
  "sienna", "dodgerblue", "firebrick", "slateblue", "mediumvioletred",
  "darkmagenta", "darkslateblue", "indigo", "crimson", "mediumblue",
  "darkcyan", "purple", "darkolivegreen"
)

#整天不平衡:平日早上配對圖
#建立配對編號
dispatch_plan_weekday_morning_match_list <- dispatch_plan_weekday_morning_match_list %>% 
  mutate(pair_id = row_number())

special_treatment_station <- dispatch_plan_weekday_morning_match_list %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_need" = "station_name")) %>% 
  rename("station_need_district" = "district") %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_extra" = "station_name")) %>% 
  rename("station_extra_district" = "district") %>% 
  filter(station_need_district %in% c( "台北市中正區", "台北市臺大公館校區") | station_extra_district %in% c( "台北市中正區","台北市臺大公館校區")) %>% 
  pull(pair_id)

#取出補車與取車資料為一張圖
dispatch_plan_weekday_morning_for_graph <- bind_rows(
  dispatch_plan_weekday_morning_match_list %>% 
    transmute(
      station = station_need,
      longitude = longitude_need, 
      latitude = latitude_need,
      role = "need_bike",
      pair_id = pair_id
    ),
  dispatch_plan_weekday_morning_match_list %>% 
    transmute(
      station = station_extra,
      longitude = longitude_extra,
      latitude = latitude_extra, 
      role = "extra_bike",
      pair_id = pair_id
    )
)

pair_ids <- unique(dispatch_plan_weekday_morning_for_graph$pair_id)

this_time_color <- setNames(color_list[1:length(pair_ids)], pair_ids)

ggplot(dispatch_plan_weekday_morning_for_graph) +
  geom_point(aes(x = longitude, y = latitude, color = factor(pair_id), shape = role), size = 2.5) +
  geom_text_repel(aes(x = longitude, y = latitude, label = station, color = factor(pair_id)), size = 3, max.overlaps = 100) +
  scale_color_manual(values = this_time_color) +
  scale_shape_manual(values = c("need_bike" = 1, "extra_bike" = 16))+
  labs(title = "YouBike 平日早上調度配對圖",
       x = "經度", y = "緯度", color = "配對 ID") +
  theme_minimal()

ggsave("調度配對圖_平日早上.png", width = 14, height = 8, dpi = 300, bg="white")

#整天不平衡:平日晚間配對圖
#建立配對編號
dispatch_plan_weekday_evening_match_list <- dispatch_plan_weekday_evening_match_list %>% 
  mutate(pair_id = row_number())

special_treatment_station <- dispatch_plan_weekday_evening_match_list %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_need" = "station_name")) %>% 
  rename("station_need_district" = "district") %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_extra" = "station_name")) %>% 
  rename("station_extra_district" = "district") %>% 
  filter(station_need_district %in% c( "台北市中正區", "台北市臺大公館校區") | station_extra_district %in% c( "台北市中正區","台北市臺大公館校區")) %>% 
  pull(pair_id)

#取出補車與取車資料為一張圖
dispatch_plan_weekday_evening_for_graph <- bind_rows(
  dispatch_plan_weekday_evening_match_list %>% 
    transmute(
      station = station_need,
      longitude = longitude_need, 
      latitude = latitude_need,
      role = "need_bike",
      pair_id = pair_id
    ),
  dispatch_plan_weekday_evening_match_list %>% 
    transmute(
      station = station_extra,
      longitude = longitude_extra,
      latitude = latitude_extra, 
      role = "extra_bike",
      pair_id = pair_id
    )
)

pair_ids <- unique(dispatch_plan_weekday_evening_for_graph$pair_id)

this_time_color <- setNames(color_list[1:length(pair_ids)], pair_ids)

ggplot(dispatch_plan_weekday_evening_for_graph) +
  geom_point(aes(x = longitude, y = latitude, color = factor(pair_id), shape = role), size = 2.5) +
  geom_text_repel(aes(x = longitude, y = latitude, label = station, color = factor(pair_id)), size = 3, max.overlaps = 100) +
  scale_color_manual(values = this_time_color) +
  scale_shape_manual(values = c("need_bike" = 1, "extra_bike" = 16))+
  labs(title = "YouBike 平日晚間調度配對圖",
       x = "經度", y = "緯度", color = "配對 ID") +
  theme_minimal()

ggsave("調度配對圖_平日晚間.png", width = 14, height = 8, dpi = 300, bg="white")

#整天不平衡:假日中午配對圖
#建立配對編號
dispatch_plan_weekend_noon_match_list <- dispatch_plan_weekend_noon_match_list %>% 
  mutate(pair_id = row_number())

special_treatment_station <- dispatch_plan_weekend_noon_match_list %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_need" = "station_name")) %>% 
  rename("station_need_district" = "district") %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_extra" = "station_name")) %>% 
  rename("station_extra_district" = "district") %>% 
  filter(station_need_district %in% c( "台北市中正區", "台北市臺大公館校區") | station_extra_district %in% c( "台北市中正區","台北市臺大公館校區")) %>% 
  pull(pair_id)

#取出補車與取車資料為一張圖
dispatch_plan_weekend_noon_for_graph <- bind_rows(
  dispatch_plan_weekend_noon_match_list %>% 
    transmute(
      station = station_need,
      longitude = longitude_need, 
      latitude = latitude_need,
      role = "need_bike",
      pair_id = pair_id
    ),
  dispatch_plan_weekend_noon_match_list %>% 
    transmute(
      station = station_extra,
      longitude = longitude_extra,
      latitude = latitude_extra, 
      role = "extra_bike",
      pair_id = pair_id
    )
)

pair_ids <- unique(dispatch_plan_weekend_noon_for_graph$pair_id)

this_time_color <- setNames(color_list[1:length(pair_ids)], pair_ids)

ggplot(dispatch_plan_weekend_noon_for_graph) +
  geom_point(aes(x = longitude, y = latitude, color = factor(pair_id), shape = role), size = 2.5) +
  geom_text_repel(aes(x = longitude, y = latitude, label = station, color = factor(pair_id)), size = 3, max.overlaps = 100) +
  scale_color_manual(values = this_time_color) +
  scale_shape_manual(values = c("need_bike" = 1, "extra_bike" = 16))+
  labs(title = "YouBike 假日中午調度配對圖",
       x = "經度", y = "緯度", color = "配對 ID") +
  theme_minimal()

ggsave("調度配對圖_假日中午.png", width = 14, height = 8, dpi = 300, bg="white")

#整天不平衡:假日晚間配對圖
#建立配對編號
dispatch_plan_weekend_night_match_list <- dispatch_plan_weekend_night_match_list %>% 
  mutate(pair_id = row_number())

special_treatment_station <- dispatch_plan_weekend_night_match_list %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_need" = "station_name")) %>% 
  rename("station_need_district" = "district") %>% 
  left_join(station_data %>% select(station_name, district), by= c("station_extra" = "station_name")) %>% 
  rename("station_extra_district" = "district") %>% 
  filter(station_need_district %in% c( "台北市中正區", "台北市臺大公館校區") | station_extra_district %in% c( "台北市中正區","台北市臺大公館校區")) %>% 
  pull(pair_id)

#取出補車與取車資料為一張圖
dispatch_plan_weekend_night_for_graph <- bind_rows(
  dispatch_plan_weekend_night_match_list %>% 
    transmute(
      station = station_need,
      longitude = longitude_need, 
      latitude = latitude_need,
      role = "need_bike",
      pair_id = pair_id
    ),
  dispatch_plan_weekend_night_match_list %>% 
    transmute(
      station = station_extra,
      longitude = longitude_extra,
      latitude = latitude_extra, 
      role = "extra_bike",
      pair_id = pair_id
    )
)

pair_ids <- unique(dispatch_plan_weekend_night_for_graph$pair_id)

this_time_color <- setNames(color_list[1:length(pair_ids)], pair_ids)

ggplot(dispatch_plan_weekend_night_for_graph) +
  geom_point(aes(x = longitude, y = latitude, color = factor(pair_id), shape = role), size = 2.5) +
  geom_text_repel(aes(x = longitude, y = latitude, label = station, color = factor(pair_id)), size = 3, max.overlaps = 100) +
  scale_color_manual(values = this_time_color) +
  scale_shape_manual(values = c("need_bike" = 1, "extra_bike" = 16))+
  labs(title = "YouBike 假日晚間調度配對圖",
       x = "經度", y = "緯度", color = "配對 ID") +
  theme_minimal()

ggsave("調度配對圖_假日晚間.png", width = 14, height = 8, dpi = 300, bg="white")

#將結果匯出
write_csv(availability_watchlist_weekday_morning, "輕度調度清單_平日早上.csv")
write_csv(availability_watchlist_weekday_evening, "輕度調度清單_平日晚間.csv")
write_csv(availability_watchlist_weekend_noon, "輕度調度清單_假日中午.csv")
write_csv(availability_watchlist_weekend_night, "輕度調度清單_假日晚間.csv")

dispatch_plan_weekday_morning_match_list <- dispatch_plan_weekday_morning_match_list %>% 
  select(station_need, need, station_extra, extra)
write_csv(dispatch_plan_weekday_morning_match_list, "調度配對清單_平日早上.csv")

dispatch_plan_weekday_evening_match_list <- dispatch_plan_weekday_evening_match_list %>% 
  select(station_need, need, station_extra, extra)
write_csv(dispatch_plan_weekday_evening_match_list, "調度配對清單_平日晚間.csv")

dispatch_plan_weekend_noon_match_list <- dispatch_plan_weekend_noon_match_list %>% 
  select(station_need, need, station_extra, extra)
write_csv(dispatch_plan_weekend_noon_match_list, "調度配對清單_假日中午.csv")

dispatch_plan_weekend_night_match_list <- dispatch_plan_weekend_night_match_list %>% 
  select(station_need, need, station_extra, extra)
write_csv(dispatch_plan_weekend_night_match_list, "調度配對清單_假日晚間.csv")