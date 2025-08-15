# YouBike 2.0 票證刷卡資料分析專題

## 專案簡介
本專案透過分析台北市 YouBike 2.0 票證刷卡資料，探討各站點的供需不平衡情形，並依據不同時間段制定調度策略，期望提升使用者體驗與調度效率。

## 使用資料
- 台北市 YouBike 2.0 每月票證刷卡資料（2024/07 ~ 2024/12）
- 站點資訊：行政區、經緯度、容量等  
⚠️ 資料檔案容量過大，未直接上傳至 GitHub。  
👉 [點此下載資料集（Google Drive）](https://drive.google.com/drive/folders/1N4KWnGpvu_o9Pbu9UpOZAAWczYceHu8P?usp=drive_link)

## 分析工具與環境
- RStudio（R 版本 4.4.3）
- 主要使用套件：
  - tidyverse（資料清理與視覺化）
  - stringdist（字串相似度比對）
  - kableExtra（美化表格）
  - geosphere（計算經緯度距離）
  - ggrepel（避免圖表文字重疊）
  - knitr（報告輸出與表格製作）
- 作業系統：Windows 11

## 分析重點
- 站點借還車流量分析
- 平假日與不同時間段供需差異
- 站點分類：短時間不平衡與整日不平衡
- 調度計畫配對與建議

## 專案結構
```plaintext
project_folder/
├── Conbine_Data.R # 合併原始資料
├── Clean_data.R # 清理與前處理資料
├── Analyst_Data.R # 進行資料分析
├── dataset/ # 原始資料
├── report/ # 分析報告
└── output/ # 匯出結果

```
## 使用說明
1. 下載原始資料並放入 `dataset/` 資料夾  
2. 執行 `Conbine_Data.R`：合併每月資料  
3. 執行 `Clean_data.R`：清理與轉換資料  
4. 執行 `Analyst_Data.R`：進行資料分析與圖表輸出  
