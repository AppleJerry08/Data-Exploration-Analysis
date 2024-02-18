install.packages("rio")
library(rio)
library(lubridate)
library(dplyr)
library(readr)
library(fixest)


filelist <- list.files(pattern = 'trends_', full.names = TRUE)

mydata <- import_list(filelist, rbind = TRUE, fill = TRUE)

mydata$monthorweek <- substr(mydata$monthorweek, 1, 10)
mydata$monthorweek <- ymd(mydata$monthorweek)
mydata$month <- floor_date(mydata$monthorweek, unit = "month")

mydata <- mydata %>%
  group_by(schname, keyword) %>%
  mutate(
    standardize_index = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)
  )


mydata_agg <- mydata %>%
  group_by(schname, month) %>%
  summarize(
    mean_standardize_index = mean(standardize_index, na.rm = TRUE),
    sd_standardize_index = sd(standardize_index, na.rm = TRUE)
    )

scorecard_data <- read_csv("Most+Recent+Cohorts+(Scorecard+Elements).csv")
id_name_link <- read_csv("id_name_link.csv")




result <- id_name_link %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  ungroup() %>%
  select(-n)

scorecard_data <- scorecard_data %>%
  rename(unitid = UNITID)

merged_data <- mydata %>%
  inner_join(result, by = 'schname') %>%
  inner_join(scorecard_data, by = 'unitid')


#pulling out predominatly bachelor granting schools
filtered_data <- merged_data %>%
  filter(PREDDEG == 3)

#renaming median earning column for readability
filtered_data <- filtered_data %>%
  rename(med_earning = `md_earn_wne_p10-REPORTED-EARNINGS`)

#changigng data type to numeric
filtered_data$`med_earning` <- as.numeric(filtered_data$`med_earning`)

#setting threshold for high earners to above 75th percentile
threshold <- quantile(filtered_data$med_earning, 0.75, na.rm = TRUE)

#filtering down to show only high earning schools
filtered_data <- filtered_data %>%
  mutate(income_cat = ifelse(med_earning > threshold, 0,1))


#create a post Scorecard data time frame

filtered_data <- filtered_data %>%
  filter(month > ymd('2015-09-15'))


