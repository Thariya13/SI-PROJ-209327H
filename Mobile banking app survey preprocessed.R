library(dplyr)

# importing data from csv
data1 <- read.csv(file.choose(), header = TRUE)

# ----
# selecting gender and usage status columns
filtered_data4 <- data1 %>% select(usage_status)

usage_status_table <- table(filtered_data4)

# performing test for a difference in proportions
# prop.test(gender_status_table, alternative = 'greater', correct=FALSE)
res <- prop.test(x = 182, n = 244, p = 0.7, alternative = 'greater', correct = FALSE)
res

# mean = 0.746

# -----
# selecting gender and usage status columns
filtered_data1 <- data1 %>% select(gender, usage_status)

# dropping prefer not to disclose rows
filtered_data1 <- filtered_data1[-c(20, 24, 142, 146), ]

# creating two way table
gender_status_table <- table(filtered_data1$gender,filtered_data1$usage_status)

# performing test for a difference in proportions
# prop.test(gender_status_table, correct=FALSE)
res <- prop.test(x = c(106, 74), n = c(122, 118))
# Printing the results
res 

# -----
# selecting security column
filtered_data2 = data1 %>% select(security)

# removing null values
filtered_data2 <- filtered_data2[!apply(filtered_data2 == "", 1, all), ]

security_table <- table(filtered_data2)

chisq.test(security_table, correct=FALSE)
chisq.test()

# -----
# selecting security and gender columns
filtered_data3 = data1 %>% select(gender, security)

# dropping prefer not to disclose rows
filtered_data3 <- filtered_data3[-c(20, 24, 142, 146), ]

# removing null values
# filtered_data5 <- filtered_data4[!apply(filtered_data4 == "", 1, all), ]
filtered_data3 <- filtered_data3[!(is.na(filtered_data3$security) | filtered_data3$security==""), ]

security_two_way_table <- table(filtered_data3$gender, filtered_data3$security)

chisq.test(security_two_way_table, correct=FALSE)

# -----
# selecting age_range, usage_rank column
filtered_data5 = data1 %>% select(age_range, usage_rank)

# removing null values
# filtered_data5 <- filtered_data5[!apply(filtered_data5 == "", 1, all), ]
filtered_data5 <- filtered_data5[!(is.na(filtered_data5$usage_rank) | filtered_data5$usage_rank==""), ]

# selecting age_range, usage_rank column
filtered_data5 = filtered_data5 %>% select(age_range)

age_range_table <- table(filtered_data5)

chisq.test(security_table, correct=FALSE)

# ----
# selecting gender and usage status columns
filtered_data6 <- data1 %>% select(age_range, usage_rank)

# removing null values
# filtered_data5 <- filtered_data5[!apply(filtered_data5 == "", 1, all), ]
filtered_data6 <- filtered_data6[!(is.na(filtered_data6$usage_rank) | filtered_data6$usage_rank==""), ]

# selecting age_range, usage_rank column
filtered_data6 = filtered_data6 %>% select(age_range)

usage_status_table <- table(filtered_data6)

# performing test for a difference in proportions
# prop.test(gender_status_table, alternative = 'greater', correct=FALSE)
res <- prop.test(x = 100, n = 182, p = 0.7, alternative = 'greater', correct = FALSE)
res
















