library("readxl")
install.packages('dplyr')
install.packages('rlang')
library(dplyr)
library(ggplot2)

# pilot_data <- read_excel("/Users/rachelren/Desktop/Bot_fakenews_correction_HSSC/AI_misinfo_correction_datacode/220021313_2_pilotdata.xlsx")
# pilot_data <- pilot_data[-c(2,3,4,5,6,7,14, 21,28)]
# 
# #rename
# colnames(pilot_data) = c("uid", rep(c("articleId", "realness","accuracy"), 6))
# colnames(pilot_data)[c(2,5,8,11,14,17)] = c(paste0("articleId",1:6))
# 
# #recode variable of articleId
# 
# pilot_update <- pilot_data[1:4]
# 
# temp<- pilot_data[c(1,17,18,19)] %>% mutate(articleId1 = recode(articleId6,
#                                                                     '1' = '16',
#                                                                     '2' = '17')) %>% select(uid, articleId1, realness, accuracy)
# pilot_update <- rbind(pilot_update, temp)
# table(pilot_update$articleId1)
# write.csv(pilot_update,"pilotupdate20230523.csv")

pilot_update <- read.csv("pilotupdate20230523.csv", row.names = 1)

pilot_update$articleId1 <- as.factor(pilot_update$articleId1)
summary(pilot_update)

#anova analysis

#remove different ones
#No.8,14,11,13,4,7,5,2
pilot_update_without <- pilot_update %>%  filter(articleId1 != 8 & articleId1 != 14 & articleId1 != 11 & articleId1 != 13 & articleId1 != 4  & articleId1 != 7 & articleId1 != 5  &articleId1 != 2)


#visualization
ggplot(pilot_update_without) +
  aes(x = articleId1, y = accuracy) +
  geom_boxplot()


#see group by descriptive (mean value)
article_mean <- group_by(pilot_update_without, articleId1) %>%
  summarise(
    mean_real = mean(realness, na.rm = TRUE),
    mean_accu = mean(accuracy, na.rm = TRUE)
  )

headlines <- c("pakistan","zhongkao","tsinghua","westlake","germanvaccine","banana",
               "mushroom","middleeast","anesthesia")

article_mean$articleName <- headlines

library(multcomp)
res_aov <- aov(accuracy ~ articleId1,
               data = pilot_update_without)

summary(res_aov)
# Tukey HSD test:
post_test <- glht(res_aov,
                  linfct = mcp(articleId1 = "Tukey")
)

summary(post_test)

res_aov <- aov(realness ~ articleId1,
               data = pilot_update_without)

summary(res_aov)
#confirmed that the left articles are not sig different

#compare the article_mean. table with second batch crowdsourced scores "news_rating"df from 202501
news_rating <- left_join(news_rating, article_mean, by = c("articleId" = "articleName"))
#upadte colnames
colnames(news_rating)[2:8] <- paste0(colnames(news_rating)[2:8], "2025")  # Attach 2025 to columns 2 to 8
colnames(news_rating)[10:11] <- paste0(colnames(news_rating)[10:11], "2024")  # Attach 2024 to columns 10 and 11

write.csv(news_rating,"pilot_newsrating20250215.csv")
news_rating <- read.csv("pilot_newsrating20250215.csv",row.names = 1)
library(tidyr)

# Reshape data to long format
news_rating <- news_rating %>% 
  pivot_longer(cols = c(2:8,10:11),  # Select columns that start with "type"
               names_to = "rating_type",    # Name the new column as 'rating_type'
               values_to = "rating_value")  # Name the new column for rating values

ggplot(news_rating, aes(x = factor(articleId), y = rating_value, fill = rating_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Article ID", y = "Rating Value", fill = "Rating Source",
       title = "Comparison of Different Ratings for News Articles") +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  scale_x_discrete(labels = paste("Article", 1:9))