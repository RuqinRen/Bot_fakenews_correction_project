library("readxl")
library(dplyr)
library(ggplot2)
library(multcomp)
library(ggsignif)

study2_data <- read_excel("/home/rstudio/AI_misinfo_correction/study2_rawdata.xlsx")
names(study2_data)
study2_data <- study2_data[-c(1:7,9,10,22,41,56,67)]

#rename
colnames(study2_data)[c(1,2,53,54)] = c("prewarning","group","gender","age")
study2_data$group <- as.factor(study2_data$group)
study2_data$prewarning <- as.factor(study2_data$prewarning)
study2_data$gender <- as.factor(study2_data$gender)
study2_data$age <- as.factor(study2_data$age)

#remove an empty row
study2_data <- study2_data[-which(study2_data$group == -3),]
study2_data <- droplevels(study2_data) #this clears up any empty factor level in a df

#divide the complete df into 2*3 grouped df (prewarning * group)
table(study2_data$prewarning)
table(study2_data$group)
split_df <- split(x=study2_data, list(study2_data$prewarning, study2_data$group))  

group1<- split_df[[1]]
summary(group1)
names(group1)
group1 <- group1[c(1:4,21:36,53,54)]
group1$groupId <- "1"

group2<- split_df[[2]]
summary(group2)
names(group2)
group2 <- group2[c(1:4,21:36,53,54)]
group2$groupId <- "2"

group3<- split_df[[3]]
summary(group3)
names(group3)
group3 <- group3[c(1:4,37:52,53,54)]
group3$groupId <- "3"

group4 <- split_df[[4]]
summary(group4)
group4 <- group4[c(1:4,37:52,53,54)]
group4$groupId <- "4"

group5 <- split_df[[5]]
summary(group5)
names(group5)
group5 <- group5[c(1:4,5:20,53,54)]
group5$groupId <- "5"

group6 <- split_df[[6]]
summary(group6)
group6 <- group6[c(1:4,5:20,53,54)]
group6$groupId <- "6"

#rename specific columns
colnames(group1) = c("prewarning","corrstrategy", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)),"gender","age","groupId" )

colnames(group2) = c("prewarning","corrstrategy", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)),"gender","age","groupId" )

colnames(group3) = c("prewarning","corrstrategy", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)),"gender","age","groupId" )

colnames(group4) = c("prewarning","corrstrategy", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)),"gender","age","groupId" )

colnames(group5) = c("prewarning","corrstrategy", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)),"gender","age","groupId")

colnames(group6) = c("prewarning","corrstrategy", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)),"gender","age","groupId" )

#combine
study2<- rbind(group1, group2, group3, group4,group5, group6)
summary(study2)

study2$uid <- 1:nrow(study2)

#add column for AI dummy and correction dummy

study2 <- study2 %>% 
  mutate(
    AIdummy = case_when(
      corrstrategy == 1 | corrstrategy == 3  ~ "AI_TRUE",
      corrstrategy == 2   ~ "AI_FALSE"
    ),
    explanation.dummy = case_when(
      corrstrategy == 1 | corrstrategy == 2  ~ "With_explanation",
      corrstrategy == 3   ~ "No_explanation"
    )
  )


df %>% mutate(result=recode(result, 'Win'='1', 'Loss'='0'))

study2_new <- data.frame()

for(i in 1:9) {
  print(c(2*i+1, 2*i + 2))
  temp <- study2[ c(1,2,2*i+1, 2*i+2, 21:26)]
  temp$articleId <- i
  colnames(temp)[c(3, 4)] <- c("accuracy","worth")
  study2_new <- rbind(study2_new, temp)
  print("finished one article")
}

names(study2_new)

study2_accu <- study2_new[,-4] #remove one column for a simpler df
study2_worth <- study2_new[,-3]


study2_new <- study2_new %>% 
  mutate(
    groupId = recode(groupId, 
                    '1'= 'News literacy x Bot explanation' ,
                    '2'= "AI literacy x Bot explanation" ,
                    '3'= "News literacy x Expert explanation",
                    '4'= 'AI literacy x Expert explanation' ,
                    '5'= 'News literacy x Bot tag',
                    '6'= "AI literacy x Bot tag"),
   prewarning = recode(prewarning,
     '1'= "News literacy" ,
     '2'="AI literacy" ),
    corrstrategy =recode(corrstrategy,
    '1'= "Bot explanation",
    '2'= 'Expert explanation',
    '3'= 'Bot tag' )
  )

study2_new <- study2_new %>% 
  mutate(
    groupId = recode(groupId, 
                    'News literacy x Bot explanation' = "NL x Bot rebuttal" ,
                     "AI literacy x Bot explanation"  = "AL x Bot rebuttal" ,
                     "News literacy x Expert explanation"= "NL x Expert rebuttal" ,
                      'AI literacy x Expert explanation' = "AL x Expert rebuttal" ,
                     'News literacy x Bot tag'= "NL x Bot tag" ,
                      "AI literacy x Bot tag" = "AL x Bot tag" ),
    prewarning = recode(prewarning,
                        "News literacy" ="News literacy" ,
                       "AI literacy" = "Algorithm literacy" ),
    corrstrategy =recode(corrstrategy,
                          "Bot explanation" = "Bot rebuttal",
                          'Expert explanation' = "Expert rebuttal",
                          'Bot tag' = "Bot tag" )
  )

write.csv(study2_new, "study2_new.csv")

#next to visualization

#summary table for presentation
accu_mean_se <- group_by(study2_new, groupId) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    upper_limit=mean_accu+se, # Upper limit
    lower_limit=mean_accu-se # Lower limit
  )

worth_mean_se <- group_by(study2_new, groupId) %>%
  summarise(
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth=sd(worth), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_worth/sqrt(N_group), # Create variable with se of cty per group
    upper_limit=mean_worth+se, # Upper limit
    lower_limit=mean_worth-se # Lower limit
  )

accu_mean_se$prewarning <- rep(c("AI literacy", "News literacy"), each = 3)


########################
## visualization
########################


#graph 5a: compare News literacy vs AI literacy, accuracy
figure5a_withlegend <- ggplot(study2_new, aes(x=prewarning, y = accuracy)) +
  geom_bar(aes(fill = prewarning), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("***"), tip_length = c(0.05)
  ) +
  theme_light(base_size=20) +
  labs( y = "Perceived Accuracy Rating (0-5)", x = element_blank(), tag = "(a)", fill = "Intervention")

#graph 5b: compare News literacy vs AI literacy, worth

figure5b <- ggplot(study2_new, aes(x=prewarning, y = worth)) +
  geom_bar(aes(fill = prewarning), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("***"), tip_length = c(0.05)
  ) +
  theme_light(base_size=20) +
  labs( y = "Perceived Worthiness of Sharing (0-5)", x = element_blank(), tag = "(b)", fill = "Intervention")

grid_arrange_shared_legend(figure5a_withlegend, figure5b, ncol = 2, position='right')



# Figure 6 - compare all six
figure6a_withlegend <- ggplot(study2_new, aes(x=groupId, y = accuracy, fill = prewarning, pattern = corrstrategy)) +
  geom_bar_pattern(aes(fill = prewarning, pattern = corrstrategy), stat = "summary", fun = "mean", position = "dodge",width = 0.3, 
                   pattern_density = 0.002,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black') +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.2, 3.7, 4.2), xmin = c(1.0, 2, 3), xmax = c(4, 5, 6),
    annotation = c("Bot rebuttal: AL > NL ***", 
                   "Bot tag: ns",
                   "Expert rebuttal: AL > NL **" ), tip_length = c(0.03, 0.03, 0.03)
  ) +
  theme_light(base_size=20) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1,  size=10, face = "bold")) +
  labs( y = "Perceived Accuracy Rating (0-5)", x = element_blank(), pattern = "Correction strategy",fill= "Intervention",  tag = "(a)")+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
            fill = guide_legend(override.aes = list(pattern = "none")))


#graph 6B:  compare all six (worth)
figure6b <-ggplot(study2_new, aes(x=groupId, y = worth, fill = prewarning, pattern = corrstrategy)) +
  geom_bar_pattern(aes(fill = prewarning, pattern = corrstrategy), stat = "summary", fun = "mean", position = "dodge",width = 0.3, 
                   pattern_density = 0.002,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black') +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.2, 3.7, 4.2), xmin = c(1.0, 2, 3), xmax = c(4, 5, 6),
    annotation = c("Bot rebuttal: AL > NL *", 
                   "Bot tag: ns",
                   "Expert rebuttal: AL > NL ***"), tip_length = c(0.03, 0.03, 0.03)
  ) +
  theme_light(base_size=20) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1,  size=10, face = "bold")) +
  labs( y = "Perceived Worthiness of Sharing (0-5)", x = element_blank(),pattern = "Correction strategy",fill= "Intervention",  tag = "(b)")

grid_arrange_shared_legend(figure6a_withlegend, figure6b, ncol = 2, position='right')

