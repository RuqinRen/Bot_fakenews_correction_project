library("readxl")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(gridExtra)
library(ggnewscale)
library(lemon)
install.packages("fixest")
library(fixest)
install.packages("texreg")
library(texreg)

study1_data <- read_excel("/Users/rachelren/Desktop/Bot_fakenews_correction_HSSC/AI_misinfo_correction_datacode/study1_rawdata.xlsx")
names(study1_data)
study1_data <- study1_data[-c(1:5,7,19,36,55,70,81)]

# import residence income five level excel sheet
# Specify the file path
file_path <- "/Users/rachelren/Desktop/Bot_fakenews_correction_HSSC/AI_misinfo_correction_datacode/study1_income_5level.xlsx"

# Read the second sheet
income_5level <- read_excel(file_path, sheet = 2)
colnames(income_5level)[c(1,4,5,6)] <- c("residence","income_national","income_sample","income_sample_abs")

#rename raw data
colnames(study1_data)[c(1,2,69,70)] = c("residence","group","gender","age")
#clean up residence text
study1_data$residence <- gsub("[0-9.()\\-]", "",study1_data$residence)

#combine income df with the raw data df
study1_data <- left_join(study1_data, income_5level[c(1,4,5,6)], by = "residence")

#####

study1_data$group <- as.factor(study1_data$group)
study1_data$gender <- as.factor(study1_data$gender)
study1_data$age <- as.factor(study1_data$age)

#rename questions with news headline names
headlines <- c("zhongkao","tsinghua","westlake","germanvaccine","banana",
               "mushroom","middleeast","anesthesia")

groupnames <- c("bot_tag","platform_tag","bot_rebuttal","platform_rebuttal")
headlinename <- paste(headlines, rep(groupnames, each = length(headlines)), sep = "_")

suffixes <- c("accuracy", "sharing")
# Append the suffixes to each item in the list
headlinename <- paste(rep(headlinename, each = length(suffixes)), suffixes, sep = "_")
#get a list of 64 names now fill in to the raw data

colnames(study1_data)[3:68] <- c("pakistan_accuracy","pakistan_sharing", headlinename)

#update group number into group names
# Mapping numbers to group names
group_mapping <- c("bot_rebuttal", "platform_rebuttal", "bot_tag", "platform_tag")

# Replace group numbers with their respective names
study1_data <- study1_data %>%
  mutate(group = group_mapping[group])

#spread pakistan this one into new format, because all participants answered this, with four different random groups.
data_expanded <- study1_data %>%
   mutate(new_column = paste("pakistan_accuracy", group, sep = "_")) %>%
   pivot_wider(names_from = new_column, values_from = pakistan_accuracy)

data_expanded <- data_expanded %>%
  mutate(new_column = paste("pakistan_sharing", group, sep = "_")) %>%
  pivot_wider(names_from = new_column, values_from = pakistan_sharing)

study1_data <- data_expanded
#now I have cleaned up the Pakistan one.

#remove the unanswered columns, reorganize the columns by group

#add uid column
study1_data$uid <- 1:nrow(study1_data)

#add column for AI dummy and correction dummy

study1_data <- study1_data %>%
  mutate(
    bot_dummy = case_when(
      group == "bot_tag" | group == "bot_rebuttal"  ~ "bot_TRUE",
      group == "platform_tag" | group == "platform_rebuttal"  ~ "bot_FALSE"
    ),
    rebuttal_dummy = case_when(
      group == "bot_rebuttal" | group == "platform_rebuttal"  ~ "rebuttal_TRUE",
      group == "platform_tag" | group == "bot_tag"  ~ "rebuttal_FALSE"
    )
  )

#check for straightlining answers

study1_data[study1_data == -3] <- NA

# Check for straightlining by checking if all ratings for each user are identical (ignoring NAs)
study1_data <- study1_data %>%
  rowwise() %>%
  mutate(straightline = ifelse(
    n_distinct(c_across(c(3:66, 72:79)), na.rm = TRUE) == 1, TRUE, FALSE
  )) %>%
  ungroup()

table(study1_data$straightline)

# Step 3: Remove rows where straightline is TRUE
study1_data <- study1_data %>%
  filter(straightline == FALSE) %>%
  dplyr::select(-straightline)  # Optionally remove the straightline column.
#2113 rows remained.
#FALSE  TRUE
#2113    33

#participant demographic
study1_demographic <- study1_data %>% dplyr::select(., uid, residence, age, gender, contains("income"))
demographic_breakdown <- study1_demographic %>%
  summarise(
    # Residence
    gender = list(
      as.data.frame(table(gender)) %>%
        mutate(Percentage = Freq / sum(Freq) * 100)
        ),

    age = list(
      as.data.frame(table(age)) %>%
        mutate(Percentage = Freq / sum(Freq) * 100)),

    income_national = list(
      as.data.frame(table(income_national)) %>%
        mutate(Percentage = Freq / sum(Freq) * 100)),

    income_sample = list(
      as.data.frame(table(income_sample)) %>%
        mutate(Percentage = Freq / sum(Freq) * 100)),

    income_sample_abs= list(
      as.data.frame(table(income_sample_abs)) %>%
        mutate(Percentage = Freq / sum(Freq) * 100)),
    )%>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Data") %>%
          unnest(Data) %>%
          rename(Category = Var1, Count = Freq)

split_df <- study1_data %>%
  group_by(group) %>%
  group_split()

temp <- split_df[[1]]
group_bot_rebuttal <- temp %>%
  dplyr::select(uid, residence, gender, age, group, bot_dummy, rebuttal_dummy,contains("income_"), contains("bot_rebuttal"))

temp <- split_df[[2]]
group_bot_tag <- temp %>%
  dplyr::select(uid, residence, gender, age, group, bot_dummy, rebuttal_dummy,contains("income_"),contains("bot_tag"))

temp <- split_df[[3]]
group_platform_rebuttal <- temp %>%
  dplyr::select(uid, residence, gender, age, group, bot_dummy, rebuttal_dummy, contains("income_"),contains("platform_rebuttal"))

temp <- split_df[[4]]
group_platform_tag <- temp %>%
  dplyr::select(uid, residence, gender, age, group, bot_dummy, rebuttal_dummy,contains("income_"),contains("platform_tag"))

#create subsets for each articleId

study1_new <- data.frame()

for(i in 1:9) {

temp <- group_bot_rebuttal[ c(1:10, (9+2*i), (10+2*i))] #select only user-level variables and the two columns of that i_th news headline
temp$articleId <- gsub("_.*", "", names(group_bot_rebuttal[9+2*i]))
print(temp$articleId)
colnames(temp)[c(11, 12)] <- c("accuracy","worth")
study1_new <- rbind(study1_new, temp)
print("finished one article")
}
#This handles group_bot_rebuttal df. Now move onto next three groups.

for(i in 1:9) {

  temp <- group_bot_tag[  c(1:10, (9+2*i), (10+2*i))] #select only user-level variables and the two columns of that i_th news headline
  temp$articleId <- gsub("_.*", "", names(group_bot_tag[9+2*i]))
  print(temp$articleId)
  colnames(temp)[c(11, 12)] <- c("accuracy","worth")
  study1_new <- rbind(study1_new, temp)
  print("finished one article")
}

for(i in 1:9) {
  print(c(6+2*i, 7+2*i))
  temp <- group_platform_rebuttal[  c(1:10, (9+2*i), (10+2*i))] #select only user-level variables and the two columns of that i_th news headline
  temp$articleId <- gsub("_.*", "", names(group_platform_rebuttal[9+2*i]))
  print(temp$articleId)
  colnames(temp)[c(11, 12)] <- c("accuracy","worth")
  study1_new <- rbind(study1_new, temp)
  print("finished one article")
}

for(i in 1:9) {
  print(c(6+2*i, 7+2*i))
  temp <- group_platform_tag[  c(1:10, (9+2*i), (10+2*i))] #select only user-level variables and the two columns of that i_th news headline
  temp$articleId <- gsub("_.*", "", names(group_platform_tag[9+2*i]))
  print(temp$articleId)
  colnames(temp)[c(11, 12)] <- c("accuracy","worth")
  study1_new <- rbind(study1_new, temp)
  print("finished one article")
}

#update cell values from characters into numeric values

study1_new <- study1_new %>%
  mutate(income_national = recode(income_national,
                         "高收入组" = 5,
                         "中间偏上收入组" = 4,
                         "中间收入组" = 3,
                         "中等偏下收入组" = 2,
                         "低收入组" = 1),
         income_sample = recode(income_sample,
                                  "高收入组" = 5,
                                  "中等偏上收入组" = 4,
                                  "中等收入组" = 3,
                                  "中等偏下收入组" = 2,
                                  "低收入组" = 1),
         income_sample_abs = recode(income_sample_abs,
                                  "高收入组" = 5,
                                  "中等偏上收入组" = 4,
                                  "中等收入组" = 3,
                                  "中等偏下收入组" = 2,
                                  "低收入组" = 1))

study1_new$gender <- as.factor(study1_new$gender)
study1_new$uid <- as.character(study1_new$uid )
study1_new$age <- as.numeric(study1_new$age)

summary(study1_new)
write.csv(study1_new, "/Users/rachelren/Desktop/Bot_fakenews_correction_HSSC/AI_misinfo_correction_datacode/study1_new260207.csv")
names(study1_new)

study1_new <- read.csv("/Users/rachelren/Desktop/Bot_fakenews_correction_HSSC/AI_misinfo_correction_datacode/study1_new.csv",row.names = 1)

#include headline news level variables.

news_char <- read_excel("/Users/rachelren/Desktop/Bot_fakenews_correction_HSSC/AI_misinfo_correction_datacode/202501news_variables.xlsx")
# Check for straightlining by checking if all ratings for each user are identical (ignoring NAs)

temp1 <- news_char %>% select(1:9) 
colnames(temp1) <- c("uid","articleId","real","accuracy","overall_tone","sensational","pos_emotion","neg_emotion","social_importance")
temp2 <- news_char %>% select(1,11:18)
colnames(temp2) <- c("uid","articleId","real","accuracy","overall_tone","sensational","pos_emotion","neg_emotion","social_importance")
temp3 <- news_char %>% select(1,20:27)
colnames(temp3) <- c("uid","articleId","real","accuracy","overall_tone","sensational","pos_emotion","neg_emotion","social_importance")
news_char <- rbind(temp1, temp2, temp3)
news_char$articleId <- as.factor(news_char$articleId)

### check if the crowdsourced ratings show any significant difference?
# use aov
temp <- news_char %>% filter(articleId !=6)

#accuracy
res_aov <- aov(accuracy ~ articleId,
               data = temp
)
summary(res_aov)

post_test <- glht(res_aov,
                  linfct = mcp(articleId = "Tukey")
)
summary(post_test)

#real
news_char$articleId <- as.factor(news_char$articleId)
res_aov <- aov(real ~ articleId,
               data = temp
)
summary(res_aov)

post_test <- glht(res_aov,
                  linfct = mcp(articleId = "Tukey")
)
summary(post_test)

news_rating <- news_char %>%
  group_by(articleId) %>%
  summarise(
    mean_real = mean(real, na.rm = TRUE),
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    mean_tone = mean(overall_tone),
    mean_sensational = mean(sensational, na.rm = TRUE),
    mean_pos_emotion = mean(pos_emotion, na.rm = TRUE),
    mean_neg_emotion = mean(neg_emotion, na.rm = TRUE),
    mean_social_importance = mean(social_importance, na.rm = TRUE)
  )

headlines9 <- c("pakistan","zhongkao","tsinghua","westlake","germanvaccine","banana",
               "mushroom","middleeast","anesthesia")

# Replace group numbers with their respective names
news_rating <- news_rating %>%
  mutate(articleId = headlines9[articleId])

study1_new <- left_join(study1_new, news_rating, by = "articleId")

#check correlation
# Select only numeric columns
study1_new_numeric <- study1_new %>% dplyr::select(c(15:20))
# Calculate the correlation matrix for the numeric columns
cor_matrix <- cor(study1_new_numeric)
colnames(cor_matrix) <- c("baseline accuracy", "overall tone", "sensationality", "pos emotion", "neg emotion","importance")  # Replace with your modified variable names
rownames(cor_matrix) <- c("baseline accuracy", "overall tone", "sensationality", "pos emotion", "neg emotion","importance")  # Replace with your modified variable names

# Plot the correlation heatmap
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45,  # Rotate the labels for better visibility
         title = "Correlation Heatmap", mar = c(0, 0, 1, 0),
         addCoef.col = "black",        # Add correlation values
         font.cex = 0.6,               # Make the coefficient font smaller
         col.lab = "black")

#neg,pos,tone,sensational are highly correlated. Do not put together
########################
# new df created
########################
########################
#robust standard error with clustering on participant and article level
########################

study1_new$group <- relevel(as.factor(study1_new$group), ref = "platform_tag")
study1_new$uid <- as.factor(study1_new$uid)
study1_new$gender <- as.factor(study1_new$gender)
study1_new$age <- as.numeric(study1_new$age)

library(fixest)
m0 = feols(accuracy ~ bot_dummy + rebuttal_dummy +
             + mean_accu2024 + mean_sensational2025 + mean_social_importance2025 
           + age + gender+ income_sample, cluster = ~uid, data = study1_new)

m1 = feols(worth ~ bot_dummy + rebuttal_dummy +
             + mean_accu2024 + mean_sensational2025 + mean_social_importance2025 
           + age + gender+ income_sample, cluster = ~uid, data = study1_new)

m2 = feols(accuracy ~ bot_dummy * rebuttal_dummy +
           + mean_accu2024*rebuttal_dummy + mean_sensational2025*rebuttal_dummy + mean_social_importance2025*rebuttal_dummy 
           + mean_accu2024*bot_dummy + mean_sensational2025*bot_dummy + mean_social_importance2025*bot_dummy  
           + age + gender+ income_sample, cluster = ~uid, data = study1_new)
m2 

m3 = feols(worth ~ bot_dummy*rebuttal_dummy +
             + mean_accu2024*rebuttal_dummy + mean_sensational2025*rebuttal_dummy + mean_social_importance2025*rebuttal_dummy 
           + mean_accu2024*bot_dummy + mean_sensational2025*bot_dummy + mean_social_importance2025*bot_dummy  
           + age + gender+ income_sample, cluster = ~uid, data = study1_new)
m3 

m4 = feols(accuracy ~ bot_dummy * rebuttal_dummy +
             + mean_accu2024*rebuttal_dummy + mean_sensational2025*rebuttal_dummy + mean_social_importance2025*rebuttal_dummy 
           + mean_accu2024*bot_dummy + mean_sensational2025*bot_dummy + mean_social_importance2025*bot_dummy  
           + age + gender+ income_sample_abs, cluster = ~uid, data = study1_new)


m5 = feols(worth ~ bot_dummy*rebuttal_dummy +
             + mean_accu2024*rebuttal_dummy + mean_sensational2025*rebuttal_dummy + mean_social_importance2025*rebuttal_dummy 
           + mean_accu2024*bot_dummy + mean_sensational2025*bot_dummy + mean_social_importance2025*bot_dummy  
           + age + gender+ income_sample_abs, cluster = ~uid, data = study1_new)

screenreg(list(m2,m3,m4,m5)) #This is good.

reg_table <- htmlreg(list(m2,m3,m4,m5),
                       custom.model.names = c("(1)", "(2)" ,"(3)","(4)"),
                      custom.coef.names = c("Intercept", "Human", "Rebuttal", "Accuracy (base)", "Sensationality","Importance",
                                            "Age","Gender","Residence Income (sample)", 
                                            "Human x Rebuttal", 
                                            "Rebuttal x Accuracy (base)", "Rebuttal x Sensationality", "Rebuttal x Importance",
                                            "Human x Accuracy (base)", "Human x Sensationality", "Human x Importance","Residence Income (sample abs)"
                      ),
                       caption.above = TRUE,
                       label = "tab:regression",
                       include.ci = FALSE,
                       include.rsquared = TRUE,
                       include.adjrs = TRUE,
                       include.nobs = TRUE)

########################
# for appendix: mixed effects
########################
library(lmerTest)

# Mixed effects model for Accuracy (m2 equivalent)
# Including random intercepts for users (uid) and news stories (headline)
m2_mixed = lmer(accuracy ~ bot_dummy * rebuttal_dummy +
                  mean_accu2024 * rebuttal_dummy + 
                  mean_sensational2025 * rebuttal_dummy + 
                  mean_social_importance2025 * rebuttal_dummy +
                  mean_accu2024 * bot_dummy + 
                  mean_sensational2025 * bot_dummy + 
                  mean_social_importance2025 * bot_dummy +
                  age + gender + income_national + 
                  (1 | uid) + (1 | articleId), 
                data = study1_new)
summary(m2_mixed)

m3_mixed = lmer(worth ~ bot_dummy * rebuttal_dummy +
                  mean_accu2024 * rebuttal_dummy + 
                  mean_sensational2025 * rebuttal_dummy + 
                  mean_social_importance2025 * rebuttal_dummy +
                  mean_accu2024 * bot_dummy + 
                  mean_sensational2025 * bot_dummy + 
                  mean_social_importance2025 * bot_dummy +
                  age + gender + income_national + 
                  (1 | uid) + (1 | articleId), 
                data = study1_new)

summary(m3_mixed)
screenreg(list(m2_mixed,m3_mixed))
#good, need to update for better visualization to include in the robustness check

reg_table_lm <- htmlreg(list(m2_mixed,m3_mixed),
                     custom.model.names = c("(1)", "(2)"),
                     custom.coef.names = c("Intercept", "Bot", "Rebuttal", "Accuracy (base)", "Sensationality","Importance",
                                           "Age","Gender","Residence Income (national)", 
                                           "Human x Rebuttal", 
                                           "Rebuttal x Accuracy (base)", "Rebuttal x Sensationality", "Rebuttal x Importance",
                                           "Human x Accuracy (base)", "Human x Sensationality", "Human x Importance"
                     ),
                     caption.above = TRUE,
                     label = "tab:regression",
                     include.ci = FALSE,
                     include.rsquared = TRUE,
                     include.adjrs = TRUE,
                     include.nobs = TRUE)





#for appendix, change covariates

ma1 = feols(accuracy ~ bot_dummy + rebuttal_dummy +bot_dummy*rebuttal_dummy
             + mean_accu2024 *bot_dummy + mean_social_importance2025*bot_dummy + mean_tone2025 *bot_dummy
            + mean_accu2024 *rebuttal_dummy + mean_social_importance2025*rebuttal_dummy + mean_tone2025 *rebuttal_dummy
           + age + gender+ income_national, cluster = ~uid, data = study1_new)
 

ma2 = feols(worth ~ bot_dummy + rebuttal_dummy +bot_dummy*rebuttal_dummy
            + mean_accu2024 *bot_dummy + mean_social_importance2025*bot_dummy + mean_tone2025 *bot_dummy
            + mean_accu2024 *rebuttal_dummy + mean_social_importance2025*rebuttal_dummy + mean_tone2025 *rebuttal_dummy
            + age + gender+ income_national, cluster = ~uid, data = study1_new)

ma3 = feols(accuracy ~ bot_dummy + rebuttal_dummy +bot_dummy*rebuttal_dummy
              + mean_accu2024*bot_dummy +mean_social_importance2025*bot_dummy+ mean_pos_emotion2025 *bot_dummy
            + mean_accu2024 *rebuttal_dummy + mean_social_importance2025*rebuttal_dummy + mean_pos_emotion2025 *rebuttal_dummy
            + age + gender+ income_national, cluster = ~uid, data = study1_new)

ma4 = feols(worth ~ bot_dummy + rebuttal_dummy +bot_dummy*rebuttal_dummy
              + mean_accu2024*bot_dummy +mean_social_importance2025*bot_dummy + mean_pos_emotion2025*bot_dummy 
            + mean_accu2024 *rebuttal_dummy + mean_social_importance2025*rebuttal_dummy + mean_pos_emotion2025 *rebuttal_dummy
            + age + gender+ income_national, cluster = ~uid, data = study1_new)

ma5 = feols(accuracy ~ bot_dummy + rebuttal_dummy +bot_dummy*rebuttal_dummy
              + mean_accu2024*bot_dummy +mean_social_importance2025*bot_dummy + mean_neg_emotion2025*bot_dummy
            + mean_accu2024 *rebuttal_dummy + mean_social_importance2025*rebuttal_dummy + mean_neg_emotion2025 *rebuttal_dummy
            + age + gender+ income_national, cluster = ~uid, data = study1_new)

ma6 = feols(worth ~ bot_dummy + rebuttal_dummy +bot_dummy*rebuttal_dummy
              + mean_accu2024*bot_dummy +mean_social_importance2025*bot_dummy+ mean_neg_emotion2025*bot_dummy
            + mean_accu2024 *rebuttal_dummy + mean_social_importance2025*rebuttal_dummy + mean_neg_emotion2025 *rebuttal_dummy
            + age + gender+ income_national, cluster = ~uid, data = study1_new)

screenreg(list(ma1,ma2,ma3,ma4,ma5,ma6)) 

reg_table <- htmlreg(list(ma1,ma2,ma3,ma4,ma5,ma6),
                     custom.model.names = c("(1)", "(2)" ,"(3)","(4)","(5)","(6)"),
                     custom.coef.names = c("Intercept", "Human", "Rebuttal", "Accuracy (base)", "Importance","Overall tone",
                                           "Age","Gender","Residence Income (national)", 
                                           "Human x Rebuttal", 
                                           "Human x Accuracy (base)", "Human x Importance", "Human x Overall tone",
                                           "Rebuttal x Accuracy (base)",  "Rebuttal x Importance","Rebuttal x Overall tone",
                                           "Pos emotion", "Human x Pos emotion", "Rebuttal x Pos emotion",
                                           "Neg emotion" ,"Human x Neg emotion", "Rebuttal x Neg emotion"
                                           ),
                     caption.above = TRUE,
                     label = "tab:regression",
                     include.ci = FALSE,
                     include.rsquared = TRUE,
                     include.adjrs = TRUE,
                     include.nobs = TRUE)




# group comparison

#summary table for presentation
accu_worth_mean_se <- group_by(study1_new, group) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    lower = mean_accu - 1.96 * se,
    upper = mean_accu + 1.96 * se,
    
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth = sd(worth),
    se_worth = sd_worth/sqrt(N_group) ,
    lower_worth = mean_worth - 1.96 * se_worth,
    upper_worth = mean_worth + 1.96 * se_worth,
  )

#summary table for rebuttal vs tag
accu_worth_mean_se <- group_by(study1_new, rebuttal_dummy) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    lower = mean_accu - 1.96 * se,
    upper = mean_accu + 1.96 * se,
    
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth = sd(worth),
    se_worth = sd_worth/sqrt(N_group) ,
    lower_worth = mean_worth - 1.96 * se_worth,
    upper_worth = mean_worth + 1.96 * se_worth,
  )

#summary table for bot vs human
accu_worth_mean_se <- group_by(study1_new, bot_dummy) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    lower = mean_accu - 1.96 * se,
    upper = mean_accu + 1.96 * se,
    
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth = sd(worth),
    se_worth = sd_worth/sqrt(N_group),
    lower_worth = mean_worth - 1.96 * se_worth,
    upper_worth = mean_worth + 1.96 * se_worth,
  )

########################
## visualization
########################
install.packages("cowplot")
library(cowplot)

#set final names for publication
study1_new <- study1_new %>%
  mutate(group = recode(group, "bot_rebuttal" = "Bot rebuttal",
                        "bot_tag" = "Bot tag",
                        "platform_rebuttal" = "Human rebuttal",
                        "platform_tag" = "Human tag"))

study1_new$group <- relevel(as.factor(study1_new$group), ref = "Human tag")
#set order for presentation
study1_new$group <- factor(study1_new$group, levels = c("Bot rebuttal", "Bot tag", "Human rebuttal", "Human tag"))

study1_new <- study1_new %>%
  mutate(bot_dummy = recode(bot_dummy, "bot_TRUE" = "Bot",
                        "bot_FALSE" = "Human"))
study1_new <- study1_new %>%
  mutate(rebuttal_dummy = recode(rebuttal_dummy, "rebuttal_TRUE" = "Rebuttal",
                            "rebuttal_FALSE" = "Only Tag"))

#graph 1A: all four groups, accuracy
#figure1a with legend
figure1a_withlegend <-ggplot(study1_new, aes(x=group, y = accuracy, fill = bot_dummy, pattern = rebuttal_dummy)) +
  geom_bar_pattern( aes(fill = bot_dummy, pattern = rebuttal_dummy),stat = "summary", fun = "mean", position = "dodge",width = 0.3,
                   pattern_density = 0.002,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill = 'black',
                   pattern_colour = 'black') +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  theme_light(base_size=20)  +
  labs(y = "Perceived Accuracy Rating (0-5)", x = element_blank(),
       fill = "Corrector identity", pattern = "Correction strategy", tag = "(a)") +
  geom_signif(
    y_position = c(3.3, 3.8, 4.3, 4.8, 5.3, 5.8), xmin = c(1, 2,3,1,1,2), xmax = c(2, 3,4,3,4,4),
    annotation = c("***", 
                   "ns",
                   "***",
                   "***",
                   "ns",
                   "***"), tip_length = c(0.02, 0.03, 0.04, 0.04, 0.02, 0.02),textsize = 7
  ) + 
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#graph 1B: all four groups, worth
figure1b <- 
  b <-ggplot(study1_new, aes(x=group, y = worth, fill = bot_dummy, pattern = rebuttal_dummy)) +
  geom_bar_pattern( aes(fill = bot_dummy, pattern = rebuttal_dummy),stat = "summary", fun = "mean", position = "dodge",width = 0.3,
                    pattern_density = 0.002,
                    pattern_spacing = 0.025,
                    pattern_key_scale_factor = 0.6,
                    pattern_fill = 'black',
                    pattern_colour = 'black') +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  theme_light(base_size=20)  +
  labs(y = "Perceived Worthiness of Sharing (0-5)", x = element_blank(), fill = "", pattern = "",  tag = "(b)") + 
  geom_signif(
    y_position = c(3.3, 3.8, 4.3, 4.8, 5.3, 5.8), xmin = c(1, 2,3,1,1,2), xmax = c(2, 3,4,3,4,4),
    annotation = c("***", 
                   "ns",
                   "***",
                   "ns",
                   "ns",
                   "***"), tip_length = c(0.02, 0.03, 0.04, 0.04, 0.02, 0.02), textsize = 7
  )+ 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#extract legend from fig a
legend <- get_legend(
  figure1a_withlegend + 
    theme(legend.position = "right")  # Put legend to the right
)

# Combine the plots and legend, placing the legend to the right
final_plot <- plot_grid(figure1a_withlegend, figure1b,ncol = 2, rel_widths = c(1, 1))  # Equal space for plots
final_layout <- plot_grid(final_plot, legend, ncol = 2, rel_widths = c(0.9, 0.2))  # Give more space to legend
final_layout

ggsave(
  filename = "final_layout1.jpeg",  # File name
  plot = final_layout,                  # ggplot object
  device = "jpeg",            # File format (can also use "pdf", "jpeg", "tiff", etc.)
  width = 16,                 # Width in inches
  height = 8,                # Height in inches
  dpi = 400,
  limitsize = FALSE  # Resolution in dots per inch (DPI)
)

#use the above template and update next images
#Figure 2a: compare rebuttal vs. tag, accuracy
figure2a_withlegend <- ggplot(study1_new, aes(x=rebuttal_dummy, y = accuracy)) +
  geom_bar_pattern(aes(pattern = rebuttal_dummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3, fill = "grey80",
                   pattern_density = 0.002,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black')  +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1), xmax = c(2),
    annotation = c("ns"), tip_length = c(0.05), textsize = 8 
  ) +
  theme_light(base_size = 20) +
  labs( y = "Perceived Accuracy Rating (0-5)", x = element_blank(), tag = "(a)", pattern = "Correction strategy", fill = "")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#graph 2b: compare rebuttal vs tag, worth
figure2b<- ggplot(study1_new, aes(x=rebuttal_dummy, y = worth)) +
  geom_bar_pattern(aes(pattern = rebuttal_dummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3,  fill = "grey80",
                   pattern_density = 0.002,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black')  +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1), xmax = c(2),
    annotation = c("ns"), tip_length = c(0.05), textsize = 8 
  ) +
 theme_light(base_size = 20) +
  labs( y = "Perceived Worthiness of Sharing (0-5)", x = element_blank(), tag = "(b)", pattern = "Correction strategy", fill = "")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Fig 2 final combined plot with shared legend
legend <- get_legend(
  figure2a_withlegend + 
    theme(legend.position = "right")  # Put legend to the right
)

# Combine the plots and legend, placing the legend to the right
final_plot2 <- plot_grid(figure2a_withlegend, figure2b,ncol = 2, rel_widths = c(1, 1))  # Equal space for plots
final_layout2 <- plot_grid(final_plot2, legend, ncol = 2, rel_widths = c(0.9, 0.2))  # Give more space to legend
final_layout2


#compare bots vs human: 3a, accu
figure3a_withlegend <- ggplot(study1_new, aes(x=bot_dummy, y = accuracy)) +
  geom_bar(aes(fill = bot_dummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("ns"), tip_length = c(0.05), textsize = 8
  ) +
  theme_light(base_size=15) +
  labs( y = "Perceived Accuracy Rating (0-5)", x = element_blank(), tag = "(a)", fill = "Corrector identity", textsize = 15)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) 


#graph 3b: compare bots vs human, worth
figure3b <- ggplot(study1_new, aes(x=bot_dummy, y = worth)) +
  geom_bar(aes(fill = bot_dummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("***"), tip_length = c(0.05), textsize = 8
  ) +
  theme_light(base_size=15) +
  labs( y = "Perceived Worthiness of Sharing (0-5)", x = element_blank(),tag = "(b)", fill = "Corrector identity", textsize = 15)+
theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) 

legend <- get_legend(
  figure3a_withlegend + 
    theme(legend.position = "right")  # Put legend to the right
)

# Combine the plots and legend, placing the legend to the right
final_plot3 <- plot_grid(figure3a_withlegend, figure3b,ncol = 2, rel_widths = c(1, 1))  # Equal space for plots
final_layout3 <- plot_grid(final_plot3, legend, ncol = 2, rel_widths = c(0.8, 0.2))  # Give more space to legend
final_layout3

#interaction plots
# study 1 accuracy
# Generate predicted values for interaction effect visualization
df_plot <- study1_new %>%
  group_by(bot_dummy, rebuttal_dummy) %>%
  summarize(mean_acc = mean(accuracy), se = sd(accuracy) / sqrt(n()))

# Convert factor levels to meaningful labels
df_plot$bot_dummy <- factor(df_plot$bot_dummy, labels = c("Human", "Bot"))
df_plot$rebuttal_dummy <- factor(df_plot$rebuttal_dummy, labels = c("Tag", "Rebuttal"))

# Create interaction plot
fig_interact1a <- ggplot(df_plot, aes(x = rebuttal_dummy, y = mean_acc, group = bot_dummy, color = bot_dummy)) +
  geom_point(size = 3) +
  geom_line( size = 1) +
  geom_errorbar(aes(ymin = mean_acc - se, ymax = mean_acc + se), width = 0.1) +
  labs( y = "Accuracy Judgment (Mean)",
       color = "Corrector Identity",
       linetype = "Corrector Identity",
       x = element_blank(), tag = "(a)") +
  theme_light(base_size=15) +
  theme(text = element_text(size = 15))+
  theme(legend.position = "none")

legend <- get_legend(
  fig_interact1a + 
    theme(legend.position = "right")  # Put legend to the right
)

#study 1 worth
# Generate predicted values for interaction effect visualization - worth
df_plot <- study1_new %>%
  group_by(bot_dummy, rebuttal_dummy) %>%
  summarize(mean_worth = mean(worth), se = sd(worth) / sqrt(n()))

# Convert factor levels to meaningful labels
df_plot$bot_dummy <- factor(df_plot$bot_dummy, labels = c("Human", "Bot"))
df_plot$rebuttal_dummy <- factor(df_plot$rebuttal_dummy, labels = c("Tag", "Rebuttal"))

# Create interaction plot
fig_interact1b <- ggplot(df_plot, aes(x = rebuttal_dummy, y = mean_worth, group = bot_dummy, color = bot_dummy)) +
  geom_point(size = 3) +
  geom_line( size = 1) +
  geom_errorbar(aes(ymin = mean_worth - se, ymax = mean_worth + se), width = 0.1) +
  labs( x =element_blank(),
       y = "Worthiness of Sharing (Mean)",
       color = "Bot Dummy",
       linetype = "Bot Dummy",
       tag = "(b)") +
  theme_light(base_size=15) +
  theme(text = element_text(size = 15))+
  theme(legend.position = "none")

# Combine the plots and legend, placing the legend to the right
final_plot_interact1 <- plot_grid(fig_interact1a, fig_interact1b,ncol = 2, rel_widths = c(1, 1))  # Equal space for plots
final_layout5<- plot_grid(final_plot_interact1, legend, ncol = 2, rel_widths = c(0.8, 0.2))  # Give more space to legend
final_layout5

########################
## multiple comparison
########################
#pairwise between all four groups
library(multcomp)
study1_new$group <- as.factor(study1_new$group )
#accuracy
res_aov <- aov(accuracy ~ group,
               data = study1_new
               )
summary(res_aov)

post_test <- glht(res_aov,
                  linfct = mcp(group = "Tukey")
)
summary(post_test)

#worth
res_aov <- aov(worth ~ group,
               data = study1_new
)
summary(res_aov)
post_test <- glht(res_aov,
                  linfct = mcp(group = "Tukey")
)
summary(post_test)

# AI vs Expert
study1_new$bot_dummy <- as.factor(study1_new$bot_dummy)
res_aov <- aov(accuracy ~ bot_dummy, data = study1_new)
summary(res_aov)

res_aov <- aov(worth ~ bot_dummy, data = study1_new)
summary(res_aov)

post_test <- glht(res_aov, linfct = mcp(bot_dummy = "Tukey"))
summary(post_test)

#compare two strategy
study1_new$correction.dummy <- as.factor(study1_new$correction.dummy)
m3 <-lm(accuracy ~ correction.dummy, data=study1_new)
anova(m3)

study1_new$correction.dummy <- as.factor(study1_new$correction.dummy)
m31 <-lm(worth ~ correction.dummy, data=study1_new)
anova(m31)


ggplot(accu_mean_se, aes(x=group, y=mean_accu)) + geom_bar(stat="identity", width = 0.5) + 
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit, width = 0.2)) + ylim(0,4)

ggplot(accu_mean_se, aes(x=group, y=mean_accu)) + geom_bar(stat="identity", width = 0.5) + 
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit, width = 0.2)) + ylim(0,4)


#####################
## add the pilot accuracy baseline score as a control group
#####################
library(dplyr)
library(purrr)
library(broom)
common_ids <- intersect(
  pilot_update_without$articleId1,
  study1_new$articleId1
)

t_test_results <- map_df(common_ids, function(id) {
  
  x <- pilot_update_without %>%
    filter(articleId1 == id) %>%
    pull(accuracy)
  
  y <- study1_new %>%
    filter(articleId1 == id) %>%
    pull(accuracy)
  
  N1 <- length(x)
  N2 <- length(y)
  
  if (N1 < 2 || N2 < 2) return(NULL)
  
  tt <- t.test(x, y, var.equal = FALSE)
  
  data.frame(
    articleId1   = id,
    N1_pilot     = N1,
    N2_study1    = N2,
    mean_pilot   = mean(x, na.rm = TRUE),
    mean_study1  = mean(y, na.rm = TRUE),
    pre_post_diff = mean(x) - mean(y),
    t_statistic  = unname(tt$statistic),
    df           = unname(tt$parameter),
    p_value      = tt$p.value,
    conf_low     = tt$conf.int[1],
    conf_high    = tt$conf.int[2],
    stringsAsFactors = FALSE
  )
})

t_test_results

##### accu vs worth comparison
t_test_results_paired <- map_df(article_ids, function(id) {
  
  dat <- study1_new %>%
    dplyr::filter(articleId1 == id) %>%
    dplyr::select(accuracy, worth) %>%
    drop_na()
  
  N_pairs <- nrow(dat)
  
  if (N_pairs < 2) return(NULL)
  
  tt <- t.test(dat$accuracy, dat$worth, paired = TRUE)
  
  diff <- dat$accuracy - dat$worth
  
  data.frame(
    articleId1        = id,
    N_pairs           = N_pairs,
    mean_accuracy     = mean(dat$accuracy),
    mean_worth        = mean(dat$worth),
    mean_difference   = mean(diff),
    t_statistic       = unname(tt$statistic),
    df                = unname(tt$parameter),
    p_value           = tt$p.value,
    conf_low          = tt$conf.int[1],
    conf_high         = tt$conf.int[2],
    stringsAsFactors = FALSE
  )
})

t_test_results_paired