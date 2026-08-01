library("readxl")
library(dplyr)
library(ggplot2)
library(multcomp)
library(ggsignif)
library(lmerTest) # For mixed effects models with p-values
library(texreg)   # For screenreg output

########################
#  use the study2_new csv file
# new study2 df created
########################
study2_new <- read.csv("/Users/rachelren/Desktop/Bot_fakenews_correction/AI_misinfo_correction_datacode/study2_new260207.csv")

study2_new$articleId <- as.character(study2_new$articleId)
news_rating$articleId1 <- as.character(news_rating$articleId1)

study2_new <- left_join(study2_new, news_rating, by = c("articleId"))
names(study2_new)

# Remove human-related conditions: retain only the 2x2 bot design
# (AL/NL × bot tag/bot rebuttal)
study2_new <- study2_new %>% filter(group != "platform_rebuttal")

########################
# Participant demographics (bot-only dataset)
# Age coding: 1=<18, 2=18-25, 3=26-30, 4=31-40, 5=41-50, 6=51-60, 7=>60
########################
participants <- study2_new %>% distinct(uid, .keep_all = TRUE)

cat("=== N valid participants ===\n")
cat(nrow(participants), "\n")

cat("\n=== N observations ===\n")
cat(nrow(study2_new), "\n")

cat("\n=== Gender ===\n")
print(table(participants$gender))
cat("Male (1):", sum(participants$gender == 1, na.rm=T),
    "(", round(sum(participants$gender == 1, na.rm=T)/nrow(participants)*100, 2), "%)\n")
cat("Female (2):", sum(participants$gender == 2, na.rm=T),
    "(", round(sum(participants$gender == 2, na.rm=T)/nrow(participants)*100, 2), "%)\n")

cat("\n=== Age distribution ===\n")
age_labels <- c("<18"=1, "18-25"=2, "26-30"=3, "31-40"=4, "41-50"=5, "51-60"=6, ">60"=7)
for(label in names(age_labels)) {
  val <- age_labels[label]
  n <- sum(participants$age == val, na.rm=T)
  cat(sprintf("%-8s: %4d (%.2f%%)\n", label, n, n/nrow(participants)*100))
}
cat("\nBelow 30 (age 1-3):", sum(participants$age <= 3, na.rm=T),
    "(", round(sum(participants$age <= 3, na.rm=T)/nrow(participants)*100, 2), "%)")
cat("\n31-40 (age 4):", sum(participants$age == 4, na.rm=T),
    "(", round(sum(participants$age == 4, na.rm=T)/nrow(participants)*100, 2), "%)")
cat("\nAbove 40 (age 5-7):", sum(participants$age >= 5, na.rm=T),
    "(", round(sum(participants$age >= 5, na.rm=T)/nrow(participants)*100, 2), "%)\n")

########################
# Demographics table for Appendix (copy-paste to Word)
########################
N <- nrow(participants)

# Gender
gender_rows <- data.frame(
  Variable = c("Gender", ""),
  Levels = c("Male", "Female"),
  Frequency = c(sum(participants$gender == 1, na.rm=T),
                sum(participants$gender == 2, na.rm=T)),
  stringsAsFactors = FALSE
)
gender_rows$Percentage <- round(gender_rows$Frequency / N * 100, 2)

# Age
age_rows <- data.frame(
  Variable = c("Age Group", "", "", "", "", "", ""),
  Levels = c("<18", "18~25", "26~30", "31~40", "41~50", "51~60", ">60"),
  Frequency = c(sum(participants$age == 1, na.rm=T),
                sum(participants$age == 2, na.rm=T),
                sum(participants$age == 3, na.rm=T),
                sum(participants$age == 4, na.rm=T),
                sum(participants$age == 5, na.rm=T),
                sum(participants$age == 6, na.rm=T),
                sum(participants$age == 7, na.rm=T)),
  stringsAsFactors = FALSE
)
age_rows$Percentage <- round(age_rows$Frequency / N * 100, 2)

# Residence income (national) — coded 4=High, 3=Upper-Middle, 2=Middle, 1=Low
income_national_rows <- data.frame(
  Variable = c("Residence income level (by national census)", "", "", ""),
  Levels = c("High Income", "Upper-Middle Income", "Middle Income", "Low Income"),
  Frequency = c(sum(participants$income_national == 4, na.rm=T),
                sum(participants$income_national == 3, na.rm=T),
                sum(participants$income_national == 2, na.rm=T),
                sum(participants$income_national == 1, na.rm=T)),
  stringsAsFactors = FALSE
)
income_national_rows$Percentage <- round(income_national_rows$Frequency / N * 100, 2)

# Residence income (sample) — coded 5=High, 4=Upper-Middle, 3=Middle, 2=Lower-Middle, 1=Low
income_sample_rows <- data.frame(
  Variable = c("Residence income level (by sample distribution)", "", "", "", ""),
  Levels = c("High Income", "Upper-Middle Income", "Middle Income", "Lower-Middle Income", "Low Income"),
  Frequency = c(sum(participants$income_sample == 5, na.rm=T),
                sum(participants$income_sample == 4, na.rm=T),
                sum(participants$income_sample == 3, na.rm=T),
                sum(participants$income_sample == 2, na.rm=T),
                sum(participants$income_sample == 1, na.rm=T)),
  stringsAsFactors = FALSE
)
income_sample_rows$Percentage <- round(income_sample_rows$Frequency / N * 100, 2)

# Residence income (sample absolute) — coded 5=High, 4=Upper-Middle, 3=Middle, 2=Lower-Middle, 1=Low
income_sample_abs_rows <- data.frame(
  Variable = c("Residence income level (sample absolute)", "", "", "", ""),
  Levels = c("High Income", "Upper-Middle Income", "Middle Income", "Lower-Middle Income", "Low Income"),
  Frequency = c(sum(participants$income_sample_abs == 5, na.rm=T),
                sum(participants$income_sample_abs == 4, na.rm=T),
                sum(participants$income_sample_abs == 3, na.rm=T),
                sum(participants$income_sample_abs == 2, na.rm=T),
                sum(participants$income_sample_abs == 1, na.rm=T)),
  stringsAsFactors = FALSE
)
income_sample_abs_rows$Percentage <- round(income_sample_abs_rows$Frequency / N * 100, 2)

# Combine and print
table_c2 <- bind_rows(gender_rows, age_rows, income_national_rows, income_sample_rows, income_sample_abs_rows)
cat("\n=== Table C2: Demographic Characteristics (N =", N, ") ===\n")
print(table_c2, right = FALSE)

# Export to CSV for copy-paste
write.csv(table_c2,
          "/Users/rachelren/Desktop/Bot_fakenews_correction/AI_misinfo_correction_datacode/table_c2_demographics.csv",
          row.names = FALSE)

########################
#descriptive table
########################

# Create 2x2 grouping variable (intervention × format)
study2_new <- study2_new %>%
  mutate(
    cell = case_when(
      prewarning == "AIL"   & group == "bot_rebuttal" ~ "AL x Bot rebuttal",
      prewarning == "AIL"   & group == "bot_tag"      ~ "AL x Bot tag",
      prewarning == "NewsL" & group == "bot_rebuttal" ~ "NL x Bot rebuttal",
      prewarning == "NewsL" & group == "bot_tag"      ~ "NL x Bot tag"
    )
  )

# --- Cell-level descriptives (4 cells) ---
cell_desc <- study2_new %>%
  group_by(cell) %>%
  summarise(
    N         = n(),
    accu_mean = mean(accuracy, na.rm = TRUE),
    accu_sd   = sd(accuracy, na.rm = TRUE),
    worth_mean = mean(worth, na.rm = TRUE),
    worth_sd   = sd(worth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    accu_se  = accu_sd / sqrt(N),
    worth_se = worth_sd / sqrt(N)
  )

# --- Marginal descriptives by intervention type ---
prewarn_desc <- study2_new %>%
  group_by(prewarning) %>%
  summarise(
    N          = n(),
    accu_mean  = mean(accuracy, na.rm = TRUE),
    accu_sd    = sd(accuracy, na.rm = TRUE),
    worth_mean = mean(worth, na.rm = TRUE),
    worth_sd   = sd(worth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cell     = ifelse(prewarning == "AIL", "AL", "NL"),
    accu_se  = accu_sd / sqrt(N),
    worth_se = worth_sd / sqrt(N)
  )

# --- Marginal descriptives by correction format ---
format_desc <- study2_new %>%
  mutate(format_label = ifelse(group == "bot_rebuttal", "Rebuttal", "Tag")) %>%
  group_by(format_label) %>%
  summarise(
    N          = n(),
    accu_mean  = mean(accuracy, na.rm = TRUE),
    accu_sd    = sd(accuracy, na.rm = TRUE),
    worth_mean = mean(worth, na.rm = TRUE),
    worth_sd   = sd(worth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cell     = format_label,
    accu_se  = accu_sd / sqrt(N),
    worth_se = worth_sd / sqrt(N)
  )

# --- Combine and format ---
table5 <- bind_rows(cell_desc, prewarn_desc, format_desc) %>%
  mutate(
    accu_mean  = round(accu_mean, 2),
    accu_sd    = round(accu_sd, 2),
    accu_se    = round(accu_se, 2),
    worth_mean = round(worth_mean, 2),
    worth_sd   = round(worth_sd, 2),
    worth_se   = round(worth_se, 2)
  ) %>%
  dplyr::select(Group = cell, N,
         Accu_Mean = accu_mean, Accu_SD = accu_sd, Accu_SE = accu_se,
         Worth_Mean = worth_mean, Worth_SD = worth_sd, Worth_SE = worth_se)

# Print for review
print(table5)

# Export to CSV (can paste into Word/Excel)
write.csv(table5, "/Users/rachelren/Desktop/Bot_fakenews_correction/AI_misinfo_correction_datacode/table5_descriptives.csv", row.names = FALSE)



########################
#regression analysis
########################
study2_new$age <- as.numeric(study2_new$age)
study2_new$gender <- as.numeric(study2_new$gender)
study2_new$uid <- as.character(study2_new$uid)
# bot_dummy removed: all remaining conditions are bot-based (2x2 design)

#treating the uid as clustered errors
library(fixest)
# m30 = feols(accuracy ~ prewarning + bot_dummy + rebuttal_dummy
#             + mean_accuracy + mean_sensational + mean_social_importance
#             +age + gender + income_national*prewarning, cluster = ~uid  , data = study2_new)
# 
# m31 = feols(worth ~ prewarning + bot_dummy + rebuttal_dummy
#             + mean_accuracy + mean_sensational + mean_social_importance
#             +age + gender + income_national*prewarning, cluster = ~uid, data = study2_new)

# Set reference group to News Literacy so regression shows AL effect
study2_new$prewarning <- relevel(as.factor(study2_new$prewarning), ref = "NewsL")

m32 = feols(accuracy ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning  
              +mean_sensational2025*prewarning 
              + mean_social_importance2025 *prewarning
            +age + gender + income_national*prewarning, cluster = ~articleId, data = study2_new)

m33 = feols(worth ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning 
            + mean_sensational2025*prewarning +
            + mean_social_importance2025 *prewarning
            +age + gender + income_national*prewarning, cluster = ~articleId, data = study2_new)

screenreg(list(m32,m33))

reg_table <- htmlreg(list(m32,m33),
                     custom.model.names = c("(1) Accuracy", "(2) Sharing"),
                     custom.coef.names = c("Intercept", "AL", "Rebuttal", "Accuracy (base)", "Sensationality","Importance",
                                           "Age","Female","Residence Income (national)",
                                           "AL x Rebuttal",
                                           "AL x Accuracy (base)",
                                           "AL x Sensationality",
                                           "AL x Importance",
                                           "AL x Residence Income (national)"
                     ),
                     caption.above = TRUE,
                     label = "tab:regression",
                     include.ci = FALSE,
                     include.rsquared = TRUE,
                     include.adjrs = TRUE,
                     include.nobs = TRUE)

# Save as HTML file (open in Word or browser to copy-paste)
writeLines(reg_table, "/Users/rachelren/Desktop/Bot_fakenews_correction/AI_misinfo_correction_datacode/table6_main_regression.html")

########################
#robustness check analysis
########################
#mean_tone 

r32 = feols(accuracy ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning + mean_tone2025*prewarning + mean_social_importance2025*prewarning
            +age + gender + income_national*prewarning, cluster = ~articleId, data = study2_new)

r33 = feols(worth ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning + mean_tone2025*prewarning + mean_social_importance2025*prewarning
            +age + gender + income_national*prewarning, cluster = ~articleId, data = study2_new)

screenreg(list(r32,r33))

#mean_neg_emotion

r34 = feols(accuracy ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning + mean_neg_emotion2025*prewarning + mean_social_importance2025*prewarning
            +age + gender + income_national*prewarning, cluster = ~uid, data = study2_new)

r35 = feols(worth ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning + mean_neg_emotion2025*prewarning + mean_social_importance2025*prewarning
            +age + gender + income_national*prewarning, cluster = ~uid, data = study2_new)


#mean_pos_emotion
r36 = feols(accuracy ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning + mean_pos_emotion2025*prewarning + mean_social_importance2025*prewarning
            +age + gender + income_national*prewarning, cluster = ~uid, data = study2_new)

r37 = feols(worth ~ prewarning * rebuttal_dummy
            + mean_accuracy2025*prewarning + mean_pos_emotion2025*prewarning + mean_social_importance2025*prewarning
            +age + gender + income_national*prewarning, cluster = ~uid, data = study2_new)

screenreg(list(r32,r33,r34,r35,r36,r37))



reg_table <- htmlreg(list(r32,r33,r34,r35,r36,r37),
                     custom.model.names = c("(1) Accuracy", "(2) Sharing","(3) Accuracy", "(4) Sharing","(5) Accuracy", "(6) Sharing"),
                     custom.coef.names = c("Intercept", "AL", "Rebuttal",
                                           "Accuracy (base)", "Overall tone", "Importance",
                                           "Age","Female","Residence Income (national)",
                                           "AL x Rebuttal",
                                           "AL x Accuracy (base)", "AL x Overall tone", "AL x Importance",
                                           "AL x Residence Income (national)",
                                           "Negative emotion", "Positive emotion",
                                           "AL x Negative emotion", "AL x Positive emotion"
                     ),
                     caption.above = TRUE,
                     label = "tab:regression",
                     include.ci = FALSE,
                     include.rsquared = TRUE,
                     include.adjrs = TRUE,
                     include.nobs = TRUE)

# Save as HTML file (open in Word or browser to copy-paste)
writeLines(reg_table, "/Users/rachelren/Desktop/Bot_fakenews_correction/AI_misinfo_correction_datacode/table_robustness_covariates.html")

#income different measures

m1 = feols(accuracy ~ prewarning * rebuttal_dummy
           + mean_accuracy2025*prewarning + mean_sensational2025*prewarning + mean_social_importance2025*prewarning
           +age + gender + income_sample*prewarning, cluster = ~uid, data = study2_new)

m2 = feols(worth ~ prewarning * rebuttal_dummy
           + mean_accuracy2025*prewarning + mean_sensational2025*prewarning + mean_social_importance2025*prewarning
           +age + gender + income_sample*prewarning, cluster = ~uid, data = study2_new)

m3 = feols(accuracy ~ prewarning * rebuttal_dummy
           + mean_accuracy2025*prewarning + mean_sensational2025*prewarning + mean_social_importance2025*prewarning
           +age + gender + income_sample_abs*prewarning, cluster = ~uid, data = study2_new)

m4 = feols(worth ~ prewarning * rebuttal_dummy
           + mean_accuracy2025*prewarning + mean_sensational2025*prewarning + mean_social_importance2025*prewarning
           +age + gender + income_sample_abs*prewarning, cluster = ~uid, data = study2_new)

screenreg(list(m1,m2,m3,m4))

reg_table <- htmlreg(list(m1,m2,m3,m4),
                     custom.model.names = c("(1) Accuracy", "(2) Sharing","(3) Accuracy", "(4) Sharing"),
                     custom.coef.names = c("Intercept", "AL", "Rebuttal",
                                           "Accuracy (base)", "Sensationality", "Importance",
                                           "Age","Female","Residence Income (sample)",
                                           "AL x Rebuttal",
                                           "AL x Accuracy (base)", "AL x Sensationality", "AL x Importance",
                                           "AL x Residence Income (sample)",
                                           "Residence Income (sample abs)", "AL x Residence Income (sample abs)"
                     ),
                     caption.above = TRUE,
                     label = "tab:regression",
                     include.ci = FALSE,
                     include.rsquared = TRUE,
                     include.adjrs = TRUE,
                     include.nobs = TRUE)

writeLines(reg_table, "/Users/rachelren/Desktop/Bot_fakenews_correction/AI_misinfo_correction_datacode/table_robustness_income.html")



r32 = feols(worth ~ rebuttal_dummy* prewarning
            + mean_tone *prewarning+ mean_social_importance*prewarning
            + mean_tone *rebuttal_dummy + mean_social_importance*rebuttal_dummy
            +age + gender + income_national*prewarning, cluster = ~uid  , data = study2_new)

r33 = feols(accuracy ~ rebuttal_dummy* prewarning
            + mean_accuracy*prewarning + mean_pos_emotion *prewarning+ mean_social_importance*prewarning
            + mean_accuracy*rebuttal_dummy  + mean_pos_emotion *rebuttal_dummy + mean_social_importance*rebuttal_dummy
            +age + gender + income_national*prewarning, cluster = ~uid  , data = study2_new)

r34 = feols(worth ~ rebuttal_dummy* prewarning
            + mean_accuracy*prewarning + mean_pos_emotion *prewarning+ mean_social_importance*prewarning
            + mean_accuracy*rebuttal_dummy  + mean_pos_emotion *rebuttal_dummy + mean_social_importance*rebuttal_dummy
            +age + gender + income_national*prewarning, cluster = ~uid  , data = study2_new)

r35 = feols(accuracy ~ rebuttal_dummy* prewarning
            + mean_accuracy*prewarning + mean_neg_emotion *prewarning+ mean_social_importance*prewarning
            + mean_accuracy*rebuttal_dummy  + mean_neg_emotion *rebuttal_dummy + mean_social_importance*rebuttal_dummy
            +age + gender + income_national*prewarning, cluster = ~uid  , data = study2_new)

r36 = feols(worth ~ rebuttal_dummy* prewarning
            + mean_accuracy*prewarning + mean_neg_emotion *prewarning+ mean_social_importance*prewarning
            + mean_accuracy*rebuttal_dummy  + mean_neg_emotion *rebuttal_dummy + mean_social_importance*rebuttal_dummy
            +age + gender + income_national*prewarning, cluster = ~uid  , data = study2_new)

screenreg(list(r31,r32))

reg_table <- htmlreg(list(m30, m31,m32,m33),
                     custom.model.names = c("(1) Accuracy", "(2) Sharing","(3) Accuracy", "(4) Sharing"),
                     custom.coef.names = c("Intercept", "AL","Bot", "Rebuttal", "Accuracy (base)", "Sensationality","Importance",
                                           "Age","Female","Residence Income (national)","AL x Residence Income (national)",
                                           "AL x Rebuttal", "AL x Bot",
                                           "AL x Accuracy (base)", "AL x Sensationality", "AL x Importance",
                                           "Rebuttal x Accuracy (base)","Rebuttal x Sensationality", "Rebuttal x Importance",
                                           "Bot x Accuracy (base)","Bot x Sensationality","Bot x Importance"
                     ),
                     caption.above = TRUE,
                     label = "tab:regression",
                     include.ci = FALSE,
                     include.rsquared = TRUE,
                     include.adjrs = TRUE,
                     include.nobs = TRUE)

########################
# for appendix: mixed effects model
########################
library(lmerTest)
#HSSC revision: mixed effect analysis with random effects
m32_mixed = lmer(accuracy ~ prewarning * rebuttal_dummy +
                   mean_real.y2024 * prewarning +
                    mean_sensational2025 * prewarning +
                   mean_social_importance2025 * prewarning +
                   age + gender + income_national*prewarning  +
                   (1 | uid) +
                   (1 | articleId),
                 data = study2_new)

# Mixed-effects model for Worthiness of Sharing (m33 equivalent)
m33_mixed = lmer(worth ~ prewarning * rebuttal_dummy +
                   mean_real.y2024 * prewarning +
                    mean_sensational2025 * prewarning +
                   mean_social_importance2025 * prewarning +
                   age + gender + income_national*prewarning +
                   (1 | uid) +
                   (1 | articleId),
                 data = study2_new)
# Display results comparison
screenreg(list(m32_mixed, m33_mixed))

# update for better visualization to include in the robustness check

reg_table_lm <- htmlreg(list(m32_mixed, m33_mixed),
                        custom.model.names = c("(1) Accuracy", "(2) Sharing"),
                        custom.coef.names = c("Intercept", "AL", "Rebuttal", "Accuracy (base)", "Sensationality","Importance",
                                              "Age","Female","Residence Income (national)",
                                              "AL x Rebuttal",
                                              "AL x Accuracy (base)",
                                              "AL x Sensationality",
                                              "AL x Importance",
                                              "AL x Residence Income (national) "
                        ),
                        caption.above = TRUE,
                        label = "tab:regression",
                        include.ci = FALSE,
                        include.rsquared = TRUE,
                        include.adjrs = TRUE,
                        include.nobs = TRUE)

# Save mixed effects table as HTML
writeLines(reg_table_lm, "/Users/rachelren/Desktop/Bot_fakenews_correction/AI_misinfo_correction_datacode/table_mixed_effects.html")

########################
# visualization
########################
#summary table for presentation
accu_mean_se <- group_by(study2_new, sixgroup) %>%
  dplyr::summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group) # Create variable with se of cty per group
  )

worth_mean_se <- group_by(study2_new, sixgroup) %>%
  summarise(
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth=sd(worth), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_worth/sqrt(N_group)# Create variable with se of cty per group
  )

accu_worth_mean_se <- group_by(study2_new, prewarning) %>%
  dplyr::summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se_accu=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth=sd(worth), # Create variable with sd of cty per group
    se_worth=sd_worth/sqrt(N_group)# Create variable with se of cty per group
  )

accu_mean_se <- group_by(study2_new, bot_dummy) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group)) # Create variable with se of cty per group)

worth_mean_se <- group_by(study2_new, bot_dummy) %>%
  summarise(
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth=sd(worth), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_worth/sqrt(N_group) # Create variable with se of cty per group
  )

accu_mean_se <- group_by(study2_new, rebuttal_dummy) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group))

worth_mean_se <- group_by(study2_new, rebuttal_dummy) %>%
  summarise(
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth=sd(worth), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_worth/sqrt(N_group)
  )


#regression
m0 = feols(accuracy ~ prewarning+group
           + mean_accuracy*group + mean_sensational*group + mean_social_importance*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m0

m1 = feols(worth ~ prewarning+group
           + mean_accuracy*group + mean_sensational*group + mean_social_importance*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m1


m4 = feols(accuracy ~ group* prewarning+
             + mean_accuracy*group + mean_sensational*group + mean_social_importance*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m4

m5 = feols(worth ~  group* prewarning+
             + mean_accuracy*group + mean_sensational*group + mean_social_importance*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m5 

screenreg(list(m0,m1,m4,m5))

#try social news
temp <- study2_new %>% dplyr::filter( articleId != "banana" )
temp <- study2_new %>% dplyr::filter( articleId == "anesthesia" | articleId == "tsinghua" | articleId == "westlake" | articleId == "zhongkao")

m0 = feols(accuracy ~ prewarning + group
           + mean_accuracy + mean_sensational*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m0

m1 = feols(worth ~ prewarning+group
           + mean_accuracy + mean_sensational*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m1

m4 = feols(accuracy ~ group* prewarning+
             + mean_accuracy + mean_sensational*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m4

m5 = feols(worth ~  group* prewarning+
             + mean_accuracy+  mean_sensational*group 
           + age + gender + income_national, cluster = ~uid, data = study2_new)
m5 

screenreg(list(m0,m1,m4,m5))

m0 = feols(accuracy ~ prewarning*rebuttal_dummy + prewarning * bot_dummy
           + mean_accuracy + mean_sensational*rebuttal_dummy  + mean_sensational * bot_dummy
           + age + gender + income_national, cluster = ~uid, data = temp)
m0

m1 = feols(worth ~ prewarning*rebuttal_dummy + prewarning * bot_dummy
           + mean_accuracy + mean_sensational*rebuttal_dummy  + mean_sensational * bot_dummy
           + age + gender + income_national, cluster = ~uid, data = temp)
m1

m4 = feols(accuracy ~ prewarning*rebuttal_dummy + prewarning * bot_dummy
           + mean_accuracy + mean_sensational*rebuttal_dummy  + mean_sensational * bot_dummy
           + age + gender + income_national, cluster = ~uid, data = temp)
m4

m5 = feols(worth ~  prewarning*rebuttal_dummy + prewarning * bot_dummy
           + mean_accuracy + mean_sensational*rebuttal_dummy  + mean_sensational * bot_dummy
           + age + gender + income_national, cluster = ~uid, data = temp)
m5 

screenreg(list(m0,m1,m4,m5))

# Extract independent variables from the model
independent_variables <- terms(model)

# Get the names of the independent variables
independent_vars_names <- labels(independent_variables)
print(independent_vars_names)

# Select the independent variables from the dataset
regression_data <- study2_new[, independent_vars_names]
head(regression_data)

# Calculate the correlation matrix for the independent variables
cor_matrix <- cor(regression_data)

# View the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "circle")

screenreg(list(m0,m1,m4,m5))

#try social news
temp <- study2_new %>% dplyr::filter( articleId == "middleeast" | articleId == "pakistan" )

m0 = feols(accuracy ~ prewarning + rebuttal_dummy +
             + mean_accuracy+ mean_sensational + mean_social_importance + mean_neg_emotion + mean_tone
           + age + gender + income_national, cluster = ~uid, data = temp)
summary(m0)

m1 = feols(worth ~ prewarning * rebuttal_dummy 
           + mean_accuracy+ mean_sensational + mean_social_importance + mean_neg_emotion + mean_tone
           + age + gender + income_national, cluster = ~uid, data = temp)
m1

m4 = feols(accuracy ~ prewarning * rebuttal_dummy +
             + mean_accuracy+ mean_sensational + mean_social_importance + mean_neg_emotion + mean_tone
           + age + gender + income_national, cluster = ~uid, data = temp)
m4

m5 = feols(worth ~  prewarning * rebuttal_dummy +
             + mean_accuracy+ mean_sensational + mean_social_importance + mean_neg_emotion + mean_tone
           + age + gender + income_national, cluster = ~uid, data = temp)
m5 

screenreg(list(m0,m1,m4,m5))



#archive

# 20240612 update the names for publication
study2_new<-read.csv("/home/rstudio/AI_misinfo_correction/study2_new.csv")
study2_new$groupId <- as.factor(study2_new$groupId )
study2_new$prewarning <- as.factor( study2_new$prewarning)
study2_new$corrstrategy <- as.factor(study2_new$corrstrategy )

study2_new <- study2_new %>% 
  mutate(groupId = recode_factor(groupId, 
                                 'News literacy x Bot explanation' = "NL x Bot rebuttal" ,
                                 "AI literacy x Bot explanation"  = "AL x Bot rebuttal" ,
                                 "News literacy x Expert explanation"= "NL x Human rebuttal" ,
                                 'AI literacy x Expert explanation' = "AL x Human rebuttal" ,
                                 'News literacy x Bot tag'= "NL x Bot tag" ,
                                 "AI literacy x Bot tag" = "AL x Bot tag" ) ,
         prewarning = recode_factor(prewarning,
                                    "News literacy" ="News literacy" ,
                                    "AI literacy" = "Algorithm literacy" ),
         corrstrategy =recode_factor(corrstrategy,
                                     "Bot explanation" = "Bot rebuttal",
                                     'Expert explanation' = "Human rebuttal",
                                     'Bot tag' = "Bot tag" ))
write.csv(study2_new, "study2_new.csv")




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



#####################
## add the pilot accuracy baseline score as a control group
#####################
library(dplyr)
library(purrr)
library(broom)
common_ids <- intersect(
  pilot_update_without$articleId1,
  study2_new$articleId1
)

t_test_results <- map_df(common_ids, function(id) {
  
  x <- pilot_update_without %>%
    filter(articleId1 == id) %>%
    pull(accuracy)
  
  y <- study2_new %>%
    filter(articleId1 == id) %>%
    pull(accuracy)
  
  N1 <- length(x)
  N2 <- length(y)
  
  if (N1 < 2 || N2 < 2) return(NULL)
  
  tt <- t.test(x, y, var.equal = FALSE)
  
  data.frame(
    articleId1   = id,
    N1_pilot     = N1,
    N2_study2    = N2,
    mean_pilot   = mean(x, na.rm = TRUE),
    mean_study2  = mean(y, na.rm = TRUE),
    t_statistic  = unname(tt$statistic),
    df           = unname(tt$parameter),
    p_value      = tt$p.value,
    conf_low     = tt$conf.int[1],
    conf_high    = tt$conf.int[2],
    stringsAsFactors = FALSE
  )
})

t_test_results