library("readxl")
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggpattern)
library(gridExtra)
library(ggnewscale)
library(lemon)

study1_data <- read_excel("/home/rstudio/AI_misinfo_correction/study1_rawdata.xlsx")
names(study1_data)
study1_data <- study1_data[-c(1:7,19,36,55,70,81)]

#rename
colnames(study1_data)[c(1,68,69)] = c("group","gender","age")
study1_data$group <- as.factor(study1_data$group)
study1_data$gender <- as.factor(study1_data$gender)
study1_data$age <- as.factor(study1_data$age)

summary(study1_data$gender)
summary(study1_data$age)/2146

#divide the complete df into 4 grouped df first
split_df <- split(study1_data, f=study1_data$group)  

group1<- split_df[[1]]
summary(group1)
names(group1)
group1 <- group1[c(1:3,36:51,68,69)]

group2<- split_df[[2]]
summary(group2)
names(group2)
group2 <- group2[c(1:3,52:67,68,69)]

group3<- split_df[[3]]
summary(group3)
names(group3)
group3 <- group3[c(1:19,68,69)]

group4 <- split_df[[4]]
summary(group4)
group4 <- group4[c(1:3,20:35,68,69)]

#rename specific columns
colnames(group1) = c("group", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)),"gender","age" )

colnames(group2) = c("group", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)) ,"gender","age")

colnames(group3) = c("group", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)) ,"gender","age")

colnames(group4) = c("group", paste0(rep(c('accuracy','worthspread'), 9),"Id",rep((1:9),each =2)) ,"gender","age")

#combine
study1 <- rbind(group1, group2, group3, group4)
summary(study1)

#add uid column
study1$uid <- 1:nrow(study1)

#add column for AI dummy and correction dummy

study1 <- study1 %>% 
  mutate(
    AIdummy = case_when(
      group == 1 | group == 3  ~ "AI_TRUE",
      group == 2 | group == 4  ~ "AI_FALSE"
    ),
    correction.dummy = case_when(
      group == 1 | group == 2  ~ "With_correction",
      group == 3 | group == 4  ~ "No_correction"
    )
  )


#create subsets for each articleId

study1_new <- data.frame()

for(i in 1:9) {
  print(c(2*i, 2*i + 1))
temp <- study1[ c(1, 2*i, 2*i+1,20,21,22,23,24)]
temp$articleId <- i
colnames(temp)[c(2, 3)] <- c("accuracy","worth")
study1_new <- rbind(study1_new, temp)
print("finished one article")
}

names(study1_new)

study1_accu <- study1_new[,-3]
study1_worth <- study1_new[,-2]

study1_new <- study1_new %>% 
  mutate(
    group = recode(group, 
                     '1'= 'Bot Explanation' ,
                     '2'= "Expert Explanation" ,
                     '3'= "Bot Tag",
                     '4'= 'Expert Tag' )
  )

study1_new <- study1_new %>% 
  mutate(
    AIdummy = recode(AIdummy, 
                   'AI_TRUE'= 'Bot' ,
                   'AI_FALSE'= "Expert" ),
    correction.dummy = recode(correction.dummy,
                              "With_correction" = "Explanation",
                              "No_correction" = "Tag"
    )
  )


study1_new$group <- as.factor(study1_new$group)

#replace value names for publication
study1_new["group"][study1_new["group"] == "Bot Explanation"] <- "Bot Rebuttal"
study1_new["group"][study1_new["group"] == "Expert Explanation"] <- "Expert Rebuttal"
study1_new["correction.dummy"][study1_new["correction.dummy"] == "Explanation"] <- "Rebuttal"

write.csv(study1_new, "/home/rstudio/AI_misinfo_correction/study1_new.csv")
study1_new <- read.csv("/home/rstudio/AI_misinfo_correction/study1_new.csv")
########################
# new df created
########################
########################
#robust standard error
########################

library(lmtest)
library(multiwayvcov)
library(lfe)

my_fe_model <- felm(accuracy ~ AIdummy + correction.dummy | uid | 0 | uid + articleId , data=study1_new )
summary(my_fe_model)

my_fe_model <- felm(worth ~ AIdummy + correction.dummy |0 | 0 | uid + articleId , data=study1_new )
summary(my_fe_model)

# group comparison

#summary table for presentation
accu_worth_mean_se <- group_by(study1_new, group) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    upper_limit=mean_accu+se, # Upper limit
    lower_limit=mean_accu-se, # Lower limit
    
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth = sd(worth),
    se_worth = sd_worth/sqrt(N_group) 
  )

#summary table for rebuttal vs tag
accu_worth_mean_se <- group_by(study1_new, correction.dummy) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    upper_limit=mean_accu+se, # Upper limit
    lower_limit=mean_accu-se, # Lower limit
    
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth = sd(worth),
    se_worth = sd_worth/sqrt(N_group) 
  )

#summary table for bot vs human
accu_mean_se <- group_by(study1_new, AIdummy) %>%
  summarise(
    mean_accu = mean(accuracy, na.rm = TRUE),
    sd_accu=sd(accuracy), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_accu/sqrt(N_group), # Create variable with se of cty per group
    upper_limit=mean_accu+se, # Upper limit
    lower_limit=mean_accu-se, # Lower limit
    
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth = sd(worth),
    se_worth = sd_worth/sqrt(N_group) 
  )

worth_mean_se <- group_by(study1_new, AIdummy) %>%
  summarise(
    mean_worth = mean(worth, na.rm = TRUE),
    sd_worth=sd(worth), # Create variable with sd of cty per group
    N_group=n(), # Create new variable N per group
    se=sd_worth/sqrt(N_group), # Create variable with se of cty per group
    upper_limit=mean_worth+se, # Upper limit
    lower_limit=mean_worth-se, # Lower limit
  )

########################
## visualization
########################
#graph 1A: all four groups, accuracy
#figure1a with legend
figure1a_withlegend <- ggplot(study1_new, aes(x=group, y = accuracy, fill = AIdummy, pattern = correction.dummy)) +
  geom_bar_pattern( aes(fill = AIdummy, pattern = correction.dummy),stat = "summary", fun = "mean", position = "dodge",width = 0.3,
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
                   "**",
                   "ns",
                   "***"), tip_length = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
  ) + 
  scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
  scale_pattern_manual(values = c(Rebuttal = "stripe", Tag = "circle"))  + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
            fill = guide_legend(override.aes = list(pattern = "none"))) 

#graph 1B: all four groups, worth
figure1b <- ggplot(study1_new, aes(x=group, y = worth, fill = AIdummy, pattern = correction.dummy)) +
  geom_bar_pattern( aes(fill = AIdummy, pattern = correction.dummy),stat = "summary", fun = "mean", position = "dodge",width = 0.3,
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
                   "***"), tip_length = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
  ) + 
  scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
  scale_pattern_manual(values = c(Rebuttal = "stripe", Tag = "circle"))  + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(legend.position = "none")

# final combined plot with shared legend
grid_arrange_shared_legend(figure1a_withlegend, figure1b, ncol = 2, position='right')



#Figure 2a: compare rebuttal vs. tag, accuracy
figure2a_withlegend <- ggplot(study1_new, aes(x=correction.dummy, y = accuracy)) +
  geom_bar_pattern(aes(pattern = correction.dummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3,  fill = "grey80",
                   pattern_density = 0.002,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black')  +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1), xmax = c(2),
    annotation = c("ns"), tip_length = c(0.05)
  ) +
  theme_light(base_size = 20) +
  labs( y = "Perceived Accuracy Rating (0-5)", x = element_blank(), tag = "(a)", pattern = "Correction strategy", fill = "")+
  scale_pattern_manual(values = c(Rebuttal = "stripe", Tag = "circle")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")))


#graph 2b: compare rebuttal vs tag, worth
figure2b<- ggplot(study1_new, aes(x=correction.dummy, y = worth)) +
  geom_bar_pattern(aes(pattern = correction.dummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3,fill = "grey80",
                   pattern_density = 0.002,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black')  +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1), xmax = c(2),
    annotation = c("ns"), tip_length = c(0.05)
  ) +
  theme_light(base_size = 20) +
  labs( y = "Perceived Worthiness of Sharing (0-5)", x = element_blank(), tag = "(b)", pattern = "")+
  theme(legend.position = "none")

# final combined plot with shared legend
grid_arrange_shared_legend(figure2a_withlegend, figure2b, ncol = 2, position='right')



#Figure 3a: compare bots vs human, accuracy
figure3a_withlegend <- ggplot(study1_new, aes(x=AIdummy, y = accuracy)) +
  geom_bar(aes(fill = AIdummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("ns"), tip_length = c(0.05)
  ) +
  theme_light(base_size=20) +
  labs( y = "Perceived Accuracy Rating (0-5)", x = element_blank(), tag = "(a)")+
  scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4))


figure3b <- ggplot(study1_new, aes(x=AIdummy, y = worth)) +
  geom_bar(aes(fill = AIdummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("***"), tip_length = c(0.05)
  ) +
  theme_light(base_size=20) +
  labs( y = "Perceived Worthiness of Sharing (0-5)", x = element_blank(), tag = "(b)")+
  scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4))

# final combined plot with shared legend
grid_arrange_shared_legend(figure3a_withlegend, figure3b, ncol = 2, position='right')



#compare bots vs human
ggplot(study1_new, aes(x=AIdummy, y = accuracy)) +
  geom_bar(aes(fill = AIdummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("***"), tip_length = c(0.05)
  ) +
  theme_light(base_size=15) +
  labs( y = "Perceived Accuracy Rating (0-5)", x = element_blank())


#graph D: compare bots vs human, worth
ggplot(study1_new, aes(x=AIdummy, y = worth)) +
  geom_bar(aes(fill = AIdummy), stat = "summary", fun = "mean", position = "dodge",width = 0.3 ) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  geom_signif(
    y_position = c(3.1), xmin = c(1.0), xmax = c(2),
    annotation = ("***"), tip_length = c(0.05)
  ) +
  theme_light(base_size=15) +
  labs( y = "Perceived Worthiness of Sharing (0-5)", x = element_blank())


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
post_test <- glht(res_aov,
                  linfct = mcp(group = "Tukey")
)
summary(post_test)


# AI vs Expert

study1_new$AIdummy <- as.factor(study1_new$AIdummy)
m2 <-lm(accuracy ~ AIdummy, data=study1_new)
anova(m2)

m21 <-lm(worth ~ AIdummy, data=study1_new)
anova(m21)

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
