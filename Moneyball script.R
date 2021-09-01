library(tidyverse)
judge_scores <- read_csv("~/Desktop/raw/judge-scores.csv")
judge_scores <- judge_scores %>% group_by(aspect_id)
judge_scores <- mutate(judge_scores, mean= mean(score))
judge_scores <- judge_scores %>% ungroup(aspect_id)

judge_scores

performances <- read_csv("~/Desktop/raw/performances.csv")
performances <- select(performances, performance_id, nation)
performances <- performances[1530:1561,]
names(performances) <- c("performance_id", "player_nation")
performances

judge_nation <- read_csv("~/Desktop/processed/judge-nation.csv")
judge_nation <- judge_nation %>% select(judge, judge_nation)
judge_nation

pids <- c(performances$performance_id)
aids <- c(judged_aspects$aspect_id)
judge_scores <- judge_scores[judge_scores$aspect_id %in% aids,]
judge_scores

judged_aspects <- read_csv("~/Desktop/raw/judged-aspects.csv")
judged_aspects <- judged_aspects[judged_aspects$performance_id %in% pids,]
judged_aspects <- judged_aspects %>% 
  group_by(performance_id) %>%
  arrange(performance_id)

judged_aspects <- judged_aspects %>% select(aspect_id, performance_id, aspect_num)
judged_aspects <- filter(judged_aspects, 
                         aspect_num == 1|
                         aspect_num ==2|
                         aspect_num ==3|
                         aspect_num ==4|
                         aspect_num ==5 )
judged_aspects

total0 <- merge(judge_nation, judge_scores, by="judge" )
total0 <- total0 %>% arrange(aspect_id)
save(total0, file = "~/Desktop/processed/total0.csv")

total1 <- merge(total0, judged_aspects, by="aspect_id")
total1 <-total1 %>% select(aspect_id, judge, judge_nation, 
                           score, mean, performance_id)
total1 <- total1 %>% arrange(aspect_id)
total1
save(total1, file = "~/Desktop/processed/total1.csv")

total2 <- merge(performances, total1, by="performance_id")
total2 <- total2 %>% arrange(aspect_id)
save(total2, file = "~/Desktop/processed/total2.csv")

total2 <- total2 %>% arrange(player_nation) %>% arrange(judge)
total2 <- total2 %>% mutate(standard_score = score - mean)

matchcount <- 0
matchsum <- 0

for(i in 1:1440){
  if(total2$player_nation[i] ==total2$judge_nation[i]){
    matchcount <- matchcount + 1
    matchsum <- matchsum + total2$standard_score[i]
  }
}

matchcount
matchsum
matchavg <- matchsum/matchcount
matchavg
#[1] 0.4152778
sd(total2$standard_score)

matches <- total2[total2$player_nation==total2$judge_nation,]
View(matches)
save(matches, file = "~/Desktop/processed/matches.csv")

matchcount1 <- 0
matchsum1 <- 0

for(i in 1:1440){
  if(total2$player_nation[i] == "USA"  & total2$judge_nation[i]== "RUS")
    {matchcount1 <- matchcount1 + 1
    matchsum1 <- matchsum1 + total2$standard_score[i]
    }
}
matchcount1
matchsum1
matchavg1 <- matchsum1/matchcount1
matchavg1

# USA 15 0.3777778 J1
# FRA 15 0.05185185 J4
# CAN 15 0.6074074 J7
# RUS 10 0.3666667 J2
# KOR 5 0.4222222 J5
# CZE 5 0.2888889 J6
# TUR 5 0.844444 J8
k4
# ISR 5 0.5777778 J3
# ESP 5 0.6666667 J9
# average of all scores 0.5660729

total2 %>% group_by(judge_nation) %>%
summarise(average = mean(standard_score))

ggplot(data=matches) + 
  geom_histogram(mapping = aes(x=standard_score), bins=15) 

ggplot(data=total2) + 
  geom_histogram(mapping = aes(x=standard_score), bins=12) 

install.packages("coin")
library(coin)

trialsums <- c(rep(NA,10000))
trialmeans <- c(rep(NA,10000))
p_count <- 0
for(i in 1:10000){
  rows <- sample(1:160, 5)
  sumscore <- 0
  for(j in 1:5){
    sumscore <- sumscore + total2[total2$judge=="J7",]$standard_score[rows[j]]
  }
  trialsums[i] <- sumscore
  trialmeans[i] <- sumscore/5
  if(sumscore/5 > mean(matches[matches$judge == "J7",]$standard_score)){
    p_count <- p_count + 1
  }
}
trialmeans
p_count
p_value <- p_count/10000
print(p_value)
mean= mean(trialmeans)

#observed value
#mean(matches[matches$judge == "J5",]$standard_score)
#0.6666667

library(tidyverse)
total_data <- tibble("totalmeans" = totalmeans)
total_data %>%
  ggplot(aes(x=totalmeans)) +
  geom_histogram(bins=40, alpha=0.7) +
  geom_vline(xintercept = 0.4152778) + 
  labs(x = "Trial mean", y = "Count", title = "Permutation test for overall bias")

ggplot(data=trialmeans[i]) + 
  geom_histogram(mapping = aes(x=trialmeans[i]), bins=20) 

mean(trialmeans)
sd(trialmeans)

#J1 -0.3507044 0.2853778
#J2 -0.1821333 0.2232709
#J3  0.1428133 0.1953083
#J4 -0.2911622 0.227104
#J5 -0.0172355 0.2102683
#J6 -0.1726244 0.2145588
#J7 0.2994778  0.1954553
#J8 0.3196511  0.2229298
#J9 0.2530444  0.2456683

# USA 15 0.3777778 J1
# FRA 15 0.05185185 J4
# CAN 15 0.6074074 J7
# RUS 10 0.3666667 J2
# KOR 5 0.4222222 J5
# CZE 5 0.2888889 J6
# TUR 5 0.844444 J8
# ISR 5 0.5777778 J3
# ESP 5 0.6666667 J9

totalcount <- 0
totalsum <- 0

for(i in 1:1440){
  if(total2$player_nation[i] == "USA"  & total2$judge_nation[i]== "RUS")
  {matchcount1 <- matchcount1 + 1
  matchsum1 <- matchsum1 + total2$standard_score[i]
  }
}


totalsums <- c(rep(NA,10000))
totalmeans <- c(rep(NA,10000))
p_count <- 0
for(i in 1:10000){
  rows <- sample(1:1440, 5)
  sumscore <- 0
  for(j in 1:5){
    sumscore <- sumscore + total2$standard_score[rows[j]]
  }
  totalsums[i] <- sumscore
  totalmeans[i] <- sumscore/5
  if(sumscore/5 > mean(matches$standard_score)){
    p_count <- p_count + 1
  }
}
p_count
p_value <- p_count/10000
p_value
