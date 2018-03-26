library(dplyr)
library(ggplot2)

campaign = read.csv("campaign_data.csv", stringsAsFactors = F)

train = read.csv("train.csv", stringsAsFactors = F)

str(campaign)

table(train$is_click)
table(train$campaign_id)
table(train$user_id)

count_campid = train%>%
  filter(is_click == 1)%>%
    group_by(campaign_id)%>%
    summarise(count = n())%>%
    left_join(campaign, by = "campaign_id")%>%
    arrange(campaign_id)

count_campid_not = train%>%
  filter(is_click == 0)%>%
  group_by(campaign_id)%>%
  summarise(count = n())%>%
  left_join(campaign, by = "campaign_id")%>%
  arrange(campaign_id)

count_campid$count_not = count_campid_not$count

count_campid$total = count_campid$count + count_campid$count_not

count_campid = count_campid%>%
  mutate(Prop = count/total)

rm(count_campid_not)

p = ggplot(count_campid, aes(total, Prop, color = campaign_id)) +
  geom_point()

ggplotly(p)

