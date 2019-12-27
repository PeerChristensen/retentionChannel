
library(tidyverse)
library(lubridate)

df <- read_csv("retention_test.csv")

colnames(df) <- c("Customer_Key","Date_Key","Medium","Source","TrafficChannel","End_Date")

# 2 months retention

df <- df %>%
  filter(str_detect(End_Date,"-")) %>%
  mutate(Start_Date = ymd(Date_Key),
         End_Date = as.Date(End_Date)) %>%
  select(-Date_Key) %>%
  mutate(days = End_Date-Start_Date) %>%
  filter(Start_Date < (today() - months(2)))

df %>%
  group_by(TrafficChannel) %>%
  mutate(active2m = case_when(days >= 60 ~ "Active",
                            days < 60 ~ "Inactive")) %>%
  count(active2m) %>%
  pivot_wider(names_from=active2m,values_from = n) %>%
  select(-`NA`) %>%
  drop_na() %>%
  mutate(Prop_Active = Active / (Active + Inactive) * 100) %>%
  ggplot(aes(reorder(TrafficChannel,Prop_Active), Prop_Active)) +
  geom_col() +
  coord_flip() +
  ggtitle("2 måndeders retention rate fordelt på Trafikkanaler")


# mean retention by channel
library(gmodels)

df %>%
 # mutate(first_month = floor_date(Start_Date,"month")) %>%
  group_by(TrafficChannel) %>% 
  filter(End_Date < today()) %>%
  summarise(m  = mean(days),
            lowCI = ci(as.numeric(days))[2],
            hiCI  = ci(as.numeric(days))[3]) %>%
  drop_na() %>%
  ggplot(aes(x=TrafficChannel,y=m)) +
  geom_bar(stat="identity",width=.7) +
  geom_errorbar(aes(ymin=lowCI,ymax=hiCI),width=.2) 



