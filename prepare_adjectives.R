# Prepare data frame for coding of free form responses (adjectives)

library(tidyverse)

base::load(file = "data/df_sbsa.rda")

df_adjectives <- df_sbsa %>% select(pid, time, rando, starts_with("sb08"), starts_with("sa08")) %>% filter(time==1)

df_adjectives <- df_adjectives %>% mutate(adj1 = ifelse(rando=="Skill-Building", sb08_01, sa08_01),
                                          adj2 = ifelse(rando=="Skill-Building", sb08_02, sa08_02),
                                          adj3 = ifelse(rando=="Skill-Building", sb08_03, sa08_03),
                                          adj4 = ifelse(rando=="Skill-Building", sb08_04, sa08_04),
                                          adj5 = ifelse(rando=="Skill-Building", sb08_05, sa08_05))

df_adjectives <- df_adjectives %>% select(pid, starts_with("adj")) %>% 
  pivot_longer(cols = -pid, names_to = "adj", values_to = "text")

df_adjectives <- df_adjectives %>% filter(!is.na(text)) 

write_csv(df_adjectives, file = "data/df_adjectives.csv")


df_paragraph <- df_sbsa %>% select(pid, time, rando, starts_with("sb14"), starts_with("sa13")) %>% 
  filter(time==2) %>% 
  mutate(open_text = ifelse(rando=="Skill-Building", sb14_01, sa13_01)) %>% 
  select(pid, time, rando, open_text)

write_csv(df_paragraph, file = "data/df_paragraph.csv")
