# Script for Data Cleaning

#### packages ####

library(renv)
library(tidyverse)
library(labelled)
library(psych)
library(GPArotation)
#library(devtools)
#install_github("cran/multicon") # not on CRAN atm
library(multicon)
library(correlation)
library(careless)

### Package / dependency version management with 'renv'
# only ran this to initialize 'renv'
# renv::init()
# renv::snapshot()
# renv::restore() # to revert to the previous state as encoded in the lockfile

#### read in data files ####

# participant data of the two groups
demog_SB <- readr::read_csv("data/prolific_export_skill_building_FU.csv")
demog_SA <- readr::read_csv("data/prolific_export_self_acceptance_FU.csv")

prolific_baseline <- readr::read_csv("data/prolific_export_baseline (SB & SA).csv") # not sure if needed
prolific_approved <- prolific_baseline %>% select("Participant id", Status)
# experimental data 
df_sbsa_all <- readxl::read_excel("data/data_PersonalityChange_2023-03-17_07-51.xlsx", col_names = F, skip = 2, guess_max = 10000)
var_names <- readxl::read_excel("data/data_PersonalityChange_2023-03-17_07-51.xlsx", col_names = F)[c(1,2), ]
var_names <- data.frame(t(var_names))
colnames(df_sbsa_all) <- stringr::str_to_lower(var_names$X1) # only lower case pls
# label data 
df_sbsa_all <- labelled::set_variable_labels(df_sbsa_all, .labels = var_names$X2)

table(df_sbsa_all$questnnr) # rename skill-building condition
df_sbsa_all <- df_sbsa_all %>% mutate(questnnr = ifelse(questnnr=="Skill-Building", "Self-Improvement", questnnr))

#### filter to relevant cases / vars ####

df_sbsa_all %>% 
  filter(str_length(dm01_01)>24) %>% select(case, dm01_01)

df_sbsa_all <- df_sbsa_all %>% 
  filter(dm01_01!="Langeberg Avenue Elandsrand, South Africa") %>% 
  mutate(dm01_01 = if_else(str_length(dm01_01)>24, str_trunc(dm01_01, 24, ellipsis = ""), dm01_01))

df_sbsa <- df_sbsa_all %>% 
  filter(str_length(dm01_01)==24)

df_sbsa <- df_sbsa %>% 
  filter(dm01_01 %in% (prolific_approved %>% filter(Status=="APPROVED") %>% pull("Participant id")))

df_sbsa %>% summarise(n_distinct(dm01_01)) # initial N (before further exclusions)

df_sbsa <- df_sbsa %>% 
  group_by(dm01_01) %>% 
  mutate(assessments = n()) %>% 
  ungroup() # %>% 
  # filter(assessments==2) # only keep those with pre and post data -> keep all for now!

df_sbsa %>% arrange(dm01_01) %>% select(case, dm01_01, questnnr)
table(df_sbsa$questnnr) 

df_sbsa <- df_sbsa %>% mutate(time = 1) %>% mutate(time = ifelse(questnnr!="Test_t0", 2, time))
table(df_sbsa$time) 

df_sbsa <- df_sbsa %>% 
  group_by(dm01_01) %>% 
  mutate(maxtime = max(time)) %>% 
  ungroup()
df_sbsa %>% filter(maxtime==1) %>% select(case, dm01_01, questnnr) %>% arrange(dm01_01) %>% print(n=Inf) # some have done Test_t0 twice (??)

df_sbsa %>% filter(case %in% c(515, 1357)) %>% select(case, dm01_01, questnnr, finished, lastpage, missing)
df_sbsa %>% filter(dm01_01=="5aabf0385e19aa00013c21a5") %>% select(case, dm01_01, questnnr, finished, lastpage, missing) # no T2 unfortunately
df_sbsa <- df_sbsa %>% filter(case!=515)
  
df_sbsa %>% filter(case %in% c(586, 944)) %>% select(case, dm01_01, questnnr, finished, lastpage, missing)
df_sbsa %>% filter(dm01_01=="5dae32c7d0be74001495a36b") %>% select(case, dm01_01, questnnr, finished, lastpage, missing) # no T2 unfortunately
df_sbsa <- df_sbsa %>% filter(case!=586)

df_sbsa %>% filter(case %in% c(914, 1374)) %>% select(case, dm01_01, questnnr, finished, lastpage, missing)
df_sbsa %>% filter(dm01_01=="606f5ff0429f650b3fa0c30d") %>% select(case, dm01_01, questnnr, finished, lastpage, missing) # no T2 unfortunately
df_sbsa <- df_sbsa %>% filter(case!=914)

df_sbsa %>% filter(case %in% c(307, 1092)) %>% select(case, dm01_01, questnnr, finished, lastpage, missing)
df_sbsa %>% filter(dm01_01=="60cb4f111e27b7ab3656e188") %>% select(case, dm01_01, questnnr, finished, lastpage, missing) # no T2 unfortunately
df_sbsa <- df_sbsa %>% filter(case!=307)

df_sbsa %>% filter(case %in% c(663, 1267)) %>% select(case, dm01_01, questnnr, finished, lastpage, missing)
df_sbsa %>% filter(dm01_01=="6170156ad4205a936a1a4773") %>% select(case, dm01_01, questnnr, finished, lastpage, missing) # no T2 unfortunately
df_sbsa <- df_sbsa %>% filter(case!=663)

# still more than 2 obs per person?
df_sbsa <- df_sbsa %>% group_by(dm01_01) %>% mutate(rows = n()) %>% ungroup()
df_sbsa %>% arrange(dm01_01, questnnr) %>% select(case, dm01_01, questnnr, finished, lastpage, missing, rows) %>% filter(rows>2) %>% print(n=300)
df_sbsa <- df_sbsa %>% group_by(dm01_01, questnnr) %>% mutate(rows_q = n()) %>% ungroup()
df_sbsa %>% arrange(dm01_01, questnnr) %>% select(case, dm01_01, questnnr, finished, lastpage, missing, rows, rows_q) %>% filter(rows_q>1) %>% print(n=300)
# most cases are clear: one questionnaire entry was aborted
df_sbsa <- df_sbsa %>% filter(!(rows_q>1 & finished==0)) # 74 dropped 
df_sbsa <- df_sbsa %>% group_by(dm01_01, questnnr) %>% mutate(rows_q = n()) %>% ungroup()
df_sbsa %>% arrange(dm01_01, questnnr) %>% select(case, dm01_01, questnnr, finished, lastpage, missing, rows, rows_q) %>% filter(rows_q>1)
# two remaining cases -> take the more complete looking one (or the first if similar)
df_sbsa <- df_sbsa %>% filter(case!=2084)
df_sbsa <- df_sbsa %>% filter(case!=1128)
df_sbsa <- df_sbsa %>% group_by(dm01_01) %>% mutate(rows = n()) %>% ungroup()
table(df_sbsa$rows) # no more remaining duplicate entries
df_sbsa %>% filter(rows==1 & questnnr!="Test_t0") %>% select(case, dm01_01, questnnr, finished, lastpage, missing, rows, rows_q)

# now we have 3 cases without baseline -> this is because they did not finish T1
table(df_sbsa$rows, df_sbsa$questnnr)
df_sbsa_all %>% filter(dm01_01 %in% (df_sbsa %>% filter(rows==1 & questnnr!="Test_t0") %>% pull(dm01_01))) %>% 
  select(case, dm01_01, questnnr, finished, lastpage, missing) %>% arrange(dm01_01, questnnr)
# for one case (1711) we don't have any T1 data at all -> not sure how this works -> leave in the data for now (FIML)
# for the other two we merge the (incomplete) T1 data again 
df_sbsa <- df_sbsa %>% bind_rows(df_sbsa_all %>% filter(case %in% c(370, 628)) %>% mutate(time=1))
df_sbsa <- df_sbsa %>% group_by(dm01_01) %>% mutate(rows = n()) %>% ungroup()
table(df_sbsa$rows, df_sbsa$questnnr) # looks good

df_sbsa <- df_sbsa %>% 
  mutate(rando = questnnr) %>% 
  mutate(rando = ifelse(rando=="Test_t0", NA, rando)) %>% 
  arrange(dm01_01, time) %>% 
  group_by(dm01_01) %>% 
  tidyr::fill(rando, .direction = "up") %>% 
  ungroup()

# this did not work for those who only filled out T1 -> adding by random variable "zf01"
df_sbsa <- df_sbsa %>% 
  mutate(rando = ifelse(is.na(rando) & zf01==1, "Self-Improvement", rando)) %>% 
  mutate(rando = ifelse(is.na(rando) & zf01==2, "Self-Acceptance", rando))

table(df_sbsa$time, df_sbsa$rando)
df_sbsa %>% group_by(rando, zf01) %>% tally() # check that this fits with our "fill" method above
# two "misfits":
df_sbsa %>% filter(rando=="Self-Acceptance" & zf01==1) %>% print(width = Inf) # -> self-improvement questions
df_sbsa %>% filter(dm01_01=="5e738bf88934222f183472e5") %>% select(case, dm01_01, questnnr, time, rando) 
df_sbsa %>% filter(rando=="Self-Improvement" & zf01==2) %>% print(width = Inf) # -> neither self-improvement nor self-acceptance questions (aborted at p. 4)
df_sbsa %>% filter(dm01_01=="616aacaa8686a08022edd852") %>% select(case, dm01_01, questnnr, time, rando) 
# sth must have gone wrong with the assignment (this is because they had multiple entries at T1 and then the randomization switched and they were invited to the wrong group at T2)
# 1st case must go (drop T2) but 2nd case can stay because the relevant questions at T1 are missing, anyways
df_sbsa <- df_sbsa %>% filter(!(dm01_01=="5e738bf88934222f183472e5" & time==2))
df_sbsa <- df_sbsa %>% mutate(rando = ifelse(dm01_01=="5e738bf88934222f183472e5", "Self-Improvement", rando))

df_sbsa %>% select(case, dm01_01, questnnr, time, rando) %>% print(n=50)

# sort & generate running ID
df_sbsa <- df_sbsa %>% arrange(dm01_01, time) %>% group_by(dm01_01) %>% mutate(pid = cur_group_id()) %>% ungroup()

#### checks for careless responding ####

##### longstring #####

# 10 item Meaning in Life Questionnaire 
longstr_meaning <- careless::longstring(df_sbsa %>% select(starts_with("ml01")))
#boxplot(longstr_meaning, main = "Boxplot of Longstring index")
table(longstr_meaning)
longstr_meaning_score <- df_sbsa[which(longstr_meaning > 7), c("pid")]
longstr_meaning_score$longstr_meaning_flag <- 1
longstr_meaning_score <- longstr_meaning_score %>% group_by(pid) %>% summarise(longstr_meaning_flag = n())
  
# 10 item Rosenberg Self-Esteem Scale
longstr_selfest <- careless::longstring(df_sbsa %>% select(starts_with("rs01")))
#boxplot(longstr_selfest, main = "Boxplot of Longstring index")
table(longstr_selfest)
longstr_selfest_score <- df_sbsa[which(longstr_selfest > 7), c("pid")]
longstr_selfest_score$longstr_selfest_flag <- 1
longstr_selfest_score <- longstr_selfest_score %>% group_by(pid) %>% summarise(longstr_selfest_flag = n())

# 12 item Self Concept Clarity Scale
longstr_concept <- careless::longstring(df_sbsa %>% select(starts_with("sc01")))
#boxplot(longstr_concept, main = "Boxplot of Longstring index")
#boxplot(longstr_selfest, main = "Boxplot of Longstring index")
table(longstr_concept)
summary(longstr_concept)
longstr_concept_score <- df_sbsa[which(longstr_concept > 8), c("pid")]
longstr_concept_score$longstr_concept_flag <- 1
longstr_concept_score <- longstr_concept_score %>% group_by(pid) %>% summarise(longstr_concept_flag = n())

# 60 item BFI-2, but in different sections
longstr_bficurrpre <- df_sbsa %>% filter(time==1) %>% select(pid, starts_with("bf01")) %>% 
  mutate(longstr_bficurrpre = careless::longstring(.)) 
summary(longstr_bficurrpre$longstr_bficurrpre)
table(longstr_bficurrpre$longstr_bficurrpre)
longstr_bficurrpre_score <- longstr_bficurrpre %>% 
  filter(longstr_bficurrpre > 7) %>% mutate(longstr_bficurrpre_flag = 1) %>% select(pid, longstr_bficurrpre_flag)

longstr_bfiidealpre <- df_sbsa %>% filter(time==1) %>% select(pid, starts_with("bf02")) %>% 
  mutate(longstr_bfiidealpre = careless::longstring(.)) 
summary(longstr_bfiidealpre$longstr_bfiidealpre)
table(longstr_bfiidealpre$longstr_bfiidealpre)
longstr_bfiidealpre_score <- longstr_bfiidealpre %>% 
  filter(longstr_bfiidealpre > 7) %>% mutate(longstr_bfiidealpre_flag = 1) %>% select(pid, longstr_bfiidealpre_flag)

longstr_bficurrpost <- df_sbsa %>% filter(time==2) %>% select(pid, starts_with("bf03")) %>% 
  mutate(longstr_bficurrpost = careless::longstring(.)) 
summary(longstr_bficurrpost$longstr_bficurrpost)
table(longstr_bficurrpost$longstr_bficurrpost)
longstr_bficurrpost_score <- longstr_bficurrpost %>% 
  filter(longstr_bficurrpost > 7) %>% mutate(longstr_bficurrpost_flag = 1) %>% select(pid, longstr_bficurrpost_flag)

longstr_bfiidealpost <- df_sbsa %>% filter(time==2) %>% select(pid, starts_with("bf04")) %>% 
  mutate(longstr_bfiidealpost = careless::longstring(.)) 
summary(longstr_bfiidealpost$longstr_bfiidealpost)
table(longstr_bfiidealpost$longstr_bfiidealpost)
longstr_bfiidealpost_score <- longstr_bfiidealpost %>% 
  filter(longstr_bfiidealpost > 7) %>% mutate(longstr_bfiidealpost_flag = 1) %>% select(pid, longstr_bfiidealpost_flag)

# 15 BFI facets - pre - self improvement
longstr_facetscurrpre <- df_sbsa %>% filter(time==1 & rando=="Self-Improvement") %>% select(pid, starts_with("sb07")) %>% 
  mutate(longstr_facetscurrpre = careless::longstring(.)) 
summary(longstr_facetscurrpre$longstr_facetscurrpre)
table(longstr_facetscurrpre$longstr_facetscurrpre)
longstr_facetscurrpre_score <- longstr_facetscurrpre %>% 
  filter(longstr_facetscurrpre > 7) %>% mutate(longstr_facetscurrpre_flag = 1) %>% select(pid, longstr_facetscurrpre_flag)

longstr_facetsidealpre <- df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(pid, starts_with("sa07")) %>% 
  mutate(longstr_facetsidealpre = careless::longstring(.)) 
summary(longstr_facetsidealpre$longstr_facetsidealpre)
table(longstr_facetsidealpre$longstr_facetsidealpre)
longstr_facetsidealpre_score <- longstr_facetsidealpre %>% 
  filter(longstr_facetsidealpre > 7) %>% mutate(longstr_facetsidealpre_flag = 1) %>% select(pid, longstr_facetsidealpre_flag)

longstr_facetscurrpost <- df_sbsa %>% filter(time==2 & rando=="Self-Improvement") %>% select(pid, starts_with("sb12")) %>% 
  mutate(longstr_facetscurrpost = careless::longstring(.)) 
summary(longstr_facetscurrpost$longstr_facetscurrpost)
table(longstr_facetscurrpost$longstr_facetscurrpost)
longstr_facetscurrpost_score <- longstr_facetscurrpost %>% 
  filter(longstr_facetscurrpost > 9) %>% mutate(longstr_facetscurrpost_flag = 1) %>% select(pid, longstr_facetscurrpost_flag)

longstr_facetsidealpost <- df_sbsa %>% filter(time==2 & rando=="Self-Acceptance") %>% select(pid, starts_with("sa14")) %>% 
  mutate(longstr_facetsidealpost = careless::longstring(.)) 
summary(longstr_facetsidealpost$longstr_facetsidealpost)
table(longstr_facetsidealpost$longstr_facetsidealpost)
longstr_facetsidealpost_score <- longstr_facetsidealpost %>% 
  filter(longstr_facetsidealpost > 9) %>% mutate(longstr_facetsidealpost_flag = 1) %>% select(pid, longstr_facetsidealpost_flag)

longstr_scores <- full_join(longstr_meaning_score, longstr_selfest_score) %>% 
  full_join(longstr_concept_score) %>% 
  full_join(longstr_bficurrpre_score) %>% full_join(longstr_bfiidealpre_score) %>% 
  full_join(longstr_bficurrpost_score) %>% full_join(longstr_bfiidealpost_score) %>% 
  full_join(longstr_facetscurrpre_score) %>% full_join(longstr_facetsidealpre_score) %>% 
  full_join(longstr_facetscurrpost_score) %>% full_join(longstr_facetsidealpost_score) %>% 
  mutate(longstr_flag_sum = rowSums(across(ends_with("_flag")), na.rm=T))

##### mahalanobis distance ##### 

# 10 item Meaning in Life Questionnaire 
mahad_meaning <- careless::mahad(df_sbsa %>% select(starts_with("ml01")), plot = FALSE)
summary(mahad_meaning)
mahad_meaning_score <- df_sbsa[which(mahad_meaning > mean(mahad_meaning, na.rm=T)+3*sd(mahad_meaning, na.rm=T)), c("pid")]
mahad_meaning_score$mahad_meaning_flag <- 1
mahad_meaning_score <- mahad_meaning_score %>% group_by(pid) %>% summarise(mahad_meaning_flag = n())

# 10 item Rosenberg Self-Esteem Scale
mahad_selfest <- careless::mahad(df_sbsa %>% select(starts_with("rs01")), plot = FALSE)
summary(mahad_selfest)
mahad_selfest_score <- df_sbsa[which(mahad_selfest > mean(mahad_selfest, na.rm=T)+3*sd(mahad_selfest, na.rm=T)), c("pid")]
mahad_selfest_score$mahad_selfest_flag <- 1
mahad_selfest_score <- mahad_selfest_score %>% group_by(pid) %>% summarise(mahad_selfest_flag = n())

# 12 item Self Concept Clarity Scale
mahad_concept <- careless::mahad(df_sbsa %>% select(starts_with("sc01")), plot = FALSE)
summary(mahad_concept)
mahad_concept_score <- df_sbsa[which(mahad_concept > mean(mahad_concept, na.rm=T)+3*sd(mahad_concept, na.rm=T)), c("pid")]
mahad_concept_score$mahad_concept_flag <- 1
mahad_concept_score <- mahad_concept_score %>% group_by(pid) %>% summarise(mahad_concept_flag = n())

# 60 item BFI-2, but in different sections
mahad_bficurrpre <- df_sbsa %>% filter(time==1) %>% select(pid, starts_with("bf01")) %>% 
  mutate(mahad_bficurrpre = careless::mahad(., plot = FALSE)) 
summary(mahad_bficurrpre$mahad_bficurrpre)
mahad_bficurrpre_score <- mahad_bficurrpre %>% 
  filter(mahad_bficurrpre > mean(mahad_bficurrpre, na.rm=T)+3*sd(mahad_bficurrpre, na.rm=T)) %>% 
  mutate(mahad_bficurrpre_flag = 1) %>% select(pid, mahad_bficurrpre_flag)

mahad_bfiidealpre <- df_sbsa %>% filter(time==1) %>% select(pid, starts_with("bf02")) %>% 
  mutate(mahad_bfiidealpre = careless::mahad(., plot = FALSE)) 
summary(mahad_bfiidealpre$mahad_bfiidealpre)
mahad_bfiidealpre_score <- mahad_bfiidealpre %>% 
  filter(mahad_bfiidealpre > mean(mahad_bfiidealpre, na.rm=T)+3*sd(mahad_bfiidealpre, na.rm=T)) %>% 
  mutate(mahad_bfiidealpre_flag = 1) %>% select(pid, mahad_bfiidealpre_flag)

mahad_bficurrpost <- df_sbsa %>% filter(time==2) %>% select(pid, starts_with("bf03")) %>% 
  mutate(mahad_bficurrpost = careless::mahad(., plot = FALSE)) 
summary(mahad_bficurrpost$mahad_bficurrpost)
mahad_bficurrpost_score <- mahad_bficurrpost %>% 
  filter(mahad_bficurrpost > mean(mahad_bficurrpost, na.rm=T)+3*sd(mahad_bficurrpost, na.rm=T)) %>% 
  mutate(mahad_bficurrpost_flag = 1) %>% select(pid, mahad_bficurrpost_flag)

mahad_bfiidealpost <- df_sbsa %>% filter(time==2) %>% select(pid, starts_with("bf04")) %>% 
  mutate(mahad_bfiidealpost = careless::mahad(., plot = FALSE)) 
summary(mahad_bfiidealpost$mahad_bfiidealpost)
mahad_bfiidealpost_score <- mahad_bfiidealpost %>% 
  filter(mahad_bfiidealpost > mean(mahad_bfiidealpost, na.rm=T)+3*sd(mahad_bfiidealpost, na.rm=T)) %>% 
  mutate(mahad_bfiidealpost_flag = 1) %>% select(pid, mahad_bfiidealpost_flag)

# 15 BFI facets - pre - self improvement
mahad_facetscurrpre <- df_sbsa %>% filter(time==1 & rando=="Self-Improvement") %>% select(pid, starts_with("sb07")) %>% 
  mutate(mahad_facetscurrpre = careless::mahad(., plot = FALSE)) 
summary(mahad_facetscurrpre$mahad_facetscurrpre)
mahad_facetscurrpre_score <- mahad_facetscurrpre %>% 
  filter(mahad_facetscurrpre > mean(mahad_facetscurrpre, na.rm=T)+3*sd(mahad_facetscurrpre, na.rm=T)) %>% 
  mutate(mahad_facetscurrpre_flag = 1) %>% select(pid, mahad_facetscurrpre_flag)

mahad_facetsidealpre <- df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(pid, starts_with("sa07")) %>% 
  mutate(mahad_facetsidealpre = careless::mahad(., plot = FALSE)) 
summary(mahad_facetsidealpre$mahad_facetsidealpre)
mahad_facetsidealpre_score <- mahad_facetsidealpre %>% 
  filter(mahad_facetsidealpre > mean(mahad_facetsidealpre, na.rm=T)+3*sd(mahad_facetsidealpre, na.rm=T)) %>% 
  mutate(mahad_facetsidealpre_flag = 1) %>% select(pid, mahad_facetsidealpre_flag)

mahad_facetscurrpost <- df_sbsa %>% filter(time==2 & rando=="Self-Improvement") %>% select(pid, starts_with("sb12")) %>% 
  mutate(mahad_facetscurrpost = careless::mahad(., plot = FALSE)) 
summary(mahad_facetscurrpost$mahad_facetscurrpost)
mahad_facetscurrpost_score <- mahad_facetscurrpost %>% 
  filter(mahad_facetscurrpost > mean(mahad_facetscurrpost, na.rm=T)+3*sd(mahad_facetscurrpost, na.rm=T)) %>% 
  mutate(mahad_facetscurrpost_flag = 1) %>% select(pid, mahad_facetscurrpost_flag)

mahad_facetsidealpost <- df_sbsa %>% filter(time==2 & rando=="Self-Acceptance") %>% select(pid, starts_with("sa14")) %>% 
  mutate(mahad_facetsidealpost = careless::mahad(., plot = FALSE)) 
summary(mahad_facetsidealpost$mahad_facetsidealpost)
mahad_facetsidealpost_score <- mahad_facetsidealpost %>% 
  filter(mahad_facetsidealpost > mean(mahad_facetsidealpost, na.rm=T)+3*sd(mahad_facetsidealpost, na.rm=T)) %>% 
  mutate(mahad_facetsidealpost_flag = 1) %>% select(pid, mahad_facetsidealpost_flag)

mahad_scores <- full_join(mahad_meaning_score, mahad_selfest_score) %>% 
  full_join(mahad_concept_score) %>% 
  full_join(mahad_bficurrpre_score) %>% full_join(mahad_bfiidealpre_score) %>% 
  full_join(mahad_bficurrpost_score) %>% full_join(mahad_bfiidealpost_score) %>% 
  full_join(mahad_facetscurrpre_score) %>% full_join(mahad_facetsidealpre_score) %>% 
  full_join(mahad_facetscurrpost_score) %>% full_join(mahad_facetsidealpost_score) %>% 
  mutate(mahad_flag_sum = rowSums(across(ends_with("_flag")), na.rm=T))

##### completion times ##### 
summary(df_sbsa$time_sum)
hist(df_sbsa$time_sum)

summary(df_sbsa$missing)
hist(df_sbsa$missing)

##### exclude based on these criteria ##### 

longstr_scores %>% filter(longstr_flag_sum>2) %>% arrange(pid) %>% pull(pid) # with >1 it would be too many cases...

df_sbsa <- df_sbsa %>% # 24 pid's excluded
  filter(!pid %in% (longstr_scores %>% filter(longstr_flag_sum>2) %>% pull(pid)))
  
mahad_scores %>% filter(mahad_flag_sum>2) %>% arrange(pid) %>% pull(pid) # After visual inspection, I don't think these are necessarily suspicious
# df_sbsa %>% filter(pid %in% (mahad_scores %>% filter(mahad_flag_sum>2) %>% arrange(pid) %>% pull(pid))) %>% print(width = Inf)

df_sbsa %>% filter(time_sum < 300) %>% arrange(pid) %>% pull(pid)

df_sbsa <- df_sbsa %>% # 1 additionally excluded
  filter(time_sum > 301)

df_sbsa %>% filter(missing > 10) %>% arrange(pid) %>% pull(pid) # too many missings -> can be dropped

df_sbsa <- df_sbsa %>% # 1 additionally excluded
  filter(missing < 10)

df_sbsa %>% summarise(n_distinct(dm01_01)) # N (after exclusions)

#### inspect / recode variables ####

# 5 item Satisfaction with Life Scale (Diener et al. 1985) 
df_sbsa %>% select(starts_with("sw06"))
df_sbsa %>% group_by(time, rando, sw06_01) %>% tally() %>% print(n=Inf)
alpha.swls <- df_sbsa %>% 
  select(starts_with("sw06")) %>%
  psych::alpha(check.keys = TRUE)
omega.swls <- df_sbsa %>%
  dplyr::select(starts_with("sw06")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(alpha.swls$keys[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=2) # 3 returns error message
df_sbsa$swls <- df_sbsa %>%
  select(starts_with("sw06")) %>%
  psych::reverse.code(keys=alpha.swls$keys[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa$swls[is.nan(df_sbsa$swls)] <- NA

# 10 item Meaning in Life Questionnaire (Steger et al., 2006)
df_sbsa %>% select(starts_with("ml01"))
df_sbsa %>% group_by(time, rando, ml01_01) %>% tally() %>% print(n=Inf)
keys_meaning <- list(meaning = c("ml01_01", "-ml01_02", "-ml01_03", "ml01_04", "ml01_05", "ml01_06", "-ml01_07", "-ml01_08", "-ml01_09", "-ml01_10")) # does not work automatically, here
alpha.meaning <- df_sbsa %>% 
  select(starts_with("ml01")) %>%
  psych::alpha(keys = keys_meaning)
omega.meaning <- df_sbsa %>%
  dplyr::select(starts_with("ml01")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(keys_meaning[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=3)
df_sbsa$meaning <- df_sbsa %>%
  select(starts_with("ml01")) %>%
  psych::reverse.code(keys = keys_meaning[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa$meaning[is.nan(df_sbsa$meaning)] <- NA

# 10 item Rosenberg Self-Esteem Scale (Rosenberg et al., 1965) 
df_sbsa %>% select(starts_with("rs01"))
df_sbsa %>% group_by(time, rando, rs01_01) %>% tally() %>% print(n=Inf)
keys_selfes <- list(meaning = c("rs01_01", "-rs01_02", "rs01_03", "rs01_04", "-rs01_05", "-rs01_06", "rs01_07", "-rs01_08", "-rs01_09", "rs01_10")) # does not work automatically, here
alpha.selfes <- df_sbsa %>% 
  select(starts_with("rs01")) %>%
  psych::alpha(keys = keys_selfes)
omega.selfes <- df_sbsa %>%
  dplyr::select(starts_with("rs01")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(keys_selfes[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=2) # 3 returns error message
df_sbsa$selfes <- df_sbsa %>%
  select(starts_with("rs01")) %>%
  psych::reverse.code(keys=keys_selfes[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa$selfes[is.nan(df_sbsa$selfes)] <- NA

# 12 item Self Concept Clarity Scale (Campbell et al., 1996)
df_sbsa %>% select(starts_with("sc01"))
df_sbsa %>% group_by(time, rando, sc01_01) %>% tally() %>% print(n=Inf)
keys_concept <- list(concept = c("-sc01_01", "-sc01_02", "-sc01_03", "-sc01_04", "-sc01_05", "sc01_06", 
                                 "-sc01_07", "-sc01_08", "-sc01_09", "-sc01_10", "sc01_11", "-sc01_12")) # does not work automatically, here
alpha.concept <- df_sbsa %>% 
  select(starts_with("sc01")) %>%
  psych::alpha(keys = keys_concept)
omega.concept <- df_sbsa %>%
  dplyr::select(starts_with("sc01")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(keys_concept[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=3)
df_sbsa$concept <- df_sbsa %>%
  select(starts_with("sc01")) %>%
  psych::reverse.code(keys = keys_concept[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa$concept[is.nan(df_sbsa$concept)] <- NA

# 60 item BFI-2, but in different sections
df_sbsa %>% group_by(time, rando, bf01_01) %>% tally() %>% print(n=Inf) # pre
df_sbsa %>% group_by(time, rando, bf02_01) %>% tally() %>% print(n=Inf) # pre -> ideal trait level (?)

df_sbsa %>% group_by(time, rando, bf03_01) %>% tally() %>% print(n=Inf) # post
df_sbsa %>% group_by(time, rando, bf04_01) %>% tally() %>% print(n=Inf) # post -> ideal trait level (?)

### BFI-2 - Traits
b5t_extraversion = c("_01", "_06", "_11", "_16", "_21", "_26", 
                     "_31", "_36", "_41", "_46", "_51", "_56")
b5t_agreeableness = c("_02", "_07", "_12", "_17", "_22", "_27", 
                      "_32", "_37", "_42", "_47", "_52", "_57")
b5t_conscientiousness = c("_03", "_08", "_13", "_18", "_23", "_28", 
                          "_33", "_38", "_43", "_48", "_53", "_58")
b5t_neuroticism = c("_04", "_09", "_14", "_19", "_24", "_29", 
                    "_34", "_39", "_44", "_49", "_54", "_59")
b5t_openness = c("_05", "_10", "_15", "_20", "_25", "_30", 
                 "_35", "_40", "_45", "_50", "_55", "_60")
### BFI-2 - Facets
# facets of extraversion
b5f_sociability = c("_01", "_16", "_31", "_46") 
b5f_assertiveness = c("_06", "_21", "_36", "_51")
b5f_energy = c("_11", "_26", "_41", "_56")
# facets of agreeableness
b5f_compassion = c("_02", "_17", "_32", "_47")
b5f_respectfulness = c("_07", "_22", "_37", "_52")
b5f_trust = c("_12", "_27", "_42", "_57")
# facets of conscientiousness
b5f_organization = c("_03", "_18", "_33", "_48")
b5f_productiveness = c("_08", "_23", "_38", "_53")
b5f_responsibility = c("_13", "_28", "_43", "_58")
# facets of neuroticism
b5f_anxiety = c("_04", "_19", "_34", "_49")
b5f_depression = c("_09", "_24", "_39", "_54")
b5f_volatility = c("_14", "_29", "_44", "_59")
# facets of openness
b5f_curiosity = c("_10", "_25", "_40", "_55")
b5f_aesthetic = c("_05", "_20", "_35", "_50")
b5f_imagination = c("_15", "_30", "_45", "_60")

# add keys list (to indicate reverse scoring) - unfortunately this does not work (for the facets) automatically with psych function
keys_extraversion = c("+", "+", "-", "-", "+", "-", "-", "-", "+", "+", "-", "+")
keys_agreeableness = c("+", "+", "-", "-", "-", "+", "+", "-", "-", "-", "+", "+")
keys_conscientiousness = c("-", "-", "+", "+", "-", "-", "+", "+", "+", "-", "+", "-")
keys_neuroticism = c("-", "-", "+", "+", "-", "-", "+", "+", "-", "-", "+", "+")
keys_openness = c("-", "+", "+", "+", "-", "-", "+", "+", "-", "-", "-", "+")
# facets of extraversion
keys_sociability = c("+", "-", "-", "+") 
keys_assertiveness = c("+", "+", "-", "-")
keys_energy = c("-", "-", "+", "+")
# facets of agreeableness
keys_compassion = c("+", "-", "+", "-")
keys_respectfulness = c("+", "-", "-", "+")
keys_trust = c("-", "+", "-", "+")
# facets of conscientiousness
keys_organization = c("-", "+", "+", "-")
keys_productiveness = c("-", "-", "+", "+")
keys_responsibility = c("+", "-", "+", "-")
# facets of neuroticism
keys_anxiety = c("-", "+", "+", "-")
keys_depression = c("-", "-", "+", "+")
keys_volatility = c("+", "-", "-", "+")
# facets of openness
keys_curiosity = c("+", "-", "+", "-")
keys_aesthetic = c("-", "+", "+", "-")
keys_imagination = c("+", "-", "-", "+")

b5_vars <- list(list(b5t_extraversion, keys_extraversion), 
                list(b5t_agreeableness, keys_agreeableness), 
                list(b5t_conscientiousness, keys_conscientiousness), 
                list(b5t_neuroticism, keys_neuroticism), 
                list(b5t_openness, keys_openness),
                list(b5f_sociability, keys_sociability), 
                list(b5f_assertiveness, keys_assertiveness), 
                list(b5f_energy, keys_energy), 
                list(b5f_compassion, keys_compassion), 
                list(b5f_respectfulness, keys_respectfulness), 
                list(b5f_trust, keys_trust),
                list(b5f_organization, keys_organization), 
                list(b5f_productiveness, keys_productiveness), 
                list(b5f_responsibility, keys_responsibility),
                list(b5f_anxiety, keys_anxiety), 
                list(b5f_depression, keys_depression), 
                list(b5f_volatility, keys_volatility),
                list(b5f_curiosity, keys_curiosity), 
                list(b5f_aesthetic, keys_aesthetic), 
                list(b5f_imagination, keys_imagination))
names(b5_vars) = c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness",
                   "sociability", "assertiveness", "energy", 
                   "compassion", "respectfulness", "trust",
                   "organization", "productiveness", "responsibility",
                   "anxiety", "depression", "volatility",
                   "curiosity", "aesthetic", "imagination")

bfi_versions <- list("bf01", "bf02", "bf03", "bf04",
                     "bf05", "bf06") # last 2 are created below
names(bfi_versions) <- c("pre_curr", "pre_ideal", "post_curr", "post_ideal",
                         "comb_curr", "comb_ideal") # last 2 are created below

# create combined variables across the two assessments (but separate for current "05" / ideal "06")
all_bfi_items = paste0("_", str_pad(1:60, 2, pad = "0"))

for (i in 1:length(all_bfi_items)) {
    for (j in 1:2) { # for current and ideal
    # start with pre
    df_sbsa[, paste0(bfi_versions[[j+4]], all_bfi_items[i])] = 
      rowSums(df_sbsa[, c(paste0(bfi_versions[[j]], all_bfi_items[i]),
                          paste0(bfi_versions[[j+2]], all_bfi_items[i]))], na.rm = T)
    # attention: creates 0 if both items NA
    df_sbsa <- df_sbsa %>% mutate_at(c(paste0(bfi_versions[[j+4]], all_bfi_items[i])), ~na_if(., 0))
  }
}

# now score traits and compute reliabilities
for (i in 1:length(b5_vars)) {
  # loop across 5 traits AND 15 facets
  item_nrs = b5_vars[[i]][[1]]
  short_name = str_trunc(names(b5_vars)[i], 5, ellipsis = "")
  # loop across 6 BFI versions
  for (j in 1:length(bfi_versions)) {
    items = paste0(bfi_versions[[j]], item_nrs)
    items_keyed = paste0(b5_vars[[i]][[2]], items)
    items_keyed = ifelse(str_detect(items_keyed, "-"), -1, 1) # apparently, the function now requires the 1/-1 coding
    # reliability objects
    alpha <- df_sbsa %>% 
      select(all_of(items)) %>%
      psych::alpha(keys = items_keyed)
    eval(call("<-", as.name(paste0("alpha_", short_name, "_", names(bfi_versions)[j])), alpha)) # save to environment
    if (i %in% c(1:5)) { # omega only for B5 traits
      omega <- df_sbsa %>% 
        select(all_of(items)) %>%
        psych::omega(m = ., key = items_keyed, plot = FALSE, nfactors=3)
      eval(call("<-", as.name(paste0("omega_", short_name, "_", names(bfi_versions)[j])), omega))
    }
    # mean scores
    trait = df_sbsa %>%
      select(all_of(items)) %>%
      psych::reverse.code(keys = items_keyed, items = .) %>%
      rowMeans(na.rm=T)
    trait[is.nan(trait)] <- NA
    df_sbsa[, paste0(short_name, "_", names(bfi_versions)[j])] <- trait # add to df
  }
}  


# put all measures of internal consistency in one table (for supplement)
int_consist_traits_st1 <- tibble(
  trait = c("lifesat", "meaning", "selfes", "concept", str_trunc(names(b5_vars), 5, ellipsis = "")[1:5]),
  rel_alpha_current = c(# well-being 
    alpha.swls$total$raw_alpha, alpha.meaning$total$raw_alpha, 
    alpha.selfes$total$raw_alpha, alpha.concept$total$raw_alpha, 
    # bfi dimensions current 
    alpha_extra_comb_curr$total$raw_alpha, alpha_agree_comb_curr$total$raw_alpha, 
    alpha_consc_comb_curr$total$raw_alpha, alpha_neuro_comb_curr$total$raw_alpha, 
    alpha_openn_comb_curr$total$raw_alpha), 
  rel_alpha_ideal = c(# well-being 
    NA, NA, NA, NA,
    # bfi dimensions ideal 
    alpha_extra_comb_ideal$total$raw_alpha, alpha_agree_comb_ideal$total$raw_alpha, 
    alpha_consc_comb_ideal$total$raw_alpha, alpha_neuro_comb_ideal$total$raw_alpha, 
    alpha_openn_comb_ideal$total$raw_alpha),
  rel_omega_t_current = c(# well-being 
    omega.swls$omega.tot, omega.meaning$omega.tot, 
    omega.selfes$omega.tot, omega.concept$omega.tot, 
    # bfi dimensions current 
    omega_extra_comb_curr$omega.tot, omega_agree_comb_curr$omega.tot, 
    omega_consc_comb_curr$omega.tot, omega_neuro_comb_curr$omega.tot, 
    omega_openn_comb_curr$omega.tot), 
  rel_omega_t_ideal = c(# well-being 
    NA, NA, NA, NA,
    # bfi dimensions ideal 
    omega_extra_comb_ideal$omega.tot, omega_agree_comb_ideal$omega.tot, 
    omega_consc_comb_ideal$omega.tot, omega_neuro_comb_ideal$omega.tot, 
    omega_openn_comb_ideal$omega.tot),
  rel_omega_h_current = c(# well-being 
    omega.swls$omega_h, omega.meaning$omega_h, 
    omega.selfes$omega_h, omega.concept$omega_h, 
    # bfi dimensions current 
    omega_extra_comb_curr$omega_h, omega_agree_comb_curr$omega_h, 
    omega_consc_comb_curr$omega_h, omega_neuro_comb_curr$omega_h, 
    omega_openn_comb_curr$omega_h), 
  rel_omega_h_ideal = c(# well-being 
    NA, NA, NA, NA,
    # bfi dimensions ideal 
    omega_extra_comb_ideal$omega_h, omega_agree_comb_ideal$omega_h, 
    omega_consc_comb_ideal$omega_h, omega_neuro_comb_ideal$omega_h, 
    omega_openn_comb_ideal$omega_h))

int_consist_facets_st1 <- tibble(
  facet = str_trunc(names(b5_vars), 5, ellipsis = "")[6:20],
  rel_alpha_current = c(
    # bfi facets current 
    alpha_socia_comb_curr$total$raw_alpha, alpha_asser_comb_curr$total$raw_alpha, 
    alpha_energ_comb_curr$total$raw_alpha, alpha_compa_comb_curr$total$raw_alpha, 
    alpha_respe_comb_curr$total$raw_alpha, alpha_trust_comb_curr$total$raw_alpha, 
    alpha_organ_comb_curr$total$raw_alpha, alpha_produ_comb_curr$total$raw_alpha, 
    alpha_respo_comb_curr$total$raw_alpha, alpha_anxie_comb_curr$total$raw_alpha, 
    alpha_volat_comb_curr$total$raw_alpha, alpha_depre_comb_curr$total$raw_alpha, 
    alpha_curio_comb_curr$total$raw_alpha, alpha_aesth_comb_curr$total$raw_alpha, 
    alpha_imagi_comb_curr$total$raw_alpha), 
  rel_alpha_ideal = c(
    # bfi facets ideal 
    alpha_socia_comb_ideal$total$raw_alpha, alpha_asser_comb_ideal$total$raw_alpha, 
    alpha_energ_comb_ideal$total$raw_alpha, alpha_compa_comb_ideal$total$raw_alpha, 
    alpha_respe_comb_ideal$total$raw_alpha, alpha_trust_comb_ideal$total$raw_alpha, 
    alpha_organ_comb_ideal$total$raw_alpha, alpha_produ_comb_ideal$total$raw_alpha, 
    alpha_respo_comb_ideal$total$raw_alpha, alpha_anxie_comb_ideal$total$raw_alpha, 
    alpha_volat_comb_ideal$total$raw_alpha, alpha_depre_comb_ideal$total$raw_alpha, 
    alpha_curio_comb_ideal$total$raw_alpha, alpha_aesth_comb_ideal$total$raw_alpha, 
    alpha_imagi_comb_ideal$total$raw_alpha))

# squared difference between current and ideal self
for (i in 1:length(b5_vars)) {
  # loop across 5 traits AND 15 facets
  short_name = str_trunc(names(b5_vars)[i], 5, ellipsis = "")
  current = df_sbsa[, paste0(short_name, "_comb_curr")]
  ideal = df_sbsa[, paste0(short_name, "_comb_ideal")]
  sqdiff = (ideal - current)^2
  df_sbsa[, paste0(short_name, "_sqdiff")] <- sqdiff # add to df
}  

# variables to indicate missings in Big Five:
df_sbsa <- df_sbsa %>% 
  mutate(na_pre_curr = rowSums(is.na(dplyr::select(., starts_with("bf01")))),
         na_pre_ideal = rowSums(is.na(dplyr::select(., starts_with("bf02")))),
         na_post_curr = rowSums(is.na(dplyr::select(., starts_with("bf03")))),
         na_post_ideal = rowSums(is.na(dplyr::select(., starts_with("bf04")))),
         na_comb_curr = rowSums(is.na(dplyr::select(., starts_with("bf05")))),
         na_comb_ideal = rowSums(is.na(dplyr::select(., starts_with("bf06"))))) %>% 
  mutate(na_pre_curr = ifelse(time==2, 0, na_pre_curr),
         na_pre_ideal = ifelse(time==2, 0, na_pre_ideal),
         na_post_curr = ifelse(time==1, 0, na_post_curr),
         na_post_ideal = ifelse(time==1, 0, na_post_ideal))
  
df_sbsa %>% filter(na_comb_curr > 15 | na_comb_ideal > 15) %>% select(pid, time, na_comb_curr, na_comb_ideal, finished, lastpage)

# profile correlations Big Five - ideal self
# item-level
df_sbsa <- df_sbsa %>% 
  mutate(profile_corr_item = multicon::Profile.r(df_sbsa %>% select(starts_with("bf05")), 
                                                 df_sbsa %>% select(starts_with("bf06")), nomiss = .5))

summary(df_sbsa$profile_corr_item)
df_sbsa %>% filter(is.na(profile_corr_item)) %>% select(pid, time, na_comb_curr, na_comb_ideal, finished, lastpage)

df_sbsa %>% filter(is.na(profile_corr_item) & na_comb_curr==0 & na_comb_ideal==0) %>% 
  select(pid, time, starts_with("bf05"), starts_with("bf06")) %>% print(width=Inf) 
# none

df_sbsa %>% filter(profile_corr_item==-1 | profile_corr_item==1) %>%
  select(pid, time, starts_with("bf05"), starts_with("bf06")) %>% 
  pivot_longer(cols = bf05_01:bf06_60, names_to = c("version", ".value"), names_pattern = "(.)_(.*)") %>% print(width=Inf)
# none

# facet-level
df_sbsa <- df_sbsa %>% 
  mutate(profile_corr_facet = 
           multicon::Profile.r(df_sbsa %>% select(all_of(paste0(str_trunc(names(b5_vars)[6:20], 5, ellipsis = ""), "_comb_curr"))), 
                               df_sbsa %>% select(all_of(paste0(str_trunc(names(b5_vars)[6:20], 5, ellipsis = ""), "_comb_ideal"))), nomiss = .5))
summary(df_sbsa$profile_corr_facet)
df_sbsa %>% filter(is.na(profile_corr_facet)) %>% select(pid, time, na_comb_curr, na_comb_ideal, finished, lastpage)

# Fisher z transform profile correlations
df_sbsa <- df_sbsa %>% 
  mutate(profile_corr_item_z = correlation::z_fisher(profile_corr_item),
         profile_corr_facet_z = correlation::z_fisher(profile_corr_facet))

# 15 BFI facets - pre - self improvement
df_sbsa %>% group_by(time, rando, sb07_01) %>% tally() %>% print(n=Inf)
df_sbsa %>% filter(time==1 & rando=="Self-Improvement") %>% select(starts_with("sb07"))

# 15 BFI facets - pre - self acceptance 
df_sbsa %>% group_by(time, rando, sa07_01) %>% tally() %>% print(n=Inf)
df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(starts_with("sa07"))

# 15 BFI facets - post - self improvement
df_sbsa %>% group_by(time, rando, sb12_01) %>% tally() %>% print(n=Inf)
df_sbsa %>% filter(time==2 & rando=="Self-Improvement") %>% select(starts_with("sb12"))

# 15 BFI facets - post - self acceptance
df_sbsa %>% group_by(time, rando, sa14_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Self-Acceptance") %>% select(starts_with("sa14"))


# Willingness to change - self improvement (pre)
str(df_sbsa$sb05) # Do you want to change your personality in general?
table(df_sbsa$sb05) # 1 = yes, 2 = no
df_sbsa %>% group_by(time, rando, sb05) %>% tally() %>% print(n=Inf) 
str(df_sbsa$sb06_01) # How much do you want to change your personality in general?
df_sbsa %>% group_by(time, rando, sb06_01) %>% tally() %>% print(n=Inf) 

# Personal project dimensions - self improvement
# pre
str(df_sbsa$sb01_01) # How important is it for you to change your personality?
str(df_sbsa$sb01_02) # How difficult is it for you to work on changing your personality?
df_sbsa %>% group_by(time, rando, sb01_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==1 & rando=="Self-Improvement") %>% select(starts_with("sb01"))
# post
str(df_sbsa$sb13_01) # During this study, how important was it for you to change your personality?
str(df_sbsa$sb13_02) # During this study, how difficult was it for you to work on changing your personality?
df_sbsa %>% group_by(time, rando, sb13_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Self-Improvement") %>% select(starts_with("sb13"))

# Manipulation check - self improvement
str(df_sbsa$sb04_01) # I thought about the ways I would like to change
str(df_sbsa$sb04_02) # I sought out environments that would help me be the kind of person I want to be
str(df_sbsa$sb04_03) # I practiced new habits to achieve my change goals
df_sbsa %>% group_by(time, rando, sb04_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Self-Improvement") %>% select(starts_with("sb04"))


# Importance to accept - self acceptance (pre)
str(df_sbsa$sa05) # Do you want to be better at accepting yourself for who you are?
table(df_sbsa$sa05) # 1 = yes, 2 = no
df_sbsa %>% group_by(time, rando, sa05) %>% tally() %>% print(n=Inf) 
str(df_sbsa$sa06_01) # How much do you want to be better at accepting yourself for who you are?
df_sbsa %>% group_by(time, rando, sa06_01) %>% tally() %>% print(n=Inf) 

# Personal project dimensions - self acceptance
# pre
str(df_sbsa$sa01_01) # How important is it for you to accept your personality?
str(df_sbsa$sa01_02) # How difficult is it for you to work on accepting your personality?
df_sbsa %>% group_by(time, rando, sa01_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(starts_with("sa01"))
# post
str(df_sbsa$sa12_01) # During this study, how important was it for you to accept your personality?
str(df_sbsa$sa12_02) # During this study, how difficult was it for you to work on accepting your personality?
df_sbsa %>% group_by(time, rando, sa12_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Self-Acceptance") %>% select(starts_with("sa12"))

# Manipulation check - self acceptance
str(df_sbsa$sa04_01) # I thought about how I could be less judgmental and more self-affirming
str(df_sbsa$sa04_02) # I sought out environments that would help me accept who I am
str(df_sbsa$sa04_03) # I practiced new self-acceptance habits to affirm who I am
df_sbsa %>% group_by(time, rando, sa04_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Self-Acceptance") %>% select(starts_with("sa04"))

# adjectives - self improvement
# Please list five adjectives you would like to change and indicate 
# whether you would like to become more or less like those traits. 
str(df_sbsa$sb08_01) 
df_sbsa %>% filter(!is.na(sb08_01)) %>% group_by(time, rando) %>% tally()
df_sbsa %>% filter(time==1 & rando=="Self-Improvement") %>% select(starts_with("sb08"))

str(df_sbsa$sb09_01) 
df_sbsa %>% group_by(time, rando, sb09_01) %>% tally() %>% print(n=Inf) # 1 = too low / 2 = too high
df_sbsa %>% filter(time==1 & rando=="Self-Improvement") %>% select(starts_with("sb09"))

# adjectives - self acceptance
# Please list five adjectives that describe you and that you would like to accept 
# more than you currently do. Indicate whether you think you are too high or too low in those traits.
str(df_sbsa$sa08_01) 
df_sbsa %>% filter(!is.na(sa08_01)) %>% group_by(time, rando) %>% tally()
df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(starts_with("sa08"))

str(df_sbsa$sa09_01) 
df_sbsa %>% group_by(time, rando, sa09_01) %>% tally() %>% print(n=Inf) # 1 = too low / 2 = too high
df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(starts_with("sa09"))

#### add prolific demographic data ####

# Age, Sex, Ethnicity, Country of residence, Student status, Employment status
demog <- prolific_baseline %>% select(dm01_01 = "Participant id", age = "Age", gender = "Sex", ethnicity = "Ethnicity simplified", 
                                      country = "Country of residence", student = "Student status", employed = "Employment status")
demog <- demog %>% mutate_at(c("age", "ethnicity", "country", "student", "employed"), ~ifelse(. == "DATA_EXPIRED" | . == "CONSENT_REVOKED", NA, .))
demog <- demog %>% mutate(age = as.numeric(age))

df_sbsa <- df_sbsa %>% left_join(demog)

#### save cleaned data set ####

# drop potentially sensitive variables
df_sbsa <- df_sbsa %>% select(-c(case, serial, ref, started, lastdata, starts_with("dm0"), # identifiers, timestamps
                                 sb02_01, sa02_01, starts_with("sb08"), starts_with("sa08"))) # , free-form texts
                                 # sb14_01, sa13_01)) # still need those for the word clouds

base::save(df_sbsa, file = "data/df_sbsa.rda")
