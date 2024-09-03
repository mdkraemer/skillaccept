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

# participant data containing demographic information (based on T1)
demog_all <- bind_rows(
  readr::read_csv("data/St3_prolific_export_demog_group1.csv") %>% mutate(group=1),
  readr::read_csv("data/St3_prolific_export_demog_group2.csv") %>% mutate(group=2),
  readr::read_csv("data/St3_prolific_export_demog_group3.csv") %>% mutate(group=3)
)
  
approved_ID <- demog_all %>% select(dm01_01 = `Participant id`, status = Status)
table(approved_ID$status)

# data grom all three groups - T1
df_sbsa3_t1 <- readxl::read_excel("data/St3_T1_data_PersonalityChange_2024-07-19_15-59.xlsx", col_names = F, skip = 2, guess_max = 10000)
var_names_t1 <- readxl::read_excel("data/St3_T1_data_PersonalityChange_2024-07-19_15-59.xlsx", col_names = F)[c(1,2), ]
var_names_t1 <- data.frame(t(var_names_t1))
colnames(df_sbsa3_t1) <- stringr::str_to_lower(var_names_t1$X1) # only lower case pls
# label data 
df_sbsa3_t1 <- labelled::set_variable_labels(df_sbsa3_t1, .labels = var_names_t1$X2)

table(df_sbsa3_t1$questnnr) # name experimental condition
df_sbsa3_t1 <- df_sbsa3_t1 %>% 
  mutate(group = ifelse(questnnr=="S3_T1_Gr1", "Group 1", ifelse(questnnr=="S3_T1_Gr2", "Group 2", "Group 3")))
table(df_sbsa3_t1$group)

# data grom all three groups - T2
df_sbsa3_t2 <- readxl::read_excel("data/St3_T2_data_PersonalityChange_2024-07-29_09-53.xlsx", col_names = F, skip = 2, guess_max = 10000)
var_names_t2 <- readxl::read_excel("data/St3_T2_data_PersonalityChange_2024-07-29_09-53.xlsx", col_names = F)[c(1,2), ]
var_names_t2 <- data.frame(t(var_names_t2))
colnames(df_sbsa3_t2) <- stringr::str_to_lower(var_names_t2$X1) # only lower case pls
# label data 
df_sbsa3_t2 <- labelled::set_variable_labels(df_sbsa3_t2, .labels = var_names_t2$X2)

table(df_sbsa3_t2$questnnr) # name experimental condition
df_sbsa3_t2 <- df_sbsa3_t2 %>% 
  mutate(group = ifelse(questnnr=="S3_T2_Gr1", "Group 1", ifelse(questnnr=="S3_T2_Gr2", "Group 2", "Group 3")))
table(df_sbsa3_t2$group)

# join all time points
df_sbsa3_all <- bind_rows(df_sbsa3_t1 %>% mutate(time=1), df_sbsa3_t2 %>% mutate(time=2)) %>% 
  arrange(dm01_01, time) 

#### filter to relevant cases / vars ####

df_sbsa3_all %>% 
  filter(str_length(dm01_01)>24) %>% select(case, dm01_01)

df_sbsa3_all <- df_sbsa3_all %>% 
  filter(!is.na(dm01_01)) %>% 
  mutate(dm01_01 = if_else(str_length(dm01_01)>24, str_trunc(dm01_01, 24, ellipsis = ""), dm01_01))

df_sbsa3 <- df_sbsa3_all %>% 
  filter(str_length(dm01_01)==24)

# filter approved cases (at T1):
df_sbsa3 <- df_sbsa3 %>% left_join(approved_ID) %>% filter(status=="APPROVED") %>% select(-status)

df_sbsa3 %>% group_by(group) %>% summarise(n_distinct(dm01_01)) # initial N (before further exclusions)

# we might still have duplicate cases in here -> started questionnaire twice 

df_sbsa3 <- df_sbsa3 %>% 
  group_by(dm01_01, time) %>% 
  mutate(trials = n()) %>% 
  mutate(max_fin = max(finished)) %>% 
  ungroup()

df_sbsa3 <- df_sbsa3 %>% filter(!(trials>1 & finished==0)) # 7 dropped 

# update count
df_sbsa3 <- df_sbsa3 %>% 
  group_by(dm01_01, time) %>% 
  mutate(trials = n()) %>% 
  ungroup()

df_sbsa3 %>% select(case, dm01_01, questnnr, finished, max_fin, lastpage, missing, trials) %>% filter(trials>1)
# 6 remaining cases who finished twice at T2
df_sbsa3 %>% filter(trials>1) %>% print(width = Inf) # both look relatively complete -> take first trial each
df_sbsa3 <- df_sbsa3 %>% 
  filter(case!=9856) %>% filter(case!=9830) %>% filter(case!=9864) %>% 
  filter(case!=9903) %>% filter(case!=9739) %>% filter(case!=9838)

# update count
df_sbsa3 <- df_sbsa3 %>% 
  group_by(dm01_01, time) %>% 
  mutate(trials = n()) %>% 
  ungroup()

df_sbsa3 %>% select(case, dm01_01, questnnr, finished, max_fin, lastpage, missing, trials) %>% filter(trials>1)

# those who did not finish their questionnaire -> leave them as is models will use FIML where needed
df_sbsa3 %>% filter(max_fin==0) %>% select(case, dm01_01, questnnr, finished, max_fin, lastpage, missing, trials) %>% print(n=300)

# overview
df_sbsa3 %>% group_by(group, time) %>% tally()
# a few more cases in each group at T2 than we currently approved -> check compensation!

# sort & generate running ID
df_sbsa3 <- df_sbsa3 %>% arrange(dm01_01, time) %>% group_by(dm01_01) %>% mutate(pid = cur_group_id()) %>% ungroup()

#### checks for careless responding ####

##### longstring #####

# 10 item Meaning in Life Questionnaire 
longstr_meaning <- careless::longstring(df_sbsa3 %>% select(starts_with("ml01")))
#boxplot(longstr_meaning, main = "Boxplot of Longstring index")
table(longstr_meaning)
longstr_meaning_score <- df_sbsa3[which(longstr_meaning > 7), c("pid")]
longstr_meaning_score$longstr_meaning_flag <- 1
longstr_meaning_score <- longstr_meaning_score %>% group_by(pid) %>% summarise(longstr_meaning_flag = n())
  
# 10 item Rosenberg Self-Esteem Scale
longstr_selfest <- careless::longstring(df_sbsa3 %>% select(starts_with("rs01")))
#boxplot(longstr_selfest, main = "Boxplot of Longstring index")
table(longstr_selfest)
longstr_selfest_score <- df_sbsa3[which(longstr_selfest > 7), c("pid")]
longstr_selfest_score$longstr_selfest_flag <- 1
longstr_selfest_score <- longstr_selfest_score %>% group_by(pid) %>% summarise(longstr_selfest_flag = n())

# 12 item Self Concept Clarity Scale
longstr_concept <- careless::longstring(df_sbsa3 %>% select(starts_with("sc01")))
#boxplot(longstr_concept, main = "Boxplot of Longstring index")
#boxplot(longstr_selfest, main = "Boxplot of Longstring index")
table(longstr_concept)
summary(longstr_concept)
longstr_concept_score <- df_sbsa3[which(longstr_concept > 9), c("pid")]
longstr_concept_score$longstr_concept_flag <- 1
longstr_concept_score <- longstr_concept_score %>% group_by(pid) %>% summarise(longstr_concept_flag = n())

# 60 item BFI-2, but in different sections (Groups 2 & 3 only had the current personality which was saved in another object, BF05)
longstr_bficurrpre <- df_sbsa3 %>% filter(time==1 & group=="Group 1") %>% select(pid, starts_with("bf01")) %>% 
  mutate(longstr_bficurrpre = careless::longstring(.)) 
summary(longstr_bficurrpre$longstr_bficurrpre)
table(longstr_bficurrpre$longstr_bficurrpre)
longstr_bficurrpre_score <- longstr_bficurrpre %>% 
  filter(longstr_bficurrpre > 7) %>% mutate(longstr_bficurrpre_flag = 1) %>% select(pid, longstr_bficurrpre_flag)

longstr_bficurrpre2 <- df_sbsa3 %>% filter(time==1 & group %in% c("Group 2", "Group 3")) %>% 
  select(pid, starts_with("bf05")) %>% 
  mutate(longstr_bficurrpre = careless::longstring(.)) 
summary(longstr_bficurrpre2$longstr_bficurrpre)
table(longstr_bficurrpre2$longstr_bficurrpre)
longstr_bficurrpre2_score <- longstr_bficurrpre2 %>% 
  filter(longstr_bficurrpre > 7) %>% mutate(longstr_bficurrpre_flag = 1) %>% select(pid, longstr_bficurrpre_flag)

longstr_bfiidealpre <- df_sbsa3 %>% filter(time==1 & group=="Group 1") %>% select(pid, starts_with("bf02")) %>% 
  mutate(longstr_bfiidealpre = careless::longstring(.)) 
summary(longstr_bfiidealpre$longstr_bfiidealpre)
table(longstr_bfiidealpre$longstr_bfiidealpre)
longstr_bfiidealpre_score <- longstr_bfiidealpre %>% 
  filter(longstr_bfiidealpre > 7) %>% mutate(longstr_bfiidealpre_flag = 1) %>% select(pid, longstr_bfiidealpre_flag)

longstr_bficurrpost <- df_sbsa3 %>% filter(time==2 & group=="Group 1") %>% select(pid, starts_with("bf01")) %>% 
  mutate(longstr_bficurrpost = careless::longstring(.)) 
summary(longstr_bficurrpost$longstr_bficurrpost)
table(longstr_bficurrpost$longstr_bficurrpost)
longstr_bficurrpost_score <- longstr_bficurrpost %>% 
  filter(longstr_bficurrpost > 7) %>% mutate(longstr_bficurrpost_flag = 1) %>% select(pid, longstr_bficurrpost_flag)

longstr_bficurrpost2 <- df_sbsa3 %>% filter(time==2 & group %in% c("Group 2", "Group 3")) %>% 
  select(pid, starts_with("bf05")) %>% 
  mutate(longstr_bficurrpost = careless::longstring(.)) 
summary(longstr_bficurrpost2$longstr_bficurrpost)
table(longstr_bficurrpost2$longstr_bficurrpost)
longstr_bficurrpost2_score <- longstr_bficurrpost2 %>% 
  filter(longstr_bficurrpost > 7) %>% mutate(longstr_bficurrpost_flag = 1) %>% select(pid, longstr_bficurrpost_flag)

longstr_bfiidealpost <- df_sbsa3 %>% filter(time==2 & group=="Group 1") %>% 
  select(pid, starts_with("bf02")) %>% 
  mutate(longstr_bfiidealpost = careless::longstring(.)) 
summary(longstr_bfiidealpost$longstr_bfiidealpost)
table(longstr_bfiidealpost$longstr_bfiidealpost)
longstr_bfiidealpost_score <- longstr_bfiidealpost %>% 
  filter(longstr_bfiidealpost > 7) %>% mutate(longstr_bfiidealpost_flag = 1) %>% select(pid, longstr_bfiidealpost_flag)

longstr_scores <- full_join(longstr_meaning_score, longstr_selfest_score) %>% 
  full_join(longstr_concept_score) %>% 
  full_join(longstr_bficurrpre_score) %>% full_join(longstr_bficurrpre2_score) %>% 
  full_join(longstr_bfiidealpre_score) %>% 
  full_join(longstr_bficurrpost_score) %>% full_join(longstr_bficurrpost2_score) %>% 
  full_join(longstr_bfiidealpost_score) %>% 
  mutate(longstr_flag_sum = rowSums(across(ends_with("_flag")), na.rm=T))

##### mahalanobis distance ##### 

# 10 item Meaning in Life Questionnaire 
mahad_meaning <- careless::mahad(df_sbsa3 %>% select(starts_with("ml01")), plot = FALSE)
summary(mahad_meaning)
mahad_meaning_score <- df_sbsa3[which(mahad_meaning > mean(mahad_meaning, na.rm=T)+3*sd(mahad_meaning, na.rm=T)), c("pid")]
mahad_meaning_score$mahad_meaning_flag <- 1
mahad_meaning_score <- mahad_meaning_score %>% group_by(pid) %>% summarise(mahad_meaning_flag = n())

# 10 item Rosenberg Self-Esteem Scale
mahad_selfest <- careless::mahad(df_sbsa3 %>% select(starts_with("rs01")), plot = FALSE)
summary(mahad_selfest)
mahad_selfest_score <- df_sbsa3[which(mahad_selfest > mean(mahad_selfest, na.rm=T)+3*sd(mahad_selfest, na.rm=T)), c("pid")]
mahad_selfest_score$mahad_selfest_flag <- 1
mahad_selfest_score <- mahad_selfest_score %>% group_by(pid) %>% summarise(mahad_selfest_flag = n())

# 12 item Self Concept Clarity Scale
mahad_concept <- careless::mahad(df_sbsa3 %>% select(starts_with("sc01")), plot = FALSE)
summary(mahad_concept)
mahad_concept_score <- df_sbsa3[which(mahad_concept > mean(mahad_concept, na.rm=T)+3*sd(mahad_concept, na.rm=T)), c("pid")]
mahad_concept_score$mahad_concept_flag <- 1
mahad_concept_score <- mahad_concept_score %>% group_by(pid) %>% summarise(mahad_concept_flag = n())

# 60 item BFI-2, but in different sections
mahad_bficurrpre <- df_sbsa3 %>% filter(time==1 & group=="Group 1") %>% select(pid, starts_with("bf01")) %>% 
  mutate(mahad_bficurrpre = careless::mahad(., plot = FALSE)) 
summary(mahad_bficurrpre$mahad_bficurrpre)
mahad_bficurrpre_score <- mahad_bficurrpre %>% 
  filter(mahad_bficurrpre > mean(mahad_bficurrpre, na.rm=T)+3*sd(mahad_bficurrpre, na.rm=T)) %>% 
  mutate(mahad_bficurrpre_flag = 1) %>% select(pid, mahad_bficurrpre_flag)

mahad_bficurrpre2 <- df_sbsa3 %>% filter(time==1 & group %in% c("Group 2", "Group 3")) %>% 
  select(pid, starts_with("bf05")) %>% 
  mutate(mahad_bficurrpre = careless::mahad(., plot = FALSE)) 
summary(mahad_bficurrpre2$mahad_bficurrpre)
mahad_bficurrpre2_score <- mahad_bficurrpre2 %>% 
  filter(mahad_bficurrpre > mean(mahad_bficurrpre, na.rm=T)+3*sd(mahad_bficurrpre, na.rm=T)) %>% 
  mutate(mahad_bficurrpre_flag = 1) %>% select(pid, mahad_bficurrpre_flag)

mahad_bfiidealpre <- df_sbsa3 %>% filter(time==1 & group=="Group 1") %>% select(pid, starts_with("bf02")) %>% 
  mutate(mahad_bfiidealpre = careless::mahad(., plot = FALSE)) 
summary(mahad_bfiidealpre$mahad_bfiidealpre)
mahad_bfiidealpre_score <- mahad_bfiidealpre %>% 
  filter(mahad_bfiidealpre > mean(mahad_bfiidealpre, na.rm=T)+3*sd(mahad_bfiidealpre, na.rm=T)) %>% 
  mutate(mahad_bfiidealpre_flag = 1) %>% select(pid, mahad_bfiidealpre_flag)

mahad_bficurrpost <- df_sbsa3 %>% filter(time==2 & group=="Group 1") %>% select(pid, starts_with("bf01")) %>% 
  mutate(mahad_bficurrpost = careless::mahad(., plot = FALSE)) 
summary(mahad_bficurrpost$mahad_bficurrpost)
mahad_bficurrpost_score <- mahad_bficurrpost %>% 
  filter(mahad_bficurrpost > mean(mahad_bficurrpost, na.rm=T)+3*sd(mahad_bficurrpost, na.rm=T)) %>% 
  mutate(mahad_bficurrpost_flag = 1) %>% select(pid, mahad_bficurrpost_flag)

mahad_bficurrpost2 <- df_sbsa3 %>% filter(time==2 & group %in% c("Group 2", "Group 3")) %>% 
  select(pid, starts_with("bf05")) %>% 
  mutate(mahad_bficurrpost = careless::mahad(., plot = FALSE)) 
summary(mahad_bficurrpost2$mahad_bficurrpost)
mahad_bficurrpost2_score <- mahad_bficurrpost2 %>% 
  filter(mahad_bficurrpost > mean(mahad_bficurrpost, na.rm=T)+3*sd(mahad_bficurrpost, na.rm=T)) %>% 
  mutate(mahad_bficurrpost_flag = 1) %>% select(pid, mahad_bficurrpost_flag)

mahad_bfiidealpost <- df_sbsa3 %>% filter(time==2 & group=="Group 1") %>% select(pid, starts_with("bf02")) %>% 
  mutate(mahad_bfiidealpost = careless::mahad(., plot = FALSE)) 
summary(mahad_bfiidealpost$mahad_bfiidealpost)
mahad_bfiidealpost_score <- mahad_bfiidealpost %>% 
  filter(mahad_bfiidealpost > mean(mahad_bfiidealpost, na.rm=T)+3*sd(mahad_bfiidealpost, na.rm=T)) %>% 
  mutate(mahad_bfiidealpost_flag = 1) %>% select(pid, mahad_bfiidealpost_flag)

mahad_scores <- full_join(mahad_meaning_score, mahad_selfest_score) %>% 
  full_join(mahad_concept_score) %>% 
  full_join(mahad_bficurrpre_score) %>% full_join(mahad_bficurrpre2_score) %>% 
  full_join(mahad_bfiidealpre_score) %>% 
  full_join(mahad_bficurrpost_score) %>% full_join(mahad_bficurrpost2_score) %>% 
  full_join(mahad_bfiidealpost_score) %>% 
  mutate(mahad_flag_sum = rowSums(across(ends_with("_flag")), na.rm=T))

##### completion times ##### 
summary(df_sbsa3$time_sum)
hist(df_sbsa3$time_sum)

summary(df_sbsa3$missing)
hist(df_sbsa3$missing)

##### exclude based on these criteria ##### 

longstr_scores %>% filter(longstr_flag_sum>2) %>% arrange(pid) %>% pull(pid) # with >1 it would be too many cases...

df_sbsa3 <- df_sbsa3 %>% # 4 pid's excluded
  filter(!pid %in% (longstr_scores %>% filter(longstr_flag_sum>2) %>% pull(pid)))
  
mahad_scores %>% filter(mahad_flag_sum>2) %>% arrange(pid) %>% pull(pid) # After visual inspection, I don't think this case is necessarily suspicious
# df_sbsa3 %>% filter(pid %in% (mahad_scores %>% filter(mahad_flag_sum>2) %>% arrange(pid) %>% pull(pid))) %>% print(width = Inf)

df_sbsa3 %>% filter(time_sum < 100) %>% arrange(pid) %>% pull(pid) # these is an incomplete case who only started the interview (but we have valid T1 data)
df_sbsa3 %>% filter(time_sum < 100 & finished==1) %>% print(width = Inf) 
# no further exclusions based on time 

df_sbsa3 %>% filter(missing > 10) %>% arrange(pid) %>% pull(pid) # 1 additional case
df_sbsa3 %>% filter(missing > 10) %>% print(width = Inf) # FIML should be able to deal with these!

#df_sbsa3 <- df_sbsa3 %>% filter(missing < 10)

# overview
df_sbsa3 %>% group_by(group, time) %>% tally()

#### inspect / recode variables ####

# 5 item Satisfaction with Life Scale (Diener et al. 1985) 
df_sbsa3 %>% select(starts_with("sw06"))
df_sbsa3 %>% group_by(time, group, sw06_01) %>% tally() %>% print(n=Inf)
alpha.swls <- df_sbsa3 %>% 
  select(starts_with("sw06")) %>%
  psych::alpha(check.keys = TRUE)
omega.swls <- df_sbsa3 %>%
  dplyr::select(starts_with("sw06")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(alpha.swls$keys[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=3) # 3 returns error message
df_sbsa3$swls <- df_sbsa3 %>%
  select(starts_with("sw06")) %>%
  psych::reverse.code(keys=alpha.swls$keys[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa3$swls[is.nan(df_sbsa3$swls)] <- NA

# 10 item Meaning in Life Questionnaire (Steger et al., 2006)
df_sbsa3 %>% select(starts_with("ml01"))
df_sbsa3 %>% group_by(time, group, ml01_01) %>% tally() %>% print(n=Inf)
keys_meaning <- list(meaning = c("ml01_01", "-ml01_02", "-ml01_03", "ml01_04", "ml01_05", "ml01_06", "-ml01_07", "-ml01_08", "-ml01_09", "-ml01_10")) # does not work automatically, here
alpha.meaning <- df_sbsa3 %>% 
  select(starts_with("ml01")) %>%
  psych::alpha(keys = keys_meaning)
omega.meaning <- df_sbsa3 %>%
  dplyr::select(starts_with("ml01")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(keys_meaning[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=4) # 3 returns error message
df_sbsa3$meaning <- df_sbsa3 %>%
  select(starts_with("ml01")) %>%
  psych::reverse.code(keys = keys_meaning[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa3$meaning[is.nan(df_sbsa3$meaning)] <- NA

# 10 item Rosenberg Self-Esteem Scale (Rosenberg et al., 1965) 
df_sbsa3 %>% select(starts_with("rs01"))
df_sbsa3 %>% group_by(time, group, rs01_01) %>% tally() %>% print(n=Inf)
keys_selfes <- list(meaning = c("rs01_01", "-rs01_02", "rs01_03", "rs01_04", "-rs01_05", "-rs01_06", "rs01_07", "-rs01_08", "-rs01_09", "rs01_10")) # does not work automatically, here
alpha.selfes <- df_sbsa3 %>% 
  select(starts_with("rs01")) %>%
  psych::alpha(keys = keys_selfes)
omega.selfes <- df_sbsa3 %>%
  dplyr::select(starts_with("rs01")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(keys_selfes[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=4) # 3 returns error message
df_sbsa3$selfes <- df_sbsa3 %>%
  select(starts_with("rs01")) %>%
  psych::reverse.code(keys = keys_selfes[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa3$selfes[is.nan(df_sbsa3$selfes)] <- NA

# 12 item Self Concept Clarity Scale (Campbell et al., 1996)
df_sbsa3 %>% select(starts_with("sc01"))
df_sbsa3 %>% group_by(time, group, sc01_01) %>% tally() %>% print(n=Inf)
keys_concept <- list(concept = c("-sc01_01", "-sc01_02", "-sc01_03", "-sc01_04", "-sc01_05", "sc01_06", 
                                 "-sc01_07", "-sc01_08", "-sc01_09", "-sc01_10", "sc01_11", "-sc01_12")) # does not work automatically, here
alpha.concept <- df_sbsa3 %>% 
  select(starts_with("sc01")) %>%
  psych::alpha(keys = keys_concept)
omega.concept <- df_sbsa3 %>%
  dplyr::select(starts_with("sc01")) %>% 
  psych::omega(m = ., key = ifelse(str_detect(keys_concept[[1]], "-"), -1, 1), # function now requires the 1/-1 coding
               plot = FALSE, nfactors=3)
df_sbsa3$concept <- df_sbsa3 %>%
  select(starts_with("sc01")) %>%
  psych::reverse.code(keys = keys_concept[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa3$concept[is.nan(df_sbsa3$concept)] <- NA

# 60 item BFI-2, but in different sections depending on the group
df_sbsa3 %>% group_by(time, group, bf01_01) %>% tally() %>% print(n=Inf) # current trait level - group 1
df_sbsa3 %>% group_by(time, group, bf02_01) %>% tally() %>% print(n=Inf) # ideal trait level - group 1

df_sbsa3 %>% group_by(time, group, bf05_01) %>% tally() %>% print(n=Inf) # current trait level - groups 2 & 3

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

bfi_versions <- list("bf01", "bf02", "bf03", "bf04", # this was a bit different than in Study 1 & 2 (see changes below)
                     "bf05", "bf06") # last 2 are created below
names(bfi_versions) <- c("pre_curr", "pre_ideal", "post_curr", "post_ideal",
                         "comb_curr", "comb_ideal") # last 2 are created below

# create combined variables across the two assessments (but separate for current "05" / ideal "06")
all_bfi_items = paste0("_", str_pad(1:60, 2, pad = "0"))

table(df_sbsa3$bf01_01, df_sbsa3$bf05_01, useNA = "always")

for (i in 1:length(all_bfi_items)) {
  # integrate 'bf05' (confusingly named) with bf01: both refer to current personality, regardless of time point
  df_sbsa3[, paste0(bfi_versions[[1]], all_bfi_items[i])] = 
    rowSums(df_sbsa3[, c(paste0(bfi_versions[[1]], all_bfi_items[i]),
                       paste0(bfi_versions[[5]], all_bfi_items[i]))], na.rm = T)
  # attention: creates 0 if both items NA
  df_sbsa3 <- df_sbsa3 %>% mutate_at(c(paste0(bfi_versions[[1]], all_bfi_items[i])), ~na_if(., 0))
}
table(df_sbsa3$bf01_01, df_sbsa3$bf05_01, useNA = "always")
table(df_sbsa3$bf01_01, useNA = "always")

# drop 'bf05' vars
df_sbsa3 = df_sbsa3 %>% select(-starts_with("bf05_"))

for (i in 1:length(all_bfi_items)) {
  # rename again so that it corresponds with Study 1 & 2 naming convention
  df_sbsa3[, paste0(bfi_versions[[5]], all_bfi_items[i])] = df_sbsa3[, paste0(bfi_versions[[1]], all_bfi_items[i])]
  df_sbsa3[, paste0(bfi_versions[[6]], all_bfi_items[i])] = df_sbsa3[, paste0(bfi_versions[[2]], all_bfi_items[i])]
}

# now score traits and compute reliabilities
for (i in 1:length(b5_vars)) {
  # loop across 5 traits AND 15 facets
  item_nrs = b5_vars[[i]][[1]]
  short_name = str_trunc(names(b5_vars)[i], 5, ellipsis = "")
  # loop across BFI versions -> here we don't have version 3, 4 as previously 
  for (j in c(1,2,5,6)) {
    items = paste0(bfi_versions[[j]], item_nrs)
    items_keyed = paste0(b5_vars[[i]][[2]], items)
    items_keyed = ifelse(str_detect(items_keyed, "-"), -1, 1) # apparently, the function now require the 1/-1 coding
    # reliability objects
    alpha <- df_sbsa3 %>% 
      select(all_of(items)) %>%
      psych::alpha(keys = items_keyed)
    eval(call("<-", as.name(paste0("alpha_", short_name, "_", names(bfi_versions)[j])), alpha)) # save to environment
    if (i %in% c(1:5)) { # omega only for B5 traits
      omega <- df_sbsa3 %>% 
        select(all_of(items)) %>%
        psych::omega(m = ., key = items_keyed, plot = FALSE, nfactors=3)
      eval(call("<-", as.name(paste0("omega_", short_name, "_", names(bfi_versions)[j])), omega))
    }
    # mean scores
    trait = df_sbsa3 %>%
      select(all_of(items)) %>%
      psych::reverse.code(keys = items_keyed, items = .) %>%
      rowMeans(na.rm=T)
    trait[is.nan(trait)] <- NA
    df_sbsa3[, paste0(short_name, "_", names(bfi_versions)[j])] <- trait # add to df
  }
}  

# put all measures of internal consistency in one table (for supplement)
int_consist_traits_st3 <- tibble(
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
base::save(int_consist_traits_st3, file = "data/int_consist_traits_st3.rda") # save for later import

int_consist_facets_st3 <- tibble(
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
base::save(int_consist_facets_st3, file = "data/int_consist_facets_st3.rda") # save for later import

# squared difference between current and ideal self
for (i in 1:length(b5_vars)) {
  # loop across 5 traits AND 15 facets
  short_name = str_trunc(names(b5_vars)[i], 5, ellipsis = "")
  current = df_sbsa3[, paste0(short_name, "_comb_curr")]
  ideal = df_sbsa3[, paste0(short_name, "_comb_ideal")]
  sqdiff = (ideal - current)^2
  df_sbsa3[, paste0(short_name, "_sqdiff")] <- sqdiff # add to df
}  

# variables to indicate missings in Big Five:
df_sbsa3 <- df_sbsa3 %>% 
  mutate(na_pre_curr = rowSums(is.na(dplyr::select(., starts_with("bf01")))),
         na_pre_ideal = rowSums(is.na(dplyr::select(., starts_with("bf02")))),
         na_post_curr = rowSums(is.na(dplyr::select(., starts_with("bf01")))),
         na_post_ideal = rowSums(is.na(dplyr::select(., starts_with("bf02")))),
         na_comb_curr = rowSums(is.na(dplyr::select(., starts_with("bf05")))),
         na_comb_ideal = rowSums(is.na(dplyr::select(., starts_with("bf06"))))) %>% 
  mutate(na_pre_curr = ifelse(time==2, 0, na_pre_curr),
         na_pre_ideal = ifelse(time==2 & group=="Group 1", 0, na_pre_ideal),
         na_pre_ideal = ifelse(group %in% c("Group 2","Group 3"), 0, na_pre_ideal),
         na_post_curr = ifelse(time==1, 0, na_post_curr),
         na_post_ideal = ifelse(time==1 & group=="Group 1", 0, na_post_ideal),
         na_post_ideal = ifelse(group %in% c("Group 2","Group 3"), 0, na_post_ideal),
         na_comb_ideal = ifelse(group %in% c("Group 2","Group 3"), 0, na_comb_ideal)) # ideal was only asked in Group 1
  
df_sbsa3 %>% filter(na_comb_curr > 15 | na_comb_ideal > 15) %>% select(pid, time, na_comb_curr, na_comb_ideal, finished, lastpage)
# first person just skipped current pers altogether at time==1
# second person skipped T2 more or less completely 
# probably still better to exclude those with all missings for the Big Five... (despite FIML)
df_sbsa3 <- df_sbsa3 %>% filter(na_comb_curr!=60) # N = 2 excluded

# profile correlations Big Five - ideal self -> only possible for Group 1
# item-level
df_sbsa3 <- df_sbsa3 %>% 
  mutate(profile_corr_item = multicon::Profile.r(df_sbsa3 %>% select(starts_with("bf05")), 
                                                 df_sbsa3 %>% select(starts_with("bf06")), nomiss = .5))

summary(df_sbsa3$profile_corr_item)
df_sbsa3 %>% filter(is.na(profile_corr_item) & group=="Group 1") %>% select(pid, time, na_comb_curr, na_comb_ideal, finished, lastpage)

# facet-level
df_sbsa3 <- df_sbsa3 %>% 
  mutate(profile_corr_facet = 
           multicon::Profile.r(df_sbsa3 %>% select(all_of(paste0(str_trunc(names(b5_vars)[6:20], 5, ellipsis = ""), "_comb_curr"))), 
                               df_sbsa3 %>% select(all_of(paste0(str_trunc(names(b5_vars)[6:20], 5, ellipsis = ""), "_comb_ideal"))), nomiss = .5))
summary(df_sbsa3$profile_corr_facet)
df_sbsa3 %>% filter(is.na(profile_corr_facet) & group=="Group 1") %>% select(pid, time, na_comb_curr, na_comb_ideal, finished, lastpage)

# Fisher z transform profile correlations
df_sbsa3 <- df_sbsa3 %>% 
  mutate(profile_corr_item_z = correlation::z_fisher(profile_corr_item),
         profile_corr_facet_z = correlation::z_fisher(profile_corr_facet))

#### add prolific demographic data ####

# Age, Sex, Ethnicity, Country of residence, Student status, Employment status
demog_all <- demog_all %>% select(dm01_01 = "Participant id", age = "Age", gender = "Sex", ethnicity = "Ethnicity simplified", 
                                  country = "Country of residence", student = "Student status", employed = "Employment status")
demog_all <- demog_all %>% mutate_at(c("age", "ethnicity", "country", "student", "employed"), ~ifelse(. == "DATA_EXPIRED" | . == "CONSENT_REVOKED", NA, .))
demog_all <- demog_all %>% mutate(age = as.numeric(age))

df_sbsa3 <- df_sbsa3 %>% left_join(demog_all)

#### save cleaned data set ####

# drop potentially sensitive variables
df_sbsa3 <- df_sbsa3 %>% select(-c(case, serial, ref, started, lastdata, starts_with("dm0"), # identifiers, timestamps
                                   sa13_01))

base::save(df_sbsa3, file = "data/df_sbsa3.rda")
