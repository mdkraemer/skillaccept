# Study 2: check data quality - who gets compensated?

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

# load data
demog_st2 <- readr::read_csv("data/St2_prolific_export_demog_080923.csv") # prolific
sosci_st2 <- readxl::read_excel("data/St2_data_PersonalityChange_080923.xlsx", col_names = F, skip = 2, guess_max = 10000) # sosci
var_names <- readxl::read_excel("data/St2_data_PersonalityChange_080923.xlsx", col_names = F)[c(1,2), ]
var_names <- data.frame(t(var_names))
colnames(sosci_st2) <- stringr::str_to_lower(var_names$X1) # only lower case pls
# label data 
sosci_st2 <- labelled::set_variable_labels(sosci_st2, .labels = var_names$X2)
# filter irrelevant cases:
sosci_st2 %>% 
  filter(str_length(dm01_01)>24 | str_length(dm01_01)<24) %>% select(case, dm01_01)
sosci_st2 <- sosci_st2 %>% filter(str_length(dm01_01)==24)

# check prolific groups
demog_st2 %>% group_by(Status) %>% tally()
demog_st2 %>% group_by(`Completion code`) %>% tally()
# check for duplicate IDs
demog_st2 %>% group_by(`Participant id`) %>% mutate(id_row = n()) %>% ungroup() %>% group_by(id_row) %>% tally()
demog_st2 %>% pull(`Participant id`) %>% unique() %>% length()


#### check APPROVED in SoSci data -> preliminary data check based on three groups ####
ids_approv <- demog_st2 %>% filter(Status=="APPROVED") %>% pull(`Participant id`)

sosci_st2_approv <- sosci_st2 %>% filter(dm01_01 %in% ids_approv)
sosci_st2_approv %>% group_by(dm01_01) %>% mutate(id_row = n()) %>% ungroup() %>% group_by(id_row) %>% tally()
# quite a few people participated multiple times (due to refreshing the link...)
sosci_st2_approv %>% group_by(finished, lastpage) %>% tally() # n=93 rows that can be dropped (incomplete ones that were repeated!)


sosci_st2_approv %>% group_by(dm01_01) %>% mutate(id_row = n()) %>% ungroup() %>% filter(id_row>=2) %>% 
  arrange(id_row, dm01_01, finished) %>% 
  select(dm01_01, time_sum, finished, lastpage, missing, zf02, rs01_10, sb02_01, sa02_01) %>% print(n=Inf)

# distribution across random groups
sosci_st2_approv %>% group_by(zf02) %>% tally()
sosci_st2_approv %>% filter(finished==1) %>% group_by(zf02) %>% tally() # nice!

# sb group
sosci_st2_approv_sb <- sosci_st2_approv %>% filter(zf02==1 | zf02==4)
sosci_st2_approv_sb %>% select(starts_with("bf0")) %>% print(n=Inf)
sosci_st2_approv_sb %>% select(starts_with("sb")) %>% print(n=Inf)

# sa group
sosci_st2_approv_sa <- sosci_st2_approv %>% filter(zf02==2 | zf02==5)
sosci_st2_approv_sa %>% select(starts_with("bf0")) %>% print(n=Inf)
sosci_st2_approv_sa %>% select(starts_with("sa")) %>% print(n=Inf)

# control group
sosci_st2_approv_cg <- sosci_st2_approv %>% filter(zf02==3)
sosci_st2_approv_cg %>% select(starts_with("bf0")) %>% print(n=Inf)

#### check AWAITING REVIEW in SoSci data ####
ids_await <- demog_st2 %>% filter(Status=="AWAITING REVIEW") %>% pull(`Participant id`)

sosci_st2_await <- sosci_st2 %>% filter(dm01_01 %in% ids_await)
sosci_st2_await <- sosci_st2_await %>% group_by(dm01_01) %>% mutate(id_count = n()) %>% ungroup() 

sosci_st2_await %>% group_by(id_count) %>% tally()
# some people participated multiple times (due to refreshing the link...)
sosci_st2_await %>% filter(id_count>1) %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# -> all didn't finish
  
sosci_st2_await %>% group_by(zf02) %>% tally()

# sb group
sosci_st2_await_sb <- sosci_st2_await %>% filter(zf02==1 | zf02==4)
sosci_st2_await_sb %>% select(starts_with("bf0")) %>% print(n=Inf)
sosci_st2_await_sb %>% select(starts_with("sb")) %>% print(n=Inf)
# approve all who finished
sosci_st2_await_sb %>% filter(lastpage==8) %>% select(starts_with("bf0")) %>% print(n=Inf, width=Inf)
sosci_st2_await_sb %>% filter(lastpage==8) %>% select(starts_with("sb")) %>% print(n=Inf, width=Inf)
approve_str <- remove_labels(sosci_st2_await_sb %>% filter(lastpage==8) %>% pull(dm01_01))

# check those who finished on page 7
sosci_st2_await_sb %>% filter(lastpage==7) %>% select(starts_with("bf0")) %>% print(n=Inf, width=Inf)
sosci_st2_await_sb %>% filter(lastpage==7) %>% select(starts_with("sb")) %>% print(n=Inf, width=Inf)
approve_str <- c(approve_str, remove_labels(sosci_st2_await_sb %>% 
                                              filter(lastpage==7 & !is.na(sb02_01)) %>% dplyr::pull(dm01_01)))

# sa group
sosci_st2_await_sa <- sosci_st2_await %>% filter(zf02==2 | zf02==5)
sosci_st2_await_sa %>% select(starts_with("bf0")) %>% print(n=Inf)
sosci_st2_await_sa %>% select(starts_with("sa")) %>% print(n=Inf)
# approve all who finished
sosci_st2_await_sa %>% filter(lastpage==8) %>% select(starts_with("bf0")) %>% print(n=Inf, width=Inf)
sosci_st2_await_sa %>% filter(lastpage==8) %>% select(starts_with("sa")) %>% print(n=Inf, width=Inf)
approve_str <- c(approve_str, remove_labels(sosci_st2_await_sa %>% filter(lastpage==8) %>% pull(dm01_01)))

# check those who finished on page 7
sosci_st2_await_sa %>% filter(lastpage==7) %>% select(starts_with("bf0")) %>% print(n=Inf, width=Inf)
sosci_st2_await_sa %>% filter(lastpage==7) %>% select(starts_with("sa")) %>% print(n=Inf, width=Inf)
approve_str <- c(approve_str, remove_labels(sosci_st2_await_sa %>% 
                                              filter(lastpage==7 & !is.na(sa02_01)) %>% dplyr::pull(dm01_01)))

# control group
sosci_st2_await_cg <- sosci_st2_await %>% filter(zf02==3)
sosci_st2_await_cg %>% select(starts_with("bf0")) %>% print(n=Inf)
# approve all who finished
sosci_st2_await_cg %>% filter(lastpage==8) %>% select(starts_with("bf0")) %>% print(n=Inf, width=Inf)
sosci_st2_await_cg %>% filter(lastpage==8) %>% select(c(starts_with("rs"), starts_with("sc"))) %>% print(n=Inf, width=Inf)
approve_str <- c(approve_str, remove_labels(sosci_st2_await_cg %>% filter(lastpage==8) %>% pull(dm01_01)))
# check those who finished on page 4
sosci_st2_await_cg %>% filter(lastpage==4) %>% select(starts_with("bf0")) %>% print(n=Inf, width=Inf)
sosci_st2_await_cg %>% filter(lastpage==4) %>% select(c(starts_with("rs"), starts_with("sc"))) %>% print(n=Inf, width=Inf)
approve_str <- c(approve_str, remove_labels(sosci_st2_await_cg %>% 
                                              filter(lastpage==4 & !is.na(sc01_12)) %>% dplyr::pull(dm01_01)))

# these will be compensated!
sort(approve_str) %>% unique() 
# these will NOT be compensated!
sort(remove_labels(sosci_st2_await %>% filter(!dm01_01 %in% approve_str) %>% pull(dm01_01))) %>% unique()
  
#### check RETURNED in SoSci data ####
ids_return <- demog_st2 %>% filter(Status=="RETURNED") %>% pull(`Participant id`)
sosci_st2_return <- sosci_st2 %>% filter(dm01_01 %in% ids_return)
sosci_st2_return %>% print(width=Inf) # both not eligible

# these will NOT be compensated!
sort(ids_return) %>% unique() 

#### check single cases from prolific messages ####
"60cce1dff5946c42b4402084" %in% approve_str # approved
"6111d08247ef6871bf11ec6b" %in% approve_str
sosci_st2 %>% filter(dm01_01=="6111d08247ef6871bf11ec6b") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
"611243e1a08d9e3f1528021a" %in% approve_str # approved
"615a28d0a19e964b2f63625c" %in% approve_str # approved
"6393d8ef9d45e4b8822c877a" %in% approve_str 
sosci_st2 %>% filter(dm01_01=="6393d8ef9d45e4b8822c877a") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# -> one of those under "returned" -> also approved
"5d7d467f306346001a7a6844" %in% approve_str 
sosci_st2 %>% filter(dm01_01=="5d7d467f306346001a7a6844") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# -> one of those under "timed out" -> also approved
"6131460946d0022f68082db6" %in% approve_str 
sosci_st2 %>% filter(dm01_01=="6131460946d0022f68082db6") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# -> one of those under "timed out" -> also approved
"61002d85a0e999eed53e30e8" %in% approve_str 
sosci_st2 %>% filter(dm01_01=="61002d85a0e999eed53e30e8") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# -> "timed out”, but no data recorded on SoSci (two empty entries)
"5ecef11b1ed62917125bb673" %in% approve_str 
sosci_st2 %>% filter(dm01_01=="5ecef11b1ed62917125bb673") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# -> one of those under "timed out" -> also approved
"60e18c89acd5fb822f5291db" %in% approve_str 

#### check further timed-out cases ####
sosci_st2 %>% filter(dm01_01=="5eee1b1961f27903554a7bf1") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="61516c7d3c833383901cdb54") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="616550455f8f691a658e06b0") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="6126c02e2dc530e0738d0043") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="611424d6d8ab51566db0adc1") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="611e327c7a46d3814231d581") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="61002d85a0e999eed53e30e8") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed (2x)

sosci_st2 %>% filter(dm01_01=="617035677662e85823da8fba") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="5e431e754bddb5000b1877fb") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5ec48aac33b93f000ab96b7f") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="616c026cadd0c9c23c911037") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# CG -> completed!

sosci_st2 %>% filter(dm01_01=="5f1ad5152f74fe000b4ec14d") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed (2x)

sosci_st2 %>% filter(dm01_01=="5f22f26341e9e8025879eda6") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5fad8f592f488e1271524b06") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="6161c0a63a36cd2224159da5") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="611f4665b4d9519095682405") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="58d11f7947aa1e000104718a") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5ae482e87edeb000014a72b0") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="615572313c3a6169680520dc") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="5d7d467f306346001a7a6844") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# SB -> completed!

sosci_st2 %>% filter(dm01_01=="604a021e3a2335af6dc8695c") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="58df4816aedfd50001526cb9") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5f1c2a59b42e4048b89ad309") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="614dd477b69221b863e025af") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="60ff01c7fcf070c6d7df7068") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="6156fef09d1ab07148870099") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data


#### check again those left awaiting review ####
sosci_st2 %>% filter(dm01_01=="61541ab0876a2758ac6dc72f") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5ef7a1c5939d494c57e89117") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="60d4613121b754739be92895") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="60a408eb4c8c4603f3e29df1") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5eb3a734d249ac18a413063a") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5c840aedbad8fb000106cb44") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5b477fdb56e0140001ac7720") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="60d8ef203a9c6570ff952bb7") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# reached p. 7 but did not fill out intervention, sb02_01

sosci_st2 %>% filter(dm01_01=="5f720f7fb7dd520e26f3c0b3") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="611795067c650aaf70d5c25b") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="6136460eb5fcd7529008ebb8") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="611dde2ebbb7d08531863d6d") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5f070ba7da774e0534090fac") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="611d08ce4a184b5b3334a088") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="612b47fce0d4f1a36ff8a758") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="610a545e57efd9a197696c77") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="61253c280188c5503a64af66") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="63653e83df827cf29079cf92") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="60803263b0176209fa2bf2a5") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="60fb536387af6781abbafe8a") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="61657a9f51cff2ba1e3b4fdf") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="612360cf14256bedc9501fd0") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="60db9412ba928de1fdc2269e") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="610d842e204deeeccfc472d2") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5ccab088f2532b0001b88aca") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="603ab6ce208b756898441c4b") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5fd7c98ecffb0b080c0f1c84") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5f392fae9168cc472f0468f6") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="61079db3e10fdd6232985023") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5fe93b6dad8fba884beb4035") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

sosci_st2 %>% filter(dm01_01=="61013bb782e4014ecabafee1") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="5c75c7c92c662600014eb328") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed

sosci_st2 %>% filter(dm01_01=="6111d08247ef6871bf11ec6b") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not completed (3x)

sosci_st2 %>% filter(dm01_01=="647e5f4c5b7dfbc365335f72") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# not in the data

#### check again after people retook the survey ####

sosci_st2_new <- readxl::read_excel("data/St2_data_PersonalityChange_110923.xlsx", col_names = F, skip = 2, guess_max = 10000) # sosci
var_names <- readxl::read_excel("data/St2_data_PersonalityChange_110923.xlsx", col_names = F)[c(1,2), ]
var_names <- data.frame(t(var_names))
colnames(sosci_st2_new) <- stringr::str_to_lower(var_names$X1) # only lower case pls
# label data 
sosci_st2_new <- labelled::set_variable_labels(sosci_st2_new, .labels = var_names$X2)
# filter irrelevant cases:
sosci_st2_new %>% 
  filter(str_length(dm01_01)>24 | str_length(dm01_01)<24) %>% select(case, dm01_01)
# 610a545   efd9a1976  c77
# 610a545e57efd9a197696c77
# I assume that these are the same -> also confirmed in DM 
sosci_st2_new <- sosci_st2_new %>% 
  mutate(dm01_01 = ifelse(dm01_01=="610a545efd9a1976c77", "610a545e57efd9a197696c77", dm01_01))
                     
sosci_st2_new <- sosci_st2_new %>% filter(str_length(dm01_01)==24)

sosci_st2_new %>% filter(dm01_01=="60db9412ba928de1fdc2269e") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="61253c280188c5503a64af66") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="603ab6ce208b756898441c4b") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5f392fae9168cc472f0468f6") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5b477fdb56e0140001ac7720") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5f070ba7da774e0534090fac") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="612b47fce0d4f1a36ff8a758") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="610d842e204deeeccfc472d2") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="6111d08247ef6871bf11ec6b") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5f720f7fb7dd520e26f3c0b3") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5eb3a734d249ac18a413063a") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="611dde2ebbb7d08531863d6d") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="647e5f4c5b7dfbc365335f72") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="611d08ce4a184b5b3334a088") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="61657a9f51cff2ba1e3b4fdf") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5fd7c98ecffb0b080c0f1c84") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5c840aedbad8fb000106cb44") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="5fe93b6dad8fba884beb4035") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good!

sosci_st2_new %>% filter(dm01_01=="60d4613121b754739be92895") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good now!

sosci_st2_new %>% filter(dm01_01=="60fb536387af6781abbafe8a") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good now!

sosci_st2_new %>% filter(dm01_01=="5ef7a1c5939d494c57e89117") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# no, still incomplete (even though it says "completed")

sosci_st2_new %>% filter(dm01_01=="610a545e57efd9a197696c77") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good now! (this is the corrected ID)

sosci_st2_new %>% filter(dm01_01=="6136460eb5fcd7529008ebb8") %>% select(dm01_01, zf02, time_sum, finished, lastpage, missing)
# yes, looks good now!





