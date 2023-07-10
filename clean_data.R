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
    
df_sbsa <- df_sbsa %>% 
  group_by(dm01_01) %>% 
  mutate(assessments = n()) %>% 
  ungroup() %>% 
  filter(assessments==2) # only keep those with pre and post data -> keep for now!

df_sbsa %>% arrange(dm01_01) %>% select(case, dm01_01, questnnr)
table(df_sbsa$questnnr) 

df_sbsa <- df_sbsa %>% mutate(time = 1) %>% mutate(time = ifelse(questnnr!="Test_t0", 2, time))
table(df_sbsa$time) 

df_sbsa <- df_sbsa %>% 
  group_by(dm01_01) %>% 
  mutate(maxtime = max(time)) %>% 
  ungroup()
df_sbsa %>% filter(maxtime==1) %>% select(case, dm01_01, questnnr) %>% arrange(dm01_01) %>% print(n=Inf) # some have done Test_t0 twice (??)

df_sbsa <- df_sbsa %>% 
  filter(maxtime==2) %>% select(-maxtime)

df_sbsa <- df_sbsa %>% 
  mutate(rando = questnnr) %>% 
  mutate(rando = ifelse(rando=="Test_t0", NA, rando)) %>% 
  arrange(dm01_01, time) %>% 
  tidyr::fill(rando, .direction = "up")

df_sbsa %>% select(case, dm01_01, questnnr, time, rando) %>% print(n=50)

#### checks for careless responding ####

# longstring 

# semantic differential 

# mahalanobis distance

# how many missings in BF

# completion times

#### inspect / recode variables ####

# generate running ID
df_sbsa <- df_sbsa %>% group_by(dm01_01) %>% mutate(pid = cur_group_id()) %>% ungroup()

# 5 item Satisfaction with Life Scale (Diener et al. 1985) 
df_sbsa %>% select(starts_with("sw06"))
df_sbsa %>% group_by(time, rando, sw06_01) %>% tally() %>% print(n=Inf)
alpha.swls <- df_sbsa %>% 
  select(starts_with("sw06")) %>%
  psych::alpha(check.keys = TRUE)
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
df_sbsa$meaning <- df_sbsa %>%
  select(starts_with("ml01")) %>%
  psych::reverse.code(keys = keys_meaning[[1]], items = .) %>%
  rowMeans(na.rm=T)
df_sbsa$meaning[is.nan(df_sbsa$meaning)] <- NA

# 10 item Rosenberg Self-Esteem Scale (Rosenberg et al., 1965) 
df_sbsa %>% select(starts_with("rs01"))
df_sbsa %>% group_by(time, rando, rs01_01) %>% tally() %>% print(n=Inf)
alpha.selfes <- df_sbsa %>% 
  select(starts_with("rs01")) %>%
  psych::alpha(check.keys = TRUE)
df_sbsa$selfes <- df_sbsa %>%
  select(starts_with("rs01")) %>%
  psych::reverse.code(keys=alpha.selfes$keys[[1]], items = .) %>%
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

# create combined variables across the two assessments (but separate for current / ideal)
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
    items_keyed = str_replace(paste0(b5_vars[[i]][[2]], items), "\\+", "")
    # reliability objects
    alpha <- df_sbsa %>% 
      select(all_of(items)) %>%
      psych::alpha(keys = items_keyed)
    eval(call("<-", as.name(paste0("alpha_", short_name, "_", names(bfi_versions)[j])), alpha)) # save to environment
    if (i %in% c(1:5)) { # omega only for B5 traits
      omega <- df_sbsa %>% 
        select(all_of(items)) %>%
        psych::omega(m = ., keys = items_keyed, plot = FALSE, nfactors=3)
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
  
df_sbsa %>% filter(na_comb_curr > 15 | na_comb_ideal > 15) %>% select(pid, time, na_comb_curr, na_comb_ideal)

# profile correlations Big Five - ideal self
# item-level
df_sbsa <- df_sbsa %>% 
  mutate(profile_corr_item = multicon::Profile.r(df_sbsa %>% select(starts_with("bf05")), 
                                                 df_sbsa %>% select(starts_with("bf06")), nomiss = .5))

summary(df_sbsa$profile_corr_item)
df_sbsa %>% filter(is.na(profile_corr_item)) %>% select(pid, time, na_comb_curr, na_comb_ideal)

df_sbsa %>% filter(is.na(profile_corr_item) & na_comb_curr==0 & na_comb_ideal==0) %>% 
  select(pid, time, starts_with("bf05"), starts_with("bf06")) %>% print(width=Inf) # no profile correlation possible -> only answered "3" for ideal (needs to be filtered out above)

df_sbsa %>% filter(profile_corr_item==-1 | profile_corr_item==1) %>% # this person answered exactly the same for current and ideal at pre and post (not very likely...)
  select(pid, time, starts_with("bf05"), starts_with("bf06")) %>% 
  pivot_longer(cols = bf05_01:bf06_60, names_to = c("version", ".value"), names_pattern = "(.)_(.*)") %>% print(width=Inf)

# facet-level
df_sbsa <- df_sbsa %>% 
  mutate(profile_corr_facet = 
           multicon::Profile.r(df_sbsa %>% select(all_of(paste0(str_trunc(names(b5_vars)[6:20], 5, ellipsis = ""), "_comb_curr"))), 
                               df_sbsa %>% select(all_of(paste0(str_trunc(names(b5_vars)[6:20], 5, ellipsis = ""), "_comb_ideal"))), nomiss = .5))
summary(df_sbsa$profile_corr_facet)
df_sbsa %>% filter(is.na(profile_corr_facet)) %>% select(pid, time, na_comb_curr, na_comb_ideal)

# Fisher z transform profile correlations
df_sbsa <- df_sbsa %>% 
  mutate(profile_corr_item = ifelse(profile_corr_item==1, 0.99, profile_corr_item), # solution for now because 1 produces "Inf" error -> case will probably be filtered out, anyways
         profile_corr_facet = ifelse(profile_corr_facet==1, 0.99, profile_corr_facet)) %>% 
  mutate(profile_corr_item_z = correlation::z_fisher(profile_corr_item),
         profile_corr_facet_z = correlation::z_fisher(profile_corr_facet))

# 15 BFI facets - pre - skill building
df_sbsa %>% group_by(time, rando, sb07_01) %>% tally() %>% print(n=Inf)
df_sbsa %>% filter(time==1 & rando=="Skill-Building") %>% select(starts_with("sb07"))

# 15 BFI facets - pre - self acceptance 
df_sbsa %>% group_by(time, rando, sa07_01) %>% tally() %>% print(n=Inf)
df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(starts_with("sa07"))

# 15 BFI facets - post - skill building
df_sbsa %>% group_by(time, rando, sb12_01) %>% tally() %>% print(n=Inf)
df_sbsa %>% filter(time==2 & rando=="Skill-Building") %>% select(starts_with("sb12"))

# 15 BFI facets - post - self acceptance
df_sbsa %>% group_by(time, rando, sa14_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Self-Acceptance") %>% select(starts_with("sa14"))


# Willingness to change - skill building (pre)
str(df_sbsa$sb05) # Do you want to change your personality in general?
table(df_sbsa$sb05) # 1 = yes, 2 = no
df_sbsa %>% group_by(time, rando, sb05) %>% tally() %>% print(n=Inf) 
str(df_sbsa$sb06_01) # How much do you want to change your personality in general?
df_sbsa %>% group_by(time, rando, sb06_01) %>% tally() %>% print(n=Inf) 

# Personal project dimensions - skill building
# pre
str(df_sbsa$sb01_01) # How important is it for you to change your personality?
str(df_sbsa$sb01_02) # How difficult is it for you to work on changing your personality?
df_sbsa %>% group_by(time, rando, sb01_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==1 & rando=="Skill-Building") %>% select(starts_with("sb01"))
# post
str(df_sbsa$sb13_01) # During this study, how important was it for you to change your personality?
str(df_sbsa$sb13_02) # During this study, how difficult was it for you to work on changing your personality?
df_sbsa %>% group_by(time, rando, sb13_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Skill-Building") %>% select(starts_with("sb13"))

# Manipulation check - skill building
str(df_sbsa$sb04_01) # I thought about the ways I would like to change
str(df_sbsa$sb04_02) # I sought out environments that would help me be the kind of person I want to be
str(df_sbsa$sb04_03) # I practiced new habits to achieve my change goals
df_sbsa %>% group_by(time, rando, sb04_01) %>% tally() %>% print(n=Inf) 
df_sbsa %>% filter(time==2 & rando=="Skill-Building") %>% select(starts_with("sb04"))


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

# adjectives - skill building
# Please list five adjectives that describe you and that you would like to accept 
# more than you currently do. Indicate whether you think you are too high or too low in those traits.
str(df_sbsa$sb08_01) 
df_sbsa %>% filter(!is.na(sb08_01)) %>% group_by(time, rando) %>% tally()
df_sbsa %>% filter(time==1 & rando=="Skill-Building") %>% select(starts_with("sb08"))

str(df_sbsa$sb09_01) 
df_sbsa %>% group_by(time, rando, sb09_01) %>% tally() %>% print(n=Inf) # 1 = too low / 2 = too high
df_sbsa %>% filter(time==1 & rando=="Skill-Building") %>% select(starts_with("sb09"))

# adjectives - self acceptance
# Please list five adjectives that describe you and that you would like to accept 
# more than you currently do. Indicate whether you think you are too high or too low in those traits.
str(df_sbsa$sa08_01) 
df_sbsa %>% filter(!is.na(sa08_01)) %>% group_by(time, rando) %>% tally()
df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(starts_with("sa08"))

str(df_sbsa$sa09_01) 
df_sbsa %>% group_by(time, rando, sa09_01) %>% tally() %>% print(n=Inf) # 1 = too low / 2 = too high
df_sbsa %>% filter(time==1 & rando=="Self-Acceptance") %>% select(starts_with("sa09"))

