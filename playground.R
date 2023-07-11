# Playground analyses skillaccept

cormat1 <- cor(df_sbsa[, c("swls", "meaning", "selfes", "concept",
                           "profile_corr_item_z", "profile_corr_facet_z")], use = "pairwise.complete.obs")

rownames(cormat1) <- c("Life Satisfaction", "Meaning in Life", "Self-Esteem" , "Self Concept Clarity", 
                      "Item-Level Profile Corr (z)", "Facet-Level Profile Corr (z)")
colnames(cormat1) <- c("Life Satisfaction", "Meaning in Life", "Self-Esteem" , "Self Concept Clarity", 
                      "Item-Level Profile Corr (z)", "Facet-Level Profile Corr (z)")

# function to create plots
corrplot(cormat1, type = "lower", order = "original", tl.col = "black", tl.srt = 10,
         addCoef.col ='black', number.cex = 0.7, diag = FALSE) # also add numbers


# reshape well-being data

df_sbsa_wide_wb <- df_sbsa %>% 
  arrange(pid, time) %>% 
  select(pid, time, starts_with(c("sw06", "ml01", "rs01", "sc01"))) %>% 
  pivot_wider(names_from = time,
              names_sep = "_t",
              values_from = starts_with(c("sw06", "ml01", "rs01", "sc01")))
colnames(df_sbsa_wide_wb)

# latent change score model

# code from EID course (with indicator-specific latent vars)

swls.latchange <- '
eta1 =~ 1*sw06_01_t1 + lamb1*sw06_02_t1 + lamb2*sw06_03_t1 + lamb3*sw06_04_t1 + lamb4*sw06_05_t1 + 
1*sw06_01_t2 + lamb1*sw06_02_t2 + lamb2*sw06_03_t2 + lamb3*sw06_04_t2 + lamb4*sw06_05_t2
diff =~ 1*sw06_01_t2 + lamb1*sw06_02_t2 + lamb2*sw06_03_t2 + lamb3*sw06_04_t2 + lamb4*sw06_05_t2
IS2  =~ sw06_02_t1 + 1*sw06_02_t2
IS3  =~ sw06_03_t1 + 1*sw06_03_t2
IS4  =~ sw06_04_t1 + 1*sw06_04_t2
IS5  =~ sw06_05_t1 + 1*sw06_05_t2
eta1 ~~0*IS2
eta1 ~~0*IS3
eta1 ~~0*IS4
eta1 ~~0*IS5
diff ~~0*IS2
diff ~~0*IS3
diff ~~0*IS4
diff ~~0*IS5
sw06_01_t1 ~0*1
sw06_02_t1 ~0*1
sw06_03_t1 ~0*1
sw06_04_t1 ~0*1
sw06_05_t1 ~0*1
sw06_01_t2 ~c*1
sw06_02_t2 ~c*1
sw06_03_t2 ~c*1
sw06_04_t2 ~c*1
sw06_05_t2 ~c*1
eta1 ~NA*1 
diff ~NA*1'
fit.swls.latchange <- sem(swls.latchange, data=df_sbsa_wide_wb, meanstructure=T, estimator="MLR", missing="fiml")
summary(fit.swls.latchange, fit.measures=T, standardized=T, rsquare=T, modindices=T, ci=T)


library(lcsm)
mod1 <- specify_uni_lcsm(timepoints = 2,
                 var = "sw06_01_t",  
                 change_letter = "g",
                 model = list(alpha_constant = TRUE, 
                              beta = F, 
                              phi = F))
cat(mod1)
fit1 <- fit_uni_lcsm(data = df_sbsa_wide_wb, 
                     var =  "sw06_01_t",
                     model = list(alpha_constant = TRUE, 
                                  beta = FALSE, 
                                  phi = TRUE))
# error ??

extract_param(fit.swls.latchange, printp = TRUE)
extract_fit(fit.swls.latchange)

# Code snippets adapted from from Kievit et al. (2018) -- CC-BY -- https://doi.org/10.1016/j.dcn.2017.11.007

# Fit the multiple indicator Univariate Latent Change Score model
MILCS <- '
swls_t1 =~ 1*sw06_01_t1 + lamb2*sw06_02_t1 + lamb3*sw06_03_t1 + lamb4*sw06_04_t1 + lamb5*sw06_05_t1 # This specifies the measurement model for swls_t1 
swls_t2 =~ 1*sw06_01_t2 + lamb2*sw06_02_t2 + lamb3*sw06_03_t2 + lamb4*sw06_04_t2 + lamb5*sw06_05_t2 # This specifies the measurement model for swls_t2 with the equality constrained factor loadings

swls_t2 ~ 1*swls_t1     # This parameter regresses swls_t2 perfectly on swls_t1
d_swls_1 =~ 1*swls_t2   # This defines the latent change score factor as measured perfectly by scores on swls_t2
swls_t2 ~ 0*1           # This line constrains the intercept of swls_t2 to 0
swls_t2 ~~ 0*swls_t2    # This fixes the variance of swls_t2 to 0

d_swls_1 ~ 1           # This estimates the intercept of the change score 
swls_t1 ~ 1            # This estimates the intercept of swls_t1 
d_swls_1 ~~ d_swls_1   # This estimates the variance of the change scores 
swls_t1 ~~ swls_t1     # This estimates the variance of the swls_t1 
d_swls_1 ~ swls_t1     # This estimates the self-feedback parameter

sw06_01_t1 ~~ sw06_01_t2   # This allows residual covariance on indicator X1 across T1 and T2
sw06_02_t1 ~~ sw06_02_t2   # This allows residual covariance on indicator X2 across T1 and T2
sw06_03_t1 ~~ sw06_03_t2   # This allows residual covariance on indicator X3 across T1 and T2
sw06_04_t1 ~~ sw06_04_t2   # This allows residual covariance on indicator X4 across T1 and T2
sw06_05_t1 ~~ sw06_05_t2   # This allows residual covariance on indicator X5 across T1 and T2

sw06_01_t1 ~~ res1*sw06_01_t1   # This allows residual variance on indicator X1 at T1 
sw06_02_t1 ~~ res2*sw06_02_t1   # This allows residual variance on indicator X2 at T1
sw06_03_t1 ~~ res3*sw06_03_t1   # This allows residual variance on indicator X3 at T1
sw06_04_t1 ~~ res4*sw06_04_t1   # This allows residual variance on indicator X4 at T1
sw06_05_t1 ~~ res5*sw06_05_t1   # This allows residual variance on indicator X5 at T1

sw06_01_t2 ~~ res1*sw06_01_t2  # This allows residual variance on indicator X1 at T2 
sw06_02_t2 ~~ res2*sw06_02_t2  # This allows residual variance on indicator X2 at T2 
sw06_03_t2 ~~ res3*sw06_03_t2  # This allows residual variance on indicator X3 at T2
sw06_04_t2 ~~ res4*sw06_04_t2  # This allows residual variance on indicator X3 at T2
sw06_05_t2 ~~ res5*sw06_05_t2  # This allows residual variance on indicator X3 at T2

sw06_01_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
sw06_02_t1 ~ m2*1     # This estimates the intercept of X2 at T1
sw06_03_t1 ~ m3*1     # This estimates the intercept of X3 at T1
sw06_04_t1 ~ m4*1     # This estimates the intercept of X2 at T1
sw06_05_t1 ~ m5*1     # This estimates the intercept of X3 at T1
sw06_01_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
sw06_02_t2 ~ m2*1     # This estimates the intercept of X2 at T2
sw06_03_t2 ~ m3*1     # This estimates the intercept of X3 at T2
sw06_04_t2 ~ m4*1     # This estimates the intercept of X2 at T2
sw06_05_t2 ~ m5*1     # This estimates the intercept of X3 at T2
'
fitMILCS <- lavaan(MILCS, data=df_sbsa_wide_wb, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitMILCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

broom::glance(fitMILCS)
broom::tidy(fitMILCS)

# test measurement invariance
# https://quantdev.ssri.psu.edu/sites/qdev/files/LongitudinalMeasurementInvariance_2017_1108.html

library(semTools)
baseline_swls <- 'swls1 =~ sw06_01_t1 + sw06_02_t1 + sw06_03_t1 + sw06_04_t1 + sw06_05_t1 
                  swls2 =~ sw06_01_t2 + sw06_02_t2 + sw06_03_t2 + sw06_04_t2 + sw06_05_t2'

# Configural invariance model
configural_swls <- '
# Define the latent factors
swls1 =~ NA*sw06_01_t1 + lambda1*sw06_01_t1 + sw06_02_t1 + sw06_03_t1 + sw06_04_t1 + sw06_05_t1 
swls2 =~ NA*sw06_01_t2 + lambda1*sw06_01_t2 + sw06_02_t2 + sw06_03_t2 + sw06_04_t2 + sw06_05_t2

# Intercepts
sw06_01_t1 ~ i1*1
sw06_02_t1 ~ 1
sw06_03_t1 ~ 1
sw06_04_t1 ~ 1
sw06_05_t1 ~ 1

sw06_01_t2 ~ i1*1
sw06_02_t2 ~ 1
sw06_03_t2 ~ 1
sw06_04_t2 ~ 1
sw06_05_t2 ~ 1

# Unique Variances
sw06_01_t1 ~~ sw06_01_t1
sw06_02_t1 ~~ sw06_02_t1
sw06_03_t1 ~~ sw06_03_t1
sw06_04_t1 ~~ sw06_04_t1
sw06_05_t1 ~~ sw06_05_t1

sw06_01_t2 ~~ sw06_01_t2
sw06_02_t2 ~~ sw06_02_t2
sw06_03_t2 ~~ sw06_03_t2
sw06_04_t2 ~~ sw06_04_t2
sw06_05_t2 ~~ sw06_05_t2

# Latent Variable Means
swls1 ~ 0*1
swls2 ~ 1

# Latent Variable Variances and Covariance
swls1 ~~ 1*swls1
swls2 ~~ swls2
swls1 ~~ swls2
'
fit_configural_swls <- cfa(configural_swls, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_configural_swls, fit.measures = TRUE)

semPaths(fit_configural_swls, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

# Weak invariance model
weak_swls <- '
# Define the latent factors
swls1 =~ NA*sw06_01_t1 + lambda1*sw06_01_t1 + lambda2*sw06_02_t1 + lambda3*sw06_03_t1 + lambda4*sw06_04_t1 + lambda5*sw06_05_t1 
swls2 =~ NA*sw06_01_t2 + lambda1*sw06_01_t2 + lambda2*sw06_02_t2 + lambda3*sw06_03_t2 + lambda4*sw06_04_t2 + lambda5*sw06_05_t2

# Intercepts
sw06_01_t1 ~ i1*1
sw06_02_t1 ~ 1
sw06_03_t1 ~ 1
sw06_04_t1 ~ 1
sw06_05_t1 ~ 1

sw06_01_t2 ~ i1*1
sw06_02_t2 ~ 1
sw06_03_t2 ~ 1
sw06_04_t2 ~ 1
sw06_05_t2 ~ 1

# Unique Variances
sw06_01_t1 ~~ sw06_01_t1
sw06_02_t1 ~~ sw06_02_t1
sw06_03_t1 ~~ sw06_03_t1
sw06_04_t1 ~~ sw06_04_t1
sw06_05_t1 ~~ sw06_05_t1

sw06_01_t2 ~~ sw06_01_t2
sw06_02_t2 ~~ sw06_02_t2
sw06_03_t2 ~~ sw06_03_t2
sw06_04_t2 ~~ sw06_04_t2
sw06_05_t2 ~~ sw06_05_t2

# Latent Variable Means
swls1 ~ 0*1
swls2 ~ 1

# Latent Variable Variances and Covariance
swls1 ~~ 1*swls1
swls2 ~~ swls2
swls1 ~~ swls2
'
fit_weak_swls <- cfa(weak_swls, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_weak_swls, fit.measures = TRUE)

semPaths(fit_weak_swls, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

# Strong invariance model (additional constraints on manifest intercepts)
strong_swls <- '
# Define the latent factors
swls1 =~ NA*sw06_01_t1 + lambda1*sw06_01_t1 + lambda2*sw06_02_t1 + lambda3*sw06_03_t1 + lambda4*sw06_04_t1 + lambda5*sw06_05_t1 
swls2 =~ NA*sw06_01_t2 + lambda1*sw06_01_t2 + lambda2*sw06_02_t2 + lambda3*sw06_03_t2 + lambda4*sw06_04_t2 + lambda5*sw06_05_t2

# Intercepts
sw06_01_t1 ~ i1*1
sw06_02_t1 ~ i2*1
sw06_03_t1 ~ i3*1
sw06_04_t1 ~ i4*1
sw06_05_t1 ~ i5*1

sw06_01_t2 ~ i1*1
sw06_02_t2 ~ i2*1
sw06_03_t2 ~ i3*1
sw06_04_t2 ~ i4*1
sw06_05_t2 ~ i5*1

# Unique Variances
sw06_01_t1 ~~ sw06_01_t1
sw06_02_t1 ~~ sw06_02_t1
sw06_03_t1 ~~ sw06_03_t1
sw06_04_t1 ~~ sw06_04_t1
sw06_05_t1 ~~ sw06_05_t1

sw06_01_t2 ~~ sw06_01_t2
sw06_02_t2 ~~ sw06_02_t2
sw06_03_t2 ~~ sw06_03_t2
sw06_04_t2 ~~ sw06_04_t2
sw06_05_t2 ~~ sw06_05_t2

# Latent Variable Means
swls1 ~ 0*1
swls2 ~ 1

# Latent Variable Variances and Covariance
swls1 ~~ 1*swls1
swls2 ~~ swls2
swls1 ~~ swls2
'
fit_strong_swls <- cfa(strong_swls, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_strong_swls, fit.measures = TRUE)

semPaths(fit_strong_swls, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)


# Strict invariance model (additional constraints on manifest variances)
strict_swls <- '
# Define the latent factors
swls1 =~ NA*sw06_01_t1 + lambda1*sw06_01_t1 + lambda2*sw06_02_t1 + lambda3*sw06_03_t1 + lambda4*sw06_04_t1 + lambda5*sw06_05_t1 
swls2 =~ NA*sw06_01_t2 + lambda1*sw06_01_t2 + lambda2*sw06_02_t2 + lambda3*sw06_03_t2 + lambda4*sw06_04_t2 + lambda5*sw06_05_t2

# Intercepts
sw06_01_t1 ~ i1*1
sw06_02_t1 ~ i2*1
sw06_03_t1 ~ i3*1
sw06_04_t1 ~ i4*1
sw06_05_t1 ~ i5*1

sw06_01_t2 ~ i1*1
sw06_02_t2 ~ i2*1
sw06_03_t2 ~ i3*1
sw06_04_t2 ~ i4*1
sw06_05_t2 ~ i5*1

# Unique Variances
sw06_01_t1 ~~ u1*sw06_01_t1
sw06_02_t1 ~~ u2*sw06_02_t1
sw06_03_t1 ~~ u3*sw06_03_t1
sw06_04_t1 ~~ u4*sw06_04_t1
sw06_05_t1 ~~ u5*sw06_05_t1

sw06_01_t2 ~~ u1*sw06_01_t2
sw06_02_t2 ~~ u2*sw06_02_t2
sw06_03_t2 ~~ u3*sw06_03_t2
sw06_04_t2 ~~ u4*sw06_04_t2
sw06_05_t2 ~~ u5*sw06_05_t2

# Latent Variable Means
swls1 ~ 0*1
swls2 ~ 1

# Latent Variable Variances and Covariance
swls1 ~~ 1*swls1
swls2 ~~ swls2
swls1 ~~ swls2
'
fit_strict_swls <- cfa(strict_swls, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_strict_swls, fit.measures = TRUE)

semPaths(fit_strict_swls, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

bind_rows(broom::glance(fit_configural_swls) %>% 
            select(nobs, npar, chisq, AIC, BIC, cfi, tli, rmsea, srmr),
          broom::glance(fit_weak_swls) %>% 
            select(nobs, npar, chisq, AIC, BIC, cfi, tli, rmsea, srmr),
          broom::glance(fit_strong_swls) %>% 
            select(nobs, npar, chisq, AIC, BIC, cfi, tli, rmsea, srmr),
          broom::glance(fit_strict_swls) %>% 
            select(nobs, npar, chisq, AIC, BIC, cfi, tli, rmsea, srmr)) %>% 
  mutate(model = c("configural", "weak", "strong", "strict")) %>% 
  select(model, everything())

anova(fit_configural_swls, fit_weak_swls)

anova(fit_weak_swls, fit_strong_swls)

anova(fit_strong_swls, fit_strict_swls)


mod_traits_sqdiff <- df_sbsa_wide_profdiff %>% 
  select(-starts_with("profile")) %>% 
  pivot_longer(-c(pid), 
               names_to = c("test", "time"), values_to = "score", 
               names_pattern = "(.*)_(t1|t2)") %>% 
  filter(!is.na(score)) %>% 
  group_by(pid, test) %>% 
  mutate(assessments = n()) %>% 
  ungroup() %>% 
  filter(assessments==2) %>% 
  select(-assessments) %>% 
  group_nest(test) %>% 
  mutate(t_tests = map(data, ~t.test(score ~ time, data = .x, paired = TRUE))) %>% 
  pull(t_tests) %>% 
  purrr::set_names(names(b5_vars))

mod_traits_sqdiff[[1]] %>% tidy()
