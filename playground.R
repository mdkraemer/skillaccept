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

# adding correlation with manifest change goal variable to the latent change score model:
# Fit the multiple indicator univariate latent change score model
mi_lcs_extra_hyp4 <- '
extra_t1 =~ 1*bf05_01_t1 + lamb2*bf05_06_t1 + lamb3*bf05_11_t1 + lamb4*bf05_16_t1 + lamb5*bf05_21_t1 + lamb6*bf05_26_t1 + lamb7*bf05_31_t1 + lamb8*bf05_36_t1 + lamb9*bf05_41_t1 + lamb10*bf05_46_t1 + lamb11*bf05_51_t1 + lamb12*bf05_56_t1 # This specifies the measurement model for extra_t1 
extra_t2 =~ 1*bf05_01_t2 + lamb2*bf05_06_t2 + lamb3*bf05_11_t2 + lamb4*bf05_16_t2 + lamb5*bf05_21_t2 + lamb6*bf05_26_t2 + lamb7*bf05_31_t2 + lamb8*bf05_36_t2 + lamb9*bf05_41_t2 + lamb10*bf05_46_t2 + lamb11*bf05_51_t2 + lamb12*bf05_56_t2 # This specifies the measurement model for extra_t2 with the equality constrained factor loadings

extra_t2 ~ 1*extra_t1     # This parameter regresses extra_t2 perfectly on extra_t1
d_extra_1 =~ 1*extra_t2   # This defines the latent change score factor as measured perfectly by scores on extra_t2
extra_t2 ~ 0*1            # This line constrains the intercept of extra_t2 to 0
extra_t2 ~~ 0*extra_t2    # This fixes the variance of extra_t2 to 0

d_extra_1 ~ 1              # This estimates the intercept of the change score 
extra_t1 ~ 1               # This estimates the intercept of extra_t1 
d_extra_1 ~~ d_extra_1     # This estimates the variance of the change scores 
extra_t1 ~~ extra_t1       # This estimates the variance of the extra_t1 
d_extra_1 ~ extra_t1       # This estimates the self-feedback parameter

d_extra_1 ~~ sb06_01_t1     # estimates the covariance/correlation with change goal variable

bf05_01_t1 ~~ bf05_01_t2   # This allows residual covariance on indicator X1 across T1 and T2
bf05_06_t1 ~~ bf05_06_t2   # This allows residual covariance on indicator X2 across T1 and T2
bf05_11_t1 ~~ bf05_11_t2   # This allows residual covariance on indicator X3 across T1 and T2
bf05_16_t1 ~~ bf05_16_t2   # This allows residual covariance on indicator X4 across T1 and T2
bf05_21_t1 ~~ bf05_21_t2   # This allows residual covariance on indicator X5 across T1 and T2
bf05_26_t1 ~~ bf05_26_t2   # This allows residual covariance on indicator X6 across T1 and T2
bf05_31_t1 ~~ bf05_31_t2   # This allows residual covariance on indicator X7 across T1 and T2
bf05_36_t1 ~~ bf05_36_t2   # This allows residual covariance on indicator X8 across T1 and T2
bf05_41_t1 ~~ bf05_41_t2   # This allows residual covariance on indicator X9 across T1 and T2
bf05_46_t1 ~~ bf05_46_t2   # This allows residual covariance on indicator X10 across T1 and T2
bf05_51_t1 ~~ bf05_51_t2   # This allows residual covariance on indicator X11 across T1 and T2
bf05_56_t1 ~~ bf05_56_t2   # This allows residual covariance on indicator X12 across T1 and T2

bf05_01_t1 ~~ res1*bf05_01_t1   # This allows residual variance on indicator X1 at T1 
bf05_06_t1 ~~ res2*bf05_06_t1   # This allows residual variance on indicator X2 at T1
bf05_11_t1 ~~ res3*bf05_11_t1   # This allows residual variance on indicator X3 at T1
bf05_16_t1 ~~ res4*bf05_16_t1   # This allows residual variance on indicator X4 at T1
bf05_21_t1 ~~ res5*bf05_21_t1   # This allows residual variance on indicator X5 at T1
bf05_26_t1 ~~ res6*bf05_26_t1   # This allows residual variance on indicator X6 at T1 
bf05_31_t1 ~~ res7*bf05_31_t1   # This allows residual variance on indicator X7 at T1
bf05_36_t1 ~~ res8*bf05_36_t1   # This allows residual variance on indicator X8 at T1
bf05_41_t1 ~~ res9*bf05_41_t1   # This allows residual variance on indicator X9 at T1
bf05_46_t1 ~~ res10*bf05_46_t1  # This allows residual variance on indicator X10 at T1
bf05_51_t1 ~~ res11*bf05_51_t1  # This allows residual variance on indicator X11 at T1
bf05_56_t1 ~~ res12*bf05_56_t1  # This allows residual variance on indicator X12 at T1

bf05_01_t2 ~~ res1*bf05_01_t2  # This allows residual variance on indicator X1 at T2 
bf05_06_t2 ~~ res2*bf05_06_t2  # This allows residual variance on indicator X2 at T2 
bf05_11_t2 ~~ res3*bf05_11_t2  # This allows residual variance on indicator X3 at T2
bf05_16_t2 ~~ res4*bf05_16_t2  # This allows residual variance on indicator X4 at T2
bf05_21_t2 ~~ res5*bf05_21_t2  # This allows residual variance on indicator X5 at T2
bf05_26_t2 ~~ res6*bf05_26_t2  # This allows residual variance on indicator X6 at T2 
bf05_31_t2 ~~ res7*bf05_31_t2  # This allows residual variance on indicator X7 at T2 
bf05_36_t2 ~~ res8*bf05_36_t2  # This allows residual variance on indicator X8 at T2
bf05_41_t2 ~~ res9*bf05_41_t2  # This allows residual variance on indicator X9 at T2
bf05_46_t2 ~~ res10*bf05_46_t2 # This allows residual variance on indicator X10 at T2
bf05_51_t2 ~~ res11*bf05_51_t2 # This allows residual variance on indicator X11 at T2
bf05_56_t2 ~~ res12*bf05_56_t2 # This allows residual variance on indicator X12 at T2

bf05_01_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
bf05_06_t1 ~ m2*1     # This estimates the intercept of X2 at T1
bf05_11_t1 ~ m3*1     # This estimates the intercept of X3 at T1
bf05_16_t1 ~ m4*1     # This estimates the intercept of X4 at T1
bf05_21_t1 ~ m5*1     # This estimates the intercept of X5 at T1
bf05_26_t1 ~ m6*1     # This estimates the intercept of X6 at T1
bf05_31_t1 ~ m7*1     # This estimates the intercept of X7 at T1
bf05_36_t1 ~ m8*1     # This estimates the intercept of X8 at T1
bf05_41_t1 ~ m9*1     # This estimates the intercept of X9 at T1
bf05_46_t1 ~ m10*1    # This estimates the intercept of X10 at T1
bf05_51_t1 ~ m11*1    # This estimates the intercept of X11 at T1
bf05_56_t1 ~ m12*1    # This estimates the intercept of X12 at T1

bf05_01_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
bf05_06_t2 ~ m2*1     # This estimates the intercept of X2 at T2
bf05_11_t2 ~ m3*1     # This estimates the intercept of X3 at T2
bf05_16_t2 ~ m4*1     # This estimates the intercept of X4 at T2
bf05_21_t2 ~ m5*1     # This estimates the intercept of X5 at T2
bf05_26_t2 ~ m6*1     # This estimates the intercept of X6 at T2
bf05_31_t2 ~ m7*1     # This estimates the intercept of X7 at T2
bf05_36_t2 ~ m8*1     # This estimates the intercept of X8 at T2
bf05_41_t2 ~ m9*1     # This estimates the intercept of X9 at T2
bf05_46_t2 ~ m10*1    # This estimates the intercept of X10 at T2
bf05_51_t2 ~ m11*1    # This estimates the intercept of X11 at T2
bf05_56_t2 ~ m12*1    # This estimates the intercept of X12 at T2

sb06_01_t1 ~~ sb06_01_t1
sb06_01_t1 ~ 1
'
fit_mi_lcs_extra_hyp4 <- lavaan(mi_lcs_extra_hyp4, data=df_sbsa_wide_pers_sb, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(fit_mi_lcs_extra_hyp4, fit.measures=TRUE, standardized=TRUE, rsquare=F)

# adding correlation with manifest change goal variable to the latent change score model:
# Fit the multiple indicator univariate latent change score model
mi_lcs_extra_curr_specif_hyp4 <- '
extra_t1 =~ 1*bf05_01_t1 + lamb2*bf05_06_t1 + lamb3*bf05_11_t1 + lamb4*bf05_16_t1 + lamb5*bf05_21_t1 + lamb6*bf05_26_t1 + lamb7*bf05_31_t1 + lamb8*bf05_36_t1 + lamb9*bf05_41_t1 + lamb10*bf05_46_t1 + lamb11*bf05_51_t1 + lamb12*bf05_56_t1 # This specifies the measurement model for extra_t1 
extra_t2 =~ 1*bf05_01_t2 + lamb2*bf05_06_t2 + lamb3*bf05_11_t2 + lamb4*bf05_16_t2 + lamb5*bf05_21_t2 + lamb6*bf05_26_t2 + lamb7*bf05_31_t2 + lamb8*bf05_36_t2 + lamb9*bf05_41_t2 + lamb10*bf05_46_t2 + lamb11*bf05_51_t2 + lamb12*bf05_56_t2 # This specifies the measurement model for extra_t2 with the equality constrained factor loadings

goals =~ 1*sb07_01_t1 + sb07_02_t1 + sb07_03_t1 # latent change goal variable (three facets per trait)

extra_t2 ~ 1*extra_t1     # This parameter regresses extra_t2 perfectly on extra_t1
d_extra_1 =~ 1*extra_t2   # This defines the latent change score factor as measured perfectly by scores on extra_t2
extra_t2 ~ 0*1            # This line constrains the intercept of extra_t2 to 0
extra_t2 ~~ 0*extra_t2    # This fixes the variance of extra_t2 to 0

d_extra_1 ~ 1              # This estimates the intercept of the change score 
extra_t1 ~ 1               # This estimates the intercept of extra_t1 
d_extra_1 ~~ d_extra_1     # This estimates the variance of the change scores 
extra_t1 ~~ extra_t1       # This estimates the variance of the extra_t1 
d_extra_1 ~ extra_t1       # This estimates the self-feedback parameter

d_extra_1 ~~ goals     # estimates the covariance/correlation with the (latent) change goal variable
goals ~ 0*1            # This fixes the intercept of the (latent) change goal variable to 0
goals ~~ goals         # This estimates the variance of the (latent) change goal variable

bf05_01_t1 ~~ bf05_01_t2   # This allows residual covariance on indicator X1 across T1 and T2
bf05_06_t1 ~~ bf05_06_t2   # This allows residual covariance on indicator X2 across T1 and T2
bf05_11_t1 ~~ bf05_11_t2   # This allows residual covariance on indicator X3 across T1 and T2
bf05_16_t1 ~~ bf05_16_t2   # This allows residual covariance on indicator X4 across T1 and T2
bf05_21_t1 ~~ bf05_21_t2   # This allows residual covariance on indicator X5 across T1 and T2
bf05_26_t1 ~~ bf05_26_t2   # This allows residual covariance on indicator X6 across T1 and T2
bf05_31_t1 ~~ bf05_31_t2   # This allows residual covariance on indicator X7 across T1 and T2
bf05_36_t1 ~~ bf05_36_t2   # This allows residual covariance on indicator X8 across T1 and T2
bf05_41_t1 ~~ bf05_41_t2   # This allows residual covariance on indicator X9 across T1 and T2
bf05_46_t1 ~~ bf05_46_t2   # This allows residual covariance on indicator X10 across T1 and T2
bf05_51_t1 ~~ bf05_51_t2   # This allows residual covariance on indicator X11 across T1 and T2
bf05_56_t1 ~~ bf05_56_t2   # This allows residual covariance on indicator X12 across T1 and T2

bf05_01_t1 ~~ res1*bf05_01_t1   # This allows residual variance on indicator X1 at T1 
bf05_06_t1 ~~ res2*bf05_06_t1   # This allows residual variance on indicator X2 at T1
bf05_11_t1 ~~ res3*bf05_11_t1   # This allows residual variance on indicator X3 at T1
bf05_16_t1 ~~ res4*bf05_16_t1   # This allows residual variance on indicator X4 at T1
bf05_21_t1 ~~ res5*bf05_21_t1   # This allows residual variance on indicator X5 at T1
bf05_26_t1 ~~ res6*bf05_26_t1   # This allows residual variance on indicator X6 at T1 
bf05_31_t1 ~~ res7*bf05_31_t1   # This allows residual variance on indicator X7 at T1
bf05_36_t1 ~~ res8*bf05_36_t1   # This allows residual variance on indicator X8 at T1
bf05_41_t1 ~~ res9*bf05_41_t1   # This allows residual variance on indicator X9 at T1
bf05_46_t1 ~~ res10*bf05_46_t1  # This allows residual variance on indicator X10 at T1
bf05_51_t1 ~~ res11*bf05_51_t1  # This allows residual variance on indicator X11 at T1
bf05_56_t1 ~~ res12*bf05_56_t1  # This allows residual variance on indicator X12 at T1

bf05_01_t2 ~~ res1*bf05_01_t2  # This allows residual variance on indicator X1 at T2 
bf05_06_t2 ~~ res2*bf05_06_t2  # This allows residual variance on indicator X2 at T2 
bf05_11_t2 ~~ res3*bf05_11_t2  # This allows residual variance on indicator X3 at T2
bf05_16_t2 ~~ res4*bf05_16_t2  # This allows residual variance on indicator X4 at T2
bf05_21_t2 ~~ res5*bf05_21_t2  # This allows residual variance on indicator X5 at T2
bf05_26_t2 ~~ res6*bf05_26_t2  # This allows residual variance on indicator X6 at T2 
bf05_31_t2 ~~ res7*bf05_31_t2  # This allows residual variance on indicator X7 at T2 
bf05_36_t2 ~~ res8*bf05_36_t2  # This allows residual variance on indicator X8 at T2
bf05_41_t2 ~~ res9*bf05_41_t2  # This allows residual variance on indicator X9 at T2
bf05_46_t2 ~~ res10*bf05_46_t2 # This allows residual variance on indicator X10 at T2
bf05_51_t2 ~~ res11*bf05_51_t2 # This allows residual variance on indicator X11 at T2
bf05_56_t2 ~~ res12*bf05_56_t2 # This allows residual variance on indicator X12 at T2

bf05_01_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
bf05_06_t1 ~ m2*1     # This estimates the intercept of X2 at T1
bf05_11_t1 ~ m3*1     # This estimates the intercept of X3 at T1
bf05_16_t1 ~ m4*1     # This estimates the intercept of X4 at T1
bf05_21_t1 ~ m5*1     # This estimates the intercept of X5 at T1
bf05_26_t1 ~ m6*1     # This estimates the intercept of X6 at T1
bf05_31_t1 ~ m7*1     # This estimates the intercept of X7 at T1
bf05_36_t1 ~ m8*1     # This estimates the intercept of X8 at T1
bf05_41_t1 ~ m9*1     # This estimates the intercept of X9 at T1
bf05_46_t1 ~ m10*1    # This estimates the intercept of X10 at T1
bf05_51_t1 ~ m11*1    # This estimates the intercept of X11 at T1
bf05_56_t1 ~ m12*1    # This estimates the intercept of X12 at T1

bf05_01_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
bf05_06_t2 ~ m2*1     # This estimates the intercept of X2 at T2
bf05_11_t2 ~ m3*1     # This estimates the intercept of X3 at T2
bf05_16_t2 ~ m4*1     # This estimates the intercept of X4 at T2
bf05_21_t2 ~ m5*1     # This estimates the intercept of X5 at T2
bf05_26_t2 ~ m6*1     # This estimates the intercept of X6 at T2
bf05_31_t2 ~ m7*1     # This estimates the intercept of X7 at T2
bf05_36_t2 ~ m8*1     # This estimates the intercept of X8 at T2
bf05_41_t2 ~ m9*1     # This estimates the intercept of X9 at T2
bf05_46_t2 ~ m10*1    # This estimates the intercept of X10 at T2
bf05_51_t2 ~ m11*1    # This estimates the intercept of X11 at T2
bf05_56_t2 ~ m12*1    # This estimates the intercept of X12 at T2

sb07_01_t1 ~~ sb07_01_t1
sb07_02_t1 ~~ sb07_02_t1
sb07_03_t1 ~~ sb07_03_t1

sb07_01_t1 ~ 1
sb07_02_t1 ~ 1
sb07_03_t1 ~ 1
'
fit_mi_lcs_extra_curr_specif_hyp4 <- lavaan(mi_lcs_extra_curr_specif_hyp4, data=df_sbsa_wide_pers_sb, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(fit_mi_lcs_extra_curr_specif_hyp4, fit.measures=TRUE, standardized=TRUE, rsquare=F)

semPaths(fit_mi_lcs_extra_curr_specif_hyp4, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

m1a  <- ' goals  =~ sb07_01_t1 + sb07_02_t1 + sb07_03_t1'
onefac3items_a <- cfa(m1a, data=df_sbsa_wide_pers_sb, meanstructure = TRUE) 
summary(onefac3items_a, fit.measures=TRUE, standardized=TRUE) 

# adding correlation with manifest change goal variable to the latent change score model:
# Fit the multiple indicator univariate latent change score model
# --> alternative to latent variable of goals: mean score 
df_sbsa_wide_pers_sb$goals_e <- rowMeans(df_sbsa_wide_pers_sb[, c("sb07_01_t1", "sb07_02_t1", "sb07_03_t1")], na.rm = T)
summary(df_sbsa_wide_pers_sb$goals_e)

mi_lcs_extra_curr_specif_hyp4_alt <- '
extra_t1 =~ 1*bf05_01_t1 + lamb2*bf05_06_t1 + lamb3*bf05_11_t1 + lamb4*bf05_16_t1 + lamb5*bf05_21_t1 + lamb6*bf05_26_t1 + lamb7*bf05_31_t1 + lamb8*bf05_36_t1 + lamb9*bf05_41_t1 + lamb10*bf05_46_t1 + lamb11*bf05_51_t1 + lamb12*bf05_56_t1 # This specifies the measurement model for extra_t1 
extra_t2 =~ 1*bf05_01_t2 + lamb2*bf05_06_t2 + lamb3*bf05_11_t2 + lamb4*bf05_16_t2 + lamb5*bf05_21_t2 + lamb6*bf05_26_t2 + lamb7*bf05_31_t2 + lamb8*bf05_36_t2 + lamb9*bf05_41_t2 + lamb10*bf05_46_t2 + lamb11*bf05_51_t2 + lamb12*bf05_56_t2 # This specifies the measurement model for extra_t2 with the equality constrained factor loadings

extra_t2 ~ 1*extra_t1     # This parameter regresses extra_t2 perfectly on extra_t1
d_extra_1 =~ 1*extra_t2   # This defines the latent change score factor as measured perfectly by scores on extra_t2
extra_t2 ~ 0*1            # This line constrains the intercept of extra_t2 to 0
extra_t2 ~~ 0*extra_t2    # This fixes the variance of extra_t2 to 0

d_extra_1 ~ 1              # This estimates the intercept of the change score 
extra_t1 ~ 1               # This estimates the intercept of extra_t1 
d_extra_1 ~~ d_extra_1     # This estimates the variance of the change scores 
extra_t1 ~~ extra_t1       # This estimates the variance of the extra_t1 
d_extra_1 ~ extra_t1       # This estimates the self-feedback parameter

d_extra_1 ~~ goals_e     # estimates the covariance/correlation with change goal variable

bf05_01_t1 ~~ bf05_01_t2   # This allows residual covariance on indicator X1 across T1 and T2
bf05_06_t1 ~~ bf05_06_t2   # This allows residual covariance on indicator X2 across T1 and T2
bf05_11_t1 ~~ bf05_11_t2   # This allows residual covariance on indicator X3 across T1 and T2
bf05_16_t1 ~~ bf05_16_t2   # This allows residual covariance on indicator X4 across T1 and T2
bf05_21_t1 ~~ bf05_21_t2   # This allows residual covariance on indicator X5 across T1 and T2
bf05_26_t1 ~~ bf05_26_t2   # This allows residual covariance on indicator X6 across T1 and T2
bf05_31_t1 ~~ bf05_31_t2   # This allows residual covariance on indicator X7 across T1 and T2
bf05_36_t1 ~~ bf05_36_t2   # This allows residual covariance on indicator X8 across T1 and T2
bf05_41_t1 ~~ bf05_41_t2   # This allows residual covariance on indicator X9 across T1 and T2
bf05_46_t1 ~~ bf05_46_t2   # This allows residual covariance on indicator X10 across T1 and T2
bf05_51_t1 ~~ bf05_51_t2   # This allows residual covariance on indicator X11 across T1 and T2
bf05_56_t1 ~~ bf05_56_t2   # This allows residual covariance on indicator X12 across T1 and T2

bf05_01_t1 ~~ res1*bf05_01_t1   # This allows residual variance on indicator X1 at T1 
bf05_06_t1 ~~ res2*bf05_06_t1   # This allows residual variance on indicator X2 at T1
bf05_11_t1 ~~ res3*bf05_11_t1   # This allows residual variance on indicator X3 at T1
bf05_16_t1 ~~ res4*bf05_16_t1   # This allows residual variance on indicator X4 at T1
bf05_21_t1 ~~ res5*bf05_21_t1   # This allows residual variance on indicator X5 at T1
bf05_26_t1 ~~ res6*bf05_26_t1   # This allows residual variance on indicator X6 at T1 
bf05_31_t1 ~~ res7*bf05_31_t1   # This allows residual variance on indicator X7 at T1
bf05_36_t1 ~~ res8*bf05_36_t1   # This allows residual variance on indicator X8 at T1
bf05_41_t1 ~~ res9*bf05_41_t1   # This allows residual variance on indicator X9 at T1
bf05_46_t1 ~~ res10*bf05_46_t1  # This allows residual variance on indicator X10 at T1
bf05_51_t1 ~~ res11*bf05_51_t1  # This allows residual variance on indicator X11 at T1
bf05_56_t1 ~~ res12*bf05_56_t1  # This allows residual variance on indicator X12 at T1

bf05_01_t2 ~~ res1*bf05_01_t2  # This allows residual variance on indicator X1 at T2 
bf05_06_t2 ~~ res2*bf05_06_t2  # This allows residual variance on indicator X2 at T2 
bf05_11_t2 ~~ res3*bf05_11_t2  # This allows residual variance on indicator X3 at T2
bf05_16_t2 ~~ res4*bf05_16_t2  # This allows residual variance on indicator X4 at T2
bf05_21_t2 ~~ res5*bf05_21_t2  # This allows residual variance on indicator X5 at T2
bf05_26_t2 ~~ res6*bf05_26_t2  # This allows residual variance on indicator X6 at T2 
bf05_31_t2 ~~ res7*bf05_31_t2  # This allows residual variance on indicator X7 at T2 
bf05_36_t2 ~~ res8*bf05_36_t2  # This allows residual variance on indicator X8 at T2
bf05_41_t2 ~~ res9*bf05_41_t2  # This allows residual variance on indicator X9 at T2
bf05_46_t2 ~~ res10*bf05_46_t2 # This allows residual variance on indicator X10 at T2
bf05_51_t2 ~~ res11*bf05_51_t2 # This allows residual variance on indicator X11 at T2
bf05_56_t2 ~~ res12*bf05_56_t2 # This allows residual variance on indicator X12 at T2

bf05_01_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
bf05_06_t1 ~ m2*1     # This estimates the intercept of X2 at T1
bf05_11_t1 ~ m3*1     # This estimates the intercept of X3 at T1
bf05_16_t1 ~ m4*1     # This estimates the intercept of X4 at T1
bf05_21_t1 ~ m5*1     # This estimates the intercept of X5 at T1
bf05_26_t1 ~ m6*1     # This estimates the intercept of X6 at T1
bf05_31_t1 ~ m7*1     # This estimates the intercept of X7 at T1
bf05_36_t1 ~ m8*1     # This estimates the intercept of X8 at T1
bf05_41_t1 ~ m9*1     # This estimates the intercept of X9 at T1
bf05_46_t1 ~ m10*1    # This estimates the intercept of X10 at T1
bf05_51_t1 ~ m11*1    # This estimates the intercept of X11 at T1
bf05_56_t1 ~ m12*1    # This estimates the intercept of X12 at T1

bf05_01_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
bf05_06_t2 ~ m2*1     # This estimates the intercept of X2 at T2
bf05_11_t2 ~ m3*1     # This estimates the intercept of X3 at T2
bf05_16_t2 ~ m4*1     # This estimates the intercept of X4 at T2
bf05_21_t2 ~ m5*1     # This estimates the intercept of X5 at T2
bf05_26_t2 ~ m6*1     # This estimates the intercept of X6 at T2
bf05_31_t2 ~ m7*1     # This estimates the intercept of X7 at T2
bf05_36_t2 ~ m8*1     # This estimates the intercept of X8 at T2
bf05_41_t2 ~ m9*1     # This estimates the intercept of X9 at T2
bf05_46_t2 ~ m10*1    # This estimates the intercept of X10 at T2
bf05_51_t2 ~ m11*1    # This estimates the intercept of X11 at T2
bf05_56_t2 ~ m12*1    # This estimates the intercept of X12 at T2

goals_e ~~ goals_e
goals_e ~ 1
'
fit_mi_lcs_extra_curr_specif_hyp4_alt <- lavaan(mi_lcs_extra_curr_specif_hyp4_alt, data=df_sbsa_wide_pers_sb, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(fit_mi_lcs_extra_curr_specif_hyp4_alt, fit.measures=TRUE, standardized=TRUE, rsquare=F)



# loop over facets within one go:

facet_template <- '
facet_t1 =~ 1*ind1_t1 + lamb2*ind2_t1 + lamb3*ind3_t1 + lamb4*ind4_t1 # This specifies the measurement model for facet at T1
facet_t2 =~ 1*ind1_t2 + lamb2*ind2_t2 + lamb3*ind3_t2 + lamb4*ind4_t2 # This specifies the measurement model for facet at T2 (with equality constraints)

facet_t2 ~ 1*facet_t1     # This parameter regresses facet_t2 perfectly on facet_t1
d_facet_1 =~ 1*facet_t2   # This defines the latent change score factor as measured perfectly by scores on facet_t2
facet_t2 ~ 0*1            # This line constrains the intercept of facet_t2 to 0
facet_t2 ~~ 0*facet_t2    # This fixes the variance of facet_t2 to 0

d_facet_1 ~ 1              # This estimates the intercept of the change score 
facet_t1 ~ 1               # This estimates the intercept of facet_t1 
d_facet_1 ~~ d_facet_1     # This estimates the variance of the change scores 
facet_t1 ~~ facet_t1       # This estimates the variance of facet_t1 
d_facet_1 ~ facet_t1       # This estimates the self-feedback parameter

d_facet_1 ~~ ind_goal     # estimates the covariance/correlation with change goal variable

ind1_t1 ~~ ind1_t2   # This allows residual covariance on indicator X1 across T1 and T2
ind2_t1 ~~ ind2_t2   # This allows residual covariance on indicator X2 across T1 and T2
ind3_t1 ~~ ind3_t2   # This allows residual covariance on indicator X3 across T1 and T2
ind4_t1 ~~ ind4_t2   # This allows residual covariance on indicator X4 across T1 and T2

ind1_t1 ~~ res1*ind1_t1   # This allows residual variance on indicator X1 at T1 
ind2_t1 ~~ res2*ind2_t1   # This allows residual variance on indicator X2 at T1
ind3_t1 ~~ res3*ind3_t1   # This allows residual variance on indicator X3 at T1
ind4_t1 ~~ res4*ind4_t1   # This allows residual variance on indicator X4 at T1

ind1_t2 ~~ res1*ind1_t2  # This allows residual variance on indicator X1 at T2 
ind2_t2 ~~ res2*ind2_t2  # This allows residual variance on indicator X2 at T2 
ind3_t2 ~~ res3*ind3_t2  # This allows residual variance on indicator X3 at T2
ind4_t2 ~~ res4*ind4_t2  # This allows residual variance on indicator X4 at T2

ind1_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
ind2_t1 ~ m2*1     # This estimates the intercept of X2 at T1
ind3_t1 ~ m3*1     # This estimates the intercept of X3 at T1
ind4_t1 ~ m4*1     # This estimates the intercept of X4 at T1

ind1_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
ind2_t2 ~ m2*1     # This estimates the intercept of X2 at T2
ind3_t2 ~ m3*1     # This estimates the intercept of X3 at T2
ind4_t2 ~ m4*1     # This estimates the intercept of X4 at T2

ind_goal ~~ ind_goal
ind_goal ~ 1
'
mi_lcs_sociab_curr_hyp4 <- str_replace_all(facet_template, 
                                           c("facet" = str_trunc(names(b5_vars)[6], 5, ellipsis = ""),
                                             "ind1" = paste0("bf05", b5_vars[6][[1]][[1]][1]),
                                             "ind2" = paste0("bf05", b5_vars[6][[1]][[1]][2]),
                                             "ind3" = paste0("bf05", b5_vars[6][[1]][[1]][3]),
                                             "ind4" = paste0("bf05", b5_vars[6][[1]][[1]][4]),
                                             "ind_goal" = "sb06_01_t1"))
fit_mi_lcs_sociab_curr_hyp4 <- lavaan(mi_lcs_sociab_curr_hyp4, data=df_sbsa_wide_pers_sb, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(fit_mi_lcs_sociab_curr_hyp4, fit.measures=TRUE, standardized=TRUE, rsquare=F)

# loop across 15 facets
for (i in 6:length(b5_vars)) {
  item_nrs = b5_vars[[i]][[1]]
  short_name = str_trunc(names(b5_vars)[i], 5, ellipsis = "")
  # loop across 2 BFI versions (combined pre&post current/ideal)
  for (j in 5:length(bfi_versions)) {
    items = paste0(bfi_versions[[j]], item_nrs)
    # loop across 2 different goal operationalizations (sb06_01_t1 & sb07_XX_t1)
    for (k in 1:2) {
      if (k==1) {
        goal_op = "sb06_01_t1"
      } else{
        goal_op = paste0("sb07_", str_pad(i-5, 2, pad = "0"), "_t1")
      }
      template_filled <- str_replace_all(facet_template, 
                                         c("facet" = short_name,
                                           "ind1" = items[1], "ind2" = items[2], "ind3" = items[3], "ind4" = items[4],
                                           "ind_goal" = goal_op))
      facet_model_fit <- lavaan(template_filled, data=df_sbsa_wide_pers_sb, estimator='mlr', fixed.x=FALSE, missing='fiml')
      # save to environment
      if (k==1) {
        eval(call("<-", as.name(paste0("fit_mi_lcs_", short_name, "_", 
                                       str_sub(names(bfi_versions)[j], 6), "_hyp4")), facet_model_fit))
      } else{
        eval(call("<-", as.name(paste0("fit_mi_lcs_", short_name, "_", 
                                       str_sub(names(bfi_versions)[j], 6), "_specif_hyp4")), facet_model_fit))
      }
    }
  }
}  


# THIS MODEL DID NOT CONVERGE:
  
# Fit the multiple indicator univariate latent change score model
mi_lcs_openn_ideal_hyp5 <- '
openn_t1 =~ 1*bf06_05_t1 + lamb2*bf06_10_t1 + lamb3*bf06_15_t1 + lamb4*bf06_20_t1 + lamb5*bf06_25_t1 + lamb6*bf06_30_t1 + lamb7*bf06_35_t1 + lamb8*bf06_40_t1 + lamb9*bf06_45_t1 + lamb10*bf06_50_t1 + lamb11*bf06_55_t1 + lamb12*bf06_60_t1 # This specifies the measurement model for openn_t1 
openn_t2 =~ 1*bf06_05_t2 + lamb2*bf06_10_t2 + lamb3*bf06_15_t2 + lamb4*bf06_20_t2 + lamb5*bf06_25_t2 + lamb6*bf06_30_t2 + lamb7*bf06_35_t2 + lamb8*bf06_40_t2 + lamb9*bf06_45_t2 + lamb10*bf06_50_t2 + lamb11*bf06_55_t2 + lamb12*bf06_60_t2 # This specifies the measurement model for openn_t2 with the equality constrained factor loadings

openn_t2 ~ 1*openn_t1     # This parameter regresses openn_t2 perfectly on openn_t1
d_openn_1 =~ 1*openn_t2   # This defines the latent change score factor as measured perfectly by scores on openn_t2
openn_t2 ~ 0*1            # This line constrains the intercept of openn_t2 to 0
openn_t2 ~~ 0*openn_t2    # This fixes the variance of openn_t2 to 0

d_openn_1 ~ 1              # This estimates the intercept of the change score 
openn_t1 ~ 1               # This estimates the intercept of openn_t1 
d_openn_1 ~~ d_openn_1     # This estimates the variance of the change scores 
openn_t1 ~~ openn_t1       # This estimates the variance of the openn_t1 
d_openn_1 ~ openn_t1       # This estimates the self-feedback parameter

d_openn_1 ~~ sa06_01_t1     # estimates the covariance/correlation with acceptance goal variable

bf06_05_t1 ~~ bf06_05_t2   # This allows residual covariance on indicator X1 across T1 and T2
bf06_10_t1 ~~ bf06_10_t2   # This allows residual covariance on indicator X2 across T1 and T2
bf06_15_t1 ~~ bf06_15_t2   # This allows residual covariance on indicator X3 across T1 and T2
bf06_20_t1 ~~ bf06_20_t2   # This allows residual covariance on indicator X4 across T1 and T2
bf06_25_t1 ~~ bf06_25_t2   # This allows residual covariance on indicator X5 across T1 and T2
bf06_30_t1 ~~ bf06_30_t2   # This allows residual covariance on indicator X6 across T1 and T2
bf06_35_t1 ~~ bf06_35_t2   # This allows residual covariance on indicator X7 across T1 and T2
bf06_40_t1 ~~ bf06_40_t2   # This allows residual covariance on indicator X8 across T1 and T2
bf06_45_t1 ~~ bf06_45_t2   # This allows residual covariance on indicator X9 across T1 and T2
bf06_50_t1 ~~ bf06_50_t2   # This allows residual covariance on indicator X10 across T1 and T2
bf06_55_t1 ~~ bf06_55_t2   # This allows residual covariance on indicator X11 across T1 and T2
bf06_60_t1 ~~ bf06_60_t2   # This allows residual covariance on indicator X12 across T1 and T2

bf06_05_t1 ~~ res1*bf06_05_t1   # This allows residual variance on indicator X1 at T1 
bf06_10_t1 ~~ res2*bf06_10_t1   # This allows residual variance on indicator X2 at T1
bf06_15_t1 ~~ res3*bf06_15_t1   # This allows residual variance on indicator X3 at T1
bf06_20_t1 ~~ res4*bf06_20_t1   # This allows residual variance on indicator X4 at T1
bf06_25_t1 ~~ res5*bf06_25_t1   # This allows residual variance on indicator X5 at T1
bf06_30_t1 ~~ res6*bf06_30_t1   # This allows residual variance on indicator X6 at T1 
bf06_35_t1 ~~ res7*bf06_35_t1   # This allows residual variance on indicator X7 at T1
bf06_40_t1 ~~ res8*bf06_40_t1   # This allows residual variance on indicator X8 at T1
bf06_45_t1 ~~ res9*bf06_45_t1   # This allows residual variance on indicator X9 at T1
bf06_50_t1 ~~ res10*bf06_50_t1  # This allows residual variance on indicator X10 at T1
bf06_55_t1 ~~ res11*bf06_55_t1  # This allows residual variance on indicator X11 at T1
bf06_60_t1 ~~ res12*bf06_60_t1  # This allows residual variance on indicator X12 at T1

bf06_05_t2 ~~ res1*bf06_05_t2  # This allows residual variance on indicator X1 at T2 
bf06_10_t2 ~~ res2*bf06_10_t2  # This allows residual variance on indicator X2 at T2 
bf06_15_t2 ~~ res3*bf06_15_t2  # This allows residual variance on indicator X3 at T2
bf06_20_t2 ~~ res4*bf06_20_t2  # This allows residual variance on indicator X4 at T2
bf06_25_t2 ~~ res5*bf06_25_t2  # This allows residual variance on indicator X5 at T2
bf06_30_t2 ~~ res6*bf06_30_t2  # This allows residual variance on indicator X6 at T2 
bf06_35_t2 ~~ res7*bf06_35_t2  # This allows residual variance on indicator X7 at T2 
bf06_40_t2 ~~ res8*bf06_40_t2  # This allows residual variance on indicator X8 at T2
bf06_45_t2 ~~ res9*bf06_45_t2  # This allows residual variance on indicator X9 at T2
bf06_50_t2 ~~ res10*bf06_50_t2 # This allows residual variance on indicator X10 at T2
bf06_55_t2 ~~ res11*bf06_55_t2 # This allows residual variance on indicator X11 at T2
bf06_60_t2 ~~ res12*bf06_60_t2 # This allows residual variance on indicator X12 at T2

bf06_05_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
bf06_10_t1 ~ m2*1     # This estimates the intercept of X2 at T1
bf06_15_t1 ~ m3*1     # This estimates the intercept of X3 at T1
bf06_20_t1 ~ m4*1     # This estimates the intercept of X4 at T1
bf06_25_t1 ~ m5*1     # This estimates the intercept of X5 at T1
bf06_30_t1 ~ m6*1     # This estimates the intercept of X6 at T1
bf06_35_t1 ~ m7*1     # This estimates the intercept of X7 at T1
bf06_40_t1 ~ m8*1     # This estimates the intercept of X8 at T1
bf06_45_t1 ~ m9*1     # This estimates the intercept of X9 at T1
bf06_50_t1 ~ m10*1    # This estimates the intercept of X10 at T1
bf06_55_t1 ~ m11*1    # This estimates the intercept of X11 at T1
bf06_60_t1 ~ m12*1    # This estimates the intercept of X12 at T1

bf06_05_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
bf06_10_t2 ~ m2*1     # This estimates the intercept of X2 at T2
bf06_15_t2 ~ m3*1     # This estimates the intercept of X3 at T2
bf06_20_t2 ~ m4*1     # This estimates the intercept of X4 at T2
bf06_25_t2 ~ m5*1     # This estimates the intercept of X5 at T2
bf06_30_t2 ~ m6*1     # This estimates the intercept of X6 at T2
bf06_35_t2 ~ m7*1     # This estimates the intercept of X7 at T2
bf06_40_t2 ~ m8*1     # This estimates the intercept of X8 at T2
bf06_45_t2 ~ m9*1     # This estimates the intercept of X9 at T2
bf06_50_t2 ~ m10*1    # This estimates the intercept of X10 at T2
bf06_55_t2 ~ m11*1    # This estimates the intercept of X11 at T2
bf06_60_t2 ~ m12*1    # This estimates the intercept of X12 at T2

sa06_01_t1 ~~ sa06_01_t1
sa06_01_t1 ~ 1
'
fit_mi_lcs_openn_ideal_hyp5 <- lavaan(mi_lcs_openn_ideal_hyp5, data=df_sbsa_wide_pers_sa, estimator='WLSMV', fixed.x=FALSE, ordered="sa06_01_t1")
# This model did not converge properly (when adding the 'sa06_01_t1' goal variable). Declaring 'sa06_01_t1' as an 
# ordered variable and using the WLSMV estimator (sadly without FIML) worked in the end. Results 
# https://lavaan.ugent.be/tutorial/cat.html
summary(fit_mi_lcs_openn_ideal_hyp5, fit.measures=TRUE, standardized=TRUE, rsquare=F)

# with WLSMV estimator:
kable(broom::tidy(fit_mi_lcs_openn_ideal_hyp5, conf.int = FALSE, conf.level = 0.95) %>% 
  select(term, estimate, std.all, statistic, p.value) %>% 
  filter(term %in% c("openn_t2 ~ openn_t1", "d_openn_1 =~ openn_t2", "d_openn_1 ~ openn_t1", # change parameters
                     "d_openn_1 ~~ sa06_01_t1", "sa06_01_t1 ~~ sa06_01_t1", "sa06_01_t1 ~1 ", # acceptance goals
                     "d_openn_1 ~1 ", "openn_t1 ~1 ", "", # means
                     "d_openn_1 ~~ d_openn_1")), digits = 3)

# whole sample:
kable(broom::tidy(fit_mi_lcs_openn_ideal_hyp4, conf.int = FALSE, conf.level = 0.95) %>% 
        select(term, estimate, std.all, statistic, p.value) %>% 
        filter(term %in% c("openn_t2 ~ openn_t1", "d_openn_1 =~ openn_t2", "d_openn_1 ~ openn_t1", # change parameters
                           "d_openn_1 ~~ sb06_01_t1", "sb06_01_t1 ~~ sb06_01_t1", "sb06_01_t1 ~1 ", # acceptance goals
                           "d_openn_1 ~1 ", "openn_t1 ~1 ", "", # means
                           "d_openn_1 ~~ d_openn_1")), digits = 3)




model <- '
openn_t1 =~ 1*bf06_05_t1 + lamb2*bf06_10_t1 + lamb3*bf06_15_t1 + lamb4*bf06_20_t1 + lamb5*bf06_25_t1 + lamb6*bf06_30_t1 + lamb7*bf06_35_t1 + lamb8*bf06_40_t1 + lamb9*bf06_45_t1 + lamb10*bf06_50_t1 + lamb11*bf06_55_t1 + lamb12*bf06_60_t1 # This specifies the measurement model for extra_t1 
openn_t2 =~ 1*bf06_05_t2 + lamb2*bf06_10_t2 + lamb3*bf06_15_t2 + lamb4*bf06_20_t2 + lamb5*bf06_25_t2 + lamb6*bf06_30_t2 + lamb7*bf06_35_t2 + lamb8*bf06_40_t2 + lamb9*bf06_45_t2 + lamb10*bf06_50_t2 + lamb11*bf06_55_t2 + lamb12*bf06_60_t2 # This specifies the measurement model for extra_t2 with the equality constrained factor loadings

frequ =~ 1*sa04_01_t2 + sa04_02_t2 + sa04_03_t2 # latent variable for moderator

openn_t2 ~ 1*openn_t1     # This parameter regresses openn_t2 perfectly on openn_t1
d_openn_1 =~ 1*openn_t2   # This defines the latent change score factor as measured perfectly by scores on openn_t2
openn_t2 ~ 0*1            # This line constrains the intercept of openn_t2 to 0
openn_t2 ~~ 0*openn_t2    # This fixes the variance of openn_t2 to 0

d_openn_1 ~ 1              # This estimates the intercept of the change score 
openn_t1 ~ 1               # This estimates the intercept of openn_t1 
d_openn_1 ~~ d_openn_1     # This estimates the variance of the change scores 
openn_t1 ~~ openn_t1         # This estimates the variance of openn_t1 
openn_t1 ~ frequ               # This estimates the moderation effect on personality at T1
d_openn_1 ~ openn_t1 + frequ   # This estimates the self-feedback parameter and the moderation effect on the change score

frequ ~ 0*1          # This fixes the intercept of the moderator to 0
frequ ~~ frequ         # This estimates the variance of the moderator

bf06_05_t1 ~~ bf06_05_t2   # This allows residual covariance on indicator X1 across T1 and T2
bf06_10_t1 ~~ bf06_10_t2   # This allows residual covariance on indicator X2 across T1 and T2
bf06_15_t1 ~~ bf06_15_t2   # This allows residual covariance on indicator X3 across T1 and T2
bf06_20_t1 ~~ bf06_20_t2   # This allows residual covariance on indicator X4 across T1 and T2
bf06_25_t1 ~~ bf06_25_t2   # This allows residual covariance on indicator X5 across T1 and T2
bf06_30_t1 ~~ bf06_30_t2   # This allows residual covariance on indicator X6 across T1 and T2
bf06_35_t1 ~~ bf06_35_t2   # This allows residual covariance on indicator X7 across T1 and T2
bf06_40_t1 ~~ bf06_40_t2   # This allows residual covariance on indicator X8 across T1 and T2
bf06_45_t1 ~~ bf06_45_t2   # This allows residual covariance on indicator X9 across T1 and T2
bf06_50_t1 ~~ bf06_50_t2   # This allows residual covariance on indicator X10 across T1 and T2
bf06_55_t1 ~~ bf06_55_t2   # This allows residual covariance on indicator X11 across T1 and T2
bf06_60_t1 ~~ bf06_60_t2   # This allows residual covariance on indicator X12 across T1 and T2

bf06_05_t1 ~~ res1*bf06_05_t1   # This allows residual variance on indicator X1 at T1 
bf06_10_t1 ~~ res2*bf06_10_t1   # This allows residual variance on indicator X2 at T1
bf06_15_t1 ~~ res3*bf06_15_t1   # This allows residual variance on indicator X3 at T1
bf06_20_t1 ~~ res4*bf06_20_t1   # This allows residual variance on indicator X4 at T1
bf06_25_t1 ~~ res5*bf06_25_t1   # This allows residual variance on indicator X5 at T1
bf06_30_t1 ~~ res6*bf06_30_t1   # This allows residual variance on indicator X6 at T1 
bf06_35_t1 ~~ res7*bf06_35_t1   # This allows residual variance on indicator X7 at T1
bf06_40_t1 ~~ res8*bf06_40_t1   # This allows residual variance on indicator X8 at T1
bf06_45_t1 ~~ res9*bf06_45_t1   # This allows residual variance on indicator X9 at T1
bf06_50_t1 ~~ res10*bf06_50_t1  # This allows residual variance on indicator X10 at T1
bf06_55_t1 ~~ res11*bf06_55_t1  # This allows residual variance on indicator X11 at T1
bf06_60_t1 ~~ res12*bf06_60_t1  # This allows residual variance on indicator X12 at T1

bf06_05_t2 ~~ res1*bf06_05_t2  # This allows residual variance on indicator X1 at T2 
bf06_10_t2 ~~ res2*bf06_10_t2  # This allows residual variance on indicator X2 at T2 
bf06_15_t2 ~~ res3*bf06_15_t2  # This allows residual variance on indicator X3 at T2
bf06_20_t2 ~~ res4*bf06_20_t2  # This allows residual variance on indicator X4 at T2
bf06_25_t2 ~~ res5*bf06_25_t2  # This allows residual variance on indicator X5 at T2
bf06_30_t2 ~~ res6*bf06_30_t2  # This allows residual variance on indicator X6 at T2 
bf06_35_t2 ~~ res7*bf06_35_t2  # This allows residual variance on indicator X7 at T2 
bf06_40_t2 ~~ res8*bf06_40_t2  # This allows residual variance on indicator X8 at T2
bf06_45_t2 ~~ res9*bf06_45_t2  # This allows residual variance on indicator X9 at T2
bf06_50_t2 ~~ res10*bf06_50_t2 # This allows residual variance on indicator X10 at T2
bf06_55_t2 ~~ res11*bf06_55_t2 # This allows residual variance on indicator X11 at T2
bf06_60_t2 ~~ res12*bf06_60_t2 # This allows residual variance on indicator X12 at T2

bf06_05_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
bf06_10_t1 ~ m2*1     # This estimates the intercept of X2 at T1
bf06_15_t1 ~ m3*1     # This estimates the intercept of X3 at T1
bf06_20_t1 ~ m4*1     # This estimates the intercept of X4 at T1
bf06_25_t1 ~ m5*1     # This estimates the intercept of X5 at T1
bf06_30_t1 ~ m6*1     # This estimates the intercept of X6 at T1
bf06_35_t1 ~ m7*1     # This estimates the intercept of X7 at T1
bf06_40_t1 ~ m8*1     # This estimates the intercept of X8 at T1
bf06_45_t1 ~ m9*1     # This estimates the intercept of X9 at T1
bf06_50_t1 ~ m10*1    # This estimates the intercept of X10 at T1
bf06_55_t1 ~ m11*1    # This estimates the intercept of X11 at T1
bf06_60_t1 ~ m12*1    # This estimates the intercept of X12 at T1

bf06_05_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
bf06_10_t2 ~ m2*1     # This estimates the intercept of X2 at T2
bf06_15_t2 ~ m3*1     # This estimates the intercept of X3 at T2
bf06_20_t2 ~ m4*1     # This estimates the intercept of X4 at T2
bf06_25_t2 ~ m5*1     # This estimates the intercept of X5 at T2
bf06_30_t2 ~ m6*1     # This estimates the intercept of X6 at T2
bf06_35_t2 ~ m7*1     # This estimates the intercept of X7 at T2
bf06_40_t2 ~ m8*1     # This estimates the intercept of X8 at T2
bf06_45_t2 ~ m9*1     # This estimates the intercept of X9 at T2
bf06_50_t2 ~ m10*1    # This estimates the intercept of X10 at T2
bf06_55_t2 ~ m11*1    # This estimates the intercept of X11 at T2
bf06_60_t2 ~ m12*1    # This estimates the intercept of X12 at T2

sa04_01_t2 ~~ sa04_01_t2
sa04_02_t2 ~~ sa04_02_t2
sa04_03_t2 ~~ sa04_03_t2

sa04_01_t2 ~ 1
sa04_02_t2 ~ 1
sa04_03_t2 ~ 1
'

model_fit <- lavaan(model, data=df_sbsa_wide_pers_sa_mod, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(model_fit, fit.measures=TRUE, standardized=TRUE, rsquare=F)
# does not converge...

mod_traits_sqdiff_groups <- df_sbsa %>% 
  select(pid, time, rando, ends_with("_sqdiff")) %>% 
  pivot_longer(ends_with("_sqdiff"), 
               names_to = "test", names_prefix = "facet", values_to = "score", values_drop_na = TRUE) %>% 
  group_by(pid, test) %>% 
  mutate(assessments = n()) %>% 
  ungroup() %>% 
  filter(assessments==2) %>% 
  select(-assessments) %>% 
  group_nest(test) %>% 
  mutate(lmer_mods = map(data, ~lmerTest::lmer(score ~ time * rando + (1 | pid), data = .x))) %>% 
  pull(lmer_mods) %>% 
  purrr::set_names(names(b5_vars))

mod_traits_sqdiff_groups_unlist <- as.data.frame(summary(mod_traits_sqdiff_groups[[1]])$coefficients) %>% as_tibble()
for (i in 2:length(mod_traits_sqdiff_groups)) {
  mod_traits_sqdiff_groups_unlist <- bind_rows(mod_traits_sqdiff_groups_unlist, 
                                               as.data.frame(summary(mod_traits_sqdiff_groups[[i]])$coefficients) %>% as_tibble())
}

kable(mod_traits_sqdiff_groups_unlist %>% 
        mutate(outcome = rep(names(b5_vars), each=4), 
               term = c(rep(c("Intercept", "time", "group", "time*group"), 20))) %>% 
        select(outcome, term, everything()) %>% rename(p = `Pr(>|t|)`), 
      digits = 3)


# item parcels -> meaning in life scale

configural_meaning <- '
# Define the latent factors
meaning1 =~ NA*ml01_01_t1 + lambda1*ml01_01_t1 + lambda2*ml01_02_t1 + lambda3*ml01_03_t1 + lambda4*ml01_04_t1 + lambda5*ml01_05_t1 + lambda6*ml01_06_t1 + lambda7*ml01_07_t1 + lambda8*ml01_08_t1 + lambda9*ml01_09_t1 + lambda10*ml01_10_t1

# Intercepts
ml01_01_t1 ~ i1*1
ml01_02_t1 ~ 1
ml01_03_t1 ~ 1
ml01_04_t1 ~ 1
ml01_05_t1 ~ 1
ml01_06_t1 ~ 1
ml01_07_t1 ~ 1
ml01_08_t1 ~ 1
ml01_09_t1 ~ 1
ml01_10_t1 ~ 1

# Unique Variances
ml01_01_t1 ~~ ml01_01_t1
ml01_02_t1 ~~ ml01_02_t1
ml01_03_t1 ~~ ml01_03_t1
ml01_04_t1 ~~ ml01_04_t1
ml01_05_t1 ~~ ml01_05_t1
ml01_06_t1 ~~ ml01_06_t1
ml01_07_t1 ~~ ml01_07_t1
ml01_08_t1 ~~ ml01_08_t1
ml01_09_t1 ~~ ml01_09_t1
ml01_10_t1 ~~ ml01_10_t1

# Latent Variable Means
meaning1 ~ 0*1

# Latent Variable Variances and Covariance
meaning1 ~~ 1*meaning1
'
fit_configural_meaning <- cfa(configural_meaning, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_configural_meaning, fit.measures = TRUE)
tidy(fit_configural_meaning) %>% filter(str_detect(label, "lambda")) %>% mutate(abs_loading = abs(estimate)) %>% arrange(desc(abs_loading))

df_sbsa_wide_wb <- df_sbsa_wide_wb %>% 
  mutate(ml01_09_t1_r = ml01_09_t1,
         ml01_10_t1_r = ml01_10_t1,
         ml01_02_t1_r = ml01_02_t1,
         ml01_08_t1_r = ml01_08_t1,
         ml01_07_t1_r = ml01_07_t1,
         ml01_03_t1_r = ml01_03_t1,
         ml01_09_t2_r = ml01_09_t2,
         ml01_10_t2_r = ml01_10_t2,
         ml01_02_t2_r = ml01_02_t2,
         ml01_08_t2_r = ml01_08_t2,
         ml01_07_t2_r = ml01_07_t2,
         ml01_03_t2_r = ml01_03_t2) %>% 
  mutate(across(ends_with("_r"), ~ recode(.x, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L, .default = NA_integer_))) %>% 
  mutate(meaning_par1_t1 = rowMeans(across(c(ml01_04_t1, ml01_10_t1_r, ml01_07_t1_r, ml01_03_t1_r)), na.rm=T),
         meaning_par2_t1 = rowMeans(across(c(ml01_06_t1, ml01_01_t1, ml01_08_t1_r)), na.rm=T),
         meaning_par3_t1 = rowMeans(across(c(ml01_09_t1_r, ml01_05_t1, ml01_02_t1_r)), na.rm=T),
         meaning_par1_t2 = rowMeans(across(c(ml01_04_t2, ml01_10_t2_r, ml01_07_t2_r, ml01_03_t2_r)), na.rm=T),
         meaning_par2_t2 = rowMeans(across(c(ml01_06_t2, ml01_01_t2, ml01_08_t2_r)), na.rm=T),
         meaning_par3_t2 = rowMeans(across(c(ml01_09_t2_r, ml01_05_t2, ml01_02_t2_r)), na.rm=T))

configural_meaning_par <- '
meaning1 =~ meaning_par1_t1 + meaning_par2_t1 + meaning_par3_t1
'
fit_configural_meaning_par <- cfa(configural_meaning_par, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_configural_meaning_par, fit.measures = TRUE)


# Configural invariance model
configural_meaning <- '
# Define the latent factors
meaning1 =~ NA*meaning_par1_t1 + lambda1*meaning_par1_t1 + meaning_par2_t1 + meaning_par3_t1
meaning2 =~ NA*meaning_par1_t2 + lambda1*meaning_par1_t2 + meaning_par2_t2 + meaning_par3_t2

# Intercepts
meaning_par1_t1 ~ i1*1
meaning_par2_t1 ~ 1
meaning_par3_t1 ~ 1

meaning_par1_t2 ~ i1*1
meaning_par2_t2 ~ 1
meaning_par3_t2 ~ 1

# Unique Variances
meaning_par1_t1 ~~ meaning_par1_t1
meaning_par2_t1 ~~ meaning_par2_t1
meaning_par3_t1 ~~ meaning_par3_t1

meaning_par1_t2 ~~ meaning_par1_t2
meaning_par2_t2 ~~ meaning_par2_t2
meaning_par3_t2 ~~ meaning_par3_t2

# Latent Variable Means
meaning1 ~ 0*1
meaning2 ~ 1

# Latent Variable Variances and Covariance
meaning1 ~~ 1*meaning1
meaning2 ~~ meaning2
meaning1 ~~ meaning2
'
fit_configural_meaning <- cfa(configural_meaning, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_configural_meaning, fit.measures = TRUE)


# SWLS without item 5
mod_swls <- '
swls1 =~ sw06_01_t1 + sw06_02_t1 + sw06_03_t1 + sw06_04_t1 + sw06_05_t1
'
fit_swls <- cfa(mod_swls, data = df_sbsa_wide_wb, mimic = "mplus")
summary(fit_swls, fit.measures = TRUE)



# Configural invariance model
configural_openn_ideal <- '
# Define the latent factors
openn_ideal1 =~ NA*openn_ideal_par1_t1 + lambda1*openn_ideal_par1_t1 + openn_ideal_par2_t1 + openn_ideal_par3_t1
openn_ideal2 =~ NA*openn_ideal_par1_t2 + lambda1*openn_ideal_par1_t2 + openn_ideal_par2_t2 + openn_ideal_par3_t2

# Intercepts
openn_ideal_par1_t1 ~ i1*1
openn_ideal_par2_t1 ~ 1
openn_ideal_par3_t1 ~ 1

openn_ideal_par1_t2 ~ i1*1
openn_ideal_par2_t2 ~ 1
openn_ideal_par3_t2 ~ 1

# Unique Variances
openn_ideal_par1_t1 ~~ openn_ideal_par1_t1
openn_ideal_par2_t1 ~~ openn_ideal_par2_t1
openn_ideal_par3_t1 ~~ openn_ideal_par3_t1

openn_ideal_par1_t2 ~~ openn_ideal_par1_t2
openn_ideal_par2_t2 ~~ openn_ideal_par2_t2
openn_ideal_par3_t2 ~~ openn_ideal_par3_t2

# Latent Variable Means
openn_ideal1 ~ 0*1
openn_ideal2 ~ 1

# Latent Variable Variances and Covariance
openn_ideal1 ~~ 1*openn_ideal1
openn_ideal2 ~~ openn_ideal2
openn_ideal1 ~~ openn_ideal2
'
fit_configural_openn_ideal <- cfa(configural_openn_ideal, data = df_sbsa_wide_pers, mimic = "mplus")
summary(fit_configural_openn_ideal, fit.measures = TRUE)


### goals as moderator

trait_template_goals_goal <- '
neuro_t1 =~ 1*neuro_curr_par1_t1 + lamb2*neuro_curr_par2_t1 + lamb3*neuro_curr_par3_t1 # This specifies the measurement model for neuro_curr_t1 
neuro_t2 =~ 1*neuro_curr_par1_t2 + lamb2*neuro_curr_par2_t2 + lamb3*neuro_curr_par3_t2 # This specifies the measurement model for neuro_curr_t2 with the equality constrained factor loadings

goals =~ 1*sb07_10_t1 + sb07_11_t1 + sb07_12_t1 # latent variable for moderator

neuro_t2 ~ 1*neuro_t1     # This parameter regresses neuro_t2 perfectly on neuro_t1
d_neuro_1 =~ 1*neuro_t2   # This defines the latent change score factor as measured perfectly by scores on neuro_t2
neuro_t2 ~ 0*1            # This line constrains the intercept of neuro_t2 to 0
neuro_t2 ~~ 0*neuro_t2    # This fixes the variance of neuro_t2 to 0

d_neuro_1 ~ 1              # This estimates the intercept of the change score 
neuro_t1 ~ 1               # This estimates the intercept of neuro_t1 
d_neuro_1 ~~ d_neuro_1     # This estimates the variance of the change scores 
neuro_t1 ~~ neuro_t1         # This estimates the variance of neuro_t1 
neuro_t1 ~~ d_neuro_1      # This estimates the self-feedback parameter, as a covariance! -> therefore, the interpretation of the change score remains unconditional
neuro_t1 ~ goals               # This estimates the moderation effect on personality at T1
d_neuro_1 ~ goals   # This estimates the moderation effect on the change score

goals ~ 0*1            # This fixes the intercept of the moderator to 0
goals ~~ goals         # This estimates the variance of the moderator

neuro_curr_par1_t1 ~~ neuro_curr_par1_t2   # This allows residual covariance on indicator X1 across T1 and T2
neuro_curr_par2_t1 ~~ neuro_curr_par2_t2   # This allows residual covariance on indicator X2 across T1 and T2
neuro_curr_par3_t1 ~~ neuro_curr_par3_t2   # This allows residual covariance on indicator X3 across T1 and T2

neuro_curr_par1_t1 ~~ res1*neuro_curr_par1_t1   # This allows residual variance on indicator X1 at T1 
neuro_curr_par2_t1 ~~ res2*neuro_curr_par2_t1   # This allows residual variance on indicator X2 at T1
neuro_curr_par3_t1 ~~ res3*neuro_curr_par3_t1   # This allows residual variance on indicator X3 at T1

neuro_curr_par1_t2 ~~ res1*neuro_curr_par1_t2  # This allows residual variance on indicator X1 at T2 
neuro_curr_par2_t2 ~~ res2*neuro_curr_par2_t2  # This allows residual variance on indicator X2 at T2 
neuro_curr_par3_t2 ~~ res3*neuro_curr_par3_t2  # This allows residual variance on indicator X3 at T2

neuro_curr_par1_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
neuro_curr_par2_t1 ~ m2*1     # This estimates the intercept of X2 at T1
neuro_curr_par3_t1 ~ m3*1     # This estimates the intercept of X3 at T1

neuro_curr_par1_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
neuro_curr_par2_t2 ~ m2*1     # This estimates the intercept of X2 at T2
neuro_curr_par3_t2 ~ m3*1     # This estimates the intercept of X3 at T2

sb07_10_t1 ~~ sb07_10_t1
sb07_11_t1 ~~ sb07_11_t1
sb07_12_t1 ~~ sb07_12_t1

sb07_10_t1 ~ 1
sb07_11_t1 ~ 1
sb07_12_t1 ~ 1
'
fit_trait_template_goals_goal <- lavaan(trait_template_goals_goal, data=df_sbsa_wide_pers_sb_mod, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(fit_trait_template_goals_goal, fit.measures=TRUE, standardized=TRUE, rsquare=F)

### personal project dimensions:

trait_template_ppd_goal <- '
agree_curr_t1 =~ 1*agree_curr_par1_t1 + lamb2*agree_curr_par2_t1 + lamb3*agree_curr_par3_t1 # This specifies the measurement model for agree_curr_t1 
agree_curr_t2 =~ 1*agree_curr_par1_t2 + lamb2*agree_curr_par2_t2 + lamb3*agree_curr_par3_t2 # This specifies the measurement model for agree_curr_t2 with the equality constrained factor loadings

agree_curr_t2 ~ 1*agree_curr_t1     # This parameter regresses agree_curr_t2 perfectly on agree_curr_t1
d_agree_curr_1 =~ 1*agree_curr_t2   # This defines the latent change score factor as measured perfectly by scores on agree_curr_t2
agree_curr_t2 ~ 0*1            # This line constrains the intercept of agree_curr_t2 to 0
agree_curr_t2 ~~ 0*agree_curr_t2    # This fixes the variance of agree_curr_t2 to 0

d_agree_curr_1 ~ 1              # This estimates the intercept of the change score 
agree_curr_t1 ~ 1               # This estimates the intercept of agree_curr_t1 
d_agree_curr_1 ~~ d_agree_curr_1     # This estimates the variance of the change scores 
agree_curr_t1 ~~ agree_curr_t1         # This estimates the variance of agree_curr_t1 
agree_curr_t1 ~ ppd               # This estimates the moderation effect on personality at T1
d_agree_curr_1 ~ agree_curr_t1 + ppd   # This estimates the self-feedback parameter and the moderation effect on the change score

agree_curr_par1_t1 ~~ agree_curr_par1_t2   # This allows residual covariance on indicator X1 across T1 and T2
agree_curr_par2_t1 ~~ agree_curr_par2_t2   # This allows residual covariance on indicator X2 across T1 and T2
agree_curr_par3_t1 ~~ agree_curr_par3_t2   # This allows residual covariance on indicator X3 across T1 and T2

agree_curr_par1_t1 ~~ res1*agree_curr_par1_t1   # This allows residual variance on indicator X1 at T1 
agree_curr_par2_t1 ~~ res2*agree_curr_par2_t1   # This allows residual variance on indicator X2 at T1
agree_curr_par3_t1 ~~ res3*agree_curr_par3_t1   # This allows residual variance on indicator X3 at T1

agree_curr_par1_t2 ~~ res1*agree_curr_par1_t2  # This allows residual variance on indicator X1 at T2 
agree_curr_par2_t2 ~~ res2*agree_curr_par2_t2  # This allows residual variance on indicator X2 at T2 
agree_curr_par3_t2 ~~ res3*agree_curr_par3_t2  # This allows residual variance on indicator X3 at T2

agree_curr_par1_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
agree_curr_par2_t1 ~ m2*1     # This estimates the intercept of X2 at T1
agree_curr_par3_t1 ~ m3*1     # This estimates the intercept of X3 at T1

agree_curr_par1_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
agree_curr_par2_t2 ~ m2*1     # This estimates the intercept of X2 at T2
agree_curr_par3_t2 ~ m3*1     # This estimates the intercept of X3 at T2

ppd ~~ ppd

ppd ~ 1
'
fit_trait_template_ppd_goal <- lavaan(trait_template_ppd_goal, data=df_sbsa_wide_pers_sb_ppd, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(fit_trait_template_ppd_goal, fit.measures=TRUE, standardized=TRUE, rsquare=F)


### plotting 

df_table_hyp4_plot <- bind_rows(
  #traits
  params_lcs_extra_curr_hyp4 %>% filter(term=="d_extra_1 ~~ sb06_01_t1"),
  params_lcs_extra_ideal_hyp4 %>% filter(term=="d_extra_1 ~~ sb06_01_t1"),
  params_lcs_extra_curr_specif_hyp4 %>% filter(term=="goals ~~ d_extra_1"),
  params_lcs_extra_ideal_specif_hyp4 %>% filter(term=="goals ~~ d_extra_1"),
  params_lcs_agree_curr_hyp4 %>% filter(term=="d_agree_1 ~~ sb06_01_t1"),
  params_lcs_agree_ideal_hyp4 %>% filter(term=="d_agree_1 ~~ sb06_01_t1"),
  params_lcs_agree_curr_specif_hyp4 %>% filter(term=="goals ~~ d_agree_1"),
  params_lcs_agree_ideal_specif_hyp4 %>% filter(term=="goals ~~ d_agree_1"),
  params_lcs_consc_curr_hyp4 %>% filter(term=="d_consc_1 ~~ sb06_01_t1"),
  params_lcs_consc_ideal_hyp4 %>% filter(term=="d_consc_1 ~~ sb06_01_t1"),
  params_lcs_consc_curr_specif_hyp4 %>% filter(term=="goals ~~ d_consc_1"),
  params_lcs_consc_ideal_specif_hyp4 %>% filter(term=="goals ~~ d_consc_1"),
  params_lcs_neuro_curr_hyp4 %>% filter(term=="d_neuro_1 ~~ sb06_01_t1"),
  params_lcs_neuro_ideal_hyp4 %>% filter(term=="d_neuro_1 ~~ sb06_01_t1"),
  params_lcs_neuro_curr_specif_hyp4 %>% filter(term=="goals ~~ d_neuro_1"),
  params_lcs_neuro_ideal_specif_hyp4 %>% filter(term=="goals ~~ d_neuro_1"),
  params_lcs_openn_curr_hyp4 %>% filter(term=="d_openn_1 ~~ sb06_01_t1"),
  params_lcs_openn_ideal_hyp4 %>% filter(term=="d_openn_1 ~~ sb06_01_t1"),
  params_lcs_openn_curr_specif_hyp4 %>% filter(term=="goals ~~ d_openn_1"),
  params_lcs_openn_ideal_specif_hyp4 %>% filter(term=="goals ~~ d_openn_1"),
  #facets
  params_lcs_socia_curr_hyp4 %>% filter(term=="d_socia_1 ~~ sb06_01_t1"),
  params_lcs_socia_ideal_hyp4 %>% filter(term=="d_socia_1 ~~ sb06_01_t1"),
  params_lcs_socia_curr_specif_hyp4 %>% filter(term=="d_socia_1 ~~ sb07_01_t1"),
  params_lcs_socia_ideal_specif_hyp4 %>% filter(term=="d_socia_1 ~~ sb07_01_t1"),
  params_lcs_asser_curr_hyp4 %>% filter(term=="d_asser_1 ~~ sb06_01_t1"),
  params_lcs_asser_ideal_hyp4 %>% filter(term=="d_asser_1 ~~ sb06_01_t1"),
  params_lcs_asser_curr_specif_hyp4 %>% filter(term=="d_asser_1 ~~ sb07_02_t1"),
  params_lcs_asser_ideal_specif_hyp4 %>% filter(term=="d_asser_1 ~~ sb07_02_t1"),
  params_lcs_energ_curr_hyp4 %>% filter(term=="d_energ_1 ~~ sb06_01_t1"),
  params_lcs_energ_ideal_hyp4 %>% filter(term=="d_energ_1 ~~ sb06_01_t1"),
  params_lcs_energ_curr_specif_hyp4 %>% filter(term=="d_energ_1 ~~ sb07_03_t1"),
  params_lcs_energ_ideal_specif_hyp4 %>% filter(term=="d_energ_1 ~~ sb07_03_t1"),
  params_lcs_compa_curr_hyp4 %>% filter(term=="d_compa_1 ~~ sb06_01_t1"),
  params_lcs_compa_ideal_hyp4 %>% filter(term=="d_compa_1 ~~ sb06_01_t1"),
  params_lcs_compa_curr_specif_hyp4 %>% filter(term=="d_compa_1 ~~ sb07_04_t1"),
  params_lcs_compa_ideal_specif_hyp4 %>% filter(term=="d_compa_1 ~~ sb07_04_t1"),
  params_lcs_respe_curr_hyp4 %>% filter(term=="d_respe_1 ~~ sb06_01_t1"),
  params_lcs_respe_ideal_hyp4 %>% filter(term=="d_respe_1 ~~ sb06_01_t1"),
  params_lcs_respe_curr_specif_hyp4 %>% filter(term=="d_respe_1 ~~ sb07_05_t1"),
  params_lcs_respe_ideal_specif_hyp4 %>% filter(term=="d_respe_1 ~~ sb07_05_t1"),
  params_lcs_trust_curr_hyp4 %>% filter(term=="d_trust_1 ~~ sb06_01_t1"),
  params_lcs_trust_ideal_hyp4 %>% filter(term=="d_trust_1 ~~ sb06_01_t1"),
  params_lcs_trust_curr_specif_hyp4 %>% filter(term=="d_trust_1 ~~ sb07_06_t1"),
  params_lcs_trust_ideal_specif_hyp4 %>% filter(term=="d_trust_1 ~~ sb07_06_t1"),
  params_lcs_organ_curr_hyp4 %>% filter(term=="d_organ_1 ~~ sb06_01_t1"),
  params_lcs_organ_ideal_hyp4 %>% filter(term=="d_organ_1 ~~ sb06_01_t1"),
  params_lcs_organ_curr_specif_hyp4 %>% filter(term=="d_organ_1 ~~ sb07_07_t1"),
  params_lcs_organ_ideal_specif_hyp4 %>% filter(term=="d_organ_1 ~~ sb07_07_t1"),
  params_lcs_produ_curr_hyp4 %>% filter(term=="d_produ_1 ~~ sb06_01_t1"),
  params_lcs_produ_ideal_hyp4 %>% filter(term=="d_produ_1 ~~ sb06_01_t1"),
  params_lcs_produ_curr_specif_hyp4 %>% filter(term=="d_produ_1 ~~ sb07_08_t1"),
  params_lcs_produ_ideal_specif_hyp4 %>% filter(term=="d_produ_1 ~~ sb07_08_t1"),
  params_lcs_respo_curr_hyp4 %>% filter(term=="d_respo_1 ~~ sb06_01_t1"),
  params_lcs_respo_ideal_hyp4 %>% filter(term=="d_respo_1 ~~ sb06_01_t1"),
  params_lcs_respo_curr_specif_hyp4 %>% filter(term=="d_respo_1 ~~ sb07_09_t1"),
  params_lcs_respo_ideal_specif_hyp4 %>% filter(term=="d_respo_1 ~~ sb07_09_t1"),
  params_lcs_anxie_curr_hyp4 %>% filter(term=="d_anxie_1 ~~ sb06_01_t1"),
  params_lcs_anxie_ideal_hyp4 %>% filter(term=="d_anxie_1 ~~ sb06_01_t1"),
  params_lcs_anxie_curr_specif_hyp4 %>% filter(term=="d_anxie_1 ~~ sb07_10_t1"),
  params_lcs_anxie_ideal_specif_hyp4 %>% filter(term=="d_anxie_1 ~~ sb07_10_t1"),
  params_lcs_depre_curr_hyp4 %>% filter(term=="d_depre_1 ~~ sb06_01_t1"),
  params_lcs_depre_ideal_hyp4 %>% filter(term=="d_depre_1 ~~ sb06_01_t1"),
  params_lcs_depre_curr_specif_hyp4 %>% filter(term=="d_depre_1 ~~ sb07_11_t1"),
  params_lcs_depre_ideal_specif_hyp4 %>% filter(term=="d_depre_1 ~~ sb07_11_t1"),
  params_lcs_volat_curr_hyp4 %>% filter(term=="d_volat_1 ~~ sb06_01_t1"),
  params_lcs_volat_ideal_hyp4 %>% filter(term=="d_volat_1 ~~ sb06_01_t1"),
  params_lcs_volat_curr_specif_hyp4 %>% filter(term=="d_volat_1 ~~ sb07_12_t1"),
  params_lcs_volat_ideal_specif_hyp4 %>% filter(term=="d_volat_1 ~~ sb07_12_t1"),
  params_lcs_curio_curr_hyp4 %>% filter(term=="d_curio_1 ~~ sb06_01_t1"),
  params_lcs_curio_ideal_hyp4 %>% filter(term=="d_curio_1 ~~ sb06_01_t1"),
  params_lcs_curio_curr_specif_hyp4 %>% filter(term=="d_curio_1 ~~ sb07_13_t1"),
  params_lcs_curio_ideal_specif_hyp4 %>% filter(term=="d_curio_1 ~~ sb07_13_t1"),
  params_lcs_aesth_curr_hyp4 %>% filter(term=="d_aesth_1 ~~ sb06_01_t1"),
  params_lcs_aesth_ideal_hyp4 %>% filter(term=="d_aesth_1 ~~ sb06_01_t1"),
  params_lcs_aesth_curr_specif_hyp4 %>% filter(term=="d_aesth_1 ~~ sb07_14_t1"),
  params_lcs_aesth_ideal_specif_hyp4 %>% filter(term=="d_aesth_1 ~~ sb07_14_t1"),
  params_lcs_imagi_curr_hyp4 %>% filter(term=="d_imagi_1 ~~ sb06_01_t1"),
  params_lcs_imagi_ideal_hyp4 %>% filter(term=="d_imagi_1 ~~ sb06_01_t1"),
  params_lcs_imagi_curr_specif_hyp4 %>% filter(term=="d_imagi_1 ~~ sb07_15_t1"),
  params_lcs_imagi_ideal_specif_hyp4 %>% filter(term=="d_imagi_1 ~~ sb07_15_t1"),
) %>% 
  mutate(trait = rep(names(b5_vars), each=4),
         ref = rep(rep(c("current", "ideal"), 2), 20),
         goal = rep(c(rep("general", 2), rep("specific", 2)), 20)) %>% 
  select(trait, ref, goal, estimate, conf.low, conf.high, std.all, statistic, p.value)

df_table_hyp4_plot <- df_table_hyp4_plot %>% 
  mutate(include_0 = ifelse(conf.low < 0 & conf.high > 0, "n.s.", "*")) %>% 
  mutate(ref = factor(ref, levels = c("current", "ideal"), labels = c("current", "ideal"))) %>% 
  mutate(goal = factor(goal, levels = c("general", "specific"), labels = c("general", "specific"))) %>% 
  mutate(trait = factor(trait, levels = names(b5_vars), labels = names(b5_vars)))

# plot:
ggplot(df_table_hyp4_plot, aes(x = fct_rev(trait), y = estimate, color = include_0)) +
  geom_hline(yintercept=0, linetype = 3) +
  geom_point(size=3, position=position_dodge(0.4)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = include_0), width=.2, position=position_dodge(0.4)) +
  scale_color_manual(values = c("#000000","#A9A9A9")) +   
  facet_wrap( ~ ref + goal, ncol = 2) +
  theme_bw() +
  scale_shape_manual(values=c(18)) + 
  ylab("Effect Estimates (95% CI)") +
  xlab("") +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=12)) +
  coord_flip() +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text.x=element_text(size=10, angle = 45, hjust = 1), axis.text.y=element_text(size=12)) + 
  guides(color=FALSE)


# plot goals distribution

table(df_sbsa$sb06_01) # How much do you want to change your personality in general?
summary(df_sbsa$sb06_01)

table(df_sbsa$sa06_01) # How much do you want to be better at accepting yourself for who you are?
summary(df_sbsa$sa06_01)

ggplot(df_sbsa %>% filter(!is.na(sb06_01)), aes(x=sb06_01)) + geom_histogram(bins=5) + 
  labs(x="General change goal", y="Frequency") + theme_bw()

### self-feedback parameter as a covariance!

trait_template_test <- '
trait_t1 =~ 1*ind01_t1 +  lamb2*ind02_t1 + lamb3*ind03_t1 # This specifies the measurement model for trait_t1 
trait_t2 =~ 1*ind01_t2 +  lamb2*ind02_t2 + lamb3*ind03_t2 # This specifies the measurement model for trait_t2 with the equality constrained factor loadings

trait_t2 ~ 1*trait_t1     # This parameter regresses trait_t2 perfectly on trait_t1
d_trait_1 =~ 1*trait_t2   # This defines the latent change score factor as measured perfectly by scores on trait_t2
trait_t2 ~ 0*1            # This line constrains the intercept of trait_t2 to 0
trait_t2 ~~ 0*trait_t2    # This fixes the variance of trait_t2 to 0

d_trait_1 ~ 1              # This estimates the intercept of the change score 
trait_t1 ~ 1               # This estimates the intercept of trait_t1 
d_trait_1 ~~ d_trait_1     # This estimates the variance of the change scores 
trait_t1 ~~ trait_t1         # This estimates the variance of trait_t1 
d_trait_1 ~~ trait_t1   # This estimates the self-feedback parameter, as a covariance! -> therefore, the interpretation of the change score remains unconditional

ind01_t1 ~~ ind01_t2   # This allows residual covariance on indicator X1 across T1 and T2
ind02_t1 ~~ ind02_t2   # This allows residual covariance on indicator X2 across T1 and T2
ind03_t1 ~~ ind03_t2   # This allows residual covariance on indicator X3 across T1 and T2

ind01_t1 ~~ res1*ind01_t1   # This allows residual variance on indicator X1 at T1 
ind02_t1 ~~ res2*ind02_t1   # This allows residual variance on indicator X2 at T1
ind03_t1 ~~ res3*ind03_t1   # This allows residual variance on indicator X3 at T1

ind01_t2 ~~ res1*ind01_t2  # This allows residual variance on indicator X1 at T2 
ind02_t2 ~~ res2*ind02_t2  # This allows residual variance on indicator X2 at T2 
ind03_t2 ~~ res3*ind03_t2  # This allows residual variance on indicator X3 at T2

ind01_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
ind02_t1 ~ m2*1     # This estimates the intercept of X2 at T1
ind03_t1 ~ m3*1     # This estimates the intercept of X3 at T1

ind01_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
ind02_t2 ~ m2*1     # This estimates the intercept of X2 at T2
ind03_t2 ~ m3*1     # This estimates the intercept of X3 at T2
'
template_filled_test <- str_replace_all(trait_template_test, 
                                        c("trait" = "neuro",
                                          "ind01" = "neuro_curr_par1", # three item parcels
                                          "ind02" = "neuro_curr_par2", 
                                          "ind03" = "neuro_curr_par3"))
trait_main_fit_test <- lavaan(template_filled_test, 
                              data = bind_rows(df_sbsa_wide_pers_sa_mod, df_sbsa_wide_pers_sb_mod) %>% left_join(group_assign), 
                              estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(trait_main_fit_test)

### standardization in single/manifest goal variable

trait_template_test2 <- '
facet_t1 =~ 1*ind1_t1 + lamb2*ind2_t1 + lamb3*ind3_t1 + lamb4*ind4_t1 # This specifies the measurement model for facet at T1
facet_t2 =~ 1*ind1_t2 + lamb2*ind2_t2 + lamb3*ind3_t2 + lamb4*ind4_t2 # This specifies the measurement model for facet at T2 (with equality constraints)

facet_t2 ~ 1*facet_t1     # This parameter regresses facet_t2 perfectly on facet_t1
d_facet_1 =~ 1*facet_t2   # This defines the latent change score factor as measured perfectly by scores on facet_t2
facet_t2 ~ 0*1            # This line constrains the intercept of facet_t2 to 0
facet_t2 ~~ 0*facet_t2    # This fixes the variance of facet_t2 to 0

d_facet_1 ~ 1              # This estimates the intercept of the change score 
facet_t1 ~ 1               # This estimates the intercept of facet_t1 
d_facet_1 ~~ d_facet_1     # This estimates the variance of the change scores 
facet_t1 ~~ facet_t1       # This estimates the variance of facet_t1 
facet_t1 ~~ d_facet_1      # This estimates the self-feedback parameter, as a covariance! -> therefore, the interpretation of the change score remains unconditional
facet_t1 ~ ind_goal        # This estimates the moderation effect on personality at T1
d_facet_1 ~ ind_goal       # This estimates the moderation effect on the change score

ind1_t1 ~~ ind1_t2   # This allows residual covariance on indicator X1 across T1 and T2
ind2_t1 ~~ ind2_t2   # This allows residual covariance on indicator X2 across T1 and T2
ind3_t1 ~~ ind3_t2   # This allows residual covariance on indicator X3 across T1 and T2
ind4_t1 ~~ ind4_t2   # This allows residual covariance on indicator X4 across T1 and T2

ind1_t1 ~~ res1*ind1_t1   # This allows residual variance on indicator X1 at T1 
ind2_t1 ~~ res2*ind2_t1   # This allows residual variance on indicator X2 at T1
ind3_t1 ~~ res3*ind3_t1   # This allows residual variance on indicator X3 at T1
ind4_t1 ~~ res4*ind4_t1   # This allows residual variance on indicator X4 at T1

ind1_t2 ~~ res1*ind1_t2  # This allows residual variance on indicator X1 at T2 
ind2_t2 ~~ res2*ind2_t2  # This allows residual variance on indicator X2 at T2 
ind3_t2 ~~ res3*ind3_t2  # This allows residual variance on indicator X3 at T2
ind4_t2 ~~ res4*ind4_t2  # This allows residual variance on indicator X4 at T2

ind1_t1 ~ 0*1      # This constrains the intercept of X1 to 0 at T1
ind2_t1 ~ m2*1     # This estimates the intercept of X2 at T1
ind3_t1 ~ m3*1     # This estimates the intercept of X3 at T1
ind4_t1 ~ m4*1     # This estimates the intercept of X4 at T1

ind1_t2 ~ 0*1      # This constrains the intercept of X1 to 0 at T2
ind2_t2 ~ m2*1     # This estimates the intercept of X2 at T2
ind3_t2 ~ m3*1     # This estimates the intercept of X3 at T2
ind4_t2 ~ m4*1     # This estimates the intercept of X4 at T2

ind_goal ~~ ind_goal
ind_goal ~ 1
'

df_sbsa_wide_pers_sb_mod <- df_sbsa_wide_pers_sb_mod %>% 
  mutate(asb07_01_t1 = scale(sb07_01_t1)) %>% as.vector()

items = paste0(bfi_versions[[5]], b5_vars[[6]][[1]])
mod_name = paste0("asb07_", str_pad(6-5, 2, pad = "0"), "_t1")

template_filled_test2 <- str_replace_all(trait_template_test2, 
                                         c("facet" = "socia",
                                           "ind1" = items[1], "ind2" = items[2], "ind3" = items[3], "ind4" = items[4],
                                           "ind_goal" = mod_name))
trait_main_fit_test2 <- lavaan(template_filled_test2, data = df_sbsa_wide_pers_sb_mod, estimator='mlr', fixed.x=FALSE, missing='fiml')
summary(trait_main_fit_test2, fit.measures = TRUE)





