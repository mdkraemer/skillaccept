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

# latent change score model

depmi.latchange <- '
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
fit.depmi.latchange <- sem(depmi.latchange, data=long.data, meanstructure=T, estimator="MLR", missing="fiml")
summary(fit.depmi.latchange, fit.measures=T, standardized=T, rsquare=T, modindices=T, ci=T)