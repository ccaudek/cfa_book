##############################################################################
##
## Table 7.9
##
## Tests of measurement invariance and population heterogeneity of DSM-IV
## major depressive disorder in men and women
##
##############################################################################

library(tidyverse)
library(lavaan)

Data <- read.table("http://people.bu.edu/tabrown/Ch7/MDDALL.dat")
names(Data) <- c("sex", paste("mdd", 1:9, sep = ""))
Data$sex <- factor(Data$sex, levels = c(0, 1), labels = c("female", "male"))

model.mdd <- '
  MDD =~ mdd1 + mdd2 + mdd3 + mdd4 + mdd5 + mdd6 + mdd7 + mdd8 + mdd9
  mdd1 ~~ mdd2
'

# configural invariance
fit.ef <- cfa(model.mdd, data = Data, group = "sex", meanstructure = TRUE)

fit.efl <- update(fit.ef, group.equal = c("loadings")) # equal factor laodings
fit.eii <- update(fit.efl, group.equal = c("loadings", "intercepts")) # equal indicator intercepts
fit.eir <- update(fit.eii, group.equal = c("loadings", "intercepts", "residuals")) # equal indicator error variances
fit.fv <- update(fit.eir, group.equal = c("loadings", "intercepts", "residuals", "lv.variances")) # equal factor variances
fit.fm <- update(fit.fv, group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means")) # equal latent means

# model comparison tests
lavTestLRT(fit1, fit2, fit3, fit4, fit5, fit6)




# weak invariance
fit2 <- cfa(model.mdd, data = Data, group = "sex", group.equal = "loadings")

# strong invariance
fit3 <- cfa(model.mdd, data = Data, group = "sex",
            group.equal = c("intercepts", "loadings"))

fit4 <- cfa(model.mdd, data = Data, group = "sex",
            group.equal = c("intercepts", "loadings", "residuals"))

fit5 <- cfa(model.mdd, data = Data, group = "sex",
            group.equal = c("intercepts", "loadings", "residuals", "lv.variances"))

fit6 <- cfa(model.mdd, data = Data, group = "sex",
            group.equal = c("intercepts", "loadings", "residuals",
                            "lv.variances", "means"))


# model comparison tests
lavTestLRT(fit1, fit2, fit3, fit4, fit5, fit6)




# using lavaan
# measurementInvariance doesn't do equal factor variance. But, this can be accomplished as follows
fit.ef <- cfa(model.mdd, data = Data, group = "sex", meanstructure = TRUE) # equal form
fit.efl <- update(fit.ef, group.equal = c("loadings")) # equal factor laodings
fit.eii <- update(fit.efl, group.equal = c("loadings", "intercepts")) # equal indicator intercepts
fit.eir <- update(fit.eii, group.equal = c("loadings", "intercepts", "residuals")) # equal indicator error variances
fit.fv <- update(fit.eir, group.equal = c("loadings", "intercepts", "residuals", "lv.variances")) # equal factor variances
fit.fm <- update(fit.fv, group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means")) # equal latent means

# chi-squared diff tests
anova(fit.ef, fit.efl, fit.eii, fit.eir, fit.fv, fit.fm, test = "chisq")

