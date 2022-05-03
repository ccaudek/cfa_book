
sds <- '5.7  5.6  6.4  5.7  6.0  6.2  5.7  5.6'

cors <- '
 1.000
 0.767  1.000
 0.731  0.709  1.000
 0.778  0.738  0.762  1.000
-0.351  -0.302  -0.356  -0.318  1.000
-0.316  -0.280  -0.300  -0.267  0.675  1.000
-0.296  -0.289  -0.297  -0.296  0.634  0.651  1.000
-0.282  -0.254  -0.292  -0.245  0.534  0.593  0.566  1.000'

covs <- getCov(cors, sds = sds, names = c("N1", "N2", "N3", "N4", "E1", "E2", "E3", "E4"))

model <- 'onefactor  =~ n1 + n2 + n3 + n4 + e1 + e2 + e3 + e4'

fit <- cfa(model, sample.cov = covs, sample.nobs = 250, mimic = "mplus")
summary(fit, fit.measures = TRUE)
## RMSEA, CFI, TLI differ

# Standardized Residual Matrix
# Brown reports LISREL 8.72 output which is different from the lavaan result
resid(fit, type = "standardized")$cov


# modification indices
modindices(fit)

model2 <- '
   fn =~ n1 + n2 + n3 + n4
   fe =~ e1 + e2 + e3 + e4
'

fit2 <- cfa(model2, sample.cov = covs, sample.nobs = 250, mimic = "mplus")
summary(fit2, fit.measures = TRUE)


# 1-factor model
f1 <- '
efa("efa")*f1 =~ N1 + N2 + N3 + N4 + E1 + E2 + E3 + E4
'
# 2-factor model
f2 <- '
efa("efa")*f1 +
efa("efa")*f2 =~ N1 + N2 + N3 + N4 + E1 + E2 + E3 + E4
'

efa_f1 <-
  cfa(
    model = f1,
    sample.cov = covs,
    sample.nobs = 250,
    rotation = "oblimin"
  )

summary(
  efa_f1,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

efa_f2 <-
  cfa(
    model = f2,
    sample.cov = covs,
    sample.nobs = 250,
    rotation = "oblimin"
  )

summary(
  efa_f2,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# define the fit measures
fit_measures_robust <- c(
  "chisq", "df", "pvalue",
  "cfi", "tli", "rmsea", "srmr"
)

# collect them for each model
rbind(
  fitmeasures(efa_f1, fit_measures_robust),
  fitmeasures(efa_f2, fit_measures_robust)
) %>%
  # wrangle
  data.frame() %>%
  mutate(
    chisq = round(chisq, digits = 0),
    df = as.integer(df),
    pvalue = ifelse(pvalue == 0, "< .001", pvalue)
  ) %>%
  mutate_at(vars(cfi:srmr), ~ round(., digits = 3))










