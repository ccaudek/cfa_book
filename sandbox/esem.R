## ESEM

```{r}
fit_mod <- psych::fa(
  r = psychot_cor_mat,
  nfactors = 2,
  rotate = "geominQ",
  fm = "ml"
)
```

```{r}
load <- fit_mod$loadings %>%
  matrix(
    ncol = 2,
    dimnames = list(
      c("N1", "N2", "N3", "N4", "E1", "E2", "E3", "E4"), paste0("F", 1:2))
  ) %>%
  round(digits = 3)
load
```

```{r}
esemmodel <- vector()

for (i in 1:2) {
  esemmodel[i] <- paste0(
    "F", i, " =~ ", paste0(c(load[, i]), "*", names(load[,1]), collapse = " + "))
}

esemmodel <- paste0(esemmodel, collapse = "\n") #Model Specification
esemmodel
```

```{r}
esemfit <- cfa(
  model = esemmodel,
  sample.cov = psychot_cor_mat,
  sample.nobs = 200,
  std.lv = TRUE
) #fitting the model
```


```{r}
summary(
  esemfit,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)
```

