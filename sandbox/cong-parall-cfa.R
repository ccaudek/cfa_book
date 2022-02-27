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


## CFA: Modelli congenerici, tau-equivalenti e paralleli

Consideriamo un esempio discusso da @brown2015confirmatory. Viene qui considerato un set di dati in cui le prime tre misure osservate (X1, X2, X3) sono indicatori di un costrutto latente corrispondente alla Memoria uditiva e il secondo insieme di misure (X4, X5, X6) sono indicatori di un altro costrutto latente, Memoria visiva. Le tre misure usate quali indicatori del costrutto di memoria uditiva sono: X1 = memoria logica, X2 = associazione verbale a coppie, X3 = liste di parole; le tre misure usate come indicatori del costrutto di memoria visiva sono: X4 = immagini di facce, X5 = foto di famiglia, X6 = generiche riproduzioni visive. I dati sono i seguenti:

  ```{r}
sds <- '2.610  2.660  2.590  1.940  2.030  2.050'

cors <-'
  1.000
  0.661  1.000
  0.630  0.643  1.000
  0.270  0.300  0.268  1.000
  0.297  0.265  0.225  0.805  1.000
  0.290  0.287  0.248  0.796  0.779  1.000'

covs <- getCov(cors, sds = sds, names = paste("x", 1:6, sep = ""))
```

Adattiamo i cinque modelli discussi da @brown2015confirmatory.

### Modello congenerico a due fattori

```{r}
model.congeneric <- '
  auditorymemory =~ x1 + x2 + x3
  visualmemory   =~ x4 + x5 + x6
'
```

```{r}
fit.congeneric <- cfa(
  model.congeneric,
  sample.cov = covs,
  sample.nobs = 200,
  std.lv = TRUE
)
```

L'output (qui non fornito) si ottiene con:

```{r, eval=FALSE}
summary(
  fit.congeneric,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)
```

### Modello tau-equivalente

Solo memoria auditiva:

```{r}
model.tau.a <- '
auditorymemory =~ x1 + v1*x1 + v1*x2 + v1*x3
visualmemory   =~ x4 + x5 + x6
'
```

```{r}
fit.tau.a <- cfa(
  model.tau.a,
  sample.cov = covs,
  sample.nobs = 200,
  std.lv = TRUE
)
```

Memoria auditiva e visiva:

```{r}
model.tau.av <- '
auditorymemory =~ x1 + v1*x1 + v1*x2 + v1*x3
visualmemory   =~ x4 + v2*x4 + v2*x5 + v2*x6
'
```

```{r}
fit.tau.av <- cfa(
  model.tau.av,
  sample.cov = covs,
  sample.nobs = 200,
  std.lv = TRUE
)
```

### Modello parallelo

Solo memoria auditiva:

```{r}
model.parallel.a <- '
auditorymemory =~ x1 + v1*x1 + v1*x2 + v1*x3
visualmemory   =~ x4 + v2*x4 + v2*x5 + v2*x6
x1 ~~ v3 * x1
x2 ~~ v3 * x2
x3 ~~ v3 * x3
'
```

```{r}
fit.parallel.a <- cfa(
  model.parallel.a,
  sample.cov = covs,
  sample.nobs = 200,
  std.lv = TRUE
)
```


Memoria auditiva e visiva:

```{r}
model.parallel.av <- '
auditorymemory =~ x1 + v1*x1 + v1*x2 + v1*x3
visualmemory   =~ x4 + v2*x4 + v2*x5 + v2*x6
x1 ~~ v3 * x1
x2 ~~ v3 * x2
x3 ~~ v3 * x3

x4 ~~ v4 * x4
x5 ~~ v4 * x5
x6 ~~ v4 * x6
'
```

```{r}
fit.parallel.av <- cfa(
  model.parallel.av,
  sample.cov = covs,
  sample.nobs = 200,
  std.lv = TRUE
)
```

### Confronto di modelli

Il confronto tra i modelli si esegue valutando in maniera relativa la bontà di adattamento di ciascun modello per mezzo della statistica chi-quadrato:

```{r}
anova(
  fit.congeneric,
  fit.tau.a,
  fit.tau.av,
  fit.parallel.a,
  fit.parallel.av,
  test = "chisq"
)
```

I test precedenti indicano come non vi sia una perdita di adattamento passando dal modello congenerico al modello più restrittivo (ovvero, il modello parallelo per entrambi i fattori). Per questi dati, dunque, può essere adottato il modello più semplice, cioè il modello parallelo.


