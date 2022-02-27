#### Animazione

È facile creare un'animazione che illustra il progresso dell'algoritmo di Metropolis. In $\textsf{R}$, sono necessari i seguenti pacchetti:

  ```{r, eval=FALSE}
library("gganimate")
library("magick")
```

Creiamo un DataFrame che contiene i primi 1000 valori della catena che è stata generata in precedenza:

  ```{r, eval=FALSE}
d <- tibble(
  iter = 1:1000,
  mcmc = chain[1:1000]
)
```

L'animazione del trace plot si ottiene mediante la funzione `transition_reveal()`:

```{r, eval=FALSE}
ggplot(d, aes(x = iter, y = mcmc)) +
  geom_line() +
  transition_reveal(iter)
```

Ovviamente, è necessario fare tutto questo in RStudio: non ho inserito un GIF animato nel presente file pdf.
