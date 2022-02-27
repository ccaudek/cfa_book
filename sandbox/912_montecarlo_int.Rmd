# Integrazione di Monte Carlo {#integration-mc}

Il termine Monte Carlo si riferisce al fatto che la computazione fa ricorso ad un ripetuto campionamento casuale attraverso la generazione di sequenze di numeri casuali. Una delle sue applicazioni più potenti è il calcolo degli integrali mediante simulazione numerica.  Sia l'integrale da calcolare

$$
\int_a^b h(y) dy.
$$
Se decomponiamo $h(y)$ nel prodotto di una funzione $f(y)$ e una funzione di densità
di probabilità $p(y)$ definita nell'intervallo $(a, b)$ avremo:

$$
\int_a^b h(y) dy = \int_a^b f(y) p(y) dy = \E[f(y)],
$$
così che l'integrale può essere espresso come una funzione di aspettazione $f(y)$ sulla densità $p(y)$. Se definiamo un gran numero di variabili casuali $y_1, y_2, \dots, y_n$ appartenenti alla densità di probabilità $p(y)$ allora avremo

$$
\int_a^b h(y) dy = \int_a^b f(y) p(y) dy = \E[f(y)] \approx \frac{1}{n}\sum_{i=1}^n f(y)
$$
che è l'integrale di Monte Carlo.

<!-- Il calcolo del valore atteso richiede la somma dei prodotti $y p(y)$ per tutti gli infinitesimi "incrementi" $dy$ dei possibili valori $y$. -->
<!-- Vediamo come si può risolvere questo problema senza dovere calcolare l'integrale esatto per via analitica. -->

L'integrazione con metodo Monte Carlo trova la sua giustificazione nella *Legge forte dei grandi numeri*. Data una successione di variabili casuali $Y_{1}, Y_{2},\dots, Y_{n},\dots$ indipendenti e identicamente distribuite con media $\mu$, ne segue che

$$
P\left( \lim_{n \rightarrow \infty} \frac{1}{n} \sum_{i=1}^n Y_i = \mu \right) = 1.
$$
Ciò significa che, al crescere di $n$, la media delle realizzazioni di $Y_{1}, Y_{2},\dots, Y_{n},\dots$ converge con probabilità 1 al vero valore $\mu$.

Possiamo fornire un esempio intuitivo della legge forte dei grandi numeri facendo riferimento ad una serie di lanci di una moneta dove $Y=1$ significa "testa" e $Y=0$ significa "croce". Per la legge forte dei grandi numeri, nel caso di una moneta equilibrata la proporzione di eventi "testa" converge alla vera probabilità dell'evento "testa" 

$$
  \frac{1}{n} \sum_{i=1}^n Y_i \rightarrow \frac{1}{2}
$$ 
con probabilità di uno.

Quello che è stato detto sopra non è che un modo sofisticato per dire che, se vogliamo calcolare un'approssimazione del valore atteso di una variabile casuale, non dobbiamo fare altro che la media aritmetica di un grande numero di realizzazioni della variabile casuale. Come è facile intuire, l'approssimazione migliora al crescere del numero di dati che abbiamo a disposizione. 

<!-- ::: {.rmdnote} -->
<!-- Questa è la giustificazione dell'affermazione secondo la quale, nella statistica bayesiana, una stima del valore atteso di $p(\theta \mid y)$ può essere ottenuta mediante un grande numero di campioni casuali della distribuzionea a posteriori. -->
<!-- :::  -->

L'integrazione di Monte Carlo può essere usata per approssimare la distribuzione a posteriori richiesta da una analisi Bayesiana: una stima di $p(\theta \mid y)$ può essere ottenuta mediante un grande numero di campioni casuali della distribuzionea a posteriori.






