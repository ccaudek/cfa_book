## Reti bayesiane

<!-- si può dunque definire una rete bayesiana come la rappresentazione della probabilità congiunta sull’insieme delle variabili aleatorie. Utilizzando la condizione di Markov si può affermare che la distribuzione di probabilità congiunta è formalizzata come prodotto di un insieme di probabilità locali, permettendo in tal modo di ridurre la complessità del problema e di facilitare la specificazione del modello probabilistico. -->
  <!-- La definizione di condizione di Markov è la seguente: la distribuzione di probabilità congiunta P() soddisfa la condizione di Markov per un DAG se ogni variabile è condizionatamente indipendente da ogni altra variabile escludendo i figli e i genitori, dati i genitori: -->

  La figura che abbiamo esaminato in precedenza per rappresentare l'indipendenza condizionale di $A$ e $B$ dato $C$ è un esempio di rete bayesiana (BN, Bayesian network). Una rete bayesiana è rappresentate mediante un grafo orientato aciclico (_Directed Acyclic Graph_, DAG)
<!-- usati per rappresentare un insieme di variabili casuali $X \in U$ e le loro dipendenze condizionali. Ogni nodo disponibile nelle BN rappresenta una variabile casuale e gli archi orientati specificano le relazioni di dipendenza condizionale.  -->
che definisce una fattorizzazione di una distribuzione di probabilità congiunta sulle variabili che sono rappresentate dai nodi del DAG, dove la fattorizzazione è data dai collegamenti diretti del DAG. Più precisamente, per un DAG, GD $\mathcal{G} = (V,E)$, dove $V$ indica un insieme di nodi ed $E$ un insieme di collegamenti diretti (o archi) tra coppie di nodi, una distribuzione di probabilità congiunta, $P(X_V)$, sull'insieme di variabili (tipicamente discrete) $X_V$ indicizzate da $V$ possono essere fattorizzate come

\begin{equation}
P(X_V) = \prod_{v \in V} P(P x_v \mid X_{\text{pa}(v)}),
(\#eq:bayes-net)
 \end{equation}

 dove $X_{\text{pa}(v)}$ denota l'insieme (preferibilmente piccolo) di nodi genitori della variabile $X_v$ per ciascun nodo $v \in V$.

La fattorizzazione in \@ref(eq:bayes-net) esprime un insieme di ipotesi di indipendenza, che sono rappresentate in un DAG in termini di coppie di nodi che non sono direttamente collegati tra loro da un collegamento diretto. È l'esistenza di tali assunzioni di indipendenza, e il fatto che vi è un piccolo insieme di genitori per ciascun nodo, che rende possibile specificare le probabilità condizionali ed eseguire l'inferenza in modo efficiente in una rete bayesiana.





<!-- Una BN è quindi una rappresentazione grafica di un sistema. Nello specifico, una BN rappresenta la distribuzione congiunta di un sistema facendo una serie di assunzioni relativamente all'indipendenza condizionale tra le variabili (un nome migliore per la BN sarebbe infatti "diagramma di indipendenza", ma il termine "modello grafico" è ormai radicato). I nodi rappresentano quindi le variabili del sistema, gli archi (frecce) ne mostrano qualitativamente le relazioni di dipendenza e le probabilita condizionali ne definiscono quantitativamente i valori. -->

   <!-- Utilizzando la condizione di Markov si può affermare che la distribuzione di probabilità congiunta dell'insieme di variabili rappresentate è formalizzata come il prodotto di un insieme di probabilità locali, permettendo in tal modo di ridurre la complessità del problema e di facilitare la specificazione del modello probabilistico. La condizione di Markov afferma ogni variabile $X \in U$ è indipendente dai suoi non-discendenti condizionatamente ai suoi genitori. -->

<!-- I nodi nei BN sono di tre tipi. -->

<!-- 1. Le costanti sono fissate per disegno: sono sempre nodi radice (cioè non hanno genitori) e sono indicate come rettangoli nel grafico. -->
<!-- 2. I nodi stocastici sono variabili a cui viene data una distribuzione di probabilità e sono indicate come ellissi nel grafico; possono essere genitori o figli (o entrambi). I nodi stocastici possono essere osservati, nel qual caso sono dati, o possono essere non osservati, e quindi sono parametri. -->
<!-- 3. I nodi deterministici sono funzioni logiche di altri nodi. -->

<!-- I collegamenti diretti possono essere di due tipi: una freccia piena e sottile indica una dipendenza stocastica mentre una freccia trateggiata (o spessa) indica una funzione logica (vedremo un esempio di funzione logica, ovvero di relazione deterministica, nella parte VI della dispensa, in relazione al modello lineare).  -->

<!-- Utilizzando la condizione di Markov si può affermare che la distribuzione di probabilità congiunta dell'insieme di variabili rappresentate è formalizzata come il prodotto di un insieme di probabilità locali, permettendo in tal modo di ridurre la complessità del problema e di facilitare la specificazione del modello probabilistico. La condizione di Markov afferma ogni variabile nel sistema considerato è indipendente dai suoi non-discendenti condizionatamente ai suoi genitori. In altre parole, la distribuzione congiunta completa di tutte le quantità $U$ ha una semplice fattorizzazione in termini di distribuzione condizionale $p(x \mid \text{genitori}[x])$ di ciascun nodo dati i suoi genitori, in modo che -->

   <!-- $$ -->
   <!-- P(X) = \prod_{i=1}^n p(x \mid \text{genitori}[x]), \quad \forall x \in U. -->
   <!-- $$ -->



   Ad esempio, quando inferiamo i parametri dai dati $x_1, \dots, x_n$, spesso assumiamo che le osservazioni siano i.i.d.. Utilizzando un BN possiamo rappresentare questa ipotesi come indicato nella figura seguente.

 ![](images/plates_example_1.png){width=30%}

 La figura illustra l'ipotesi che ogni osservazione $x_i$ sia stata generata indipendentemente dalla stessa distribuzione. Si noti che i dati sono condizionalmente indipendenti dal parametro $\theta$; marginalmente, invece, le osservazioni sono dipendenti.

Per evitare una rappresentazione grafica disordinata, nei BN è comune utilizzare una forma di zucchero sintattico (_syntactic sugar_) chiamato _"plates"_ (piastre): si disegna  un piccolo riquadro attorno alle variabili ripetute, con la convenzione che i nodi all'interno del riquadro vengano ripetuti il numero indicato di volte.

 ![](images/plates_example_2.png){width=30%}

 La distribuzione congiunta corrispondente alla figura precedente ha la forma

 $$
   p(\theta, \mathcal{D}) = p(\theta) \left[ \prod_{i=1}^n p(x_i \mid \theta) \right].
 $$

   Gli autori del linguaggio _OpenBUGS_ raccomandano la costruzione di un _directed graphical model_ prima di qualsiasi analisi statistica.

 ### Topologia delle Reti Bayesiane

 In sintesi, una BN è una struttura di dati utilizzata per rappresentare, sia visivamente, tramite la connessione nodi –- archi, sia matematicamente, tramite la distribuzione di probabilità congiunta, la dipendenza fra le variabili di un sistema.
 Possiamo perciò definire le BN come un grafo per cui valgono le seguenti caratteristiche, già in parte precedentemente anticipate:

   1. Un insieme di variabili casuali costituiscono i nodi della rete.
 2. Un insieme di archi con verso connette le coppie di nodi. Il significato di
 una freccia dal nodo $X_1$ al nodo $X_2$ è che $X_1$ ha un'influenza diretta su $X_2$.
3. Può essere associato ad ogni nodo un insieme di probabilità condizionate che quantificano gli effetti che i genitori hanno sul nodo.
4. I genitori di un nodo sono tutti quei nodi che hanno frecce che puntano al
nodo.
5. I nodi radice sono tutti quei nodi che non hanno genitori.
6. I nodi foglia sono tutti quei nodi che non hanno alcun figlio; non hanno
nessuna influenza causale sulle altre variabili.
7. Il grafo non genera e non contiene cicli diretti (grafo diretto aciclico,
DAG, dall'inglese _Directed Acyclic Graph_)

