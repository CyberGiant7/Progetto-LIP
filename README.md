# Lip-Progetto 2022

Studenti:
- Dessì Leonardo
- Corda Erica
- Dessì Matteo
- Giuffrida Simone

# Il linguaggio Repeat
Il linguaggio di programmazione di cui dovrete scrivere l’interprete, il lexer ed il parser è descritto di seguito.
Dovrete realizzare la small step semantics, mentre in questa descrizione viene data la big step semantics. Il
linguaggio ha dichiarazioni di variabili che devono essere di tipo intero (pur non avendo nè un type checker
nè si fa inferenza di tipo), di array, sempre di tipo intero, e procedure che hanno un solo parametro formale
che può essere passato per valore o riferimento. Le dichiarazioni di procedura possono essere fatte solo nel
blocco più esterno, quindi sono dichiarazioni globali. Il linguaggio ha side effects.

### Dichiarazioni
Per le dichiarazioni di variabile e array abbiamo la seguente grammatica:
$$d ::= \texttt{nullvar} \mid dv_1; dv_2 \mid \texttt{var}(X) \mid \texttt{array}(X, dim)$$
mentre le procedure abbiamo
$$dp ::= \texttt{nullproc} \mid dp_1; dp_2 \mid \texttt{proc } X(pf) \{c\}$$ 
$\texttt{nullvar}$ e $\texttt{nullproc}$ sono le dichiarazioni vuote.

La dichiarazione di una variabile associa all’identificatore una *locazione*, e nella dichiarazione di array $X$ è
il nome dell’array mentre $dim$ è la dimensione, nell’ambiente viene associata al nome $X$ la coppia (locazione
del primo elemento, dimensione dell’array). Nella dichiarazione di una procedura $\texttt{proc}(X, prm, c)$ $X$ è
l’identificatore (il nome della procedura), e nell’ambiente si associano al nome il corpo della procedura e il
parametro formale ( $pf$ ),


### Espressioni
Le espressioni seguono la seguente grammatica
$$e ::= n \mid e_1 + e_2 \mid e_1 − e_2 \mid e_1 \times e_2 \mid true \mid false \mid e_1 \wedge e_2 \mid e_1 \vee e_2 \mid \neg e_1 \mid e_1 = e_2 \mid e_1 \le e_2 \mid X \mid X[e]$$
dove 
- $n$ indica il numero $n$,
- <tt>true</tt> e <tt>false</tt> sono i valori base,
- $X$ è una variabile mentre $X[e]$ è la variabile associata all’ $i$ -esima posizione dell’array,
    - dove $i$ è la valutazione dell’espressione e che deve restituire un intero positivo, entrambe restituiscono il valore associato in memoria alla locazione associata al nome, con la differenza che nel caso dell’array viene fatto anche un controllo sulla dimensione.

### Comandi:
I comandi sono i seguenti
$$c ::= skip \mid break \mid X := e \mid X[e_1] := e_2 \mid c_1; c_2 \mid \texttt{repeat } c \texttt{ forever} \mid \texttt{if } e \texttt{ then } c_1 \texttt{ else } c_2 \mid \texttt{block}(dv, c) \mid X(pa)$$
Il comando <tt>break</tt> interrompe la computazione. Viene usato sostanzialmente per interrompere la computazione di un $\texttt{repeat } c \texttt{ forever}$ che ripete l’esecuzione del comando indefinitamente. Gli altri costrutti sono standard.
$X(par)$ indica la chiamata alla procedura $X$ con parametro attuale $pa$. I blocchi sono delimitati
da parentesi graffe e possono avere dichiarazioni locali ma solo di variabili, non di procedure.

### Parametri 
Il parametro (sia formale che attuale) può essere passato per valore o per riferimento. Quindi abbiamo che nei parametri formali $(pf)$ dobbiamo specificare come è passato un parametro, mentre i parametri attuali $(pa)$ possono essere espressioni qualsiasi.

$$
pf ::= \texttt{val}\ X \mid \texttt{ref}\ X
$$

$$
pa ::= e
$$

### Programma
Un programma è fatto da una dichiarazione ed un comando, quindi la sintassi è la seguente:
$$p ::= dv; dp; c$$

## La semantica del linguaggio **Repeat**
Di seguito, per ogni categoria sintattica, vengono date le regole della semantica. Si assume che lo scope sia **dinamico**. 
- Con $\rho$ indichiamo l’**ambiente** (una funzione tra identificatori ed oggetti denotabili)
- con $\sigma$ indichiamo la memoria (una funzione da locazioni a valori memorizzabili)
- infine con $\gamma$ abbiamo un **flag** che ha due valori: 
    - <tt>br</tt> 
    - <tt>ok</tt>. 
- $\rho\bot$ è la funzione ambiente ovunque indefinita
- $\sigma\bot$ è la memoria ovunque indefinita. 
- $\ell$ è una locazione (un intero positivo) e indica la prima locazione libera
- l’aggiornamento della locazione libera
è indicato con $\leftarrow$.
Assumiamo che le locazioni siano interi e che partano da 0.
### Programma
![image](https://user-images.githubusercontent.com/81624394/209585923-5b1d0a5b-f593-497d-829a-37e1a489d6d3.png)

### Dichiarazioni
Per le dichiarazioni di variabili e array abbiamo:

![image](https://user-images.githubusercontent.com/81624394/209585944-8058736f-3e5e-4e6d-a1c8-da14ab749e84.png)

mentre le regole per le procedure sono

![image](https://user-images.githubusercontent.com/81624394/209585970-e14614e2-3574-45bc-8149-2ddf12020912.png)

### Espressioni
![image](https://user-images.githubusercontent.com/81624394/209585993-0503e01e-648b-4d5e-b09d-3af87e24e6f5.png)


### Comandi
Nelle regole seguenti $\gamma \in \{\texttt{ok}, \texttt{br}\}$. Dato che i comandi ora hanno una sorta di tipo di terminazione ( $\texttt{ok}$ o $\texttt{br}$ ) ci sono due regole big step per il ';'.
![image](https://user-images.githubusercontent.com/81624394/209586013-82403158-74d2-40a0-9795-ce0a5089df58.png)
![image](https://user-images.githubusercontent.com/81624394/209586032-e313c2f0-9ab3-45e2-89f8-402b46060f7d.png)
![image](https://user-images.githubusercontent.com/81624394/209586043-78295f9a-162c-4d7b-b018-6d79af7c94ee.png)

[202223ProgettoLiP.pdf](https://github.com/CyberGiant7/Progetto-LIP/files/10304900/202223ProgettoLiP.pdf)

