# Progetto pr2 caml

Il progetto ha l’obiettivo di applicare a casi specifici i concetti e le tecniche di implementazione dei linguaggi
di programmazione esaminate durante la seconda parte del corso. Il progetto consiste nella progettazione
e realizzazione di un interprete per un semplice linguaggio di programmazione.

## Descrizione: Progettazione e sviluppo di un interprete in OCaml
Si consideri un’estensione del linguaggio didattico funzionale presentato a lezione che permetta di
manipolare come dati primitivi dizionari di elementi, ovvero una collezione di coppie (chiave, valore).
Assumiamo che la chiave sia un identificatore.

Introduciamo brevemente la sintassi concreta per operare con dizionari.
  - *creazione di un dizionario vuoto* \
    my_dict = {} \
    :- my_dict {}

  - *creazione di un dizionario con valori* \
    my_dict = {'name': 'Giovanni', 'matricola': 123456} \
    :- my_dict = {'name': 'Giovanni', 'matricola': 123456}

  - *accedere a un elemento di un dizionario* \
    my_dict['name']) \
    :- 'Giovanni' \
    my_dict('matricola') \
    :- 123456

  - *operazioni (dizionari sono immutable)* \
    my_dict1 = my_dict['età'] = 22 \
    -: my_dict1{'età': 22,'name': 'Giovanni', 'matricola': 123456} \
    my_dict2 = rm(my_dict1, 'name') \
    -: my_dict2{'età': 22,'matricola': 123456} \
    my_dict3 = clear(my_dict2) \
    -: my_dict3{} \
    ApplyOver((fun x -> x+1), my_dict2)\
    :- {'età': 23,'matricola': 123457}

Il significato di ApplyOver(ex, dict) diventa ovvio: si tratta di applicare la funzione denotata dal primo
parametro ex al valore associato a ogni elemento del; dizionario denotato dal secondo parametro dict,
creando di conseguenza un nuovo dizionario.

## Consegna
1. Si definisca la sintassi concreta del linguaggio e la sintassi astratta tramite una opportuna definizione di
tipo in OCaML.
2. Si definisca l’interprete OCaMl del linguaggio funzionale assumendo la regola di scoping statico.
3. Si fornisca di conseguenza il type checker dinamico del linguaggio risultante, in alternativa si può
fornire il type checker statico del linguaggio
4. Si verifichi la correttezza dell’interprete progettando ed eseguendo una quantità di casi di test
sufficiente a testare tutti gli operatori aggiuntivi.
