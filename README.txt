Implementation of a database in Racket (variant of Lisp), using functional programming.

Tema 1 - Paradigme de programare
Mini Database - Racket

Nume: Mihalache Florin-Razvan
Grupa: 323CC

Mod de implementare a bazei de date: o baza de date reprezinta o lista de tabele,
iar o tabela reprezinta o lista, unde capul listei este numele tabelei si restul
listei sunt coloanele (in principiu, coloanele sunt si ele liste - capul listei
este numele coloanei si restul listei e reprezentat de elemente din intrarile
din tabela, asta daca nicio coloana nu e goala - daca toate sunt goale, atunci
coloana va fi un string reprezentat de numele acesteia)

Fiecare functie din tema este descrisa (rolul ei) in cod (e mai usor de urmarit asa).

Task 0 - functiile de baza
- remove-table - am o functie helper in care adaug toate tabelele din baza de date
intr-o alta baza de date, cu exceptia tabelei cautate
- get-columns - vad daca tabela are coloanele goale sau nu si adaug numele acestora
intr-o lista
- get-name - capul tabelei
- get-table - caut numele tabelei in baza de date si o intorc tabela (daca exista)
- get-tables - baza de date in sine
- init-database - null
- add-table - adaug o tabela la lista ce reprezinta baza de date

Task 1 - simple-select + insert
simple-select - am o functie (find-column) in care caut o coloana intr-o tabela prin
numele acesteia si daca o gasesc o returnez (o folosesc cand iterez lista de coloane)

insert - iau lista de perechi, fac fill-in cu NULL pentru coloanele care nu se regasesc
in lista de perechi, apoi fac o conversie pair -> lista cu 2 elemente (coloana si elementul)
ordonata conform ordinii coloanelor din tabela (pentru a face inserarea usoara), apoi
reconstruiesc tabela (adica o iau de la zero cu ea) in care adaug coloanele cu elementele
care trebuie sa fie inserate

Task 2 - select
Taskul cel mai greu din tema si cel mai muncitoresc. Mai intai fac functile max, min, sum,
avg, sort-asc, sort-desc si count (aici am o functie helper unde iau doar prima aparitie
a fiecarui element intr-o lista). Filtrez tabela conform conditiilor din select, adica
iau entry-urile din tabela (intrarile aka liniile), ma folosesc de indicii coloanelor in tabela
pentru a satisface conditiile (asta ca sa accesez coloana dintr-un entry pentru filtrare), 
apoi recream tabela din entry-urile filtrate si am creat un fel de simple-select 
(simple-select-filtered) in acest caz pe care il apelez in select (daca nu am 
operatii in lista de coloane), altfel apelez functia eval-columns (evaluarea 
functiilor pe coloane) pe tabela filtrata.

Task 4 - delete (pe asta am facut-o mai intai, era mai usor aici)
Aici m-am folosit de filtrarea de la select si am luat entry-urile care nu erau in tabela
filtrata cu conditiile data si am cu ele am refacut tabela care inlocuieste tabela veche
in baza de date.

Task 3 - update
La acest task m-am folosit de delete (mi-a fost usor asa) si am facut in felul urmator:
tabela cautata am spart-o in doua cu ajutorul lui delete: cea cu liniile ce trebuie
sa fie updatate si cealalta care ramane la fel. Am o functie replace-all-values ce
inlocuieste in liniile date dintr-o tabela data cu valorile care sunt tip pereche
(coloana . valoare), apoi intercalez liniile updatate si cele nemodificate pentru
a se asemana cu tabelul original (adica sa fac tabelul updatat).

Task 5 (bonus) - natural-join
Aici am aplicat o strategie: filtrez prima tabela cu conditiile date, apoi ma plimb
pe coloana comuna celor 2 tabele in prima tabela si iau liniile din a doua tabela, 
ca sa concatenez coloanele cu valorile corespunzatoare liniilor (+ creez tabela cu
toate coloanele tabelelor, in care inserez liniile create cand ma plimbam prin prima
tabela, iar pe tabela rezultata aplic simple-select).


