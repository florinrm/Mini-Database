#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces

;

(define init-database
  (λ ()
    null)) ;; initializez baza de date

(define (create-table-helper table columns) (
                                             if (null? columns)
                                                table
                                                (create-table-helper (append table (list (car columns))) (cdr columns))
                                             )) ;; adaugarea numelor de coloane la lista tabela (aia care are doar numele tabelei)

(define create-table
  (λ (table-name columns-name)
    (create-table-helper (list table-name) columns-name))) ;; cream o tabela cu goala, doar cu numele coloanelor
  

(define get-name
  (λ (table)
    (car table))) ;; numele tabelei

(define get-columns
  (λ (table)
    (cdr table))) ;; coloanele dintr-o baza de date

(define get-tables
  (λ (db)
    db)) ;; tabelele din baza de date aka baza de date in sine (care e o lista de tabele, iar o tabela o lista in sine)

(define get-table
  (λ (db table-name)
    (cond ((null? db) null)
          ((equal? table-name (car (car db))) (car db))
          (else (get-table (cdr db) table-name))
          )))

(define add-table
  (λ (db table)
    (cons table db)))

(define remove-table-helper (λ (db table-name acc)
                              (
                               cond ((null? db) acc)
                               ((not (equal? table-name (car (car db)))) (remove-table-helper (cdr db) table-name (append acc (car db))))
                               (else (remove-table-helper (cdr db) table-name acc))
                               )))

(define remove-table
  (λ (db table-name)
    (remove-table-helper db table-name null)))

(define table1 (create-table "studenti" (list '("numar matricol") '("nume") '("prenume") '("grupa") '("medie"))))
(define table2 (create-table "tabela cursuri" (list '("anul") '("semestru") '("disciplina") '("numar credite") '("numar teme"))))
(define table3 (create-table "tabela cursuri" (list '("anul" 1 2) '("semestru" 1 2)
                                                   '("disciplina" "Programarea calculatoarelor" "Paradigme de programare") '("numar credite" 5 6) '("numar teme" 2 3))))

;; create-table OK!

;(display table2)
;(display "\n")
;(display table1)
;(define data-base (init-database))
;(get-columns (get-table (add-table (add-table data-base table1) table2) (car table1))) ;; add-table OK! remove-table OK! get-table OK! ;; get-columns OK!


;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db (add-table (add-table (init-database) table1) table2))
db
;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

(define pairs (list '("disciplina" . "Structuri de date") '("anul" . 1) '("numar teme" . 3)))

(define (get-columns-name-helper table) (
                                  if (null? table)
                                     null
                                     (cons (car (car table)) (get-columns-name-helper (cdr table)))
                                  )) ;; helper pentru numele coloanelor

(define (get-columns-names table) (get-columns-name-helper (get-columns table))) ;; iau numele coloanelor

(define (get-pair element list-pair) (
                                      cond ((null? list-pair) null)
                                           ((equal? element (car (car list-pair))) (car list-pair))
                                           (else (get-pair element (cdr list-pair)))
                                      )) ;; caut perechea dupa primul element din pereche --> it's ok!
;(get-pair "anul" pairs)

(define (build-pair list-pair list-name) (
                                          cond ((null? list-name) '())
                                               ((null? (get-pair (car list-name) list-pair)) (cons (list (car list-name) null) (build-pair list-pair (cdr list-name))))
                                               (else (cons (get-pair (car list-name) list-pair) (build-pair list-pair (cdr list-name)))
                                             ))) ;; reconstruiesc lista de perechi incat sa am NULL si la cele care nu erau in lista originala de perechi pt insert --> merge yey

(define (add-in-columns table list-pair) (
                                          if (or (null? table) (null? list-pair))
                                             null
                                             (cons (append (car table) (list (cdr (car list-pair)))) (add-in-columns (cdr table) (cdr list-pair)))
                                          ))
;(car pairs)
;(append (list (car table3)) (add-in-columns (get-columns table3) (build-pair pairs (get-columns-names table3)))) ;;gata, folosim la insert, pairs e lista data in argument la insert

;(build-pair pairs (get-columns-names table3))

(define insert
  (λ (db table-name record)
    (
     cond ((or (null? db) (null? record)) null)
          ((equal? (car (car db)) table-name) (cons (append (list (car (car db))) (add-in-columns (get-columns (car db)) (build-pair record (get-columns-names (car db)))))
                                                      (insert (cdr db) table-name record)))
          (else (cons (car db) (insert (cdr db) table-name record))
     ))))

(insert db "tabela cursuri" pairs) ;; YAAAAY AM FACUT INSERTUUUUL

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

(define find-column (λ (table column) (
                                       cond ((null? table) null)
                                            ((equal? column (car (car table))) (cdr (car table))) ;; e returnata coloana fara numele acesteia
                                            (else (find-column (cdr table) column)) ;; cautam in continuare coloana
                                       ))) ;; cauta o coloana intr-o tabela

(define simple-select-helper (λ (table columns acc) (
                                                     cond ((or (null? table) (null? columns)) acc)
                                                          ((not (null? (find-column table (car columns))))
                                                                (simple-select-helper table (cdr columns) (append acc (list (find-column table (car columns))))))
                                                          (else (simple-select-helper (table (cdr columns) acc)))
                                                     ))) ;; adauga coloanele din lista existente in tabela intr-o lista de coloane

(define simple-select
  (λ (db table-name columns)
    (
     cond ((or (null? db) (null? columns)) db)
          ((equal? table-name (car (car db))) (simple-select-helper (get-columns (car db)) columns null))
          (else simple-select (cdr db) table-name columns)
     )))

;; simple-select done!


;(define db1 (add-table (init-database) table3))
;(simple-select db1 "tabela cursuri" (list "disciplina" "anul"))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================
(define select
  (λ (db table-name columns conditions)
    'your-code-here))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define update
  (λ (db table-name values conditions)
    'your-code-here))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
(define delete
  (λ (db table-name conditions)
    'your-code-here))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    'your-code-here))
