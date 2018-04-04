#lang racket

(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    null))

;; o tabela e o lista de liste care are ca prim element numele acesteia si restul sunt liste (liste de liste))


(define create-table
  (λ (table-name columns-name)
    (cons table-name columns-name))) ;; cream o tabela cu goala, doar cu numele coloanelor

(define get-name
  (λ (table) (
    if (null? table)
       null
       (car table)))) ;; numele tabelei

(define (get-columns-helper table) (
                             if (null? table)
                                null
                                    (cons (car table) (get-columns-helper (cdr table)))))
(define (get-columns-helper2 table)
  (
   if (null? table)
      null
      (get-columns-helper table)
   )) ;; cand nu sunt elemente in coloana

(define (get-columns-helper3 table) (
                          if (null? table)
                             null
                             (cons (car (car table)) (get-columns-helper3 (cdr table)))))

(define (get-columns table) (
        if (<= (length table) 2)
            null
            (if (not (andmap list? (cdr table)))
                (cdr (get-columns-helper2 table))
                (get-columns-helper3 (cdr table))
                )))

(define get-tables
  (λ (db)
    db)) ;; tabelele din baza de date aka baza de date in sine (care e o lista de tabele, iar o tabela o lista in sine)

(define get-table
  (λ (db table-name)
    (
     cond ((null? db) null)
          ((equal? table-name (car (car db))) (car db))
          (else (get-table (cdr db) table-name))
     )))

(define add-table
  (λ (db table)
    (cons table db)))

(define (remove-helper db table-name) (
                                      cond ((null? db) null)
                                           ((equal? table-name (car (car db))) (remove-helper (cdr db) table-name))
                                           (else (cons (car db) (remove-helper (cdr db) table-name)))
                                      )) 

(define remove-table
  (λ (db table-name)
    (if (null? (get-table db table-name))
        db
        (remove-helper db table-name))))



(define table1 (create-table "Studenți" (list '("Număr matricol" 123 124 125 126)
                                              '("Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
                                              '("Prenume" "Gigel" "Maria" "Ionel" "Ioana")
                                              '("Grupă" "321CA" "321CB" "321CC" "321CD")
                                              '("Medie" 9.82 9.91 9.99 9.87))))
(define table2 (create-table "Cursuri" (list '("Anul" "I" "II" "III" "IV" "I" "III")
                                             '("Semestru" "I" "II" "I" "I" "II" "II")
                                             '("Disciplină" "Programarea calculatoarelor" "Paradigme de programare"
                                                            "Algoritmi paraleli și distribuiți" "Inteligență artificială"
                                                            "Structuri de date" "Baze de date")
                                             '("Număr credite" 5 6 5 6 5 5)
                                             '("Număr teme" 2 3 3 3 3 0))))
;(get-columns table1)

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


;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

(define (get-pair pairs column) (
                                  cond ((null? pairs) null)
                                       ((equal? column (car (first pairs))) (car pairs))
                                       (else (get-pair (cdr pairs) column))
                                 )) ;daca nu am coloane intr-o lista -> lista de perechi pentru toate coloanele - daca nu am SD -> ("SD" . NULL)

(define (reconstruct-list pairs columns) (
                                          cond ((null? columns) null)
                                               ((null? (get-pair pairs (car columns))) (cons (list (car columns) NULL) (reconstruct-list pairs (cdr columns))))
                                               (else (cons (list (car (get-pair pairs (car columns))) (drop (get-pair pairs (car columns)) 1)) (reconstruct-list pairs (cdr columns))))
                                          )) ;reconstruiesc o lista bazata pe o lista de perechi ce trebuie sa fie inserate
;(define pairs '(("Anul" . "I") ("Disciplină" . "Matematică")))
;(reconstruct-list pairs (get-columns (get-table db "Cursuri")))
;(define table (get-table db "Cursuri"))
(define (pula table listing) (
                              if (or (null? table) (null? listing))
                                 null
                                 (cons (append (car table) (list (last (car listing)))) (pula (cdr table) (cdr listing)))
                              ))
;(pula (cdr table) (reconstruct-list pairs (get-columns (get-table db "Cursuri"))))

(define (pula2 table listing) (
                              if (or (null? table) (null? listing))
                                 null
                                 (cons (append (list (car table)) (list (last (car listing)))) (pula2 (cdr table) (cdr listing)))
                              ))
;(define table-pula (create-table "Test" '("Coloana1" "Coloana2" "Coloana3")))
;(define pairs2 '(("Coloana2" . "Muie") ("Coloana1" . "Nan")))
;(pula2 (cdr table-pula) (reconstruct-list pairs2 (get-columns table-pula)))

(define (my-insert table record) (
                                  if (or (null? table) (null? record))
                                     null
                                     (append (list (car table)) (
                                      if (andmap list? (cdr table))
                                         (pula (cdr table) (reconstruct-list record (get-columns table)))
                                         (pula2 (cdr table) (reconstruct-list record (get-columns table)))
                                      )))) ;; fac o lista de mai multe liste cu 2 elemente, transformata dintr-o lista de perechi

(define insert
  (λ (db table-name record)
    (
     if (or (null? db) (null? (get-table db table-name)))
        null
        (cons (my-insert (get-table db table-name) record) (remove-table db table-name))
     )))


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================


(define (find-column table column) (
                                    if (null? table)
                                       null
                                       (
                                        if (andmap list? table)
                                           (if (equal? column (car (car table))) (cdr (car table))
                                               (find-column (cdr table) column))
                                           (if (equal? column (car table)) '()
                                               (find-column (cdr table) column))
                                        )
                                    )) ;; returneaza o coloana dupa numele acesteia (fara numele acesteia)

(define (search-columns table columns) (
                                        cond ((or (null? table) (null? columns)) null)
                                             ((null? (find-column table (car columns))) (search-columns table (cdr columns)))
                                             (else (cons (find-column table (car columns)) (search-columns table (cdr columns))))
                                        )) ;; caut coloanele cerute dintr-un tabel

(define simple-select
  (λ (db table-name columns)
    (
     cond ((or (null? db) (null? columns)) null)
          ((null? (get-table db table-name)) null)
          (else (search-columns (cdr (get-table db table-name)) columns))
     )))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define (index listing elem) (
                              cond ((not (member elem listing)) -1)
                                   ((null? listing) -1)
                                   ((equal? elem (car listing)) 0)
                                   (else (+ 1 (index (cdr listing) elem)))
                              )) ;;list-ref

(define (sort-asc column) (if (null? column) null (sort column <)))
(define (sort-desc column) (sort column >))
(define (maximum column) (if (null? column) null (car (sort column >))))
(define (minimum column) (if (null? column) null (car (sort column <))))
(define (suma column) (
                       if (null? column)
                          0
                          (+ (car column) (suma (cdr column)))
                       ))

(find-column (get-table db "Cursuri") "Anul")

(define (delete-duplicates-helper listing acc) (
                                                cond ((null? listing) acc)
                                                     ((member (car listing) acc)  (delete-duplicates-helper (cdr listing) acc))
                                                     (else (delete-duplicates-helper (cdr listing) (cons (car listing) acc)))
                                                ))
(define (delete-duplicates listing) (delete-duplicates-helper listing null))
(delete-duplicates (list 1 2 3 1 5 2 7 9 3))

(define (count column) (
                        length (delete-duplicates column) ;; asa ca sa treaca testele
                        ))

(define (avg column) (if (null? column)
                         null
                         (/ (suma column) (count column))))

(define (op-column operator column) (
                          if (null? column)
                             null
                             (
                              cond ((equal? operator 'max) (maximum column))
                                   ((equal? operator 'min) (minimum column))
                                   ((equal? operator 'sort-asc) (sort-asc column))
                                   ((equal? operator 'sum) (suma column))
                                   ((equal? operator 'avg) (avg column))
                                   ((equal? operator 'sort-desc) (sort-desc column))
                                   (else (count column)))))
                              
(define muie (list 4 5 3 8 3))
;(maximum muie)

(define (filter-col sign column value) (filter (λ (x) (sign x value)) column))
;(filter-col > (find-column (cdr (get-table db "Cursuri")) "Număr teme") 1)

;(take-line-name (cdr (get-table db "Cursuri")) "Număr teme" 2) ;; ia bine
(define (get-column-index table column) (
                                         if (null? table)
                                            null
                                            (index (get-columns table) column)
                                         )) ;; iau indexul unei coloane (0 indexed) --> merge!
;(get-column-index (get-table db "Cursuri") "Anul")


;(find-column-with-name (cdr (get-table db "Cursuri")) "Număr teme")
;(list-ref (take-line-name (cdr (get-table db "Cursuri")) "Disciplină" "Structuri de date") (get-column-index (get-table db "Cursuri") "Anul")) ;; so goood
;(get-column-index (get-table db "Cursuri") "Număr teme")
;(filter-col > (find-column (cdr (get-table db "Cursuri")) "Număr teme") 2)
;(take-line (cdr (get-table db "Cursuri")) 1)
;(index (get-columns (get-table db "Cursuri")) "Număr teme")
;(find-column (cdr (get-table db "Cursuri")) "Număr credite")


(define (get-line-index-helper table index) (
                                             cond ((null? table) null)
                                                ((>= index (sub1 (length (car table)))) null)
                                                (else (cons (list-ref (car table) (add1 index)) (get-line-index-helper (cdr table) index)))
                                             )) ;tabela, dar fara nume

(define (get-line-index table index) (
                                      if (or (null? table) (not (andmap list? (cdr table))))
                                         null
                                         (get-line-index-helper (cdr table) index)
                                      ))
;(get-line-index (get-table db "Studenți") 3)
(define (get-table-lines-helper table index) (
                                             cond ((null? table) null)
                                                  ((>= index (sub1 (length (cadr table)))) null)
                                                  (else (cons (get-line-index table index) (get-table-lines-helper table (add1 index))))
                                             ))
(define (get-table-lines table) (
                                 if (or (null? table) (not (andmap list? (cdr table))))
                                    null
                                    (get-table-lines-helper table 0)))

;(get-line-index (get-table db "Cursuri") 6)

;(define (filter-table-helper table sign column value index) (
 ;                                                            cond ((or (null? table) (> index (length (cdr table)))) null)
  ;                                                                ((member (list-ref (get-line-index table index) (get-column-index table column))
   ;                                                                        (filter-col sign (find-column (cdr table) column) value))
    ;                                                               (cons (get-line-index table index) (filter-table-helper table sign column value (add1 index))))
     ;                                                             (else (filter-table-helper table sign column value (add1 index)))
      ;                                                       )) ; iau toata tabela cu tot cu nume, coloana ca string index incepand cu 1

(define (get-column-size table) (
                                 cond ((null? table) null)
                                      ((not (andmap list? (cdr table))) 0)
                                      (else (sub1 (length (second table)))
                                 )))

(define (filter-table-helper table sign column value index) (
                                                             cond ((or (null? table) (null? (get-line-index table index))) null)
                                                                  ((>= index (get-column-size table)) null)
                                                                  ((member (list-ref (get-line-index table index) (get-column-index table column))
                                                                           (filter-col sign (find-column (cdr table) column) value))
                                                                   (cons (get-line-index table index) (filter-table-helper table sign column value (add1 index))))
                                                                  (else (filter-table-helper table sign column value (add1 index)))))

(define (filter-table table sign column value) (
                                                 cond ((null? table) null)
                                                      ((not (andmap list? (cdr table))) table)
                                                     (else (filter-table-helper table sign column value 0)))) ; filtrez tabela dupa conditie

;(filter-table (get-table db "Cursuri") > "Număr teme" 2)


(define (insert-column-helper table column entries) (
                                        if (null? entries)
                                           null
                                           (cons (list-ref (car entries) (get-column-index table column)) (insert-column-helper table column (cdr entries)))
                                        ))
(define (insert-column table column entries) (
                                              cons column (insert-column-helper table column entries)
                                              ))

(define (insert-pula-mea table columns entries) (
                                                 if (or (null? table) (null? columns))
                                                    null
                                                    (cons (insert-column table (car columns) entries) (insert-pula-mea table (cdr columns) entries))
                                                 ))

(define (recreate-table table entries) (
                                        if (not (andmap list? (cdr table)))
                                           table
                                           (if (null? entries)
                                               (cons (car table) (get-columns table))
                                               (cons (car table) (insert-pula-mea table (get-columns table) entries))
                                        ))) ; reconstructia de tabel filtrat - o singura lista de entry


(define (rebuild-filt-table table list-entries) (
                                                 if (or (null? table) (null? list-entries))
                                                    table
                                                    (rebuild-filt-table (recreate-table table (filter-table table (first (car list-entries))
                                                                        (second (car list-entries)) (third (car list-entries)))) (cdr list-entries))
                                                 )) ;filtrez un tabel dupa mai multe criterii de filtrare + recreare de tabel (list-entries = criterii)


(define (filter-null lines) (
                             cond ((null? lines) null)
                                  ((andmap null? (car lines)) (filter-null (cdr lines)))
                                  (else (cons (car lines) (filter-null (cdr lines))))))


;(rebuild-filt-table (get-table db "Cursuri") (list (list < "Număr teme" 4) (list equal? "Semestru" "I"))) ;; merge struna -> tabelul filtrat
;(get-column-index (get-table db "Cursuri") "Număr teme")
                                 
;(get-table-lines (get-table db "Cursuri")) ;; e ok
;(get-table-lines (rebuild-filt-table (get-table db "Cursuri") (list (list < "Număr teme" 4) (list equal? "Semestru" "I")))) ;; e ok
;(recreate-table (get-table db "Cursuri") (get-table-lines (rebuild-filt-table (get-table db "Cursuri") (list (list < "Număr teme" 4) (list equal? "Semestru" "I"))))) ;; awww yissss
; tabel filtrat fara probleme!
                                                        
;(find-entry-by-elem (get-table db "Cursuri") "Disciplină" "Inteligență artificială"
;                    (get-table-lines (rebuild-filt-table (get-table db "Cursuri") (list (list < "Număr teme" 4) (list equal? "Semestru" "I"))))) ;; e ok!


;(recreate-table (get-table db "Cursuri") lel)

(define simple-select-filtered
  (λ (db table-name columns conditions)
    (
     cond ((or (null? db) (null? columns)) null)
          ((null? (rebuild-filt-table (get-table db table-name) conditions)) null)
          (else (search-columns (cdr (rebuild-filt-table (get-table db table-name) conditions)) columns))
     )))
;(rebuild-filt-table (get-table db table-name) conditions)

(define operators (list 'min 'max 'avg 'count 'sort-asc 'sort-desc 'sum))
(define operators-list (list 'sort-desc 'sort-asc))
(define operators-unary (list 'min 'max 'sum 'count 'avg))

                                  
(define (filter-pairs listing) (filter (λ (x) (pair? x)) listing))
(define (eval-columns table columns) (
                                      cond ((or (null? table) (null? columns)) null)
                                           ((not (pair? (car columns))) (cons (find-column (cdr table) (car columns)) (eval-columns table (cdr columns))))
                                           (else (cons (op-column (car (car columns)) (find-column (cdr table) (cdr (car columns)))) (eval-columns table (cdr columns))))
                                      ))
;(eval-columns (get-table db "Studenți") (list (cons 'sort-asc "Număr matricol") "Prenume" "Nume" (cons 'count "Medie")))

(define select
  (λ (db table-name columns conditions)
    (
     if (null? db)
        null
        (
         if (null? (filter-pairs columns))
            (simple-select-filtered db table-name columns conditions)
            (eval-columns (rebuild-filt-table (get-table db table-name) conditions) columns)
         ))))


;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

(define (checking list1 list2) (
                                cond ((null? list1) '())
                                     ((member (car list1) list2) (checking (cdr list1) list2))
                                     (else (cons (car list1) (checking (cdr list1) list2)))
                                ))
;(get-table-lines (rebuild-filt-table (get-table db "Cursuri") (list (list < "Număr teme" 4) (list equal? "Semestru" "I"))))
;(cons (recreate-table (get-table db "Studenți") (checking (get-table-lines (get-table db "Studenți")) (get-table-lines (rebuild-filt-table (get-table db "Studenți")
 ;                                                                                         (list (list <= "Medie" 9.85) (list < "Număr matricol" 125)))))) (remove-table db "Studenți"))

;(rebuild-filt-table (get-table db "Studenți") (list (list <= "Medie" 9.85) (list < "Număr matricol" 125)))

;(length (rebuild-filt-table (get-table db "Cursuri") (list (list equal? "Anul" "I") (list equal? "Semestru" "II"))))

(define delete
  (λ (db table-name conditions)
    (
        cond ((null? db) null)
             ((null? conditions) (cons (cons table-name (get-columns (get-table db table-name))) (remove-table db table-name)))
             (else (cons (recreate-table (get-table db table-name) (checking (get-table-lines (get-table db table-name)) (get-table-lines (rebuild-filt-table (get-table db table-name)
                                                                                          conditions)))) (remove-table db table-name)))
        )))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

;(get-line-index (get-table db "Cursuri") 4)
(define (replace table entry column value) (
                                         if (or (null? table) (null? entry))
                                            null
                                            (append (take entry (get-column-index table column)) (list value) (drop entry (add1 (get-column-index table column))))
                                         )) ;; merge! -> inlocuiesc o valoare intr-un entry
;(replace (get-table db "Cursuri") (get-line-index (get-table db "Cursuri") 4) "Număr credite" 69)

(define (replace-values table entry pairs) (
                                            cond ((null? table) null)
                                                 ((null? entry) null)
                                                 ((null? pairs) entry)
                                                 (else (replace-values table (replace table entry (car (car pairs)) (cdr (car pairs))) (cdr pairs))) ;; merge!
                                            ))
;(replace-values (get-table db "Cursuri") (get-line-index (get-table db "Cursuri") 4) (list (cons "Număr teme" 4) (cons "Număr credite" 69) (cons "Anul" 1)))

(define (replace-all-values table entries pairs) (
                                                  cond ((null? table) null)
                                                       ((null? entries) null)
                                                       ((null? pairs) entries)
                                                       (else (cons (replace-values table (car entries) pairs) (replace-all-values table (cdr entries) pairs))))) ;; merge <3

;; valorile inlocuite
;(recreate-table (get-table db "Cursuri")
;                (replace-all-values (get-table db "Cursuri") (get-table-lines (get-table db "Cursuri")) (list (cons "Număr teme" 4) (cons "Număr credite" 69) (cons "Anul" 1))))

;(rebuild-filt-table (get-table db table-name) conditions)

(define (update-table-helper lines1 lines2 lines3 acc) (
                                             cond ((null? lines1) acc)
                                                  ((null? lines2) (append acc lines3))
                                                  ((null? lines3) (append acc lines2))
                                                  ((member (car lines1) lines2) (update-table-helper (cdr lines1) (cdr lines2) lines3 (append acc (list (car lines2)))))
                                                  (else (update-table-helper (cdr lines1) lines2 (cdr lines3) (append acc (list (car lines3)))))
                                             ))
(define (update-table lines1 lines2 lines3) (
                                                    update-table-helper lines1 lines2 lines3 null
                                                  ))

(define (filter-values table values) (
                                      cond ((or (null? values) (null? table)) null)
                                           ((member (car (car values)) (get-columns table)) (cons (car values) (filter-values table (cdr values))))
                                           (else (filter-values table (cdr values)))
                                      ))

(define update
  (λ (db table-name values conditions)
    (
     cond ((null? db) db)
          ((null? conditions) db)
          (else (cons (recreate-table (get-table db table-name) (update-table (get-table-lines (get-table db table-name))
              (get-table-lines (get-table (delete db table-name conditions) table-name))
              (replace-all-values (rebuild-filt-table (get-table db table-name) conditions)
                                  (get-table-lines (rebuild-filt-table (get-table db table-name) conditions)) (filter-values (get-table db table-name) values)))) (remove-table db table-name)))
     )))


;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================


(define table3 (list "Category" (list "CATEGORY_ID" 1 2 3 4 5)
                     (list "CATEGORY_NAME" "Mobiles" "Laptops" "Laptops" "Cameras" "Gaming")))
(define table4 (list "Product" (list "CATEGORY_ID" 1 1 2 2 3 4 NULL)
                     (list "PRODUCT_NAME" "Nokia" "Samsung" "HP" "Dell" "Apple" "Nikon" "MUIE")))
(define db2 (add-table (add-table (init-database) table3) table4))


;(rebuild-filt-table table4 (list (list >= "CATEGORY_ID" 2)))


(define (take-columns-line table line columns) (
                                                if (or (null? table) (null? columns))
                                                   null
                                                   (cons (list-ref line (get-column-index table (car columns))) (take-columns-line table line (cdr columns))
                                                ))) ; iau coloanele date de pe un entry din tabel
;(take-columns-line (get-table db "Cursuri") (get-line-index (get-table db "Cursuri") 2) '("Semestru" "Anul"))

(define (common-element list1 list2) (
                                      cond ((or (null? list1) (null? list2)) null)
                                           ((member (car list1) list2) (car list1))
                                           (else (common-element (cdr list1) list2))
                                      )) ; (primul) element colum a 2 liste --> (common-element (get-columns table1) (get-columns table2))

(define (find-lines-by-column table lines column elem) (
                                                   cond ((or (null? table) (null? lines)) null)
                                                        ((equal? elem (list-ref (car lines) (get-column-index table column)))
                                                         (cons (car lines) (find-lines-by-column table (cdr lines) column elem)))
                                                        (else (find-lines-by-column table (cdr lines) column elem)) 
                                                   )) ;iau intrarile dintr-o tabela dupa o coloana si o valoare de tip coloana aia meh 
;(find-lines-by-column (get-table db "Cursuri") (get-table-lines (get-table db "Cursuri")) "Anul" "V") ; -> yep

;(define (det-columns columns1 columns2) (
;                                         cond ((or (null? columns1) (null? columns2)) null)
;                                              ((member (car columns1)) (cons (car columns1) (det-columns (cdr columns1) columns2)))
;                                              (else (det-columns (cdr columns1) columns2))       
;                                     )) ;; a fi apelata cu (det-columns (get-columns table) columns) -> determin coloanele dintr-o tabela din cele cerute la join

(define (del-col table lines col) (
                                   if (or (null? table) (null? lines))
                                      null
                                      (cons (remove (list-ref (car lines) (get-column-index table col)) (car lines)) (del-col table (cdr lines) col))
                                   )) ; sterg coloana col din entry-uri date -> merge!

;(del-col (get-table db "Cursuri") (get-table-lines (get-table db "Cursuri")) "Semestru")

(define (remove-index listing index) (
                                     if (or (>= index (length listing)) (null? listing))
                                        listing
                                        (append (take listing index) (drop listing (add1 index)))
                                     ))

;(create-table "Final" (append (get-columns table4) (remove (common-element (get-columns table4) (get-columns table3)) (get-columns table3))))

(define (join table1 table2 lines1 lines2 column) (
                                                   cond ((or (null? lines1) (null? lines2)) null)
                                                      ((null? (find-lines-by-column table2 lines2 column (list-ref (car lines1) (get-column-index table1 column))))
                                                              (join table1 table2 (cdr lines1) lines2 column))
                                                      (else (cons (append (car lines1)
                                                                           (remove-index (car (find-lines-by-column table2 lines2 column (list-ref (car lines1) (get-column-index table1 column)))) (get-column-index table2 column))) (join table1 table2 (cdr lines1) lines2 column)))
                                                   )) ;; merge <3

(define (line-to-pair line columns) (
                                     if (or (null? line) (null? columns))
                                        null
                                        (cons (cons (car columns) (car line)) (line-to-pair (cdr line) (cdr columns)))
                                     )) ;; merge <3

;(line-to-pair (get-line-index (get-table db "Cursuri") 4) (get-columns (get-table db "Cursuri")))

(define (table-lines-to-pairs lines columns) (
                                              if (null? lines)
                                                 null
                                                 (cons (line-to-pair (car lines) columns) (table-lines-to-pairs (cdr lines) columns))
                                              ))

;(table-lines-to-pairs (get-table-lines (get-table db "Cursuri")) (get-columns (get-table db "Cursuri")))

;(define sugi-pula (join table4 table3 (get-table-lines (rebuild-filt-table table4 (list (list >= "CATEGORY_ID" 2))))
;                        (get-table-lines table3) (common-element (get-columns table4) (get-columns table3))))
;liste de entry-uri


;(recreate-table (create-table "Final" (append (get-columns table4) (remove (common-element (get-columns table4) (get-columns table3)) (get-columns table3))))
;               (join table4 table3 (get-table-lines table4) (get-table-lines table3) (common-element (get-columns table4) (get-columns table3))))





;(define my-pairs (table-lines-to-pairs sugi-pula (get-columns my-table)))
;my-pairs

(define (filter-lines-null lines) (
                                   cond ((null? lines) null)
                                        ((member NULL (car lines)) (filter-lines-null (cdr lines)))
                                        (else (cons (car lines) (filter-lines-null (cdr lines))))
                                   ))

(define (multiple-insert db table-name list-pairs) (
                                                    cond ((null? db) null)
                                                         ((null? list-pairs) db)
                                                         (else (multiple-insert (insert db table-name (car list-pairs)) table-name (cdr list-pairs)))
                                                    )) ;; inserare multipla
(define my-table (create-table "Final" (append (get-columns table4) (remove (common-element (get-columns table4) (get-columns table3)) (get-columns table3)))))
my-table

(simple-select (multiple-insert (add-table db my-table) (get-name my-table)
                                (table-lines-to-pairs
                                 (join table4 table3 (get-table-lines (rebuild-filt-table (recreate-table table4 (filter-lines-null (get-table-lines table4)))
                                                                                          (list (list >= "CATEGORY_ID" 2))))
                        (get-table-lines table3) (common-element (get-columns table4) (get-columns table3))) (get-columns my-table)))
               (get-name my-table)
               '("CATEGORY_NAME" "PRODUCT_NAME"))
;table 4 = prima tabela
;table 3 = a doua tabela

;(simple-select (multiple-insert (add-table db join-table) (get-name join-table)
;                                (table-lines-to-pairs
;                                 (join (car tables) (last tables) (get-table-lines (rebuild-filt-table (recreate-table (car tables) (filter-lines-null (get-table-lines (car tables))))
;                                                                                          conditions))
;                        (get-table-lines (last tables)) (common-element (get-columns (car tables)) (get-columns (last tables)))) (get-columns join-table)))
;               (get-name join-table) columns)

(define yay (get-table (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (add-table (add-table (init-database) (create-table "Category" '("ID" "Category_Name"))) (create-table "Product" '("ID" "Product_Name"))) "Category" (list '("ID" . 1) '("Category_Name" . "Mobiles"))) "Category" (list '("ID" . 2) '("Category_Name" . "Laptops"))) "Category" (list '("ID" . 3) '("Category_Name" . "Tablet"))) "Category" (list '("ID" . 4) '("Category_Name" . "Cameras"))) "Category" (list '("ID" . 5) '("Category_Name" . "Gaming"))) "Product" (list '("ID" . 1) '("Product_Name" . "Nokia"))) "Product" (list '("ID" . 1) '("Product_Name" . "Samsung"))) "Product" (list '("ID" . 2) '("Product_Name" . "HP"))) "Product" (list '("ID" . 2) '("Product_Name" . "Dell"))) "Product" (list '("ID" . 3) '("Product_Name" . "Apple"))) "Product" (list '("ID" . 4) '("Product_Name" . "Nikon"))) "Product" (list '("Product_Name" . "Playstation"))) "Product"))
;yay
;(insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (add-table (add-table (init-database) (create-table "Category" '("ID" "Category_Name"))) (create-table "Product" '("ID" "Product_Name"))) "Category" (list '("ID" . 1) '("Category_Name" . "Mobiles"))) "Category" (list '("ID" . 2) '("Category_Name" . "Laptops"))) "Category" (list '("ID" . 3) '("Category_Name" . "Tablet"))) "Category" (list '("ID" . 4) '("Category_Name" . "Cameras"))) "Category" (list '("ID" . 5) '("Category_Name" . "Gaming"))) "Product" (list '("ID" . 1) '("Product_Name" . "Nokia"))) "Product" (list '("ID" . 1) '("Product_Name" . "Samsung"))) "Product" (list '("ID" . 2) '("Product_Name" . "HP"))) "Product" (list '("ID" . 2) '("Product_Name" . "Dell"))) "Product" (list '("ID" . 3) '("Product_Name" . "Apple"))) "Product" (list '("ID" . 4) '("Product_Name" . "Nikon"))) "Product" (list '("Product_Name" . "Playstation")))
;(recreate-table table4 (filter-lines-null (get-table-lines table4)))
;(recreate-table (rebuild-filt-table (car tables) conditions) (filter-lines-null (get-table-lines (rebuild-filt-table (car tables) conditions))))

;(get-table db (car tables))
;(get-table db (last tables))

(define natural-join
  (λ (db tables columns conditions)
    (
     if (null? db)
        null
        (
          let ((join-table (create-table "Final" (append (get-columns (get-table db (car tables)))
                           (remove (common-element (get-columns (get-table db (car tables))) (get-columns (get-table db (last tables)))) (get-columns (get-table db (last tables))))))))
           (simple-select (multiple-insert (add-table db join-table) (get-name join-table)
                                (table-lines-to-pairs
                                 (join (get-table db (car tables)) (get-table db (last tables)) (get-table-lines (rebuild-filt-table (recreate-table (get-table db (car tables)) (filter-lines-null (get-table-lines (get-table db (car tables)))))
                                                                                          conditions))
                        (get-table-lines (get-table db (last tables))) (common-element (get-columns (get-table db (car tables))) (get-columns (get-table db (last tables))))) (get-columns join-table)))
               (get-name join-table) columns)
          ))))


(define default-results '(#f 0 () your-code-here)) ; ce rezultate default sunt întoarse în exerciții
(define show-defaults 200) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define name-ex '(testul testele trecut)) 
(define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define defaults '())
(define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (p L) (map (λ (e) (display e) (display " ")) L) (newline))
(define (check-exp given expected) (check-exp-part "" 1 given expected)) (define (check-exp-part part per given expected) (check-test part per equal? given expected "diferă de cel așteptat"))
(define (check-in  given expected) (check-in-part  "" 1 given expected)) (define (check-in-part part per given expected) (check-test part per member given expected "nu se află printre variantele așteptate"))
(define (check-set given expected) (check-set-part  "" 1 given expected)) (define (check-set-part part per given expected) (check-test part per (λ (x y) (apply equal? (map list->seteqv `(,given ,expected)))) given expected "nu este echivalent cu cel așteptat"))
(define (check-set-unique given expected) (check-set-unique-part  "" 1 given expected)) (define (check-set-unique-part part per given expected) (check-test part per (λ (x y) (and (apply = (map length `(,given ,expected))) (apply equal? (map list->seteqv `(,given ,expected))))) given expected "nu este echivalent cu cel așteptat"))
(define (check-test part percent tester given expected err-m) (if (not (tester given expected)) (and (when (member given default-results) (set! defaults (cons (if (< percent 1) (cons n-ex part) n-ex) defaults)))
  (when (or (not (member given default-results)) (<= (length defaults) show-defaults))
    (p `(NU: la ,(car name-ex) ,(if (< percent 1) (cons n-ex part) n-ex) rezultatul ,given ,err-m : ,expected))))
 (let ((pts (* p-ex percent))) (and (if prepend (printf "+~v: " pts) (printf "OK: "))
  (p (list (car name-ex) (if (< percent 1) (cons n-ex part) n-ex) (caddr name-ex) '+ pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts))))))
(define (sumar) (when (and (not (null? defaults)) (< show-defaults (length defaults))) (p `(... rezultatul implicit dat la ,(cadr name-ex) ,(reverse defaults)))) (p `(total: ,total puncte)))
(define Task ex) (define Bonus ex)

(Task 0 : 20 puncte)
(check-exp-part 'init-database .05 (get-tables (init-database)) '())
(check-exp-part 'create-table1 .05 (get-name (create-table "Company" '("Company_ID" "Company_Name" "Company_City"))) "Company")
(check-exp-part 'create-table2 .05 (get-columns (create-table "Company" '("Company_ID" "Company_Name" "Company_City"))) '("Company_ID" "Company_Name" "Company_City"))
(check-exp-part 'add-table1 .06 (map get-name (get-tables (add-table (init-database) (create-table "Test" '("Coloana1" "Coloana2" "Coloana3"))))) '("Test"))
(check-exp-part 'add-table2 .06 (sort (map get-name (get-tables (add-table db (create-table "Test" '("Coloana1" "Coloana2" "Coloana3"))))) string<?) '("Cursuri" "Studenți" "Test"))
(check-exp-part 'add-table3 .06 (sort (map get-name (get-tables (add-table (add-table db (create-table "Test" '("Coloana1" "Coloana2" "Coloana3"))) (create-table "Tabela1" '("Coloana1"))))) string<?) '("Cursuri" "Studenți" "Tabela1" "Test"))
(check-exp-part 'get-table1 .05 (get-columns (get-table db "Studenți")) '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie"))
(check-exp-part 'get-table2 .05 (get-columns (get-table db "Cursuri")) '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))
(check-exp-part 'get-tables .05 (sort (map get-name (get-tables db)) string<?) '("Cursuri" "Studenți"))
(check-exp-part 'remove-table1 .06 (map get-name (get-tables (remove-table db "Studenți"))) '("Cursuri"))
(check-exp-part 'remove-table2 .06 (map get-name (get-tables (remove-table db "Cursuri"))) '("Studenți"))
(check-exp-part 'remove-table3 .1 (map get-name (get-tables (remove-table (remove-table db "Cursuri") "Studenți"))) '())
(check-exp-part 'remove-table4 .1 (map get-name (get-tables (add-table (remove-table (remove-table db "Cursuri") "Studenți") (create-table "Test" '("Coloana1"))))) '("Test"))
(check-exp-part 'remove-table5 .1 (map get-name (get-tables (remove-table (add-table (remove-table (remove-table db "Cursuri") "Studenți") (create-table "Test" '("Coloana1"))) "Test"))) '())
(check-exp-part 'remove-table6 .1 (get-columns (get-table (remove-table db "Cursuri") "Studenți")) '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie"))

(Task 1 : 20 puncte)
(check-exp-part 'simple-select1 0.05 (simple-select db "Studenți" '("Nume" "Prenume")) '(("Ionescu" "Popescu" "Popa" "Georgescu") ("Gigel" "Maria" "Ionel" "Ioana")))
(check-exp-part 'simple-select2 0.05 (simple-select db "Studenți" '("Număr matricol" "Medie")) '((123 124 125 126) (9.82 9.91 9.99 9.87)))
(check-exp-part 'simple-select3 0.05 (simple-select db "Studenți" '("Prenume" "Nume" "Număr matricol" "Medie")) '(("Gigel" "Maria" "Ionel" "Ioana") ("Ionescu" "Popescu" "Popa" "Georgescu") (123 124 125 126) (9.82 9.91 9.99 9.87)))
(check-exp-part 'simple-select4 0.05 (simple-select db "Cursuri" '("Număr credite" "Număr teme")) '((5 6 5 6 5 5) (2 3 3 3 3 0)))
(check-exp-part 'simple-select5 0.05 (simple-select db "Cursuri" '("Anul" "Semestru" "Număr teme")) '(("I" "II" "III" "IV" "I" "III") ("I" "II" "I" "I" "II" "II") (2 3 3 3 3 0)))
(check-exp-part 'simple-select6 0.05 (simple-select db "Cursuri" '("Disciplină" "Număr credite")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") (5 6 5 6 5 5)))
(check-exp-part 'simple-select-&-insert1 0.1 (simple-select (insert db "Cursuri" '(("Disciplină" . "Matematică"))) "Cursuri" '("Disciplină" "Număr credite")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date" "Matematică") (5 6 5 6 5 5 null)))
(check-exp-part 'simple-select-&-insert2 0.1 (simple-select (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '("Disciplină" "Număr credite" "Anul" "Număr credite")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date" "Matematică") (5 6 5 6 5 5 null) ("I" "II" "III" "IV" "I" "III" "I") (5 6 5 6 5 5 null)))
(check-exp-part 'simple-select-&-insert3 0.1 (simple-select (insert (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '(("Disciplină" . "Limbaje formale și automate") ("Anul" . "III")  ("Număr teme" . 1) )) "Cursuri" '("Disciplină" "Număr credite" "Anul" "Număr teme")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date" "Matematică" "Limbaje formale și automate") (5 6 5 6 5 5 null null) ("I" "II" "III" "IV" "I" "III" "I" "III") (2 3 3 3 3 0 null 1)))
(check-exp-part 'simple-select-&-insert4 0.1 (simple-select (insert (insert (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '(("Disciplină" . "Limbaje formale și automate") ("Anul" . "III")  ("Număr teme" . 1) )) "Studenți" '(("Număr matricol" . 134) ("Nume" . "Dumitru") ("Prenume" . "Gigel"))) "Studenți" '("Prenume" "Nume" "Număr matricol")) '(("Gigel" "Maria" "Ionel" "Ioana" "Gigel") ("Ionescu" "Popescu" "Popa" "Georgescu" "Dumitru") (123 124 125 126 134)))
(check-exp-part 'simple-select-&-insert5 0.1 (simple-select (insert (insert (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '(("Disciplină" . "Limbaje formale și automate") ("Anul" . "III")  ("Număr teme" . 1) )) "Studenți" '(("Număr matricol" . 134) ("Nume" . "Dumitru") ("Prenume" . "Gigel"))) "Studenți" '("Prenume" "Nume" "Medie")) '(("Gigel" "Maria" "Ionel" "Ioana" "Gigel") ("Ionescu" "Popescu" "Popa" "Georgescu" "Dumitru") (9.82 9.91 9.99 9.87 null)))  
(check-exp-part 'simple-select-&-insert6 0.1 (simple-select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" '("Coloana3" "Coloana2" "Coloana1")) (list (for/list ([k (in-naturals 20)] [x (in-range 100)]) k) (for/list ([k (in-naturals)] [x (in-range 100)]) k) (for/list ([k (in-naturals 0)] [x (in-range 100)]) k)))
(check-exp-part 'simple-select-&-insert7 0.1 (simple-select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 1000)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" '("Coloana3" "Coloana2" "Coloana1" "Coloana4" "Coloana5")) (list (for/list ([k (in-naturals 520)] [x (in-range 1000)]) k) (for/list ([k (in-naturals)] [x (in-range 1000)]) k) (for/list ([k (in-naturals 256)] [x (in-range 1000)]) k) (for/list ([x (in-range 1000)]) NULL) (for/list ([x (in-range 1000)]) NULL)))

(Task 2 : 30 puncte)
(check-exp-part 'select1 0.05 (select db "Studenți" (list "Nume" "Medie") (list (list >= "Medie" 9.9))) '(("Popescu" "Popa") (9.91 9.99)))
(check-exp-part 'select2 0.05 (select db "Studenți" (list "Nume" "Prenume" "Medie") (list (list > "Număr matricol" 124))) '(("Popa" "Georgescu") ("Ionel" "Ioana") (9.99 9.87)))
(check-exp-part 'select3 0.05 (select db "Studenți" (list "Prenume" "Nume") (list (list < "Medie" 9.9))) '(("Gigel" "Ioana") ("Ionescu" "Georgescu")))
(check-exp-part 'select4 0.05 (select db "Studenți" (list "Prenume" "Grupă" "Nume") (list (list < "Medie" 9.9) (list >= "Număr matricol" 120))) '(("Gigel" "Ioana") ("321CA" "321CD") ("Ionescu" "Georgescu")))
(check-exp-part 'select5 0.05 (select db "Cursuri" (list "Anul" "Disciplină" "Semestru") (list (list equal? "Semestru" "I") (list >= "Număr credite" 5))) '(("I" "III" "IV") ("Programarea calculatoarelor" "Algoritmi paraleli și distribuiți" "Inteligență artificială") ("I" "I" "I")))
(check-exp-part 'select6 0.05 (select db "Cursuri" (list "Anul" "Disciplină" "Semestru") (list (list equal? "Anul" "I") (list > "Număr teme" 0))) '(("I" "I") ("Programarea calculatoarelor" "Structuri de date") ("I" "II")))
(check-exp-part 'select7 0.05 (select db "Cursuri" (list "Anul" "Disciplină" "Semestru") (list (list equal? "Semestru" "I") (list > "Număr teme" 0) (list >= "Număr credite" 5))) '(("I" "III" "IV") ("Programarea calculatoarelor" "Algoritmi paraleli și distribuiți" "Inteligență artificială") ("I" "I" "I")))
(check-exp-part 'select8 0.05 (select db "Studenți" (list "Nume" (cons 'min "Medie")) '()) '(("Ionescu" "Popescu" "Popa" "Georgescu") 9.82))
(check-exp-part 'select9 0.05 (select db "Studenți" (list "Prenume" "Nume" (cons 'max "Medie")) '()) '(("Gigel" "Maria" "Ionel" "Ioana") ("Ionescu" "Popescu" "Popa" "Georgescu") 9.99))
(check-exp-part 'select10 0.05 (select db "Studenți" (list (cons 'sort-asc "Număr matricol") "Prenume" "Nume" (cons 'count "Medie")) '()) '((123 124 125 126) ("Gigel" "Maria" "Ionel" "Ioana") ("Ionescu" "Popescu" "Popa" "Georgescu") 4))
(check-exp-part 'select11 0.05 (select db "Studenți" (list "Nume" (cons 'min "Medie")) (list (list > "Medie" 9.5))) '(("Ionescu" "Popescu" "Popa" "Georgescu") 9.82))
(check-exp-part 'select12 0.05 (select db "Studenți" (list "Prenume" "Nume" (cons 'max "Medie")) (list (list >= "Număr matricol" 125))) '(("Ionel" "Ioana") ("Popa" "Georgescu") 9.99))
(check-exp-part 'select13 0.05 (select db "Studenți" (list (cons 'sort-asc "Număr matricol") "Prenume" "Nume" (cons 'count "Medie")) (list (list > "Medie" 9.85))) '((124 125 126) ("Maria" "Ionel" "Ioana") ("Popescu" "Popa" "Georgescu") 3))
(check-exp-part 'select14 0.05 (select db "Studenți" (list (cons 'avg "Număr matricol") "Prenume" "Nume" (cons 'sort-desc "Medie")) (list (list > "Medie" 9.85))) '(125 ("Maria" "Ionel" "Ioana") ("Popescu" "Popa" "Georgescu") (9.99 9.91 9.87)))
(check-exp-part 'select15 0.1 (select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 10)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y) (cons "Coloana5" (+ x y)) (cons "Coloana4" (- x y))))) "Tabela" (list (cons 'min "Coloana3") (cons 'count "Coloana2") (cons 'sort-asc "Coloana1") (cons 'sort-desc "Coloana4") (cons 'sum "Coloana5")) '()) '(520 10 (256 257 258 259 260 261 262 263 264 265) (-520 -520 -520 -520 -520 -520 -520 -520 -520 -520) 5290))
(check-exp-part 'select16 0.1 (select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 10)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y) (cons "Coloana5" (+ x y)) (cons "Coloana4" (+ (- x y) k))))) "Tabela" (list (cons 'min "Coloana3") (cons 'count "Coloana2") (cons 'sort-asc "Coloana1") (cons 'sort-desc "Coloana4") (cons 'sum "Coloana5")) (list (list > "Coloana1" 260) (list < "Coloana4" -257)))  '(525 2 (261 262) (-258 -259) 1062))
(check-exp-part 'select17 0.1 (select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 1000)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y) (cons "Coloana5" (+ x y)) (cons "Coloana4" (- x y))))) "Tabela" (list (cons 'min "Coloana3") (cons 'max "Coloana2") (cons 'avg "Coloana1") (cons 'count "Coloana4") (cons 'sum "Coloana5")) (list (list > "Coloana1" 260))) '(525 999 758 1 1516380))

(Task 3 : 20 puncte)
(check-exp-part 'update1 0.1 (simple-select (update db "Studenți" (list (cons "Medie" 10)) (list (list >= "Medie" 9.5))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Popescu" "Popa" "Georgescu") ("Gigel" "Maria" "Ionel" "Ioana") (10 10 10 10)))
(check-exp-part 'update2 0.1 (simple-select (update db "Cursuri" (list (cons "Număr teme" 3)) (list (list < "Număr teme" 3) (list equal? "Anul" "I"))) "Cursuri" '("Disciplină" "Număr teme" "Anul")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") (3 3 3 3 3 0) ("I" "II" "III" "IV" "I" "III")))
(check-exp-part 'update3 0.1  (simple-select (update (update db "Cursuri" (list (cons "Număr teme" 3)) (list (list < "Număr teme" 3) (list equal? "Anul" "I"))) "Cursuri" (list (cons "Număr credite" 6) (cons "Semestru" "II")) (list (list equal? "Semestru" "I")))  "Cursuri" '("Anul" "Semestru" "Număr credite" "Număr teme")) '(("I" "II" "III" "IV" "I" "III") ("II" "II" "II" "II" "II" "II") (6 6 6 6 5 5) (3 3 3 3 3 0)))
(check-exp-part 'update4 0.15 (select (update (update db "Cursuri" (list (cons "Număr teme" 3)) (list (list < "Număr teme" 3) (list equal? "Anul" "I"))) "Cursuri" (list (cons "Număr credite" 6) (cons "Anul" "I") (cons "Semestru" "II")) (list (list equal? "Semestru" "I")))  "Cursuri" (list (cons 'count "Anul") (cons 'count "Semestru") (cons 'sum "Număr credite") (cons 'sort-desc "Număr teme")) (list (list > "Număr credite" 0))) '(3 1 34 (3 3 3 3 3 0)))
(check-exp-part 'update5 0.15 (select (update (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (cons "Coloana1" 222) (cons "Coloana2" 694)) (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" (list (cons 'count "Coloana1") (cons 'sum "Coloana2")) '()) '(27 53161))
(check-exp-part 'update6 0.2 (select (update (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (cons "Coloana1" 222) (cons "Coloana2" 694)) (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" (list (cons 'count "Coloana1") (cons 'sum "Coloana2")) '()) '(27 53161))
(check-exp-part 'update7 0.2 (select (update (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (cons "Coloana1" 128) (cons "Coloana2" 542) (cons "Coloana5" 452)) (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" (list (cons 'count "Coloana1") (cons 'min "Coloana2") (cons 'max "Coloana3")) (list (list > "Coloana1" 100))) '(1 542 99))

(Task 4 : 10 puncte)
(check-exp-part 'delete1 0.1 (simple-select (delete db "Studenți" (list (list < "Medie" 9.9))) "Studenți" '("Nume" "Prenume")) '(("Popescu" "Popa") ("Maria" "Ionel")))
(check-exp-part 'delete2 0.1 (simple-select (delete db "Studenți" (list (list >= "Medie" 9.9))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Georgescu") ("Gigel" "Ioana") (9.82 9.87)))
(check-exp-part 'delete3 0.1 (simple-select (delete db "Cursuri" (list (list equal? "Anul" "I") (list equal? "Semestru" "I"))) "Cursuri" '("Disciplină" "Semestru" "Număr teme")) '(("Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") ("II" "I" "I" "II" "II") (3 3 3 3 0)))
(check-exp-part 'delete4 0.1 (simple-select (delete db "Studenți" (list (list < "Medie" 5) (list equal? "Nume" "Popescu"))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Popescu" "Popa" "Georgescu") ("Gigel" "Maria" "Ionel" "Ioana") (9.82 9.91 9.99 9.87)))
(check-exp-part 'delete5 0.1 (simple-select (delete db "Studenți" (list (list >= "Medie" 9.8) (list equal? "Nume" "Popescu"))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Popa" "Georgescu") ("Gigel" "Ionel" "Ioana") (9.82 9.99 9.87)))
(check-exp-part 'delete6 0.1 (simple-select (delete db "Cursuri" (list (list equal? "Anul" "I") (list >= "Număr credite" 5))) "Cursuri" '("Semestru" "Disciplină" "Număr teme" "Număr credite")) '(("II" "I" "I" "II") ("Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Baze de date") (3 3 3 0) (6 5 6 5)))
(check-exp-part 'delete7 0.1 (simple-select (delete db "Cursuri" (list (list >= "Număr credite" 5))) "Cursuri" '("Semestru" "Disciplină" "Număr teme" "Număr credite")) '())
(check-exp-part 'delete8 0.1 (simple-select (delete db "Cursuri" (list (list equal? "Anul" "I") (list equal? "Semestru" "I") (list > "Număr credite" 5) )) "Cursuri" '("Semestru" "Disciplină" "Număr teme" "Număr credite")) '(("I" "II" "I" "I" "II" "II") ("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") (2 3 3 3 3 0) (5 6 5 6 5 5)))
(check-exp-part 'delete9 0.1  (simple-select (delete (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (list < "Coloana2" 100) (list > "Coloana1" 5))) "Tabela" '("Coloana3" "Coloana2" "Coloana1")) '((20 21 22 23 24 25) (0 1 2 3 4 5) (0 1 2 3 4 5))) 
(check-exp-part 'delete10 0.1 (simple-select (delete (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" '("Coloana1")) '((0 1 2 3 4 5 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)))

(Task 5 : 20 puncte)
(check-exp-part 'natural-join1 0.2 (natural-join (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (add-table (add-table (init-database) (create-table "Category" '("ID" "Category_Name"))) (create-table "Product" '("ID" "Product_Name"))) "Category" (list '("ID" . 1) '("Category_Name" . "Mobiles"))) "Category" (list '("ID" . 2) '("Category_Name" . "Laptops"))) "Category" (list '("ID" . 3) '("Category_Name" . "Tablet"))) "Category" (list '("ID" . 4) '("Category_Name" . "Cameras"))) "Category" (list '("ID" . 5) '("Category_Name" . "Gaming"))) "Product" (list '("ID" . 1) '("Product_Name" . "Nokia"))) "Product" (list '("ID" . 1) '("Product_Name" . "Samsung"))) "Product" (list '("ID" . 2) '("Product_Name" . "HP"))) "Product" (list '("ID" . 2) '("Product_Name" . "Dell"))) "Product" (list '("ID" . 3) '("Product_Name" . "Apple"))) "Product" (list '("ID" . 4) '("Product_Name" . "Nikon"))) "Product" (list '("Product_Name" . "Playstation"))) '("Product" "Category") '("ID" "Product_Name" "Category_Name") '()) '((1 1 2 2 3 4) ("Nokia" "Samsung" "HP" "Dell" "Apple" "Nikon") ("Mobiles" "Mobiles" "Laptops" "Laptops" "Tablet" "Cameras")))
(check-exp-part 'natural-join2 0.2 (natural-join (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (add-table (add-table (init-database) (create-table "Category" '("ID" "Category_Name"))) (create-table "Product" '("ID" "Product_Name"))) "Category" (list '("ID" . 1) '("Category_Name" . "Mobiles"))) "Category" (list '("ID" . 2) '("Category_Name" . "Laptops"))) "Category" (list '("ID" . 3) '("Category_Name" . "Tablet"))) "Category" (list '("ID" . 4) '("Category_Name" . "Cameras"))) "Category" (list '("ID" . 5) '("Category_Name" . "Gaming"))) "Product" (list '("ID" . 1) '("Product_Name" . "Nokia"))) "Product" (list '("ID" . 1) '("Product_Name" . "Samsung"))) "Product" (list '("ID" . 2) '("Product_Name" . "HP"))) "Product" (list '("ID" . 2) '("Product_Name" . "Dell"))) "Product" (list '("ID" . 3) '("Product_Name" . "Apple"))) "Product" (list '("ID" . 4) '("Product_Name" . "Nikon"))) "Product" (list '("Product_Name" . "Playstation"))) '("Product" "Category") '("Product_Name" "Category_Name") '()) '(("Nokia" "Samsung" "HP" "Dell" "Apple" "Nikon") ("Mobiles" "Mobiles" "Laptops" "Laptops" "Tablet" "Cameras")))
(check-exp-part 'natural-join3 0.2 (natural-join (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (add-table (add-table (init-database) (create-table "Category" '("ID" "Category_Name"))) (create-table "Product" '("ID" "Product_Name"))) "Category" (list '("ID" . 1) '("Category_Name" . "Mobiles"))) "Category" (list '("ID" . 2) '("Category_Name" . "Laptops"))) "Category" (list '("ID" . 3) '("Category_Name" . "Tablet"))) "Category" (list '("ID" . 4) '("Category_Name" . "Cameras"))) "Category" (list '("ID" . 5) '("Category_Name" . "Gaming"))) "Product" (list '("ID" . 1) '("Product_Name" . "Nokia"))) "Product" (list '("ID" . 1) '("Product_Name" . "Samsung"))) "Product" (list '("ID" . 2) '("Product_Name" . "HP"))) "Product" (list '("ID" . 2) '("Product_Name" . "Dell"))) "Product" (list '("ID" . 3) '("Product_Name" . "Apple"))) "Product" (list '("ID" . 4) '("Product_Name" . "Nikon"))) "Product" (list '("Product_Name" . "Playstation"))) '("Product" "Category") '("Category_Name" "Product_Name") (list (list >= "ID" 2))) '(("Laptops" "Laptops" "Tablet" "Cameras") ("HP" "Dell" "Apple" "Nikon")))
(define test_table1 (create-table "Foods" '("Item_ID" "Item_Name" "Item_Unit" "Company_ID")))
(define test_records1 (list (list '("Item_ID" . 1) '("Item_Name" . "Chex Mix") '("Item_Unit" . "Pcs") '("Company_ID" . 16))
                       (list '("Item_ID" . 6) '("Item_Name" . "Cheez-It") '("Item_Unit" . "Pcs") '("Company_ID" . 15))
                       (list '("Item_ID" . 2) '("Item_Name" . "BN Biscuit") '("Item_Unit" . "Pcs") '("Company_ID" . 15))
                       (list '("Item_ID" . 3) '("Item_Name" . "Mighty Munch") '("Item_Unit" . "Pcs") '("Company_ID" . 17))
                       (list '("Item_ID" . 4) '("Item_Name" . "Pot Rice") '("Item_Unit" . "Pcs") '("Company_ID" . 15))
                       (list '("Item_ID" . 5) '("Item_Name" . "Jaffa Cakes") '("Item_Unit" . "Pcs") '("Company_ID" . 18))
                       (list '("Item_ID" . 7) '("Item_Name" . "Salt n Shake") '("Item_Unit" . "Pcs"))))
(define test_table2 (create-table "Company" '("Company_ID" "Company_Name" "Company_City")))
(define test_records2 (list (list '("Company_ID" . 18) '("Company_Name" . "Oder All") '("Company_City" . "Boston"))
                       (list '("Company_ID" . 15) '("Company_Name" . "Jack Hill Ltd") '("Company_City" . "London"))
                       (list '("Company_ID" . 16) '("Company_Name" . "Akas Foods") '("Company_City" . "Delhi"))
                       (list '("Company_ID" . 17) '("Company_Name" . "Foodies") '("Company_City" . "London"))
                       (list '("Company_ID" . 19) '("Company_Name" . "sip-n-Bite") '("Company_City" . "New York"))))
(define test_db (add-table (add-table (init-database) test_table1) test_table2))
(define test_db1 (foldl (λ(record db) (insert db "Foods" record)) test_db test_records1))
(define test_db2 (foldl (λ(record db) (insert db "Company" record)) test_db1 test_records2))
(check-exp-part 'natural-join4 0.2 (natural-join test_db2 '("Foods" "Company") '("Company_ID" "Company_City" "Item_Name" "Company_Name" "Item_Unit") '()) '((16 15 15 15 17 18) ("Delhi" "London" "London" "London" "London" "Boston") ("Chex Mix" "Cheez-It" "BN Biscuit" "Pot Rice" "Mighty Munch" "Jaffa Cakes") ("Akas Foods" "Jack Hill Ltd" "Jack Hill Ltd" "Jack Hill Ltd" "Foodies" "Oder All") ("Pcs" "Pcs" "Pcs" "Pcs" "Pcs" "Pcs"))) 
(check-exp-part 'natural-join5 0.2 (natural-join test_db2 '("Foods" "Company") '("Company_City" "Item_Name" "Company_Name" "Item_Unit") (list (list > "Company_ID" 15))) '(("Delhi" "London" "Boston") ("Chex Mix" "Mighty Munch" "Jaffa Cakes") ("Akas Foods" "Foodies" "Oder All") ("Pcs" "Pcs" "Pcs")))
(sumar)