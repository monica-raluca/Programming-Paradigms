#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (longest-common-prefix-rec w1 w2 '()))

(define (longest-common-prefix-rec w1 w2 acc)
  (cond
    ((or (null? w1) (null? w2)) (append (list acc) (list w1 w2)))
    ((equal? (car w1) (car w2)) (longest-common-prefix-rec (cdr w1) (cdr w2) (append acc (list (car w1)))))
    (else (append (list acc) (list w1 w2)))
    )
  )


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (longest-common-prefix-of-collection-rec (collection-rest words) (collection-first words))
  )

(define (longest-common-prefix-of-collection-rec words acc)
  (cond
    ((collection-empty? words) acc)
    (else (longest-common-prefix-of-collection-rec (collection-rest words) (car (longest-common-prefix acc (collection-first words)))) )
    )
  )

(define (has-pattern pattern label)
  (cond
    ((null? pattern) #t)
    ((null? label) #f)
    ((equal? (car pattern) (car label)) (has-pattern (cdr pattern) (cdr label)))
    (else #f)
    )
  )

(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern))))
    (cond
      ((boolean? branch) (list #f '()))
      ((null? pattern) #t)
      ((has-pattern pattern (car branch)) #t)
      (else
       (if (has-pattern (get-branch-label branch) pattern)
           (list (get-branch-label branch) (list-tail pattern (length (get-branch-label branch))) (get-branch-subtree branch))
           (list #f (car (longest-common-prefix pattern (get-branch-label branch))))
           )
       )
      )
    )
  )

(define (st-has-pattern? st pattern)
  (cond
    ((null? pattern) #t)
    ((boolean? (get-ch-branch st (car pattern))) #f)
    ((has-pattern (get-branch-label (get-ch-branch st (car pattern))) pattern)
     (st-has-pattern? (get-branch-subtree (get-ch-branch st (car pattern))) (list-tail pattern (length (get-branch-label (get-ch-branch st (car pattern)))))))
    ((has-pattern pattern (car (get-ch-branch st (car pattern)))) #t)
    (else #f)
    ))


(define (get-suffixes text)
  (define (get-suffixes-rec text result)
    (cond
      ((collection-empty? text) result)
      (else (collection-cons text (get-suffixes-rec (collection-rest text) result)))
      ))
  (get-suffixes-rec text '()))


(define (get-ch-words words ch)
  (collection-filter (lambda (x) (equal? ch (car x))) words))


(define (ast-func suffixes)
  (cons (take (collection-first suffixes) 1) (collection-map (lambda (x) (list-tail x 1)) suffixes)))


(define (cst-func suffixes)
  (cons (longest-common-prefix-of-collection suffixes) (collection-map (lambda (x) (list-tail x (length (longest-common-prefix-of-collection suffixes)))) suffixes)))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (let* ((suffix-branches (collection-filter (lambda (x) (not (null? x))) (collection-map (lambda (x) (get-ch-words suffixes x)) alphabet)))
         (no-null-suffixes (collection-filter (lambda (x) (not (collection-empty? x))) suffix-branches))
         (partial-trees (collection-map (lambda (x) (labeling-func x)) no-null-suffixes))
         (no-null (collection-map (lambda (x) (if (collection-empty? (collection-first (cdr x))) (list (car x)) x)) partial-trees)))
    
    (collection-map (lambda (x) (cons (car x) (suffixes->st labeling-func (cdr x) alphabet))) no-null)
    ))

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (let* ((complete-text (append text '(#\$)))
             (alphabet (sort (remove-duplicates complete-text) char<?))
             (alphabet-stream (list->stream alphabet)))
         (suffixes->st labeling-func (get-suffixes complete-text) alphabet-stream)))))


(define (text->ast text)
  ((text->st text) ast-func))


(define (text->cst text)
  ((text->st text) cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let ((st (text->ast text)))
    (st-has-pattern? st pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let* ((st (text->cst text))
         (initial-len len))
   
    (let find-substring ((st st) (len len) (result '()))
      (cond
        ((st-empty? st) #f)
        ((<= len 0) (take result initial-len))
        (else
         (let* ((subtree (get-branch-subtree (first-branch st)))
                (label (get-branch-label (first-branch st)))
                (other-branches (other-branches st)))
           (cond
             ((not (st-empty? subtree)) (or (find-substring subtree (- len (length label)) (append result label))
                                            (find-substring other-branches len result)))
             (else (find-substring other-branches len result)))))
        )
      )
    ))