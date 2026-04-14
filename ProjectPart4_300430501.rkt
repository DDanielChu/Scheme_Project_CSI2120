#lang scheme
(define (read-f filename) (call-with-input-file filename
(lambda (input-port)
(let loop ((line (read-line input-port)))
(cond
 ((eof-object? line) '())
 (#t (begin (cons (string-split (clean-line line) ",") (loop (read-line input-port))))))))))

(define (format-resident lst)
  (list (car lst) (cadr lst) (caddr lst) (cdddr lst)))

(define (format-program lst)
  (list (car lst) (cadr lst) (string->number (caddr lst)) (map string->number(cdddr lst))))


(define (clean-line str)
  (list->string
   (filter (lambda (c) (not (or (char=? c #\") (char=? c #\[) (char=? c #\]) )))
           (string->list str))))

(define (read-residents filename)
(map (lambda(L) (format-resident (cons (string->number (car L)) (cdr L)))) (cdr (read-f filename))))

(define (read-programs filename)
(map format-program (cdr (read-f filename))))

(define PLIST (read-programs "programSmall.csv"))
(define RLIST (read-residents "residentSmall.csv"))



;Starting from here is code made by students

;Uxilery Method to create a base for matches that suits the order of the PLIST
(define (create-element-of-matches nameOfCourse)
  (list nameOfCourse '())
  )

(define (create-base-matches plist)
  (cond ((null? plist) '())
    (else (cons (create-element-of-matches (car (car plist)))  (create-base-matches (cdr plist))))

  )
)
; YOU WOULD CALL IT LIKE (create-base-matches PLIST) TO GET THE BASE LIST 


(define practiceList
  '(("OBG" ((773 . 2) (828 . 1) (616 . 0))) ("MMI" ((226 . 2))) ("HEP"
((913 . 2) (403 . 0))) ("NRS" ((126 . 5) (517 . 1) (574 . 0))))
  ) 

;first method: get-resident-info
(define (get-resident-info rid rlist)
  (cond ((null? rlist) '())
  ((= (car (car rlist)) rid) (car rlist))
  (else (get-resident-info rid (cdr rlist)))
  )
 )

;Second method : get-program-info
(define (get-program-info pid plist)
  (cond ((null? plist) '())
  ((string=? (car (car plist)) pid) (car plist))
  (else (get-program-info pid (cdr plist)))
  )
 )


;Method to help with the 3 method
;Method made to find the Index of an element of a list
(define (findIndex theList target theListSizeWithOneIncrease)
  (cond ((null? theList) (- theListSizeWithOneIncrease))
    ((equal? (car theList) target) 0)
        (else (+ 1 (findIndex (cdr theList) target theListSizeWithOneIncrease)))
        )
  )


;Third method: rank
(define (rank rid pinfo)
  (cond
    ((null? pinfo) #f)
    ((not(list? (car pinfo))) (rank rid (cdr pinfo)))
    (else (findIndex (car pinfo) rid (+ 1 (length (car pinfo)))))
   
  )
)


;Method to help with method 4
;Method to go through a list of tuples and check if the first element matches the target
(define (findIfResidentExistInListOfTupes target theList)
  (cond ((null? theList) #f)
        ((= target (car (car theList))) #t)
        (else (findIfResidentExistInListOfTupes target (cdr theList)))
        )
  )

;Method 4: matched?
(define (matched? rid matches)
  (cond ((null? matches) #f)
        ((findIfResidentExistInListOfTupes rid (car (cdr (car matches))) ) #t )
        (else (matched? rid (cdr matches)))
        )
)



;Fifth Method: get-match
(define (get-match pid matches)
  (cond ((null? matches) (cons pid (list '())))
  ((string=? pid (car (car matches))) (car matches))
  (else (get-match pid (cdr matches)))
  )
)


;Uxiliery Method
(define (compareSecondElement pair theList)
  (cond ((null? theList) (list pair) )
        ((> (cdr (car theList)) (cdr pair)) (cons (car theList) (compareSecondElement pair (cdr theList) )) )
        (else (cons pair theList))
        )
  )

;Sixth Method: add-resident
(define (add-resident-to-match pair match)
  (cond ((null? (car (cdr match))) (cons (car match) (list (list pair))))
  (else (cons (car match) (compareSecondElement pair (car (cdr match)))))
  )
)


;The McVitie-Wilson functions



;Seventh Method: offer
(define (offer rinfo rlist plist matches) 
  (let loop ((rol (cadddr rinfo)))    ; loops over programs on the residents rol
    (if (null? rol)
        matches
        (let* ((pid (car rol))
               (pinfo (get-program-info pid plist))
               (result (evaluate rinfo pinfo rlist plist matches)))
          (if (equal? result matches) ; if result = matches the resident was rejected
              (loop (cdr rol))
              result)))))


; helper for evaluate
(define (update-matches pid new-match matches)
  (if (null? matches)
      (list new-match)
      (if (string=? pid (car (car matches)))
          (cons new-match (cdr matches))
          (cons (car matches) (update-matches pid new-match (cdr matches))))))

; Evaluate: Tries to match a resident with a specific program
(define (evaluate rinfo pinfo rlist plist matches)
  (let* ((rid (car rinfo))
        (rid-rank (rank rid pinfo))
        (pid (car pinfo))
        (capacity (caddr pinfo))
        (current-match (get-match pid matches)))

  (cond
    ((or (not(number? rid-rank)) (< rid-rank 0)) ; Residen unranked, reject
      matches)

    ((null? cadr current-match)  ; Create new match entry if program isn't on it yet
       (cons (list pid (list (cons rid rid-rank))) matches))

    (else
      (let ((current-residents (cadr current-match)))
      (cond
        ((< (length current-residents) capacity) ; if capacity isn't full, add residnet
            (update-matches pid (add-resident-to-match (cons rid rid-rank) current-match) matches))

          (else ; capacity is full
            (let* ((least-pref (car current-residents))   
                   (least-rid  (car least-pref))
                   (least-rank (cdr least-pref)))
                  
            (cond
              ((< rid-rank least-rank) ; if new residnet preferred, remove least preffered

                (let* ((trimmed-residents (cdr current-residents))
                      (temp-match    (list pid trimmed-residents))
                      (updated-match (add-resident-to-match (cons rid rid-rank) temp-match))
                      (new-matches   (update-matches pid updated-match matches))
                      (removed-rinfo  (get-resident-info least-rid rlist)))

                (offer removed-rinfo rlist plist new-matches)))  ; Removed resident needs to match with a new prog
              (else ; resident rejected
                matches))))))))))
      


; Gale Shapley
; if no residents left: return matches
; else, take next resident try to match them (offer)
; update matches
; repeat with remaining residents
(define (gale-shapley rlist plist matches)
  (if (null? rlist)
    matches
    (gale-shapley (cdr r-list) plist (offer (car rlist) rlist plist matches))))


; get-not-matched-list
; display program matches
; display not matched
; get-total available positons

(define (gale-shapley-print rlist plist)
  (let* ((matches (gale-shapley rlist plist '()))
        (not-matched-list (get-not-matched-list rlist matches)))
  (for-each (lambda(m)
    (display-program-matches m rlist plist)) matches)
  (display-not-matched not-matched-list rlist)
  (display "Number of unmatched residents: ")
    (display (length not-matched-list)) (newline)
  (display "Number of positions available: ")
  (display (get-total-available-positions matches plist))
  (newline))
)



