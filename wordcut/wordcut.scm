;; -*- geiser-scheme-implementation: guile -*-

(use-modules (ice-9 format))
(use-modules (oop goops))
(use-modules (srfi srfi-1))
	     
(define (dict-seek dict policy l r offset ch)
  (define (recur l r ans)
    (if (<= l r)
	(let* ((m (floor (/ (+ l r) 2)))
	       (w (car (vector-ref dict m)))
	       (wlen (vector-length w)))
	  (if (<= wlen offset)
	      (recur (+ 1 m) r ans)
	      (let ((ch-w (vector-ref w offset)))
		(cond ((char<? ch-w ch) (recur (+ 1 m) r ans))
		      ((char>? ch-w ch) (recur l (- m 1) ans))
		      ((eq? policy 'LEFT) (recur l (- m 1) m))
		      ((eq? policy 'RIGHT) (recur (+ 1 m) r m))))))
	ans))
  (recur l r #nil))

(define (list-to-dict w-lst)
  (define (convert w)
    (list (list->vector (string->list w))))
  (list->vector (map convert w-lst)))

(define (last-i dict)
  (- (vector-length dict) 1))

(define-class Pointer ()
  (s #:init-value 0 #:accessor s #:init-keyword #:s)
  (l #:init-value 0 #:accessor l #:init-keyword #:l)
  (r #:init-value 0 #:accessor r #:init-keyword #:r)
  (offset #:init-value 0 #:accessor offset #:init-keyword #:offset)
  (dict #:init-value #nil #:accessor dict #:init-keyword #:dict)
  (is-final #:init-value #f #:accessor is-final #:init-keyword #:is-final))

(define (dict-wlen dict i)
  (vector-length (car (vector-ref dict i))))

(define-generic update)

(define-method (update (pointer Pointer) (ch <char>))
  (let* ((prev-l (slot-ref pointer 'l))
  	 (prev-r (slot-ref pointer 'r))
  	 (prev-offset (slot-ref pointer 'offset))
  	 (prev-s (slot-ref pointer 's))
  	 (dict (slot-ref pointer 'dict))
  	 (l (dict-seek dict 'LEFT prev-l prev-r prev-offset ch)))
    (if l
  	(let* ((r (dict-seek dict 'RIGHT l prev-r prev-offset ch))
  	       (offset (+ prev-offset 1))
  	       (is-final (eq? (dict-wlen dict l) (+ prev-offset 1))))
  	  (make Pointer #:s prev-s #:l l #:r r #:offset offset #:dict dict
		#:is-final is-final))
  	#nil)))

(define (transit-pointers pointers ch)
  (remove null? (map (lambda (p) (update p ch)) pointers)))

(define-class Edge ()
  (s #:init-value 0 #:accessor s #:init-keyword #:s)
  (unk #:init-value 0 #:accessor unk #:init-keyword #:unk)
  (chunk #:init-value 0 #:accessor chunk #:init-keyword #:chunk)
  (type #:init-value 'INIT #:accessor type #:init-keyword #:type)
  (payload #:init-value #nil #:accessor payload #:init-keyword #:payload))

(define (build-edges dag final-pointers edge-class)
  (define (each-edge pointer)
    (let* ((s (slot-ref pointer 's))
     	   (src (vector-ref dag s))
     	   (l (slot-ref pointer 'l))
	   (dict (slot-ref pointer 'dict))
	   (unk (slot-ref src 'unk))
	   (chunk (+ (slot-ref src 'chunk) 1))
	   (payload (vector-ref dict l)))
       (make edge-class #:s s #:unk unk #:chunk chunk
	     #:type 'DICT #:payload payload)))
  (map each-edge final-pointers))

(define-generic better?)
(define-method (better? (e0 Edge) (e1 Edge))
  (define (recur slots is-better)
    (if (or (null? slots) (not (null? is-better)))
	is-better
	(let ((this-slot (car slots)))
	  (recur (cdr slots) (< (slot-ref e0 this-slot)
				(slot-ref e1 this-slot))))))
  (recur (list 'unk 'chunk) #nil))


(define (best-edge edges)
  (define (recur best edges)
    (cond ((null? edges) best)
	  ((better? (car edges) best) (recur (car edges) (cdr edges)))
	  (else (recur best (cdr edges)))))
  (recur (car edges) (cdr edges)))

(define create-unk-edge (dag left edge-class)
  (let ((src (vector-ref dag left)))
    (make edge-class
      #:chunk (slot-ref src 'chunk)
      #:unk (+ (slot-ref src 'unk) 1)
      #:s left
      #:type 'UNK
      #:payload nil)))


;; (define dict (list-to-dict (list "กา" "ขา" "ขาม" "ค")))
;; (define p0 (make Pointer #:s 0 #:l 0 #:r 3 #:offset 0 #:dict dict))
;; (slot-ref p0 'dict)
;; (slot-ref p0 'r)

;; (slot-ref (update p0 #\ข) 'l)

