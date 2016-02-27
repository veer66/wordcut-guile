;; -*- geiser-scheme-implementation: guile -*-

(use-modules (ggspec lib))
(load "../wordcut/wordcut.scm")
;;(primitive-load-path "wordcut/wordcut.scm")

(suite
 "dict"
 (let* ((dict (list-to-dict (list "กา" "ขา" "ขาม" "ค")))
	(l 0)
	(r (- (vector-length dict) 1)))
   (tests
    (test
     "find first word"
     e	      
     (assert-equal 1 (dict-seek dict 'LEFT l r 0 #\ข))))))


(suite
 "pointer"
 (let* ((dict (list-to-dict (list "กา" "ขา" "ขาม" "ค")))
	(l 0)
	(r (- (vector-length dict) 1))
	(p0 (make Pointer #:s 0 #:l l #:r r #:offset 0 #:dict dict)))
   (tests
    (test
     "basic update"
     e
     (begin
       (define p0-u (update p0 #\ข))
       (assert-equal 1 (slot-ref p0-u 'l))
       (assert-equal 2 (slot-ref p0-u 'r))
       (assert-equal 0 (slot-ref p0-u 's))
       (assert-equal #f (slot-ref p0-u 'is-final))))
    (test
     "basic update final"
     e
     (begin
       (define p0-u (update p0 #\ข))
       (define p0-u2 (update p0-u #\า))
       (assert-equal 2 (slot-ref p0-u2 'l))
       (assert-equal 2 (slot-ref p0-u2 'r))
       (assert-equal 0 (slot-ref p0-u2 's))
       (assert-equal #t (slot-ref p0-u2 'is-final))))
    (test
     "transit"
     e
     (begin
       (define pointers (list (make Pointer #:s 0 #:l 1 #:r 2
				    #:offset 1 #:dict dict)
			      (make Pointer #:s 1 #:l 0 #:r 3
				    #:offset 0 #:dict dict)))
       (define pointers-u (transit-pointers pointers #\า))
       (assert-equal 1 (length pointers-u))
       ))
    )))

(suite
 "edge"
 (let* ((dict (list-to-dict (list "กา" "ขา" "ขาม" "ค")))
	(l 0)
	(r (- (vector-length dict) 1))
	(p0 (make Pointer #:s 0 #:l l #:r r #:offset 0 #:dict dict)))
   (tests
    (test
     "build edges"
     e
     (begin
       (define pointers (list (make Pointer #:s 0 #:l 1 #:r 2
				    #:offset 2 #:dict dict #:is-final #t)))
       (define dag (vector (make Edge #:s 0 #:unk 0 #:chunk 0
				 #:type 'INIT #:payload #nil)))
       (define edges (build-edges dag pointers Edge))
       (assert-equal 1 (length edges))
       (assert-equal 1 (slot-ref (first edges) 'chunk))
       ))
    (test
     "compare edges"
     e
     (begin
       (define e0 (make Edge #:unk 10 #:chunk 10))
       (define e1 (make Edge #:unk 5  #:chunk 20))
       (assert-equal #t (better? e1 e0))
       ))
    (test
     "best edge"
     e
     (begin
       (define e0 (make Edge #:unk 10 #:chunk 10))
       (define e1 (make Edge #:unk 5  #:chunk 20))
       (assert-equal 5 (slot-ref (best-edge (list e0 e1)) 'unk))
       ))
    )))


