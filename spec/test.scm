;; -*- geiser-scheme-implementation: guile -*-

(use-modules (ggspec lib))
(load "../wordcut/wordcut.scm")
;;(primitive-load-path "wordcut/wordcut.scm")

(define tiny-dict (vector "กา" "ขา" "ขาม" "ค"))
(define tiny-l 0)
(define tiny-r (- (vector-length tiny-dict) 1))

(suite
 "dict"
 (tests
  (test
   "find first word"
   e	      
   (assert-equal 1 (dict-seek tiny-dict 'LEFT tiny-l tiny-r 0 #\ข)))))


(define p0 (make Pointer #:s 0 #:l tiny-l #:r tiny-r #:offset 0 #:dict tiny-dict))

(suite
 "pointer"
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
				  #:offset 1 #:dict tiny-dict)
			    (make Pointer #:s 1 #:l 0 #:r 3
				  #:offset 0 #:dict tiny-dict)))
     (define pointers-u (transit-pointers pointers #\า))
     (assert-equal 1 (length pointers-u))
     ))
  ))

(suite
 "edge"
 (tests
  (test
   "build edges"
   e
   (begin
     (define pointers (list (make Pointer #:s 0 #:l 1 #:r 2
				  #:offset 2 #:dict tiny-dict #:is-final #t)))
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
  (test
   "create unknown word edge"
   e
   (begin
     (define e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
     (define dag (vector e0 #nil))
     (define left 0)
     (define e-unk (create-unk-edge dag left Edge))
     (assert-equal 1 (slot-ref e-unk 'unk))
     ))

  ))


(suite
 "dag"
 (tests
  (test
   "update dag using dict"
   e
   (begin
     (define e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
     (define dag (vector e0 #nil))
     (define pointers (list (make Pointer #:s 0 #:l 1
				  #:r 1 #:offset 1 #:is-final #t
				  #:dict tiny-dict)))
     (update-dag-dict! dag 1 pointers Edge)
     (let ((e1 (vector-ref dag 1)))
       (assert-equal 0 (slot-ref e1 's))
       (assert-equal 1 (slot-ref e1 'chunk)))))
  (test
   "update dag by unknown word"
   e
   (begin
     (define e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
     (define dag (vector e0 #nil))
     (update-dag-unk! dag 1 0 Edge)
     (let ((e1 (vector-ref dag 1)))
       (assert-equal 0 (slot-ref e1 's))
       (assert-equal 1 (slot-ref e1 'unk)))))
  (test
   "update dag when there is no final pointer"
   e
   (begin
     (define e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
     (define dag (vector e0 #nil))
     (define pointers (list (make Pointer #:s 0 #:l 1
				  #:r 1 #:offset 1 #:is-final #f
				  #:dict tiny-dict)))

     (update-dag! dag 1 0 pointers Edge)
     (let ((e1 (vector-ref dag 1)))
       (assert-equal 0 (slot-ref e1 's))
       (assert-equal 1 (slot-ref e1 'unk)))))
  (test
   "update dag when there is a final pointer"
   e
   (begin
     (define e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
     (define dag (vector e0 #nil))
     (define pointers (list (make Pointer #:s 0 #:l 1
				  #:r 1 #:offset 1 #:is-final #t
				  #:dict tiny-dict)))

     (update-dag! dag 1 0 pointers Edge)
     (let ((e1 (vector-ref dag 1)))
       (assert-equal 0 (slot-ref e1 's))
       (assert-equal 0 (slot-ref e1 'unk)))))
  (test
   "build dag"
   e
   (begin
     (define txt "ขามกา")
     (define dag (build-dag txt tiny-dict Edge))
     (assert-equal 0 (slot-ref (vector-ref dag 3) 's))
     (assert-equal 3 (slot-ref (vector-ref dag 5) 's))))
  ))
