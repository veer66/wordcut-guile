(define-module (test-wordcut-tokenizer)
  #:use-module (oop goops)  
  #:use-module (srfi srfi-64)
  #:use-module (wordcut tokenizer))

(define tiny-dict (vector "กา" "ขา" "ขาม" "ค"))
(define tiny-l 0)
(define tiny-r (- (vector-length tiny-dict) 1))

(define p0 (make Pointer #:s 0 #:l tiny-l #:r tiny-r #:offset 0 #:dict tiny-dict))

(test-begin "dict-seek")

(test-assert "find first word"
  (equal? 1 (dict-seek tiny-dict 'LEFT tiny-l tiny-r 0 #\ข)))

(test-begin "pointer")

(test-assert "basic update"
  (let* ((p0-u (update p0 #\ข)))
    (equal? 1 (slot-ref p0-u 'l))
    (equal? 2 (slot-ref p0-u 'r))
    (equal? 0 (slot-ref p0-u 's))
    (equal? #f (slot-ref p0-u 'is-final))))

(test-assert "basic update final"
  (let* ((p0-u (update p0 #\ข))
	 (p0-u2 (update p0-u #\า)))
    (equal? 2 (slot-ref p0-u2 'l))
    (equal? 2 (slot-ref p0-u2 'r))
    (equal? 0 (slot-ref p0-u2 's))
    (equal? #t (slot-ref p0-u2 'is-final))))


(test-assert "transit"
  (let* ((pointers (list (make Pointer #:s 0 #:l 1 #:r 2
			       #:offset 1 #:dict tiny-dict)
			 (make Pointer #:s 1 #:l 0 #:r 3
			       #:offset 0 #:dict tiny-dict)))
	 (pointers-u (transit-pointers pointers #\า)))
    (equal? 1 (length pointers-u))))


(test-begin "edge")
(test-assert "build edges"
  (let* ((pointers (list (make Pointer #:s 0 #:l 1 #:r 2
			       #:offset 2 #:dict tiny-dict #:is-final #t)))
	 (dag (vector (make Edge #:s 0 #:unk 0 #:chunk 0
			    #:type 'INIT #:payload #nil)))
	 (edges (build-edges dag pointers Edge)))
    (equal? 1 (length edges))
    (equal? 1 (slot-ref (car edges) 'chunk))))

(test-assert "compare edges"
  (let* ((e0 (make Edge #:unk 10 #:chunk 10))
	 (e1 (make Edge #:unk 5  #:chunk 20)))
    (equal? #t (better? e1 e0))))

(test-assert "best edge"
  (let* ((e0 (make Edge #:unk 10 #:chunk 10))
	 (e1 (make Edge #:unk 5  #:chunk 20)))
     (equal? 5 (slot-ref (best-edge (list e0 e1)) 'unk))))

(test-assert "create unknown word edge"
  (let* ((e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
	 (dag (vector e0 #nil))
	 (left 0)
	 (e-unk (create-unk-edge dag left Edge)))
    (equal? 1 (slot-ref e-unk 'unk))))

(test-begin "dag")
(test-assert "update dag using dict"
  (let* ((e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
	 (dag (vector e0 #nil))
	 (pointers (list (make Pointer #:s 0 #:l 1
			       #:r 1 #:offset 1 #:is-final #t
			       #:dict tiny-dict))))
    (update-dag-dict! dag 1 pointers Edge)
    (let ((e1 (vector-ref dag 1)))
      (equal? 0 (slot-ref e1 's))
      (equal? 1 (slot-ref e1 'chunk)))))

(test-assert "update dag by unknown word"
  (let* ((e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
	 (dag (vector e0 #nil)))
    (update-dag-unk! dag 1 0 Edge)
    (let ((e1 (vector-ref dag 1)))
      (equal? 0 (slot-ref e1 's))
      (equal? 1 (slot-ref e1 'unk)))))

(test-assert "update dag when there is no final pointer"
  (let* ((e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
	 (dag (vector e0 #nil))
	 (pointers (list (make Pointer #:s 0 #:l 1
			       #:r 1 #:offset 1 #:is-final #f
			       #:dict tiny-dict))))
    (update-dag! dag 1 0 pointers Edge)
    (let ((e1 (vector-ref dag 1)))
      (equal? 0 (slot-ref e1 's))
      (equal? 1 (slot-ref e1 'unk)))))

(test-assert "update dag when there is a final pointer"
  (let* ((e0 (make Edge #:s 0 #:unk 0 #:chunk 0))
	 (dag (vector e0 #nil))
	 (pointers (list (make Pointer #:s 0 #:l 1
			       #:r 1 #:offset 1 #:is-final #t
			       #:dict tiny-dict))))
    (update-dag! dag 1 0 pointers Edge)
    (let ((e1 (vector-ref dag 1)))
      (equal? 0 (slot-ref e1 's))
      (equal? 0 (slot-ref e1 'unk)))))

(test-assert "build dag"
  (let* ((txt "ขามกา")
	 (dag (build-dag txt tiny-dict Edge)))
    (equal? 0 (slot-ref (vector-ref dag 3) 's))
    (equal? 3 (slot-ref (vector-ref dag 5) 's))))


(test-assert "dag to list"
  (let ((dag (vector (make Edge #:s 0 #:unk 0 #:chunk 0)
		     #nil
		     #nil
		     (make Edge #:s 0 #:unk 0 #:chunk 1 #:type 'DICT)
		     #nil
		     (make Edge #:s 3 #:unk 0 #:chunk 2 #:type 'DICT))))
    (let ((lst (dag->list dag "ขามกา")))
      (equal? (list "ขาม" "กา") lst))))

(test-assert "dag to txt"
  (let ((dag (vector (make Edge #:s 0 #:unk 0 #:chunk 0)
		     #nil
		     #nil
		     (make Edge #:s 0 #:unk 0 #:chunk 1 #:type 'DICT)
		     #nil
		     (make Edge #:s 3 #:unk 0 #:chunk 2 #:type 'DICT))))
    (let ((lst (dag->txt dag "ขามกา")))
      (equal? "ขาม กา" lst))))

(test-begin "tokenize")

(test-assert "basic tokenize"
  (let ((tokenize (create-word-tokenizer tiny-dict Edge)))
    (equal? (list "ขาม" "กา") (tokenize "ขามกา"))))

(test-assert "tokenize with std dict"
  (let ((tokenize (create-word-tokenizer (load-dict "../data/tdict-std.txt")
					 Edge)))
    (equal? (list "ขาม" "กา") (tokenize "ขามกา"))))
