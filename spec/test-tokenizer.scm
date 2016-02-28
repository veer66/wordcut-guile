
(primitive-load "wordcut/wordcut.scm")
(define tiny-dict (vector "กา" "ขา" "ขาม" "ค"))
(use-modules (ggspec lib)
	     (wordcut tokenizer))

(suite
 "tokenize"
 (tests
  (test
   "basic"
   e
   (begin
     (define tokenize (create-word-tokenizer tiny-dict Edge))
     (assert-equal (list "ขาม" "กา") (tokenize "ขามกา"))))))
