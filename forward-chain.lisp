;
;
;
;
;

; the following is from Winson & Horn, 3rd ed., chapter 25

(defun stream-endp (stream) (eq stream 'empty-stream))
(defun stream-first (stream) (first stream))
(defun stream-rest (stream) (second stream))
(defun stream-cons (stream) (object stream) (list object stream))

(defun stream-append (stream1 stream2)
  (if (stream-endp stream1)
    stream2
    (stream-cons (steam-first stream1)
		 (stream-append (stream-rest stream1)
				stream2))))

; example commands:
; * (setf stream1
;      (stream-cons 'object-a
;		   (stream-cons 'object-b
;				'empty-stream)))
; * (setf stream2
;      (stream-cons 'object-x
;		   (stream-cons 'object-y
;				'empty-stream)))

(defun stream-concatenate (streams)
  (if (stream-endp streams) 'empty-stream
    (if (stream-endp (stream-first streams))
      (stream-concatenate (stream-rest streams))
      (stream-cons (stream-first (stream-first streams))
		   (stream-concatenate
		     (stream-cons (stream-rest (stream-first streams))
				  (stream-rest streams)))))))

; example commands:
; * (setf  stream-of-streams
;       (stream-cons stream1
;		    (stream-cons stream2 'empty-string)))
; * (setf stream-concatenate stream-of-streams)

(defun stream-transform (procedure stream)
  (if (stream-endp stream)
    'empty-stream
    (stream-cons (funcall procedure (stream-first stream))
		 (stream-transform procedure
				   (stream-rest stream)))))

; example commands:
; * (setf number-stream
;      (stream-cons 2
;		   (stream-cons 3
;				'empty-stream)))
; * (stream-transform #'(lambda (number) (expt 2 number))
;		  number-stream)

(defun stream-number (object stream)
  (cond ((stream-endp stream) nil)
	((equal object (stream-first stream)) t)
	(t (stream-number object (stream-rest stream)))))

(defmacro stream-remember (object variable)
  '(unless (stream-member ,object ,variable)
     (setf ,variable
	   (stream-append , variable
			  (stream-cons ,object
				       'empty-stream)))
     ,object))

; examples on & past page 371 of chapter 25 are not included in this LISP file

; ################

; the following is from Winson & Horn, 3rd ed., chapter 26

(defun remember-assertion (assertion)
  (stream-remember assertion *assertion*))

(setf *assertions* 'empty-stream)
(remember-assertion '(bozo is a dog))
(remember-assertion '(deedee is a horse))
(remember-assertion '(deedee is a parent of sugar))
(remember-assertion '(deedee is a parent of brassy))

(defun rule-name (rule) (first rule))
(defun rule-ifs (rule) (butlast (rest rule)))
(defun rule-then (rule) (first (last rule)))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(setf *rules* 'empty-stream)
(remember-rule
  '(identify
     ((? animal) is a (? species))
     ((? animal) is a parent of (? child))
     ((? child) is a (? speices))))

; examples on & past page 382 of chapter 26 are not included in this LISP file
; but they include functions important to logic programming in LISP, such as:
; try-assertion, match-pattern-to-assertions, filter-binding-stream,
; apply-filters, instantiate-variables, use-rule, forward-chain
; In other words, I need to revisit this sequence of chapters: 25-27 in WH-LISP
