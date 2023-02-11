;;;; The chanter virtual machine, this will just load in a compiled function & run it.

(defpackage #:chanter.vm
  (:use #:cl))
(in-package #:chanter.vm)

;;; Push to stack.
(defun push-elem (element stack)
  "Push element to stack."
  (vector-push-extend element stack (expt 2 16)))

;;; Pop from stack.
(defun pop-elem (stack)
  "Pop element from stack."
  (vector-pop stack))

;;; Peek at the stack.
(defun peek-stack (stack)
  "Peek at element at the stack."
  (aref stack (1- (fill-pointer stack))))

;;; Peek down N slots down the stack.
(defun peek-down (stack n)
  (aref stack (- (fill-pointer stack) (+ n 1))))

;;; Set the value that is N values deep down.
(defsetf peek-down (stack n) (new-value)
  `(progn (setf (aref ,stack (- (fill-pointer ,stack) (+ ,n 1))) ,new-value)
		  ,new-value))

;;; Set a value N values deep down to value.
(defun set-n-deep (stack n value)
  (setf (peek-down stack n) value))

;;; Fall N elements from stack.
(defun fall-off (stack n)
  (setf (fill-pointer stack) (- (fill-pointer stack) n)))

;;; Binary function macro.
(defmacro binary-dispatch (stack operation)
  `(let ((x (peek-down ,stack 1))
		 (y (peek-down ,stack 0)))
	 (fall-off ,stack 2)
	 ,(if (symbolp operation)
		  `(push-elem (,operation x y) ,stack)
		  `(push-elem (funcall ,operation x y) ,stack))))

;;; Unary dispatch.
(defmacro unary-dispatch (stack operation)
  `(let ((x (peek-down ,stack 0)))
	 (pop-elem ,stack)
	 ,(if (symbolp operation)
		  `(push-elem (,operation x) ,stack)
		  `(push-elem (funcall ,operation x) ,stack))))

;;; Consume byte sequence from the instruction vector.
(defun byte-consume (byte-count the-instructions program-counter)
  (do ((value 0)
	   (index program-counter (1+ index))
	   (bytes-done 0 (1+ bytes-done)))
	  ((= bytes-done byte-count) value)
	(setf value (ash value 8))
	(setf value (logior value (aref the-instructions index)))))

;;; Sign an integer of size size, we won't need to deal with this in C.
(defun sign-integer (integer size)
  (let ((bits-minus-1
		  (ash 1 (1- (* 8 size))))
		(largest-minus-1
		  (1- (ash 1 (1- (* 8 size))))))
	  (if (zerop (logand integer bits-minus-1))
		  (logand integer largest-minus-1)
		(- (logand integer largest-minus-1)
		   largest-minus-1
		   1))))

;;; Run the bytecode passed.
(defun execute-1 (bytecode stack)
  "Execute the given byte-code chunk."
  (let ((the-bytecode (aref bytecode 0))
		(the-constants (aref bytecode 1))
		(prog-counter 0))
	(loop do
	  (case (aref the-bytecode prog-counter)
		((0)							; (RETURN)
		 (return-from execute-1 stack))
		((1)							; (DUP)
		 (push-elem (peek-stack stack) stack)
		 (incf prog-counter))
		((2)							; (POP)
		 (pop-elem stack)
		 (incf prog-counter))
		((3)							; (CONST INDEX)
		 (push-elem (aref the-constants
						  (byte-consume 2 the-bytecode (1+ prog-counter)))
					stack)
		 (incf prog-counter 3))
		((4)							; (STACK-REF)
		 (push-elem (peek-down stack
							   (byte-consume 2 the-bytecode (1+ prog-counter)))
					stack)
		 (incf prog-counter 3))
		((5)							; (STACK-SET)
		 (let ((tos (peek-stack stack)))
		   (set-n-deep stack (byte-consume 2 the-bytecode (1+ prog-counter)) tos))
		 (pop-elem stack)
		 (incf prog-counter 3))
		((6)							; (DROP-BUT-TOP)
		 (let ((tos (peek-stack stack)))
		   (pop-elem stack)
		   (fall-off stack (byte-consume 2 the-bytecode (1+ prog-counter)))
		   (push-elem tos stack))
		 (incf prog-counter 3))
		((7 8 9 10)						; (ADD SUB MUL DIV)
		 (case (- (aref the-bytecode prog-counter) 7)
		   ((0) (binary-dispatch stack +))
		   ((1) (binary-dispatch stack -))
		   ((2) (binary-dispatch stack *))
		   ((3) (binary-dispatch stack /)))
		 (incf prog-counter))
		;; (EQ? NEQ? ZERO? LESSER? GREATER? LESSEQ? GREATEQ?)
		((13 14 15 16 17 18 19)
		 (case (- (aref the-bytecode prog-counter) 13)
		   ((0) (binary-dispatch stack eq))
		   ((1) (binary-dispatch stack (complement #'eq)))
		   ((2) (unary-dispatch stack zerop))
		   ((3) (binary-dispatch stack <))
		   ((4) (binary-dispatch stack >))
		   ((5) (binary-dispatch stack <=))
		   ((6) (binary-dispatch stack >=)))
		 (incf prog-counter))
		((24)							; (GOTO)
		 (let ((relative-addr
				 (sign-integer (byte-consume 4 the-bytecode (1+ prog-counter))
							   4)))
		   (setf prog-counter (+ prog-counter relative-addr))))
		((25)							; (GOTO-IF-T)
		 (let ((tos (peek-stack stack))
			   (relative-addr
				 (sign-integer (byte-consume 4 the-bytecode (1+ prog-counter))
							   4)))
		   (if tos
			   (progn
				 (pop-elem stack)
				 (setf prog-counter (+ prog-counter relative-addr)))
			   (progn
				 (pop-elem stack)
				 (incf prog-counter 5)))))
		((30)							; (CALL-SEGMENT)
		 (let ((return-addr (+ prog-counter 5))
			   (relative-addr
				 (sign-integer (byte-consume 4 the-bytecode (1+ prog-counter))
							   4)))
		   (push-elem return-addr stack)
		   (setf prog-counter (+ prog-counter relative-addr))))
		((31)							; (RETURN-SEGMENT)
		 (let ((tos (peek-stack stack)))
		   (pop-elem stack)
		   (setf prog-counter tos)))))))
