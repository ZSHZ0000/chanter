;;;; The chanter virtual machine, this will just load in a compiled function & run it.

(defpackage #:chanter.vm
  (:use #:cl))
(in-package #:chanter.vm)

;;; Push to stack.
(defun push-elem (element stack)
  "Push element to stack."
  (vector-push element stack))

;;; Pop from stack.
(defun pop-elem (stack)
  "Pop element from stack."
  (vector-pop stack))

;;; Peek at the stack.
(defun peek-stack (stack)
  "Peek at element at the stack."
  (aref stack (1- (fill-pointer stack))))

;;; Print instruction.
(defun print-execute (the-bytecode proc-counter)
  (case (aref the-bytecode proc-counter)
	((0)
	 (format t "~A: RETURN~%" proc-counter))
	((1)
	 (format t "~A: DUPLICATE~%" proc-counter))
	((2)
	 (format t "~A: POP~%" proc-counter))
	((3)
	 (format t "~A: CONST INDEX ~A~%" proc-counter
			 (aref the-bytecode (1+ proc-counter))))
	((4)
	 (format t "~A: STACK-REF ~A~%" proc-counter
			 (aref the-bytecode (1+ proc-counter))))
	((5 6 7 8)
	 (format t "~A: ~A~%" proc-counter
			 (case (aref the-bytecode proc-counter)
			   ((5) "ADD")
			   ((6) "SUBTRACT")
			   ((7) "MULTIPLY")
			   ((8) "DIVIDE"))))))

;;; Peek down N slots down the stack.
(defun peek-down (stack n)
  (aref stack (- (fill-pointer stack) (+ n 1))))

;;; Fall N elements from stack.
(defun fall-off (stack n)
  (setf (fill-pointer stack) (- (fill-pointer stack) n)))

;;; Binary function macro.
(defmacro binary-dispatch (stack operation)
  `(let ((x (peek-down ,stack 1))
		 (y (peek-down ,stack 0)))
	 (fall-off ,stack 2)
	 (push-elem (,operation x y) ,stack)))

;;; Run the bytecode passed.
(defun execute-1 (bytecode stack)
  "Execute the given byte-code chunk."
  (let ((the-bytecode (aref bytecode 0))
		(the-constants (aref bytecode 1))
		(proc-counter 0))
	(loop do
	  (case (aref the-bytecode proc-counter)
		((0)							; (RETURN)
		 (print-execute the-bytecode proc-counter)
		 (return-from execute-1 stack))
		((1)							; (DUPLICATE)
		 (print-execute the-bytecode proc-counter)
		 (push-elem (peek-stack stack) stack)
		 (incf proc-counter))
		((2)							; (POP)
		 (print-execute the-bytecode proc-counter)
		 (pop-elem stack)
		 (incf proc-counter))
		((3)							; (CONST INDEX)
		 (print-execute the-bytecode proc-counter)
		 (push-elem (aref the-constants
						  (aref the-bytecode (1+ proc-counter)))
					stack)
		 (incf proc-counter 2))
		((4)							; (STACK-REF)
		 (print-execute the-bytecode proc-counter)
		 (push-elem (peek-down stack
							   (aref the-bytecode (1+ proc-counter)))
					stack)
		 (incf proc-counter 2))
		((5)							; (ADD)
		 (print-execute the-bytecode proc-counter)
		 (binary-dispatch stack +)
		 (incf proc-counter))
		((6)							; (SUBTRACT)
		 (print-execute the-bytecode proc-counter)
		 (binary-dispatch stack -)
		 (incf proc-counter))
		((7)							; (MULTIPLY)
		 (print-execute the-bytecode proc-counter)
		 (binary-dispatch stack *)
		 (incf proc-counter))
		((8)							; (DIVIDE)
		 (print-execute the-bytecode proc-counter)
		 (binary-dispatch stack /)
		 (incf proc-counter))))))
