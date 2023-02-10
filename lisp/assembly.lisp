;;;; The chanter assembler, this will be used to produce actual byte-code from our low-
;;;; level assembly instruction mnemonics.

(defpackage #:chanter.asm
  (:use #:cl)
  (:export #:*instruction-set*
		   #:assemble-1))
(in-package #:chanter.asm)

;;; Instruction set, it must have the form (MNEMONIC OPCODE ARG-COUNT &REST ARG-SIZE)
(defvar *instruction-set*
  '((RETURN 0 0)
	;; Stack operations.
	(DUP 1 0)
	(POP 2 0)
	(CONST 3 1 2)
	(STACK-REF 4 1 2)
	(STACK-SET 5 1 2)
	(DROP-BUT-TOP 6 1 2)
	;; Arithmetic operations.
	(ADD 7 0)
	(SUB 8 0)
	(MUL 9 0)
	(DIV 10 0)
	;; Comparison operations.
	(EQ? 13 0)
	(NEQ? 14 0)
	(ZERO? 15 0)
	(LESSER? 16 0)
	(GREATER? 17 0)
	(LESSEQ? 18 0)
	(GREATEQ? 19 0)
	;;; Control flow operations.
	(GOTO 24 1 4)
	(GOTO-IF-T 25 1 4)))

;;; Various general instruction abstractions.
(defun get-data (mnemonic)
  (assoc mnemonic *instruction-set* :test #'string=))
(defun get-opcode (mnemonic)
  (second (get-data mnemonic)))
(defun get-argument-count (mnemonic)
  (third (get-data mnemonic)))
(defun get-argument-sizes (mnemonic)
  (cdddr (get-data mnemonic)))

;;; Emit byte.
(defun emit-byte (output byte)
  (vector-push-extend byte output 256))

;; Transform a fixnum into a bunch of bytes.
(defun byte-transform (fixnum byte-count)
  (do ((output '())
	   (offset 0 (+ offset 8))
	   (bytes-done 0 (1+ bytes-done)))
	  ((= bytes-done byte-count) output)
	(push (ldb (byte 8 offset) fixnum) output)))

;; Extract labels from the instructions themselves.
(defun extract-labels (assembly-code)
  "Extract the labels from the operations."
  (do* ((instructions-left assembly-code (rest instructions-left))
		(current-instruction (first assembly-code) (first instructions-left))
		(the-labels nil)
		(the-instructions-tail (cons 'not-an-instruction nil))
		(the-instructions the-instructions-tail)
		(virtual-prog-counter 0))
	   ((null instructions-left)
		(values (rest the-instructions) the-labels))
	(cond
	  ;; Is it an instruction?
	  ((listp current-instruction)
	   ;; If so, put it into the instructions & increase virtual program counter
	   ;; accordingly.
	   (setf the-instructions-tail (nconc the-instructions-tail (list current-instruction)))
	   (setf the-instructions-tail (rest the-instructions-tail))
	   ;; Increase virtual program counter.
	   (incf virtual-prog-counter
			 (apply #'+ 1 (get-argument-sizes (first current-instruction)))))
	  ;; Is it a label?
	  ((typep current-instruction '(or keyword symbol))
	   ;; Add it to the labels.
	   (setf the-labels
			 (acons current-instruction (1+ virtual-prog-counter) the-labels))))))

;;; Assembler
(defun assemble-2 (the-constants the-instructions the-labels)
  "Emit byte-code based on the given instructions & constants"
  (let ((the-bytecode-output (make-array 256 :element-type 'fixnum
											 :adjustable t
											 :fill-pointer 0))
		(the-constants-output (make-array 32 :element-type t
											 :adjustable t
											 :fill-pointer 0))
		(virtual-prog-counter 0))
	;; Create the constants vector.
	(dolist (constant the-constants)
	  (vector-push-extend constant the-constants-output))
	;; Create the byte-code output.
	(dolist (instruction the-instructions)
	  (cond
		;; Is the instruction a list? if so, then it's an instruction.
		((listp instruction)
		 (let* ((mnemonic (first instruction))
				(arguments (rest instruction))
				(opcode-byte (get-opcode mnemonic))
				(opcode-argument-count (get-argument-count mnemonic)))
		   (cond
			 ;; Check it is an actual instruction.
			 ((not (get-data mnemonic))
			  (error
			   (format nil "Instruction ~A does not exist, typo?"
					   mnemonic)))
			 ;; If it is an argument-less instruction just emit it right away.
			 ((= opcode-argument-count 0)
			  (when arguments
				(warn
				 (format nil "Instruction ~A that is argument-less has arguments ~A."
						 mnemonic arguments)))
			  (emit-byte the-bytecode-output opcode-byte)
			  (incf virtual-prog-counter))
			 ;; If the argument length isn't equal to the arguments required by the
			 ;; instruction, error out.
			 ((/= opcode-argument-count (length arguments))
			  (error
			   (format nil "Instruction ~A requires specifically ~A arguments but got ~A."
					   mnemonic opcode-argument-count (length arguments))))
			 ;; Handle GOTO & GOTO-IF specially.
			 ((member mnemonic '(goto goto-if-t) :test #'string=)
			  (emit-byte the-bytecode-output opcode-byte)
			  (incf virtual-prog-counter)
			  (let ((label (first arguments)))
				(unless (typep label '(or keyword symbol))
				  (error
				   (format nil "Wrong argument for instruction ~A: ~A." mnemonic label)))
				(let ((refered-label (assoc label the-labels)))
				  (if refered-label
					  ;; The address is in the cdr of the cells.
					  (dolist (byte (byte-transform (- (rest refered-label) virtual-prog-counter)
													(first (get-argument-sizes mnemonic))))
						(emit-byte the-bytecode-output byte)
						(incf virtual-prog-counter))
					  (error
					   (format nil "Unknown label used as argument for instruction ~A: ~A." mnemonic label)))))) 
			 ;; instruction with matching argument count.
			 (t
			  (emit-byte the-bytecode-output opcode-byte)
			  (incf virtual-prog-counter)
			  ;; Process arguments.
			  (do* ((arguments-left arguments (rest arguments-left))
					(argument-sizes-left (get-argument-sizes mnemonic) (rest argument-sizes-left))
					(argument (first arguments-left) (first arguments-left))
					(argument-size (first argument-sizes-left) (first argument-sizes-left)))
				   ;; Quit when there are no more argumentsuments left.
				   ((null arguments-left))
				;; Emit the argumentsument bytes byte by byte.
				(dolist (byte (byte-transform argument argument-size))
				  (emit-byte the-bytecode-output byte)
				  (incf virtual-prog-counter)))))))
		;; Otherwise error out.
		(t
		 (error "Bad type passed to the assembler."))))
	(vector the-bytecode-output the-constants-output)))

;; Assemble the input.
(defun assemble-1 (the-constants assembly-code)
  (multiple-value-bind (instructions label-addresses)
	  (extract-labels assembly-code)
	(assemble-2 the-constants instructions label-addresses)))
