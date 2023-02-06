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
	(GREATEQ? 19 0)))

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

;;; Assembler
(defun assemble-1 (the-constants the-instructions)
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
			 ;; instruction with matching argument count.
			 (t
			  (emit-byte the-bytecode-output opcode-byte)
			  (incf virtual-prog-counter)
			  ;; Process arguments.
			  (do* ((arguments-left arguments (rest argument-sizes-left))
					(argument-sizes-left (get-argument-sizes mnemonic) (rest argument-sizes-left))
					(arguments (first arguments-left) (first arguments-left))
					(arguments-size (first argument-sizes-left) (first argument-sizes-left)))
				   ;; Quit when there are no more argumentsuments left.
				   ((null arguments-left))
				;; Emit the argumentsument bytes byte by byte.
				(dolist (byte (byte-transform arguments arguments-size))
				  (emit-byte the-bytecode-output byte)
				  (incf virtual-prog-counter)))))))
		;; Otherwise error out.
		(t
		 (error "Bad type passed to the assembler."))))
	(vector the-bytecode-output the-constants-output)))
