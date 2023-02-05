;;;; The chanter assembler, this will be used to produce actual byte-code from our low-
;;;; level assembly instruction mnemonics.

(defpackage #:chanter.asm
  (:use #:cl)
  (:export #:*instruction-set*
		   #:assembly-1))
(in-package #:chanter.asm)

;;; Instruction set, it must have the form (MNEMONIC OPCODE ARG-COUNT &REST ARG-SIZE)
(defvar *instruction-set*
  '((RETURN 0 0)
	(DUP 1 0)
	(POP 2 0)
	(CONST 3 1 1)
	(STACK-REF 4 1 1)
	(ADD 5 0)
	(SUB 6 0)
	(MUL 7 0)
	(DIV 8 0)))

;;; Various abstractions.
(defun get-data (mnemonic)
  (assoc mnemonic *instruction-set* :test #'string=))
(defun get-opcode (mnemonic)
  (second (get-data mnemonic)))
(defun get-arg-count (mnemonic)
  (third (get-data mnemonic)))
(defun get-arg-sizes (mnemonic)
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

;; Add label.
(defun add-label (label hash-table program-counter)
  (setf (gethash label hash-table) program-counter))

;;; Assembler
(defun assemble-1 (the-constants the-instructions)
  "Emit byte-code based on the given instructions & constants"
  (let ((the-bytecode-output (make-array 256 :element-type 'fixnum
											 :adjustable t
											 :fill-pointer 0))
		(the-constants-output (make-array 32 :element-type t
											 :adjustable t
											 :fill-pointer 0))
		(label-addresses (make-hash-table :test #'string=))
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
				(opcode-arg-count (get-arg-count mnemonic)))
		   (cond
			 ;; If it is an argument-less instruction just emit it right away.
			 ((= opcode-arg-count 0)
			  (when arguments
				(warn
				 (format nil "Instruction ~A that is argument-less has arguments ~A"
						 mnemonic arguments)))
			  (emit-byte the-bytecode-output opcode-byte)
			  (incf virtual-prog-counter))
			 ;; If the argument length isn't equal to the arguments required by the
			 ;; instruction, error out.
			 ((/= opcode-arg-count (length arguments))
			  (error
			   (format nil "Instruction ~A requires specifically ~A arguments but got ~A"
					   mnemonic opcode-arg-count (length arguments))))
			 ;; instruction with matching argument count.
			 (t
			  (emit-byte the-bytecode-output opcode-byte)
			  (incf virtual-prog-counter)
			  ;; Process arguments.
			  (do* ((args-left arguments (rest arg-sizes-left))
					(arg-sizes-left (get-arg-sizes mnemonic) (rest arg-sizes-left))
					(arg (first args-left) (first args-left))
					(arg-size (first arg-sizes-left) (first arg-sizes-left)))
				   ((not args-left))
				;; Emit the argument bytes byte by byte.
				(dolist (byte (byte-transform arg arg-size))
				  (emit-byte the-bytecode-output byte)
				  (incf virtual-prog-counter)))))))
		;; Otherwise it is a label.
		((keywordp instruction)
		 (add-label instruction label-addresses virtual-prog-counter))))
	(vector the-bytecode-output the-constants-output label-addresses)))
