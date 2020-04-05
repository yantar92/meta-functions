;; [[id:0fe0bca3-fb51-4e0c-8b35-79a5e92975d6][Modal setup:2]]
;;; meta-functions.el --- Define meta-functions to act differently depending on major mode -*- lexical-binding: t; -*-

;; Version: 2.0
;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Created: 10 March 2018

;;; Commentary:

;; Quick implementation of meta-functions, which allows to run multiple
;; functions, which do similar logical operations by one single "meta"
;; function in different major modes. For example, one may want to define
;; meta-next-line to call `next-line' normally, but
;; `org-agenda-next-line' in org-agenda mode. Both can be binded to, say
;; "M-j", without a need to change the key-bindings on both fundamental
;; and org-agenda modes.
;; Example usage:
;; (use-package meta-functions
;;   :config
;; (meta-defun meta-next-element ()
;;   "Go to next element."
;;   :mode org-agenda-mode (org-agenda-next-item 1) ; call org-agenda-next-item in org-agenda
;;   :cond (lambda () (and (eq major-mode 'org-mode) (org-at-heading-p))) (org-next-visible-heading 1) ; call org-next-visible-headting when at heading in org-mode
;;   (next-line)) ; call-next line in any other case
;; )
;; or the same can be written as
;; (meta-defun meta-next-element "Go to next element." next-line)
;; (meta-defun meta-next-element :mode org-agenda-mode (org-agenda-next-item 1))
;; (meta-defun meta-next-element :mode org-mode :cond org-at-heading-p (org-next-visible-heading 1))
;;
;; Similar packages:
;; https://gitlab.com/jjzmajic/handle implements similar functionality to unify functions across major modes
;; However, handle is very major-mode centric. meta-functions is more function-centric
;; Also, meta-functions is not only limited to current major mode defining which function to call.

;;; Code:
(require 'seq)
(require 'dash)

(defun meta-functions--process-args-1 (args &optional recursive)
  "Return list of ((sub-body-symbol condition-func|mode-symbol number-elements-in-args) ...) in front of ARGS."
  (pcase args
    (`(:mode ,(and (pred symbolp) s) . ,_)
     (cons
      (list (make-symbol (concat "%" (symbol-name s)))
	    `(quote ,s)
            2)
      (meta-functions--process-args-1 (cddr args) 'recursive)))
    (`(:cond :symbol ,(and (pred symbolp) s) ,(and (pred #'functionp) func) . ,_)
     (cons
      (list (make-symbol (concat "%" (symbol-name s)))
	    `(function ,func)
            4)
      (meta-functions--process-args-1 (cddddr args) 'recursive)))
    (`(:cond :symbol . ,_)
     (error ":cond :symbol must be followed by symbol and function."))
    (`(:cond ,(and (pred functionp) func) . ,_)
     (cons
      (list (when (symbolp func) (make-symbol (concat "%" (symbol-name func))))
	    `(function ,func)
            2)
      (meta-functions--process-args-1 (cddr args) 'recursive)))
    (`(:cond ,form . ,_)
     (cons
      (list (make-symbol (concat "%lambda" (org-id-uuid)))
	    `(function (lambda () ,form))
	    2)
      (meta-functions--process-args-1 (cddr args) 'recursive)))
    (_
     (unless recursive
       (list (list
	      '%t
              `(function t)
              0))))))

(defun meta-functions--process-args (name args)
  "Process arguments of `meta-defun'.
The return value is a list (arglist docstring ((sub-body-symbol condition-func|mode-symbol body) ...))."
  (when (memq ':override-nil args) (setq args (delq ':override-nil args)))
  (let* ((arglist (and (listp (car args)) (pop args)))
	 (docstring (and (stringp (car args)) (pop args)))
	 conditions)
    (while args
      (add-to-list 'conditions
		   (cl-loop for (symbol subcondition num-remove) in (meta-functions--process-args-1 args)
			    with condition = nil
			    with sumsymbol = nil
			    do (when symbol
				 (if sumsymbol
				     (setq sumsymbol
					   (make-symbol (concat (symbol-name sumsymbol)
								"+"
								(symbol-name symbol))))
				   (setq sumsymbol
					 (make-symbol (concat (symbol-name name)
							      (symbol-name symbol))))))
			    do (dotimes (_ num-remove) (pop args))
			    do (push subcondition condition)
			    finally return (cons sumsymbol
						 (nreverse (push (pcase (cl-loop for el = (pop args)
										 collect el
										 until (or (seq-empty-p args) (meta-functions--process-args-1 args 'recursive)))
								   (`(,(and (pred functionp) func)) `(function ,func))
								   (`(,form) `(lambda () (interactive) ,form)))
								 condition))))
		   'append))
    (list arglist docstring conditions)))


(defun meta-functions--generate-docstring (name default-docstring conditions)
  "Generate a docstring for a function define using meta-defun."
  (->>
   (concat "[Meta function.]\n"
	   (when default-docstring default-docstring)
	   "\n"
	   "By default, run `"
	   (format "%s" (cadr (alist-get (make-symbol (concat (symbol-name name) "%t")) conditions nil nil (lambda (a b) (string= (symbol-name a) (symbol-name b))))))
	   "'\n"
	   (mapconcat (lambda (condition)
			(let* ((rcondition (seq-reverse (cdr condition)))
			       (body (car rcondition))
                               (subconditions (seq-reverse (cdr rcondition))))
                          (unless (equal subconditions '(#'t))
			    (concat "When "
				    (mapconcat (lambda (el)
						 (pcase el
						   ('#'t nil)
						   (`(function ,_)
						    (concat "`" (format "%s" el) "' returns 't"))
						   (_
						    (concat "major mode is `" (format "%s" (eval el)) "'"))))
					       subconditions
					       " and ")
				    ", run `"
				    (format "%s" body)
				    "'"))))
		      conditions
		      "\n"))
   (replace-regexp-in-string "#'" "")))

(cl-defmacro meta-defun (name &rest args)
  "Define a meta-function or update its definition.

USAGE:
(meta-defun foo [(arglist)]
[:override-nil]
[docstring]
[:mode bar-mode body-sexp]
[:mode ...]
[:cond [:symbol zen-func-symbol] zen-func body-sexp]
[:cond ...]
[default-body])

If a command foo has not yet been defined using meta-defun, the above
defines a command foo, which can call different BODY (which can be a
function or sexp) depending on major mode or :cond functions. For any
major mode, the BODY defined in the corresponding [:mode mode body]
definition is called. If none of the define mode conditions are
satisfied, the FUNCs in [:cond func body] are called sequentially
without argument until one of the return 't. Then, the corresponding
BODY is called. If none of the above conditions can be met,
DEFAULT-BODY is called.

If a command foo has been already defined with meta-defun, the
existing definition will be updated. All the omitted definitions will
be preserved (including docstring and default-body) unless
:override-nil keyword is provided. If :override-nil keyword is
present, the command foo will be redefined.

In addition, every BODY definition in :mode definition is bound to foo%bar-mode symbol.
This symbol can be used in the following [:mode ...] and [:cond ...] definitions.
In the [:cond ...] definition, it [:symbol zen-fun-symbol] is present, the BODY
is bound to foo%zen-fun-symbol.
Otherwise, it is bound to foo%zen-fun is zen a symbol.

\(fn NAME [(ARGLIST)] [DOCSTRING] ARGS...)"
  (declare (indent defun))
  `(pcase-let ((`(,arglist ,default-docstring ,conditions) (meta-functions--process-args ',name ',args))
	       (old-conditions (function-get ',name 'meta-functions-cond-plist))
               (old-default-docstring (function-get ',name 'meta-functions-default-docstring)))
     (unless (memq :override-nil ',args)
       (mapc (lambda (el)
	       (setf (alist-get (car el) old-conditions nil nil (lambda (a b) (string= (symbol-name a) (symbol-name b)))) (cdr el)))
	     (reverse conditions))
       (setq conditions old-conditions)
       (unless default-docstring (setq default-docstring old-default-docstring)))
     (mapc (lambda (el)
	     (let ((main-name  ',name)
		   (sub-name (car el))
		   (body (car (reverse el))))
	       (when sub-name
		 (eval `(defun ,sub-name ()
			  (concat "Sub-definition in meta-function `" (symbol-name ',main-name) "'.")
			  ,body)))))
	   conditions) ; define the sub-bodies
     (let ((docstring (meta-functions--generate-docstring ',name default-docstring conditions)))
       (eval `(defun ,',name ,arglist
		,docstring
                (interactive)
		(seq-some (lambda (condition)
			    (let* ((rcondition (seq-reverse (cdr condition)))
				   (body (car rcondition))
                                   (subconditions (seq-reverse (cdr rcondition))))
                              (when (seq-reduce (lambda (prev el)
						  (and prev
                                                       (pcase el
							 ('#'t t)
							 (`(function ,_) (eval `(funcall-interactively ,el)))
							 (_ (eq major-mode (eval el))))))
						subconditions
						t)
				(eval `(call-interactively ,body))
                                t)))
			  ',conditions)))) ; define meta-function
     (function-put ',name 'meta-functions-cond-plist conditions)
     (function-put ',name 'meta-functions-default-docstring default-docstring)))

(defun meta-defun-mapc (list)
  "Defun multiple meta-functions with `meta-defun' trating each element of LIST as argument."
  (mapc (lambda (body) (eval `(meta-defun ,@body))) list))

(provide 'meta-functions)

;;; meta-functions.el ends here
;; Modal setup:2 ends here
