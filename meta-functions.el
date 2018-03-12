  ;;; meta-functions.el --- Define meta-functions to act differently depending on major mode -*- lexical-binding: t; -*-

;; Version: 1.0
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
;;   :init
;;   (setq meta-funcions-list '((meta-function-1 default-function-1 "Description") (meta-function-2 default-function-2  "description")))
;;   (setq meta-functions-meta-function-1-alist '((major-mode-1 . function-1)))
;;   )
;; This will define meta-function-1 and meta-function-2 to automatically
;; choose the right real functions depending on the major mode.

  ;;; Code:

(defvar meta-functions-list '()
  "A list of meta function names, corresponding functions to be called by default, and their description.
    Each function may have meta-functions-metafunctionname-alist variable containing pairs of major mode names
    and corresponding function to be called by meta-function in that major mode.
    The default functions will be called if the major mode is not in the alist")

(defun meta-functions-add-meta-function (&rest args)
  "Add meta function or a list of metafunctions as in `meta-functions-list' and update the definitions."
  (setq meta-functions-list (append meta-functions-list args))
  (meta-functions-update-function-definitions))

(defun meta-functions-update-function-definitions ()
  "Update definitions of all the meta-functions according the present value of `meta-functions-list'."
  (mapc (lambda (elem)
	  (let* ((meta-func (car elem))
		 (default-func (cadr elem))
		 (meta-func-description (or (car (nthcdr 2 elem))
					    ""))
		 (func-alist (read (concat "meta-functions-" (format "%s" `,meta-func) "-alist"))))
	    (eval `(defvar ,func-alist '()
		     ,(concat "List of functions to be called by `"
			      (format "%s" meta-func)
			      "'.\nIt is an alist containing cons of major mode name and function name to be called.")))
	    (eval `(defun ,meta-func (&optional args)
		     ,(concat "Meta function."
			      (if (not (seq-empty-p meta-func-description))
				  (concat " The description is: \""
					  meta-func-description
					  "\"."))
			      "\nMeta function is calling `"
			      (format "%s" default-func)
			      "' by default."
			      (and (not (seq-empty-p (eval func-alist)))
				   (concat "\nCalling other functions in various major modes (major-mode . functions):\n"
					   (mapconcat (lambda(elem) (message "%s . `%s'" (car elem) (cdr elem))) (eval func-alist) "\n")
					   )))
		     (interactive)
		     (let ((real-func
			    (or (alist-get major-mode ,func-alist ',default-func)
				',default-func)))
		       (when (fboundp real-func)
			 (call-interactively real-func nil args)))))))
	meta-functions-list))

(meta-functions-update-function-definitions)

(provide 'meta-functions)

  ;;; meta-functions.el ends here
