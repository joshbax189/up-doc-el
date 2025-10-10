;;; up-doc.el --- Doctor for your use-package -*- lexical-binding: t -*-

;; Author: Josh Bax
;; Maintainer: Josh Bax
;; Version: 0.0.1
;; Package-Requires: ((emacs . "30.1"))
;; Homepage:
;; Keywords:


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; What's that? It's a linter for use-package forms, plus other tools to
;; improve your init files.

;;; Code:

(require 'use-package)
(require 'dash)
(require 'map)
(require 'cl-extra)

(defconst up-doc-code-like '(:preface :init :config)
  "The bodies of these keywords are evaluated like code.")

(defconst up-doc-defer-like '(:commands :bind :bind* :mode :magic :hook :magic-fallback :interpreter)
  "These keywords imply :defer.")

(defvar up-doc-rules nil
  "Defined linter rules.")

(defun up-doc--form-to-plist (form)
  "Convert a use-package FORM to a plist indexed by `use-package-keywords'.
The package name is available using the special keyword :package."
  (let* ((package (nth 1 form))
         (body (cddr form))
         (plist (list :package package))
         (current-keyword nil)
         (current-value nil))
    (dolist (item body)
      (if (memq item use-package-keywords)
          ;; it is a keyword
          (progn
            ;; push old one to plist
            (when current-keyword
              (setq plist (append plist (list current-keyword current-value))))
            (setq current-keyword item
                  current-value t))
        ;; not a keyword
        (unless current-keyword
          (error "Unexpected item outside of keyword: %s" item))
        (if (eq current-value t)
            (cond
             ((eq item t) nil)
             ((null item)
              (setq current-value nil))
             (t
              (setq current-value (list item))))
          (setq current-value (append current-value (list item))))))
    ;; final iteration
    (when current-keyword
      (setq plist (append plist (list current-keyword current-value))))
    plist))

(defun up-doc--normalize-mode-list (form-list mode-fn)
  "Convert :merge arguments FORM-LIST to `auto-mode-alist' format.
The result will always be a list of forms."
  ;; mode-fn can be found with use-package-as-mode

  (cond
   ;; nil
   ((null form-list)
    nil)
   ;; ("a" ...)
   ((stringp (car form-list))
    (-map (lambda (x) (cons x mode-fn)) form-list))
   ((eq 1 (length form-list))
    (let ((inner (car form-list)))
      (cond
       ;; a single cons cell
       ;; (("a" . b))
       ((and (consp inner)
             (or (symbolp (cdr inner))
                 (symbolp (cadr inner))))
        form-list)
       ;; a list of strings
       ;; (("a" "b" ...))
       ((seq-every-p #'stringp inner)
        (-map (lambda (x) (cons x mode-fn)) inner))
       ;; assume a double nested list
       (t
        (up-doc--normalize-mode-list (car form-list) mode-fn)))))
   ;; a list with multiple non-string elements
   (t
    ;; assume its the correct form?
    ;; (("a" . fn) ("b" . fn))
    form-list)))

;; cf use-package-normalize/:hook
;; TODO this does not change the names of the fns to -hook
;; it probably should, e.g. (use-package blah :hook a b c) uses a-hook etc
(defun up-doc--normalize-hook-list (form-list mode-fn)
  "Convert :hook arguments FORM-LIST to ???.
The result will always be a list of forms."
  ;; mode-fn can be found with use-package-as-mode
  (cond
   ;; nil
   ((null form-list)
    nil)
   ;; (a ...)
   ((seq-every-p #'symbolp form-list)
    (--map (cons it mode-fn) form-list))
   ((eq 1 (proper-list-p form-list))
    (let ((inner (car form-list)))
      (cond
       ;; a single cons cell
       ;; ((a . b)) or (((a...) . b))
       ((and (consp inner)
             (cdr inner)
             (symbolp (cdr inner)))
        (if (proper-list-p (car inner))
            (--map (cons it (cdr inner)) (car inner))
          form-list))
       ;; a list of symbols
       ;; ((a b ...))
       ;; or a double nested list
       (t
        (up-doc--normalize-hook-list (car form-list) mode-fn)))))
   ;; a list with multiple non-symbol elements
   (t
    ;; ((a . b)) or (((a...) . b))
    (-mapcat (-lambda ((targets . fn))
               (if (listp targets)
                   (--map (cons it fn) targets)
                 (list (cons targets fn))))
             form-list))))

(defun up-doc--rule-names ()
  "Rule names in `up-doc-rules'."
  (-uniq (map-keys up-doc-rules)))

(defmacro up-doc-rule (name docstring &rest body)
  "Declare a new linter rule.
Within BODY the symbol `package' is bound to a plist containing all of the
package's use-package keywords, as per `up-doc--form-to-plist'.
BODY should return either nil or a string which will be shown as a linter
suggestion."
  (declare (doc-string 2) (indent 2))
  `(push '(,name . (:doc ,docstring :function (lambda (package) ,@body)))
         up-doc-rules))

;;;; Rules:
(up-doc-rule ensure-redundant-with-global
    "Keyword :ensure has no effect if it matches the value of `use-package-always-ensure'."
    ;; if quelpa or straight is present, how does that effect things?
    ;; I assume straight-ensure => always ensure, so do the same?
  (let ((form-value (plist-get package :ensure)))
    (when (and (equal use-package-always-ensure form-value)
               (plist-member package :ensure))
      (format ":ensure %s is redundant when use-package-always-ensure is %s." form-value use-package-always-ensure))))

(up-doc-rule demand-redundant-with-global
    ":demand t has no effect if `use-package-always-demand' is also t."
  (when (and use-package-always-demand
             (plist-get package :demand))
    ":demand t is redundant when use-package-always-demand is non-nil."))

(up-doc-rule defer-implied-by-others
    ":defer t is implied by many other keywords."
  (when (equal (plist-get package :defer) t)
    (when-let* ((defer-kw (seq-some (lambda (kw) (and (memq kw body) kw)) upd-defer-like)))
      (format ":defer t can be removed since %s implies deferred loading" defer-kw))))

(up-doc-rule hook-inline-nested
    "Complain if :hook argument is of the form '((x-hook . fn) (y-hook . fn) ...)."
  (-when-let* ((hooks (plist-get package :hook))
               (_ (eq 1 (proper-list-p hooks))) ;; nil if a dotted cons
               (_ (proper-list-p (car hooks)))
               (_ (consp (car (car hooks)))) ;; should be a dotted cons too
               ((((a . b))) hooks)
               (_ (symbolp b)))
    (format "consider inlining contents of :hook keyword to reduce nesting")))

(up-doc-rule hook-warn-lambdas
    "Complain if :hook takes a lambda."
  ;; hook can be
  ;; 1. a symbol -- skip
  ;; 2. a cons, looks like '((x-hook . fn))
  ;; 3. a list of cons, looks like '((x-hook . fn) ...)
  ;; 4. a list of symbols '((x-hook y-hook z-hook))
  (let ((hooks (plist-get package :hook))
        (bad-hooks nil))
    (dolist (hook hooks)
      (-when-let* (((hook-sym . fn) hook)
                   (_ (listp fn))
                   ((fn-head) fn))
        (when (eq fn-head 'lambda)
          (push hook-sym bad-hooks))))
    (when bad-hooks
      (format "hooks for symbols %s should use defuns instead of lambdas" bad-hooks))))

(up-doc-rule hook-warn-double-hook
    "Complain if a :hook symbol has suffix -hook."
  ;; hook can be
  ;; 1. a symbol
  ;; 2. a list of cons, looks like '((x-hook . fn) ...)
  ;; 3. a list of symbols '((x-hook y-hook z-hook))
  ;; 4. a cons with a list of symbols '(((x-hook y-hook z-hook) . fn) ...)
  (let* ((hooks (plist-get package :hook))
         ;; attempt to normalize
         (hooks (if (not (-cons-pair-p (car hooks))) (-flatten-n 1 hooks) hooks))
         (bad-hooks nil))
    (dolist (hook hooks)
      (let ((hook-sym (cond ((symbolp hook) hook)
                            ((-cons-pair-p hook) (car hook)))))
       (--each (if (listp hook-sym) hook-sym (list hook-sym))
         (when (string-suffix-p use-package-hook-name-suffix (symbol-name it))
           (push it bad-hooks)))))
    (when bad-hooks
      (format "hooks %s should not end in default suffix %s" bad-hooks use-package-hook-name-suffix))))

(up-doc-rule add-hook-instead-of-hook
    "Suggest using :hook instead of add-hook."
  (let (warnings)
    (dolist (place up-doc-code-like)
      (-each (plist-get package place)
        (-lambda ((form &as fn-head))
          (when (eq fn-head 'add-hook)
            (-let (((_ hook fn) form))
              (push (format "Consider using :hook (%s . %s) instead of %s"
                            ;; hook is 'some-hook i.e. (quote some-hook), so use eval here
                            (string-remove-suffix use-package-hook-name-suffix (symbol-name (eval hook)))
                            fn
                            form)
                    warnings))))))
    warnings))

(up-doc-rule custom-replace-set
    "Suggest using :custom instead of setq, setq-default, or setopt."
  (let (warnings)
    (dolist (place up-doc-code-like)
      (-each (plist-get package place)
        (-lambda ((form &as fn-head v exp))
          ;; TODO also consider (setq x v y v ...)?
          (when (and (memq fn-head '(setq setq-default set-default setopt))
                     (custom-variable-p v))
            (push (format "Consider using :custom (%s . %s) instead of %s for custom variable"
                          v
                          exp
                          form)
                  warnings)))))
    warnings))

(defun up-doc-lint (form)
  "Lint a use-package FORM for common issues."
  (interactive (let ((f (read (thing-at-point 'sexp))))
                 (if (not (eq 'use-package (car f)))
                     (user-error "Move point to the start of a use-package form.")
                   (list f))))
  (let ((package (up-doc--form-to-plist form))
        (warnings '())
        (rules (up-doc--rule-names)))

    (dolist (r rules)
      (condition-case err
          (when-let* ((rule (alist-get r up-doc-rules))
                      (result (funcall (plist-get rule :function) package)))
            (if (listp result)
                (setq warnings (append result warnings))
              (push result warnings)))
        (error (message "Error in rule %s:\n  %s" (symbol-name r) err))))
    ;; print results
    (let ((result (nreverse warnings)))
      (when (called-interactively-p 'any)
        (dolist (m result) (message "%s" m)))
      result)))

(defun up-doc-lint-buffer ()
  "Lint `use-package' forms in the current buffer."
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (up-doc-results (get-buffer-create (format "*up-doc results %s*" filename))))
   (save-excursion
     (goto-char (point-min))
     ;; for first one move across comments
     (forward-sexp)
     (while (< (point) (point-max))
       (when-let* ((current-form (sexp-at-point))
                   (_ (equal 'use-package (car current-form)))
                   ;; TODO for some reason this is at the end of the form!
                   (line (progn (backward-sexp) (line-number-at-pos))))
         (forward-sexp)
         ;; TODO store marker etc here
         (when-let* ((results (up-doc-lint current-form)))
           (with-current-buffer up-doc-results
             (insert (format "%s:%s: in %s:\n"
                             filename
                             line
                             (cadr current-form)))
             (dolist (m results)
               (insert (format "- %s\n" m))))))
       (forward-sexp)))
   (pop-to-buffer up-doc-results)
   (with-current-buffer up-doc-results (compilation-mode))))

(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist '(up-doc . ("\\([[:word:]]+.el\\):\\([[:digit:]]+\\):" 1 2 nil 1)))
(add-to-list 'compilation-error-regexp-alist 'up-doc)

;;;; Tools:

(defun up-doc-list-missing-modes ()
  "List unbound targets of auto mode regexps.
This can detect cases where use-package incorrectly guesses the mode name of a package."
  (interactive)
  (with-current-buffer (get-buffer-create "*Missing Modes*")
    (erase-buffer)
    (cl-prettyprint (-filter (-lambda ((_ . fn))
                               (and (symbolp fn) ;; this guards against entries like (regexp fn flag)
                                    (not (fboundp fn))))
                             auto-mode-alist))
    (pop-to-buffer (current-buffer))))

(defun up-doc-remove-auto-mode (sym)
  "Remove all entries for SYM from `auto-mode-alist'."
  (interactive "s")
  (when (stringp sym)
    (setq sym (intern sym)))
  (setq auto-mode-alist (rassq-delete-all sym auto-mode-alist)))

(defun up-doc--autoloadable-p (symbol)
  "Whether SYMBOL can be or was once autoloaded."
  ;; There are some built in modes like text-mode, lisp-mode, fundamental-mode
  ;; that are just loaded, not autoloaded.
  (or
   ;; non-loaded functions
   ;; TODO these may not be valid however
   (autoloadp (symbol-function symbol))
   ;; functions loaded by autoload
   (seq-some 'autoloadp (function-get symbol 'function-history))))

(defun up-doc-cleanup (form)
  "Remove additional configuration that `use-package' FORM may have added."
  (interactive (let ((f (read (thing-at-point 'sexp))))
                 (if (not (eq 'use-package (car f)))
                     (user-error "Move point to the start of a use-package form.")
                   (list f))))
  (let* ((form-plist (up-doc--form-to-plist form))
         (package (plist-get form-plist :package)))
    (message "removing package %s" package)
    (with-demoted-errors "up-doc %s"
      (unload-feature package))

    ;; TODO check :interpreter format and effect
    ;; :mode :magic :magic-fallback :interpreter
    (dolist (type '(:mode :magic :magic-fallback :interpreter))
      (when-let* ((modes (plist-get form-plist type))
                  (modes (up-doc--normalize-mode-list modes (use-package-as-mode package)))
                  (mode-syms (-uniq (map-values modes))))
        (dolist (m mode-syms)
          (message "removing %s binding for %s" type m)
          (up-doc-remove-auto-mode m))))

    ;; load-path
    (when-let* ((path (plist-get form-plist :load-path)))
      (message "removing %s from load-path" path)
      (setq load-path (delete path load-path)))

    ;; autoloads
    (let ((autoload-sym (intern (concat (symbol-name package) "-autoloads"))))
      (message "removing %s" autoload-sym)
      (with-demoted-errors "up-doc: %s"
        (unload-feature autoload-sym)))

    ;; hooks
    (when-let* ((hooks (plist-get form-plist :hooks))
                (hooks (up-doc--normalize-hook-list hooks (use-package-as-mode package))))
      ;; TODO check if normalisation if done
      (dolist (hcons hooks)
        (message "remove function %s from hook %s" (cdr hcons) (car hcons))
        (with-demoted-errors "up-doc: %s"
          (remove-hook (car hcons) (cdr hcons)))))

    ;; custom
    (when-let* ((customs (plist-get form-plist :custom)))
      (dolist (ccons customs)
        (when-let* ((custom (car ccons))
                    (_ (boundp custom)))
          (message "reset custom variable %s" custom)
          ;; reset the value by removing it from the "use-package" theme
          (custom-theme-reset-variables 'use-package '(custom))
          (custom-theme-recalc-variable custom))))

    ;; TODO
    ;; bindings

    ;; eval-after-loads
    ;; note these can miss entries which are added under other package names
    ;; but it is likely these are created by the package itself, not use-package
    (message "removing entries from after-load-alist")
    (let ((package-file (concat (symbol-name package) ".el")))
     (setq after-load-alist
           (seq-remove (-lambda ((re . _))
                         (cond
                          ;; remove (regex . some-fn) where regex matches package.el, package.elc etc
                          ((stringp re) (string-match-p re package-file))
                          ;; remove entries like (package . some-fn)
                          ((symbolp re) (equal package re))
                          (t nil)))
                       after-load-alist)))))

(defun up-doc-remove-hook-at-point ()
  "Remove a hook specified in a `use-package' :hook block."
  (interactive)
  (when-let* ((hook-sexp (sexp-at-point))
              (hook (symbol-name (car hook-sexp)))
              (hook (if (string-suffix-p use-package-hook-name-suffix hook)
                        hook
                      (concat hook use-package-hook-name-suffix)))
              (fn (cdr hook-sexp)))
    (unless (boundp (intern hook))
      (user-error "Not a hook: %s" hook))
    (when (yes-or-no-p (format "Remove %s from hook %s" fn hook))
      ;; this fails silently
      (remove-hook hook fn))))

(provide 'up-doc)

;;; up-doc.el ends here
