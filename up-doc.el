;;; up-doc.el --- Doctor for your use-package -*- lexical-binding: t -*-

;; Author: Josh Bax
;; Maintainer: Josh Bax
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.20.0"))
;; Homepage: https://github.com/joshbax189/up-doc-el
;; Keywords: lisp


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

(defgroup up-doc nil
  "Lint your `use-package' forms."
  :group 'use-package)

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
  "Convert :mode arguments FORM-LIST to `auto-mode-alist' format.

MODE-FN is the function to use if none is explicitly given.
It can be found by calling `use-package-as-mode'.

The result of this function will always be a list of forms."

  ;; form-list is either
  ;; 1. [ string | cons string sym ]
  ;; 2. [ 1. ]
  (if (seq-every-p
       (lambda (x)
         (or (stringp x)
             (and (consp x)
                  (stringp (car x))
                  (cdr x)
                  (symbolp (cdr x)))))
       form-list)
      (--map
       (if (stringp it) (cons it mode-fn) it)
       form-list)
    (if (eq 1 (proper-list-p form-list))
        (up-doc--normalize-mode-list (car form-list) mode-fn)
      (error "Bad format for :mode list %S" form-list))))

(defun up-doc--symbol-as-hook (symbol)
  "Extend SYMBOL with `use-package-hook-name-suffix' unless present."
  (if (or
       (not use-package-hook-name-suffix)
       (string-suffix-p use-package-hook-name-suffix (symbol-name symbol)))
      symbol
    (intern (concat (symbol-name symbol) use-package-hook-name-suffix))))

;; cf use-package-normalize/:hook
(defun up-doc--normalize-hook-list (form-list mode-fn)
  "Convert :hook arguments FORM-LIST to an alist of (hook . fn).

All hook symbols will have `use-package-hook-name-suffix' appended.

MODE-FN is the function to use if none is explicitly given.
It can be found by calling `use-package-as-mode'.

The result of this function will always be a list of forms."
  ;; either
  ;; 1. [ symbol | cons symbol (symbol|lambda) | [symbol] | cons (symbol+) (symbol|lambda) ]
  ;; 2. [ 1. ]

  ;; proper-list-p is nil for (cons X lambda)

  (if (seq-every-p
       (lambda (x)
         (or (symbolp x)
             (and (consp x)
                  (not (proper-list-p x)))
             (and (listp x)
                  (seq-every-p #'symbolp x))))
       form-list)
      (-mapcat
       (lambda (x)
         (cond
          ((symbolp x)
           (list (cons (up-doc--symbol-as-hook x) mode-fn)))
          ((and (consp x) (not (proper-list-p x)))
           (if (symbolp (car x))
               (list (cons (up-doc--symbol-as-hook (car x)) (cdr x)))
             (--map (cons (up-doc--symbol-as-hook it) (cdr x)) (car x))))
          ((listp x) (up-doc--normalize-hook-list x mode-fn))
          (t (error "Something went wrong processing %S" x))))
       form-list)
    (if (eq 1 (proper-list-p form-list))
        (up-doc--normalize-hook-list (car form-list) mode-fn)
      (error "Bad format for :hook list %S" form-list))))

(defun up-doc--rule-names ()
  "Rule names in `up-doc-rules'."
  (-uniq (map-keys up-doc-rules)))

(defun up-doc--rule-name-to-var (rule-name)
  "Get the global var name for RULE-NAME."
  (intern (concat "up-doc-rule--" (symbol-name rule-name))))

(defun up-doc--known-libraries ()
  "Get a list of loadable library names (strings)."
  (require 'find-func)
  (read-library-name--find-files load-path (find-library-suffixes)))

(defun up-doc--find-owning-package (symbol)
  "Try to find the package that defines SYMBOL.
Returns package name as a symbol or nil.

If SYMBOL is defined by a file which does not provide a feature,
returns nil."
  (if (autoloadp (symbol-function symbol))
      ;; then symbol-function is like '(autoload "package" <docstring>)
      (intern (cadr (symbol-function symbol)))
    ;; otherwise search load-history
    (if-let* ((provides-alist
               (cdr (-first (-lambda ((_package . provided))
                              (--some (if (symbolp it)
                                          (equal it symbol)
                                        (equal (cdr it) symbol))
                                     provided))
                            load-history))))
        (alist-get 'provide provides-alist)
      ;; symbol may be a regular function in a known library that is not yet loaded...
      ;; try prefix-matching known libraries
      (when-let* ((package-name
                   (--first
                    (string-prefix-p it (symbol-name symbol))
                    (up-doc--known-libraries))))
        (intern package-name)))))

(defmacro up-doc-rule (name docstring &rest body)
  "Declare a new linter rule.

Within BODY the symbol `package' is bound to a plist containing all of the
package's use-package keywords, as per `up-doc--form-to-plist'.
BODY should return either nil or a string which will be shown as a linter
suggestion.

This creates a new custom var with the name up-doc--<name> which, if nil, will
skip rule evaluation for all forms."
  (declare (doc-string 2) (indent 2))
  `(progn
     (defcustom ,(up-doc--rule-name-to-var name) t
       ,docstring :tag ,(format "Enable rule: %s" name) :type 'boolean :group 'up-doc)
     ;; TODO don't need this plist anymore now that docstring is merged
     (push '(,name . (:doc ,docstring :function (lambda (package) ,docstring ,@body)))
           up-doc-rules)))

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
    (when-let* ((defer-kw (seq-some (lambda (kw) (and (memq kw package) kw)) up-doc-defer-like)))
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
  (let (warnings hooks)
    (dolist (place up-doc-code-like)
      (let ((code-forms (plist-get package place)))
       (if (not (sequencep code-forms))
           (push (format "Expected contents of %s to be sexps, got %s"
                         place
                         code-forms)
                       warnings)
         (-each code-forms
           (-lambda ((form &as fn-head))
             (when (eq fn-head 'add-hook)
               (-let (((_ hook fn) form))
                 ;; hook is 'some-hook i.e. (quote some-hook), so use eval here
                 (push (cons (string-remove-suffix use-package-hook-name-suffix (symbol-name (eval hook))) fn) hooks))))))))
    (when hooks
      (push (format "Instead of calling add-hook, use\n  :hook\n%s"
                    (string-join (-map (-lambda ((a . b)) (format "  (%s . %S)" a b)) hooks) "\n"))
            warnings))
    warnings))

(up-doc-rule custom-replace-set
    "Suggest using :custom instead of setq, setq-default, or setopt."
  (let (warnings custom-forms)
    (dolist (place up-doc-code-like)
      (let ((code-forms (plist-get package place)))
        (if (not (sequencep code-forms))
            (push (format "Expected contents of %s to be sexps, got %s"
                         place
                         code-forms)
                       warnings)
          (-each code-forms
            (-lambda ((fn-head v exp))
              ;; TODO also consider (setq x v y v ...)?
              (when (and (memq fn-head '(setq setq-default set-default setopt))
                         (custom-variable-p v))
                (push (cons v exp) custom-forms)))))))
    (when custom-forms
     (push (format "Instead of setting these variables individually, use\n  :custom\n%s"
                   (string-join (-map (-lambda ((a . b)) (format "  (%s . %S)" a b)) custom-forms) "\n"))
           warnings))
    warnings))

(up-doc-rule custom-symbol-exists
    "Check that symbols are real variables."
  (when (featurep (plist-get package :package))
    (let ((warnings nil)
          (customs (plist-get package :custom)))
      (-each customs
        (-lambda ((v))
          (unless (boundp v)
            (push (format "variable %s is not yet defined" v) warnings))
          (when (get v 'byte-obsolete-variable)
            (push (format "variable %s is obsolete" v) warnings))))
      warnings)))

(defun up-doc--top-level-suggest (form)
  "Maybe suggest moving FORM into a use-package form.
Returns a possibly empty list of string warnings."
  ;; TODO provide a way to disable these warnings too: like for other rules, and inline in a file
  (pcase (car form)
    ('add-hook
     (-let [(_ (_ hook-name) (_ hook-fn)) form]
       (list
        (format "Move top-level add-hook into a use-package form\n  (use-package %s\n    :hook\n    (%s . %s))"
                (or (up-doc--find-owning-package hook-fn) "emacs")
                (string-remove-suffix use-package-hook-name-suffix (symbol-name hook-name))
                hook-fn))))
    ;; custom vars
    ((or 'setq 'setq-default 'setopt)
     (let* ((pairs (-partition 2 (cdr form)))
            (package (or (up-doc--find-owning-package (car (car pairs))) "'emacs")))
       ;; TODO this assumes all vars are the same package
       (list (format "Move top-level assignment into a use-package form\n  (use-package %s\n    :custom\n    %s)"
                     package
                     (string-join
                      (-map (-lambda ((var val))
                              (format "(%s . %s)" var val))
                            pairs)
                      "\n    ")))))
    ('add-to-list
     (-let [(_ var _elt) form]
       (list (format "Move top-level form into a use-package form\n  (use-package %s\n    :config\n    %s)"
                     form
                     (or (up-doc--find-owning-package var) "emacs")))))
    ('customize-set-variable
     (-let [(_ var elt) form]
       (list (format "Move top-level assignment into a use-package form\n  (use-package %s\n    :custom\n    (%s . %s))"
                     (or (up-doc--find-owning-package var) "emacs")
                     var
                     elt))))
    ('custom-set-variables
     (-map (-lambda ((var _val)) ;; TODO may be quoted
             (unless (memq var '(package-selected-packages))
               (format "Move top-level assignment into a use-package form\n  (use-package %s\n    :custom\n    (%s . %s))"
                       (or (up-doc--find-owning-package var) "emacs")
                       var)))
           (cdr form)))
    ;; binds
    ('global-set-key
     ;; (global-set-key (kbd "M-o") 'other-window)
     (-let [(_ (_ key-string) (_ key-fn)) form]
       (list (format "Move top-level binding into a use-package form\n  (use-package %s\n    :bind\n    (\"%s\" . %s))"
                     (or (up-doc--find-owning-package key-fn) "emacs")
                     key-string
                     key-fn))))
    ('keymap-global-set) ;; TODO
    ('require
     (-let [(_ (_ package)) form]
       (list (format "Move top-level require into a use-package form\n (use-package %s :demand)" package))))
    ('eval-after-load
        (-let [(_ feat) form]
          (unless (string-match-p ".+\\..+" feat)
            (list (format "Move code from eval-after-load into a use-package form\n  (use-package %s\n    :config\n%S)"
                          feat
                          form)))))
    ;; mode invocations and other autoloaded symbols
    (fn
     (when (up-doc--autoloadable-p fn)
       (list (format "Move top-level form into a use-package form\n  (use-package %s\n    :init\n    %s)"
                     (or (up-doc--find-owning-package fn) "emacs")
                     form))))))

;;;###autoload
(defun up-doc-lint (form)
  "Lint a use-package FORM for common issues."
  (interactive (let ((f (read (thing-at-point 'sexp))))
                 (if (not (eq 'use-package (car f)))
                     (user-error "Move point to the start of a use-package form.")
                   (list f))))
  (let ((package (up-doc--form-to-plist form))
        (warnings '())
        (rules (up-doc--rule-names)))

    ;; warn if not loaded
    (let ((package-name (plist-get package :package)))
      (unless (featurep package-name)
        (push (format "%s is not currently loaded, some warnings may not apply."
                      package-name)
              warnings)))

    (dolist (r rules)
      (condition-case err
          (when-let* ((_ (symbol-value (up-doc--rule-name-to-var r)))
                      (rule (alist-get r up-doc-rules))
                      (result (funcall (plist-get rule :function) package)))
            (if (listp result)
                (setq warnings (append result warnings))
              (push result warnings)))
        (error (message "Error in rule %s:\n  %s" (symbol-name r) err))))
    ;; print results
    (let ((result (-uniq (nreverse warnings))))
      (when (called-interactively-p 'any)
        (dolist (m result) (message "%s" m)))
      result)))

;;;###autoload
(defun up-doc-lint-buffer ()
  "Lint `use-package' forms in the current buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (filename (if filename (file-name-nondirectory (buffer-file-name)) "<no file>"))
         (up-doc-results (get-buffer-create (format "*up-doc results %s*" filename))))
    (with-current-buffer up-doc-results
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (save-excursion
      (goto-char (point-min))
      ;; for first one move across comments
      (forward-sexp)
      (while (< (point) (point-max))
        (when-let* ((current-form (sexp-at-point))
                    ;; this is at the end of the form!
                    (line (progn (backward-sexp) (line-number-at-pos))))
          (forward-sexp)
          ;; TODO store marker etc here
          (when-let* ((results (if (equal 'use-package (car current-form)) (up-doc-lint current-form) (up-doc--top-level-suggest current-form))))
            (with-current-buffer up-doc-results
              (let ((inhibit-read-only t))
                (insert (format "%s:%s: in %s:\n"
                                filename
                                line
                                (cadr current-form)))
                (dolist (m results)
                  (insert (format "- %s\n" m)))))))
        (forward-sexp)))
    (pop-to-buffer up-doc-results)
    (with-current-buffer up-doc-results (compilation-mode))))

(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist '(up-doc . ("\\([[:word:]]+.el\\):\\([[:digit:]]+\\):" 1 2 nil 1)))
(add-to-list 'compilation-error-regexp-alist 'up-doc)

;;;; Tools:

;;;###autoload
(defun up-doc-list-missing-modes ()
  "List unbound targets of auto mode regexps.
This can detect when use-package incorrectly guesses the mode name of a package."
  (interactive)
  (with-current-buffer (get-buffer-create "*Missing Modes*")
    (erase-buffer)
    (cl-prettyprint (-filter (-lambda ((_ . fn))
                               (and (symbolp fn) ;; this guards against entries like (regexp fn flag)
                                    (not (fboundp fn))))
                             auto-mode-alist))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun up-doc-remove-auto-mode (sym &optional other-alist)
  "Remove all entries for SYM from `auto-mode-alist'.

If OTHER-ALIST is a symbol, then remove SYM from there instead.
This can be used for example, with `magic-mode-alist':
  (up-doc-remove-auto-mode 'foo 'magic-mode-alist)
"
  (interactive "s")
  (when (stringp sym)
    (setq sym (intern sym)))
  (let ((the-list (or other-alist 'auto-mode-alist)))
    (set the-list (rassq-delete-all sym auto-mode-alist))))

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

;;;###autoload
(defun up-doc-cleanup (form)
  "Remove additional configuration that `use-package' FORM may have added."
  (interactive (let ((f (sexp-at-point)))
                 (if (not (eq 'use-package (car f)))
                     (user-error "Move point to the start of a use-package form.")
                   (list f))))
  (let* ((form-plist (up-doc--form-to-plist form))
         (package (plist-get form-plist :package)))
    (message "removing package %s" package)
    (with-demoted-errors "up-doc %s"
      (unload-feature package t))

    ;; remove mode list entries
    (dolist (type '(:mode :magic :magic-fallback :interpreter))
      (when-let* ((modes (plist-get form-plist type))
                  (modes (up-doc--normalize-mode-list modes (use-package-as-mode package)))
                  ;; Also check package-name, e.g. (use-package foo :mode "foo") may
                  ;; use either 'foo or 'foo-mode
                  (mode-syms (-uniq (cons package (map-values modes)))))
        (dolist (m mode-syms)
          (message "removing %s binding for %s" type m)
          (pcase type
            (:mode (up-doc-remove-auto-mode m))
            (:magic (up-doc-remove-auto-mode m 'magic-mode-alist))
            (:magic-fallback (up-doc-remove-auto-mode m 'magic-fallback-mode-alist))
            (:interpreter (up-doc-remove-auto-mode m 'interpreter-mode-alist))))))

    ;; load-path
    (when-let* ((paths (plist-get form-plist :load-path)))
      (dolist (path paths)
        (message "removing %s from load-path" path)
        (setq load-path (delete path load-path))))

    ;; autoloads
    (let ((autoload-sym (intern (concat (symbol-name package) "-autoloads"))))
      (message "removing %s" autoload-sym)
      (with-demoted-errors "up-doc: %s"
        (unload-feature autoload-sym t)))

    ;; hooks
    (when-let* ((hooks (plist-get form-plist :hook))
                (hooks (up-doc--normalize-hook-list hooks (use-package-as-mode package))))
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

;;;###autoload
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

(defun up-doc-package-info (package)
  "Provide an overview of the load status of PACKAGE."
  (interactive "s") ;; TODO complete with package names
  (let* ((package-sym (intern package))
         (autoloads (concat package "-autoloads"))
         (location (locate-library package))
         (is-loaded (featurep package-sym))
         (autoload-objs (cdr (assoc-string (locate-library autoloads) load-history)))
         (autoload-defuns (--mapcat (when (and (listp it) (eq (car it) 'defun)) (list (cdr it))) autoload-objs))
         ;; currently loaded packages which require this package
         (reverse-deps (-filter (-lambda ((file . loads))
                                  (-find (lambda (x)
                                           (and (listp x)
                                                (eq (car x) 'require)
                                                (equal (cdr x) package-sym)))
                                         loads))
                                load-history))
         (reverse-deps (-map (lambda (x) (alist-get 'provide (cdr x))) reverse-deps)))
    (with-current-buffer (get-buffer-create (format "*up-doc package info for %s*" package))
      (erase-buffer)
      (insert (format "location: %s\n" location))
      (insert (format "loaded: %s\n" is-loaded))
      (insert "autoloads:\n")
      (--each (sort autoload-defuns) (insert (format " - %s\n" it)))
      (insert "reverse deps:\n")
      (--each (sort reverse-deps) (insert (format " - %s\n" it)))
      (switch-to-buffer (current-buffer)))))

(defun up-doc-describe-rule (rule)
  "Display the documentation for RULE."
  (interactive (list (intern (completing-read "rule:" (map-keys up-doc-rules)))))

  ;; We save describe-function-orig-buffer on the help xref stack, so
  ;; it is restored by the back/forward buttons.  'help-buffer'
  ;; expects (current-buffer) to be a help buffer when processing
  ;; those buttons, so we can't change the current buffer before
  ;; calling that.
  (let ((describe-function-orig-buffer
         (or describe-function-orig-buffer
             (current-buffer)))
        (help-buffer-under-preparation t)
        (function (plist-get (alist-get rule up-doc-rules) :function)))

    (help-setup-xref (list #'describe-function--helper
                           rule describe-function-orig-buffer)
                     (called-interactively-p 'interactive))

    (save-excursion
      (with-help-window (help-buffer)
        (prin1 rule)
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ " is ")
        (describe-function-1 function)
        (with-current-buffer standard-output
          (help-fns--setup-xref-backend)
          (buffer-string))))))

(provide 'up-doc)

;;; up-doc.el ends here
