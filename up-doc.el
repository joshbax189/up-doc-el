;;; up-doc.el --- Doctor for your use-package -*- lexical-binding: t -*-

;; Author: Josh Bax
;; Maintainer: Josh Bax
;; Version: 0.1.0
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
(require 'subr-x)

(defconst up-doc-code-like '(:preface :init :config)
  "The bodies of these keywords are evaluated like code.")

(defconst up-doc-defer-like '(:commands :bind :bind* :mode :magic :hook :magic-fallback :interpreter)
  "These keywords imply :defer.")

(defvar up-doc-rules nil
  "Defined linter rules.")

(defvar up-doc-origin-buffer nil
  "Buffer that was linted producing current result set.")
(make-variable-buffer-local 'up-doc-origin-buffer)

(defgroup up-doc nil
  "Lint your `use-package' forms."
  :group 'use-package)

(defcustom up-doc-load-before-check nil "Load packages before applying linter rules."
  :group 'up-doc
  :type 'boolean)

(defun up-doc--atom-like-p (sexp)
  "Whether SEXP satisfies `atom' or is a quoted symbol.
This is required because the read representation of a quoted symbol
is not an atom, but the printed representation does not allow
`down-list', so behaves like an atom."
  (or (atom sexp)
      (and (not (atom (cdr sexp))) ;; a cons cell cannot be (quote symbol)
           (not (null (cadr sexp)))
           (symbolp (cadr sexp))
           (memq (car sexp) '(quote function)))))

(defun up-doc--location-tree-at-point ()
  "Produce location tree for the sexp following point.
The location tree has nodes (location . children) where each location is the
start of a sexp."
  (unless (equal major-mode #'emacs-lisp-mode)
    (warn "Function up-doc--location-tree-at-point requires emacs-lisp-mode to be active"))
  (save-excursion
    (let ((root (point))
          children)
      (if (up-doc--atom-like-p (sexp-at-point))
          (cons root nil)
        (down-list)              ;; must be at the start of sexp
        (ignore-error scan-error ;; note that scan error will break the while loop
          (while t
            (forward-sexp) ;; leaves point at end of sexp
            (backward-sexp)
            (push (up-doc--location-tree-at-point) children)
            (forward-sexp)))
        (cons root (nreverse children))))))

(defun up-doc--sexp-diff (old new &optional prefix-stack)
  "Report the changes between OLD NEW as a list of (path . change).

Path may be
- (), if the whole of OLD was changed
- (i . path), if the change to OLD is in the ith sub-expression (0-based)

Note that paths are always relative to the structure of OLD.

Change may be
- :removed, if the sub-expression identified by path was removed
- (:added . sexps), if the sub-expression was replaced by sexps inline
  similar to ,@
- a sexp, if if the sub-expression was replaced

PREFIX-STACK accumulates the current path in reverse order."
  (cond
   ((equal old new) nil)
   ((or (atom old) (atom new))
    (list (cons (reverse prefix-stack) new)))
   ;; literal cons cells
   ((-cons-pair-p old)
    (up-doc--sexp-diff (list (car old) '\. (cdr old)) new prefix-stack))
   ((-cons-pair-p new)
    (up-doc--sexp-diff old (list (car new) '\. (cdr new)) prefix-stack))
   (t
    (cl-loop for i from 0 to (1- (length old))
             for e1 = (nth i old)
             ;; mark difference between removed and literal nil
             for e2 = (if (length< new (1+ i))
                          :removed
                        (if (and (length= old (1+ i)) (length> new (length old)))
                            (cons :added (nthcdr i new))
                          (nth i new)))
             ;; filter nil
             for change = (if (eq :added (car-safe e2))
                              ;; (:added . sexps) must not be treated as a real expression here
                              (list (cons (reverse (cons i prefix-stack)) e2))
                            (up-doc--sexp-diff e1 e2 (cons i prefix-stack)))
             when change append change))))

(defun up-doc--location-tree-elt (loc-tree path)
  "Get a location from LOC-TREE following PATH.
Returns nil if PATH does not exist."
  (dolist (n path)
    (setq loc-tree (nth n (cdr loc-tree))))
  (car-safe loc-tree))

(defun up-doc--apply-sexp-diff (diff &optional loc-tree)
  "Modify the sexp following point with the changes in DIFF.
DIFF is produced by `up-doc--sexp-diff'.
LOC-TREE is for the sexp at point."
  (unless loc-tree (setq loc-tree (up-doc--location-tree-at-point)))
  (save-excursion
    ;; apply changes in reverse order to preserve locations
    (dolist (change (sort diff :reverse t))
      (let* ((path (car change))
             (new-sexp (cdr change))
             (target (up-doc--location-tree-elt loc-tree path))
             last-sexp)
        (when target
          (goto-char target)
          (cond
           ((eq :removed new-sexp)
            (kill-sexp))
           ((eq :added (car-safe new-sexp))
            ;; first sexp after :added may be equal or different
            (if (equal (sexp-at-point) (cadr new-sexp))
                (forward-sexp) ;; TODO what if a comment follows this?
              (kill-sexp)
              (princ (cadr new-sexp) (current-buffer)))
            (setq last-sexp (cadr new-sexp))
            ;; remaining members of new-sexp are always added
            (dolist (sexp (cddr new-sexp))
              (if (eq last-sexp '\.) (insert " ") (newline-and-indent))
              (princ sexp (current-buffer))
              (setq last-sexp sexp)))
           (t
            (kill-sexp)
            (princ new-sexp (current-buffer)))))))))

(defun up-doc--diff-with-sexp-at-point (new-version)
  "Assume NEW-VERSION is a modification of sexp at point.
Produce diff as a result of applying source-level changes to match NEW-VERSION."
  (save-excursion
    (let* ((sexp (sexp-at-point))
           (start (point))
           (source-file (file-name-nondirectory (buffer-file-name)))
           (end (progn
                  ;; point may be at start or end of sexp
                  (if (looking-at-p "(") (forward-sexp) (backward-sexp))
                  (point)))
           (sexp-text (buffer-substring start end)))

      (with-current-buffer (get-buffer-create "*modified*")
        (erase-buffer)
        (emacs-lisp-mode)
        (insert sexp-text)
        (goto-char (point-min))
        (let ((tree (up-doc--location-tree-at-point))
              (changes (up-doc--sexp-diff sexp new-version)))
          (up-doc--apply-sexp-diff changes tree))
        (goto-char (point-max))
        (newline))
      (with-current-buffer (get-buffer-create "*orig*")
        (erase-buffer)
        (emacs-lisp-mode)
        (insert sexp-text)
        (newline))
      (diff-buffers "*orig*" "*modified*" "-u" t)
      (with-current-buffer "*Diff*"
        (let* ((diff-start (goto-line 2))
               (diff-end (progn (goto-char (point-max)) (forward-line -2) (point)))
               (diff-text (buffer-substring diff-start diff-end))
               ;; this enables diff-apply
               (diff-text (string-replace "#<buffer *modified*>" source-file diff-text)))
          (kill-buffer)
          (kill-buffer "*orig*")
          (kill-buffer "*modified*")
          diff-text)))))

(defun up-doc--diff-inline-transform (keyword)
  "Assume NEW-VERSION is a modification of sexp at point.
Produce diff as a result of applying source-level changes to match NEW-VERSION."
  (save-excursion
    (let* ((start (point))
           (source-file (file-name-nondirectory (buffer-file-name)))
           (end (progn
                  ;; point may be at start or end of sexp
                  (if (looking-at-p "(") (forward-sexp) (backward-sexp))
                  (point)))
           (sexp-text (buffer-substring start end))
           start-block end-block)

      (with-current-buffer (get-buffer-create "*modified*")
        (erase-buffer)
        (emacs-lisp-mode)
        (insert sexp-text)
        (goto-char (point-min))
        ;; modification goes here >>
        ;; go to keyword -- leaves point at end of match
        (search-forward (if (symbolp keyword) (symbol-name keyword) keyword))
        ;; go to start of next sexp
        (forward-sexp)
        (setq end-block (point))
        (backward-sexp)
        ;; save point
        (setq start-block (point))
        ;; go to end of sexp
        (forward-sexp)
        ;; delete )
        (delete-char -1)
        ;; back to start
        (goto-char start-block)
        ;; delete (
        (delete-char 1)
        ;; indent region -- conservative otherwise indentation can change for whole form
        (indent-region start-block end-block)
        ;; << end of modification
        (goto-char (point-max))
        (newline))
      (with-current-buffer (get-buffer-create "*orig*")
        (erase-buffer)
        (emacs-lisp-mode)
        (insert sexp-text)
        (newline))
      (diff-buffers "*orig*" "*modified*" "-u" t)
      (with-current-buffer "*Diff*"
        (let* ((diff-start (goto-line 2))
               (diff-end (progn (goto-char (point-max)) (forward-line -2) (point)))
               (diff-text (buffer-substring diff-start diff-end))
               ;; this enables diff-apply
               (diff-text (string-replace "#<buffer *modified*>" source-file diff-text)))
          (kill-buffer)
          (kill-buffer "*orig*")
          (kill-buffer "*modified*")
          diff-text)))))

(defun up-doc--form-to-plist (form)
  "Convert a `use-package' FORM to a plist indexed by `use-package-keywords'.
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

;;;; Modification of Source-Forms:
;; a source form is a non-normalized use-package form, i.e. the result of sexp-at-point
(defun up-doc--delete-keyword (form keyword)
  "Delete KEYWORD and contents from `use-package' FORM."
  (let (res
        do-remove)
    (dolist (exp form)
      (if (equal exp keyword)
          (setq do-remove t)
        (if do-remove
            (when (keywordp exp)
              (setq do-remove nil)
              (push exp res))
          (push exp res))))
    (nreverse res)))

(defun up-doc--delete-form (parent-form keyword form)
  "Delete FORM from `use-package' PARENT-FORM within KEYWORD block."
  (if-let* ((keyword-idx (-elem-index keyword parent-form)))
      (let* ((split-list (-split-at keyword-idx parent-form))
             ;; everything up to keyword
             (res-prefix (car split-list))
             (suffix-no-keyword (cdr (cadr split-list)))
             ;; car = list of forms in keyword block
             ;; cadr = from next keyword to end
             (block-and-suffix (-split-with (-not #'keywordp) suffix-no-keyword))
             (block-forms (car block-and-suffix))
             (res-suffix (cadr block-and-suffix)))
        (append res-prefix
                ;; block-forms is always a list
                (if (length= block-forms 1)
                    ;; when there is a single form, either it is equal or it is a nested list
                    ;; when equal - delete both form and keyword
                    (unless (equal (car-safe block-forms) form)
                      ;; otherwise remove inside nested list - this also flattens the list
                      (list keyword (if (proper-list-p (car block-forms)) (-remove-item form (car block-forms)) (car block-forms))))
                  ;; otherwise block is a list of forms
                  (cons keyword (-remove-item form block-forms)))
                res-suffix))
    ;; no keyword match
    parent-form))

(defun up-doc--insert-form (parent-form keyword &rest forms)
  "Insert FORMS into `use-package' PARENT-FORM within KEYWORD block.
Note that it does no uniqueness checking."
  (if (null forms)
      parent-form
    (if-let* ((keyword-idx (-elem-index keyword parent-form)))
        (let* ((split-list (-split-at keyword-idx parent-form))
               ;; everything up to keyword
               (res-prefix (car split-list))
               (suffix-no-keyword (cdr (cadr split-list)))
               ;; car = list of forms in keyword block
               ;; cadr = from next keyword to end
               (block-and-suffix (-split-with (-not #'keywordp) suffix-no-keyword))
               (block-forms (car block-and-suffix))
               (res-suffix (cadr block-and-suffix)))
          (append res-prefix
                  ;; block-forms is always a list
                  (if (length= block-forms 1)
                      ;; when there is a single form it may be a nested list
                      (if (proper-list-p (car block-forms))
                          (list keyword (append (car block-forms) forms))
                        (cons keyword (cons (car block-forms) forms)))
                    ;; otherwise block is a list of forms
                    (cons keyword (append block-forms forms)))
                  res-suffix))
      ;; no keyword match
      (append parent-form (cons keyword forms)))))

(defun up-doc--rule-names ()
  "Rule names in `up-doc-rules'."
  (-uniq (map-keys up-doc-rules)))

;; because it's used in up-doc-rule macro
(eval-and-compile
  (defun up-doc--rule-name-to-var (rule-name)
    "Get the global var name for RULE-NAME."
    (intern (concat "up-doc-rule--" (symbol-name rule-name)))))

(defun up-doc--enabled-rule-names (&optional mask)
  "Rule names in `up-doc-rules'.
MASK is a plist with keys :enabled and :disabled.
Each should have a list of rule names to either enable or
disable."
  (let* ((global (map-keys up-doc-rules))
         (enabled (--filter (or (memq it global)
                                (prog1 nil (warn "%s is not a known up-doc rule" it)))
                            (plist-get mask :enabled)))
         (disabled (--filter (or (memq it global)
                                (prog1 nil (warn "%s is not a known up-doc rule" it)))
                            (plist-get mask :disabled)))
         ;; filter globally disabled
         (result (--filter (symbol-value (up-doc--rule-name-to-var it)) global)))
    (when mask
      (setq result (append enabled result)
            result (--filter (not (memq it disabled)) result)))
    (-uniq result)))

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
                                        (and (not (equal 'require (car it)))
                                             (equal (cdr it) symbol)))
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

(defun up-doc--parse-linter-settings (settings)
  "Parse SETTINGS into a plist with :enable :disable keys."
  (when settings
    (let (res)
      (dolist (p (string-split settings " "))
        (pcase (aref p 0)
          (?+ (push (intern (substring p 1)) (plist-get res :enabled)))
          (?- (push (intern (substring p 1)) (plist-get res :disabled)))
          (_ (warn "Unrecognized up-doc setting %s" p))))
      res)))

(defun up-doc--get-linter-comment ()
  "Return any linter rule comment before the current sexp.
Format is as follows:
  ;;up-doc [[+-][rule-name] ]+
This must be on a single line."
  (save-excursion
    ;; ensure point is at start of sexp
    (unless (looking-at-p "(")
      (backward-sexp))
    ;; then look back from the start of sexp
    (when (looking-back ";;[[:space:]]*up-doc[[:space:]]+\\(.*\\)\n?[[:space:]]*" (line-beginning-position 0))
      (up-doc--parse-linter-settings (match-string-no-properties 1)))))

(defmacro up-doc-rule (name docstring &rest body)
  "Declare NAME as a new linter rule.

Within BODY the symbol `package' is bound to a plist containing all of the
package's `use-package' keywords, as per `up-doc--form-to-plist'.
BODY should return either nil or a string which will be shown as a linter
suggestion.

DOCSTRING is required and should give examples of situations where the rule
applies.

This creates a new custom var with the name up-doc--<name> which, if nil, will
skip rule evaluation for all forms."
  (declare (doc-string 2) (indent 2))
  `(progn
     (defcustom ,(up-doc--rule-name-to-var name) t
       ,docstring :tag ,(format "Enable rule: %s" name) :type 'boolean :group 'up-doc)
     ;; keeping the plist format since there may be more metadata later
     (push '(,name . (:doc ,docstring :function (lambda (package &optional marker) ,docstring ,@body)))
           up-doc-rules)))

(defun up-doc--format-diff (marker modification-fn)
  "Evaluate MODIFICATION-FN at MARKER and diff results.
Return a string or null.
MODIFICATION-FN should take a sexp and return a modified copy."
  (when-let* ((_ marker)
              (diff-text (save-excursion
                           (goto-char marker)
                           (up-doc--diff-with-sexp-at-point (funcall modification-fn (sexp-at-point))))))
    (concat "\n" diff-text)))

;;;; Rules:
(up-doc-rule ensure-redundant-with-global
    "Keyword :ensure has no effect if it matches `use-package-always-ensure'.

Bad example:
  (customize-set-value use-package-always-ensure t)
  (use-package foo
     :ensure)
"
  (let ((form-value (plist-get package :ensure)))
    (when (and (equal use-package-always-ensure form-value)
               (plist-member package :ensure))
      (concat (format ":ensure %s is redundant when use-package-always-ensure is %s." form-value use-package-always-ensure)
              (when marker (up-doc--format-diff marker (lambda (f) (up-doc--delete-keyword f :ensure))))))))

(up-doc-rule demand-redundant-with-global
    "Setting :demand t has no effect if `use-package-always-demand' is also t.

Bad example:
  (customize-set-value use-package-always-demand t)
  (use-package foo
     :demand)"
  (when (and use-package-always-demand
             (plist-get package :demand))
    (concat ":demand t is redundant when use-package-always-demand is non-nil."
            (when marker (up-doc--format-diff marker (lambda (f) (up-doc--delete-keyword f :demand)))))))

(up-doc-rule defer-implied-by-others
    "Keyword :defer is implied by many other keywords.

Bad example
  (use-package foo
     :defer t
     :hook
     (prog-mode . foo-mode)) ;; this makes foo deferred
"
  (when (equal (plist-get package :defer) t)
    (when-let* ((defer-kw (seq-some (lambda (kw) (and (memq kw package) kw)) up-doc-defer-like)))
      (concat (format ":defer t can be removed since %s implies deferred loading" defer-kw)
              (when marker (up-doc--format-diff marker (lambda (f) (up-doc--delete-keyword f :defer))))))))

(up-doc-rule inline-nested-forms
    "Arguments to keywords are assumed to be a list of cons cells or forms.
This list does not need to be explicitly written.

Bad example
  (use-package foo
    :hook
    ((x-hook . fn)
     (y-hook . fn)))

Good example
  (use-package foo
    :hook
    (x-hook . fn)
    (y-hook . fn))
"
  (let (warnings)
   (dolist (keyword '(:hook :custom :mode :bind)) ;; TODO any other keywords possible?
    (-when-let* ((forms (plist-get package keyword))
                 (_ (eq 1 (proper-list-p forms))) ;; nil if a dotted cons
                 (inner-list (car forms))
                 (_ (proper-list-p inner-list))
                 (_ (not (or (symbolp (car inner-list))
                             (stringp (car inner-list))))) ;; e.g. ("C-x" . some-fn)
                 )
      (push (concat
             (format "consider inlining contents of %s keyword to reduce nesting" keyword)
             (when marker
               (concat "\n" (save-excursion
                              (goto-char marker)
                              (up-doc--diff-inline-transform keyword)))))
            warnings)))
   warnings))

(up-doc-rule hook-warn-lambdas
    "Hooks should be named functions rather than anonymous lambdas.
This makes it easier to modify or delete the function later.
See Info node `(emacs) Hooks'.

New defuns can be added to either :init, :config or at the top-level
of the init file depending on whether the defun requires the package
to be loaded or not.

Bad example:
  (use-package foo
     :hook
     (foo-mode-hook . (lambda () (message \"foo-mode enabled\"))))

Good example:
  (use-package foo
     :hook
     (foo-mode-hook . my-foo-notification)
     :init
     (defun my-foo-notification ()
       (message \"foo-mode enabled\")))
"
  ;; hook can be
  ;; 1. a symbol -- skip
  ;; 2. a cons, looks like '((x-hook . fn))
  ;; 3. a list of cons, looks like '((x-hook . fn) ...)
  ;; 4. a list of symbols '((x-hook y-hook z-hook))
  (let ((hooks (plist-get package :hook))
        (bad-hooks nil))
    (dolist (hook hooks)
      (-when-let* (((_ . fn) hook)
                   (_ (listp fn)))
        (when (eq (car fn) 'lambda)
          (push hook bad-hooks))))
    (when bad-hooks
      (concat (format "hooks for symbols %s should use defuns instead of lambdas" (map-keys bad-hooks))
              (up-doc--format-diff marker
                             (lambda (f)
                               (let ((res f))
                                (cl-flet ((hook-name-suggest (sym)
                                            (intern (concat (symbol-name (plist-get package :package)) "--" (symbol-name sym) "-handler"))))
                                 (dolist (hook-bind bad-hooks)
                                   (let ((hook-sym (car hook-bind))
                                         (lambda-form (cdr hook-bind)))
                                     (setq res (up-doc--delete-form res :hook hook-bind)
                                           res (up-doc--insert-form res :config `(defun ,(hook-name-suggest hook-sym) ,@(cdr lambda-form)))
                                           res (up-doc--insert-form res :hook (cons hook-sym (hook-name-suggest hook-sym)))))))
                                res)))))))

(up-doc-rule hook-warn-double-hook
    "Symbols in :hook argument should not have suffix -hook.

Bad Example
  (use-package foo
    :hook
    prog-mode-hook ;; expands to (add-hook prog-mode-hook-hook foo-mode-hook)
    (prog-mode-hook . foo-mode)) ;; same as above

Good Example
  (use-package foo
    :hook
    prog-mode
    (prog-mode . foo-mode)) ;; same effect
"
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
      (concat (format "hooks %s should not end in default suffix %s" bad-hooks use-package-hook-name-suffix)
              ;; :hooks = (symbol | (cons (symbol | list symbol) symbol))*
              (up-doc--format-diff marker (lambda (sexp)
                                      (cl-flet ((rename-hook (x) (if (memq x bad-hooks) (intern (string-remove-suffix use-package-hook-name-suffix (symbol-name x))) x)))
                                       (-tree-map (lambda (sym-or-cons)
                                                    (cl-typecase sym-or-cons
                                                      (symbol (rename-hook sym-or-cons))
                                                      ;; tree-map does not descend into cons pairs
                                                      (cons (cons (-tree-map #'rename-hook (car sym-or-cons)) (cdr sym-or-cons)))
                                                      (t sym-or-cons)))
                                                  sexp))))))))

(up-doc-rule add-hook-instead-of-hook
    "Suggest using :hook instead of add-hook.
Keeping similar logic together eases maintenance."
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
               ;; (add-hook 'var function)
               (-let (((_ (_ hook) (fn-head . fn-rest)) form))
                 ;; format ((keyword . add-hook-form) . (hook-name . fn-symbol-or-lambda))
                 (push (cons (cons place form)
                             ;; (cons hook-name function)
                             (cons (intern (string-remove-suffix use-package-hook-name-suffix (symbol-name hook)))
                                   ;; unquote function if symbol
                                   (if (memq fn-head '(quote function))
                                       (car fn-rest)
                                     (cons fn-head fn-rest))))
                       hooks))))))))
    (when hooks
      (push (concat "Instead of calling add-hook, use :hook"
                    (up-doc--format-diff marker
                                   (lambda (f) (let ((res f))
                                                 ;; move each hook
                                                 (dolist (h hooks)
                                                   (-let ((((keyword . old-form) . new-form) h))
                                                     (setq res (up-doc--insert-form (up-doc--delete-form res keyword old-form) :hook new-form))))
                                                 res))))
            warnings))
    warnings))

(up-doc-rule custom-replace-set
    "Suggest using :custom instead of `setq', `setq-default', or `setopt'.
Using the :custom keyword allows disabling related settings together and
allows recording reasons alongside the assignments."
  (let (warnings custom-forms)
    (dolist (place up-doc-code-like)
      (let ((code-forms (plist-get package place)))
        (if (not (sequencep code-forms))
            (push (format "Expected contents of %s to be sexps, got %s"
                          place
                          code-forms)
                  warnings)
          (-each code-forms
            (-lambda ((form &as fn-head v exp . rest))
              (when (and (memq fn-head '(setq setq-default set-default setopt))
                         (custom-variable-p v))
                ;; format is ((keyword . old-form) . (list (var . exp)))
                (let ((origin (cons place form))
                      (binds (list (cons v exp))))
                  ;; for (setq x v y v ...)
                  (when rest
                    (dolist (pair (seq-split rest 2))
                      (push (cons (car pair) (cadr pair)) binds)))
                  (push (cons origin (nreverse binds)) custom-forms))))))))
    (when custom-forms
      (push (concat "Instead of setting these variables individually, use :custom"
                    (up-doc--format-diff marker
                                   (lambda (f)
                                     (let ((res f))
                                       ;; move each one
                                       (dolist (pair custom-forms)
                                         (-let ((((keyword . old-form) . binds) pair))
                                           ;; one origin form
                                           (setq res (up-doc--delete-form res keyword old-form))
                                           ;; multiple resulting binds
                                           (setq res (apply #'up-doc--insert-form res :custom binds))))
                                       res))))
            warnings))
    warnings))

(up-doc-rule custom-symbol-exists
    "Check that symbols are real variables and not obsolete.
If the package is not loaded, this may give false positives."
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
  "Maybe suggest moving FORM into a `use-package' form.
Returns a possibly empty list of string warnings."
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
            (package (or (up-doc--find-owning-package (car (car pairs))) "emacs")))
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
     (-let [(_ (_ var) elt) form]
       (list (format "Move top-level assignment into a use-package form\n  (use-package %s\n    :custom\n    (%s . %s))"
                     (or (up-doc--find-owning-package var) "emacs")
                     var
                     elt))))
    ('custom-set-variables ;; &rest '(SYMBOL VAL ...)
     (-map (-lambda ((_quote (var val)))
             (unless (memq var '(package-selected-packages))
               (format "Move top-level assignment into a use-package form\n  (use-package %s\n    :custom\n    (%s . %s))"
                       (or (up-doc--find-owning-package var) "emacs")
                       var
                       val)))
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
          (unless (and (stringp feat) (string-match-p ".+\\..+" feat)) ;; a relative file name
            (list (format "Move code from eval-after-load into a use-package form\n  (use-package %s\n    :config\n%S)"
                          feat
                          form)))))
    ;; mode invocations and other autoloaded symbols
    (fn
     (when (up-doc--autoloadable-p fn)
       (list (format "Move top-level form into a use-package form\n  (use-package %s\n    :init\n    %s)"
                     (or (up-doc--find-owning-package fn) "emacs")
                     form))))))

;; see usage in up-doc-lint -- name should match
(up-doc-rule top-level-suggest
    "Suggest moving form into a use-package form."
  (up-doc--top-level-suggest (save-excursion (goto-char marker) (sexp-at-point))))

(defun up-doc--get-region-comments ()
  "Scan buffer for linter comments that apply to regions.
Result is a list of forms (START-POS END-POS SETTING) ordered by START-POS."
  (let (result
        settings-stack
        ;; where the tip of settings-stack was opened
        from)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^;;[[:space:]]*\\(end\\|begin\\)_up-doc[[:space:]]*\\(.*\\)\n?" nil t)
        (pcase (match-string 1)
          ("begin"
           ;; close old stack
           (when settings-stack
             (push (list from (match-beginning 0) (-reduce #'up-doc--merge-settings settings-stack)) result))
           (push (up-doc--parse-linter-settings (match-string-no-properties 2)) settings-stack)
           (setq from (match-end 0)))
          ("end"
           (when settings-stack
             (push (list from (match-beginning 0) (-reduce #'up-doc--merge-settings settings-stack)) result)
             (pop settings-stack))
           (setq from (match-end 0)))
          (_ ()))))
    ;; if there are still open ranges
    (when settings-stack
      (push (list from (point-max) (-reduce #'up-doc--merge-settings settings-stack)) result))
    (nreverse result)))

(defun up-doc--merge-settings (form-settings region-settings)
  "Merge REGION-SETTINGS into FORM-SETTINGS overriding region where needed."
  (if (not region-settings)
      form-settings
    (let ((local-enabled (plist-get form-settings :enabled))
          (local-disabled (plist-get form-settings :disabled)))
      ;; local overrides region
      ;; enabled in local => removed from disabled
      `(:enabled ,(append local-enabled
                          (-difference (plist-get region-settings :enabled) local-disabled))
        ;; disabled in local => removed from enabled
        :disabled ,(append local-disabled
                           (-difference (plist-get region-settings :disabled) local-enabled))))))

(defun up-doc--get-settings-at-pos (range-settings position)
  "Lookup POSITION in RANGE-SETTINGS and merge settings with any form ones.
POSITION is a marker or buffer position.
RANGE-SETTINGS is produced by `up-doc--get-region-comments'."
  (when (markerp position) (setq position (marker-position position)))
  (let ((form-settings (save-excursion
                         (goto-char position)
                         (up-doc--get-linter-comment)))
        (region-settings (when range-settings
                           (-some (-lambda ((start end setting))
                                    (when (and (<= start position) (<= position end))
                                      setting))
                                  range-settings))))
    (up-doc--merge-settings form-settings region-settings)))

(defun up-doc-check-settings-at-point (pos)
  "Display enabled rules at POS (default point)."
  (interactive "d")
  (message "%S" (up-doc--enabled-rule-names (up-doc--get-settings-at-pos (up-doc--get-region-comments) pos))))

;;;###autoload
(defun up-doc-lint (form &optional rule-mask marker)
  "Lint a `use-package' FORM using `up-doc-rules'.
When called interactively, lint the form at point.
RULE-MASK is output from `up-doc--get-settings-at-pos'.
MARKER should be at the start of the FORM."
  (interactive (let ((f (read (thing-at-point 'sexp))))
                 (list f nil (point-marker))))
  (let ((rules (up-doc--enabled-rule-names rule-mask))
         (package (when (equal 'use-package (car form))
                   (up-doc--form-to-plist form)))
         warnings)

    (if (not package)
        ;; top-level-suggest is the only rule to apply to general forms
        (setq rules (when (memq 'top-level-suggest rules) '(top-level-suggest)))
      ;; but it never applies to use-package forms
      (setq rules (-difference rules '(top-level-suggest)))
      ;; warn if not loaded
      (let ((package-name (plist-get package :package)))
        (unless (featurep package-name)
          (if up-doc-load-before-check
              (condition-case err
                  (load-library (symbol-name package-name))
                (error
                 (message "up-doc: failed loading %s got error %S" package-name err)
                 (push (format "%s failed to load." package-name)
                       warnings)))
            (push (format "%s is not currently loaded, some warnings may not apply."
                          package-name)
                  warnings)))))

    (dolist (r rules)
      (condition-case err
          (when-let* ((rule (alist-get r up-doc-rules))
                      (result (funcall (plist-get rule :function) package marker)))
            (if (listp result)
                (setq warnings (append (--map (concat it "\n  rule:" (symbol-name r)) result) warnings))
              (push (concat result "\n  rule:" (symbol-name r)) warnings)))
        ((error debug) (message "Error in rule %s:\n  %s" (symbol-name r) err))))

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
         (up-doc-results (get-buffer-create (format "*up-doc results %s*" filename)))
         (origin-buffer (current-buffer))
         range-settings)
    (with-current-buffer up-doc-results
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (save-excursion
      (setq range-settings (up-doc--get-region-comments))
      (goto-char (point-min))
      ;; for first one move across comments
      (forward-sexp)
      (while (< (point) (point-max))
        (when-let* ((marker (point-marker))
                    (current-form (sexp-at-point))
                    ;; this is at the end of the form!
                    (line (progn (backward-sexp) (line-number-at-pos))))
          (forward-sexp)
          (when-let* ((results (up-doc-lint current-form (up-doc--get-settings-at-pos range-settings (point)) marker)))
            (with-current-buffer up-doc-results
              (let ((inhibit-read-only t))
                (insert (format "%s:%s: in %s:\n"
                                filename
                                line
                                (cadr current-form)))
                (dolist (m results)
                  (insert (format "∘ %s\n" m)))))))
        (forward-sexp)))
    (pop-to-buffer up-doc-results)
    (with-current-buffer up-doc-results
      (compilation-mode)
      (up-doc-results-mode)
      (setq-local up-doc-origin-buffer origin-buffer)
      (font-lock-fontify-buffer))))

(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist '(up-doc . ("\\([[:word:]]+.el\\):\\([[:digit:]]+\\):" 1 2 nil 1)))
(add-to-list 'compilation-error-regexp-alist 'up-doc)

;;;; Tools:

;;;###autoload
(defun up-doc-list-missing-modes ()
  "List unbound targets of auto mode regexps.
This can detect when `use-package' incorrectly guesses the mode name of a
package."
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
  (up-doc-remove-auto-mode \\='foo \\='magic-mode-alist)"
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
                     (user-error "Move point to the start of a use-package form")
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
    (when (yes-or-no-p (format "Remove %s from hook %s?" fn hook))
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
         (reverse-deps (-filter (-lambda ((_file . loads))
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

(defun up-doc-repeat ()
  "Re-run previous lint."
  (interactive)
  (unless up-doc-origin-buffer
    (user-error "Could not determine target buffer for linting"))
  (with-current-buffer up-doc-origin-buffer
    (up-doc-lint-buffer))
  (font-lock-fontify-buffer))

(define-minor-mode up-doc-results-mode
  "Minor mode for viewing up-doc reports in compilation buffer."
  :lighter " up-doc results"
  :keymap '(("g" . up-doc-repeat)
            ("a" . diff-apply-hunk))
  :group 'up-doc
  (require 'diff-mode)
  (if up-doc-results-mode
      (progn
        ;; enable diff highlighting
        (font-lock-add-keywords nil diff-font-lock-keywords)))
  ;; TODO Set eldoc help function
  )

(provide 'up-doc)

;;; up-doc.el ends here
