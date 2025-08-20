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
      (-let* (((hook-sym . fn) hook)
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
  (let (places)
    (dolist (place up-doc-code-like)
      (when (seq-some (-lambda ((fn-head)) (eq fn-head 'add-hook))
                      (plist-get package place))
        (push place places)))
    (when places
      (format "Consider using :hook instead of add-hook within %s" places))))

(up-doc-rule custom-replace-set-default
    "Suggest using :custom instead of setq-default or set-default."
  (let (places)
    (dolist (place up-doc-code-like)
      (when (seq-some (-lambda ((fn-head)) (or (eq fn-head 'setq-default) (eq fn-head 'set-default)))
                      (plist-get package place))
        (push place places)))
    (when places
      (format "Consider using :custom instead of setq-default or set-default within %s" places))))

(up-doc-rule custom-replace-setq
    "Suggest using :custom instead of setq."
  (let (places)
    (dolist (place up-doc-code-like)
      ;; TODO also report the variable
      (when (seq-some (-lambda ((fn-head v)) (and (eq fn-head 'setq)
                                                  (custom-variable-p v)))
                      (plist-get package place))
        (push place places)))
    (when places
      (format "Consider using :custom instead of setq for custom variables within %s" places))))

(up-doc-rule custom-replace-setopt
    "Suggest using :custom instead of setf."
  (let (places)
    (dolist (place up-doc-code-like)
      (when (seq-some (-lambda ((fn-head)) (eq fn-head 'setopt))
                      (plist-get package place))
        (push place places)))
    (when places
      (format "Consider using :custom instead of setopt within %s" places))))

(defun up-doc-lint (form)
  "Lint a use-package FORM for common issues."
  (let ((package (up-doc--form-to-plist form))
        (warnings '())
        (rules (up-doc--rule-names)))

    (dolist (r rules)
      (with-demoted-errors "rule error: %s"
        (when-let* ((rule (alist-get r up-doc-rules))
                    (result (funcall (plist-get rule :function) package)))
          (push result warnings))))
    (nreverse warnings)))

(provide 'up-doc)

;;; up-doc.el ends here
