;;; up-doc.test..el -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'up-doc)
(require 'el-mock)

(ert-deftest up-doc--form-to-plist/test-no-arg ()
  "Keywords with no argument should default to t."
  ;; when followed by another keyword
  (should (equal (up-doc--form-to-plist '(use-package foo :demand :ensure t))
                 '(:package foo :demand t :ensure t)))
  ;; when at the end of the form
  (should (equal (up-doc--form-to-plist '(use-package foo :demand))
                 '(:package foo :demand t))))

(ert-deftest up-doc--form-to-plist/test-nil ()
  "Keywords with explicit nil should be nil."
  (should (equal (up-doc--form-to-plist '(use-package foo :demand nil :ensure t))
                 '(:package foo :demand nil :ensure t))))

(ert-deftest up-doc--form-to-plist/test-multiple-forms ()
  "Keywords with multiple forms should gather those in a list."
  (should (equal (up-doc--form-to-plist '(use-package foo :init (setq foo-1 1) (setq foo-2 2) (message "hi")))
                 '(:package foo :init ((setq foo-1 1) (setq foo-2 2) (message "hi"))))))

(ert-deftest up-doc--form-to-plist/test-single-form ()
  "Keywords with a single form or atom should wrap that in a list."
  (should (equal (up-doc--form-to-plist '(use-package graphql-mode :mode ("\\.gql\\'" "\\.graphql\\'" )))
                 '(:package graphql-mode :mode (("\\.gql\\'" "\\.graphql\\'" )))))
  (should (equal (up-doc--form-to-plist '(use-package graphql-mode :mode "\\.gql\\'"))
                 '(:package graphql-mode :mode ("\\.gql\\'")))))

(ert-deftest up-doc--normalize-mode-list/test ()
  "Tests all cases."
  ;; nil
  (should (equal (up-doc--normalize-mode-list '() 'foo-mode)
                 '()))
  ;; single string
  (should (equal (up-doc--normalize-mode-list '("a") 'foo-mode)
                 '(("a" . foo-mode))))
  ;; list of strings
  (should (equal (up-doc--normalize-mode-list '("a" "b" "c") 'foo-mode)
                 '(("a" . foo-mode)
                   ("b" . foo-mode)
                   ("c" . foo-mode))))
  ;; nested list of strings
  (should (equal (up-doc--normalize-mode-list '(("a" "b" "c")) 'foo-mode)
                 '(("a" . foo-mode)
                   ("b" . foo-mode)
                   ("c" . foo-mode))))
  ;; single cons
  (should (equal (up-doc--normalize-mode-list '(("a" . foo-mode)) 'foo-mode)
                 '(("a" . foo-mode))))
  ;; list of cons
  (should (equal (up-doc--normalize-mode-list '(("a" . foo-mode) ("b" . foo-mode)) 'foo-mode)
                 '(("a" . foo-mode)
                   ("b" . foo-mode))))
  ;; double nested cons
  (should (equal (up-doc--normalize-mode-list '((("a" . foo-mode) ("b" . foo-mode))) 'foo-mode)
                 '(("a" . foo-mode)
                   ("b" . foo-mode))))
  ;; although these are valid auto-mode-alist formats, they can't be parsed by use-package
  ;; ;; extended format single
  ;; (should (equal (up-doc--normalize-mode-list '(("a" foo-mode t)) 'foo-mode)
  ;;                '(("a" foo-mode t))))
  ;; ;; extended format double nested
  ;; (should (equal (up-doc--normalize-mode-list '((("a" foo-mode t) ("b" foo-mode t))) 'foo-mode)
  ;;                '(("a" foo-mode t)
  ;;                  ("b" foo-mode t))))
  )

(ert-deftest up-doc--normalize-mode-list/test-mixed ()
  "Tests cases with both string and alist."
  (should (equal (up-doc--normalize-mode-list '("a" ("b" . bar-mode)) 'foo-mode)
                 '(("a" . foo-mode)
                   ("b" . bar-mode))))
  (should (equal (up-doc--normalize-mode-list '(("a" ("b" . bar-mode))) 'foo-mode)
                 '(("a" . foo-mode)
                   ("b" . bar-mode))))
  (should (equal (up-doc--normalize-mode-list '(("a" . foo-mode) "b") 'foo-mode)
                 '(("a" . foo-mode)
                   ("b" . foo-mode)))))

(ert-deftest up-doc--normalize-hook-list/test ()
  "Tests basic operation."
  ;; nil
  (should (equal (up-doc--normalize-hook-list '() 'foo-mode)
                 '()))
  ;; simplest cases
  (should (equal (up-doc--normalize-hook-list '(bar) 'foo-mode)
                 '((bar-hook . foo-mode))))
  (should (equal (up-doc--normalize-hook-list '(bar1 bar2) 'foo-mode)
                 '((bar1-hook . foo-mode) (bar2-hook . foo-mode))))
  (should (equal (up-doc--normalize-hook-list '((bar . baz-mode)) 'foo-mode)
                 '((bar-hook . baz-mode)))))

(ert-deftest up-doc--normalize-hook-list/test-distribution ()
  "Tests that distribution is properly handled."
  (should (equal (up-doc--normalize-hook-list '(((a b c) . foo-mode)) 'foo-mode)
                 '((a-hook . foo-mode)
                   (b-hook . foo-mode)
                   (c-hook . foo-mode))))
  (should (equal (up-doc--normalize-hook-list '(((a b c) . foo-mode) (d . bar-mode)) 'foo-mode)
                 '((a-hook . foo-mode)
                   (b-hook . foo-mode)
                   (c-hook . foo-mode)
                   (d-hook . bar-mode)))))

(ert-deftest up-doc--normalize-hook-list/test-double-nesting ()
  "Tests that nesting is properly handled."
  (should (equal (up-doc--normalize-hook-list '(((a . foo-mode))) 'foo-mode)
                 '((a-hook . foo-mode))))
  (should (equal (up-doc--normalize-hook-list '(((a))) 'foo-mode)
                 '((a-hook . foo-mode)))))

(ert-deftest up-doc--normalize-hook-list/test-double-nesting-2 ()
  "Tests that nesting is properly handled with two elements."
  (should (equal (up-doc--normalize-hook-list '(((a b))) 'foo-mode)
                 '((a-hook . foo-mode)
                   (b-hook . foo-mode))))
  (should (equal (up-doc--normalize-hook-list '(((a . foo-mode) (b . foo-mode))) 'foo-mode)
                 '((a-hook . foo-mode)
                   (b-hook . foo-mode)))))

(ert-deftest up-doc--normalize-hook-list/test-mixed ()
  "Tests that nesting is properly handled."
  (should (equal (up-doc--normalize-hook-list '((a . foo-mode) (b c) ((d e) . bar-mode)) 'foo-mode)
                 '((a-hook . foo-mode)
                   (b-hook . foo-mode)
                   (c-hook . foo-mode)
                   (d-hook . bar-mode)
                   (e-hook . bar-mode)))))

(ert-deftest up-doc--find-owning-package/test-autoload ()
  "When symbol is defined as an autoload."
  (unwind-protect
      (progn
        (autoload 'foo-bar-func "foo")
        (should (equal 'foo (up-doc--find-owning-package 'foo-bar-func))))
    (fmakunbound 'foo-bar-func)))

(ert-deftest up-doc--find-owning-package/test-loaded ()
  "When SYMBOL's package info exists in `load-history'."
  (should (equal 'ert (up-doc--find-owning-package 'ert-deftest)))
  ;; when the package name is also a function it can collide with requires
  (should (equal 'ert (up-doc--find-owning-package 'ert))))

(ert-deftest up-doc--find-owning-package/test-prefix-match ()
  "When symbol matches string prefix of known library name."
  (with-mock
    (mock (up-doc--known-libraries) => '("foo"))
    (should (equal 'foo (up-doc--find-owning-package 'foo-bar-func)))))

(ert-deftest up-doc--find-owning-package/test-unknown ()
  "Symbol undefined or unrecognized returns nil."
  (should-not (up-doc--find-owning-package 'blah)))

(ert-deftest up-doc--top-level-suggest/test-custom-set-variables ()
  "Test matching for `custom-set-variables' forms."
  ;; this should return some warning and not signal
  (should (up-doc--top-level-suggest '(custom-set-variables
                                       '(foo 123 t ragged)
                                       '(blah 405)))))

(ert-deftest up-doc--top-level-suggest/test-eval-after-load ()
  "Test matching for `custom-set-variables' forms."
  ;; this should return some warning and not signal
  (should (up-doc--top-level-suggest '(eval-after-load foo
                                       (foo 123 t ragged)))))

(ert-deftest up-doc-lint/test-1 ()
  "Tests linting."
  (should (up-doc-lint '(use-package foo-pkg
                          :ensure nil
                          :mode ("\\.gql\\'" "\\.graphql\\'" )
                          :defer t
                          :init (add-hook 'foo-hooks (lambda () (message "foo")))))))

(ert-deftest up-doc-lint/test-2 ()
  "Tests linting."
  ;; originally to test a mode regex rule
  (with-mock
    (mock (featurep 'graphql-mode) => t)
    (should-not (up-doc-lint '(use-package graphql-mode
                                :mode ("\\.gql\\'" "\\.graphql\\'" ))))))

(ert-deftest up-doc-lint/test-3 ()
  "Tests linting."
  (should (up-doc-lint '(use-package fren
                          :straight nil
                          :hook
                          ((a-hook . fn)
                           (b-hook . fn)
                           (c-hook . fn))
                          :mode
                          ("fren1" . fren-mode)
                          ("fren2" . fren-mode)
                          ("fren3" . fren-mode)))))

(ert-deftest up-doc-lint/test-hook-structure ()
  "Tests hook nesting rule."
  (with-mock
   (mock (featurep 'fren) => t)
   (let ((use-package-hook-name-suffix nil))

     ;; hook rule should not trigger on this one
     (should-not (up-doc-lint '(use-package fren
                                 :hook
                                 ((a-hook b-hook c-hook) . fn)
                                 (x-hook . fn))))
     (should (equal (up-doc-lint '(use-package fren
                                    :straight nil
                                    :hook
                                    ;; unnecessary nesting
                                    ((a-hook . fn)
                                     (x-hook . fn))))
                    '("consider inlining contents of :hook keyword to reduce nesting\n  rule:hook-inline-nested"))))))

(ert-deftest up-doc-lint/test-5 ()
  "Should suggest adding :hook."
  (should (up-doc-lint '(use-package fren
                          :init
                          (add-hook 'fren-hook (lambda () (message "hi")))))))

(ert-deftest up-doc-lint/test-6 ()
  "Tests linting."
  (should (up-doc-lint '(use-package smerge-mode
                :after hydra
                ;; amaranth color blocks all other keys
                :hydra (smerge-hydra (:color amaranth
                                             :hint nil
                                             :post (smerge-auto-leave))
                                     "
^Move^         ^Keep^             ^Diff^                 ^Other^
^^-------------^^-----------------^^---------------------^^-------
_n_ext         _b_ase             _<_: upper/base        _C_ombine
_p_rev         _u_pper            _=_: upper/lower       _r_esolve
_C-p_rev line  _l_ower            _>_: base/lower        _k_ill current
_C-n_ext line  _a_ll              _R_efine               _C-z_: undo
^^             _RET_: current     _E_diff
"
                                     ("n" smerge-next)
                                     ("p" smerge-prev)
                                     ("C-n" next-line)
                                     ("C-p" previous-line)
                                     ("b" smerge-keep-base)
                                     ("u" smerge-keep-upper)
                                     ("l" smerge-keep-lower)
                                     ("a" smerge-keep-all)
                                     ("RET" smerge-keep-current)
                                     ("\C-m" smerge-keep-current)
                                     ("<" smerge-diff-base-upper)
                                     ("=" smerge-diff-upper-lower)
                                     (">" smerge-diff-base-lower)
                                     ("R" smerge-refine)
                                     ("E" smerge-ediff)
                                     ("C" smerge-combine-with-next)
                                     ("r" smerge-resolve)
                                     ("k" smerge-kill-current)
                                     ("C-z" undo-only)
                                     ("ZZ" (lambda ()
                                             (interactive)
                                             (save-buffer)
                                             (bury-buffer))
                                      "Save and bury buffer" :color blue)
                                     ("q" nil "cancel" :color blue))
                :hook (magit-diff-visit-file . (lambda ()
                                                 (when smerge-mode
                                                   (smerge-hydra/body))))
                :bind
                (:map smerge-mode-map
                      ("C-c m" . smerge-hydra/body))))))

(ert-deftest up-doc-lint/test-bad-config ()
  "Should not report rule error when :config is empty."
  (with-mock
   (mock (featurep 'fren) => t)
   ;; also, reports should be unique
   (should (equal '("Expected contents of :init to be sexps, got t\n  rule:custom-replace-set"
                    "Expected contents of :config to be sexps, got t\n  rule:custom-replace-set"
                    "Expected contents of :init to be sexps, got t\n  rule:add-hook-instead-of-hook"
                    "Expected contents of :config to be sexps, got t\n  rule:add-hook-instead-of-hook")
                  (up-doc-lint '(use-package fren
                                  :init t
                                  :config))))))

(ert-deftest up-doc-lint/test-custom ()
  "Tests custom var warning."
  (should (up-doc-lint '(use-package foo :init (setq use-package-hook-name-suffix 1) (setq foo-2 2) (message "hi")))))

(ert-deftest up-doc-cleanup/test-trivial ()
  "Should work with a fake empty package."
  (up-doc-cleanup '(use-package blah)))

(ert-deftest up-doc-cleanup/test-unloads ()
  "Should unload the package."
  (with-mock
    (mock (unload-feature * t) => nil :times 2)
    (up-doc-cleanup '(use-package blah))))

(ert-deftest up-doc-cleanup/test-mode-alist-1 ()
  "Should remove entry from `auto-mode-alist'."
  (unwind-protect
      (with-mock
        (stub unload-feature)
        (add-to-list 'auto-mode-alist '("\\foo\\" . blah))
        (add-to-list 'auto-mode-alist '("\\foo\\" . blah-mode))
        ;; (use-package-as-mode 'blah) = blah-mode, but use-package actually adds blah to auto-mode-alist
        (up-doc-cleanup '(use-package blah :mode "\\foo\\"))
        (should-not (assoc-string "\\foo\\" auto-mode-alist)))
    (setq auto-mode-alist (rassq-delete-all 'blah auto-mode-alist))))

(ert-deftest up-doc-cleanup/test-mode-alist-2 ()
  "Should remove entry from `auto-mode-alist'."
  (unwind-protect
      (with-mock
        (stub unload-feature)
        (add-to-list 'auto-mode-alist '("\\foo\\" . blah-mode))
        (up-doc-cleanup '(use-package blah :mode ("\\foo\\" . blah-mode)))
        (should-not (assoc-string "\\foo\\" auto-mode-alist)))
    (setq auto-mode-alist (rassq-delete-all 'blah-mode auto-mode-alist))))

(ert-deftest up-doc-cleanup/test-load-path ()
  "Should remove entry from `load-path'."
  (let ((load-path-copy load-path))
   (unwind-protect
       (with-mock
         (stub unload-feature)
         (add-to-list 'load-path "/some/path")
         (up-doc-cleanup '(use-package blah :load-path "/some/path"))
         (should-not (member "/some/path" load-path)))
     (setq load-path load-path-copy))))

(ert-deftest up-doc-cleanup/test-hook ()
  "Should remove hook."
  (unwind-protect
      (with-mock
        (stub unload-feature)
        (defvar foo-hook nil "testing")
        (add-hook 'foo-hook 'my-fn)
        (up-doc-cleanup '(use-package blah :hook (foo-hook . my-fn)))
        (should-not (member 'my-fn foo-hook)))
    (makunbound 'foo-hook)))

(ert-deftest up-doc-cleanup/test-hook-nested ()
  "Should remove hook."
  (unwind-protect
      (with-mock
        (stub unload-feature)
        (defvar foo-hook nil "testing")
        (add-hook 'foo-hook 'my-fn)
        (up-doc-cleanup '(use-package blah :hook ((foo-hook . my-fn))))
        (should-not (member 'my-fn foo-hook)))
    (makunbound 'foo-hook)))

(ert-deftest up-doc-cleanup/test-hook-single-name ()
  "Should remove hook."
  (unwind-protect
      (with-mock
        (stub unload-feature)
        (defvar foo-hook nil "testing")
        (add-hook 'foo-hook 'blah-mode)
        (up-doc-cleanup '(use-package blah :hook foo))
        (should-not (member 'blah-mode foo-hook)))
    (makunbound 'foo-hook)))

(ert-deftest up-doc-cleanup/test-hook-multi-name ()
  "Should remove hook."
  (unwind-protect
      (with-mock
        (stub unload-feature)
        (defvar foo-hook nil "testing")
        (defvar bar-hook nil "testing")
        (defvar baz-hook nil "testing")
        (add-hook 'foo-hook 'blah-mode)
        (add-hook 'bar-hook 'blah-mode)
        (add-hook 'baz-hook 'blah-mode)
        (up-doc-cleanup '(use-package blah :hook foo bar baz))
        (should-not (member 'blah-mode foo-hook))
        (should-not (member 'blah-mode bar-hook))
        (should-not (member 'blah-mode baz-hook)))
    (makunbound 'foo-hook)
    (makunbound 'bar-hook)
    (makunbound 'baz-hook)))

(ert-deftest up-doc-cleanup/test-custom-reset ()
  "Should reset custom variable."
  (unwind-protect
      (let ((original-value 123))
        (defcustom my-custom-var original-value
          "My custom var."
          :type 'integer)
        ;; assume use-package theme is defined at this stage
        ;; TODO this might fail in CI?
        (custom-theme-set-variables 'use-package
                                    '(my-custom-var 456 nil
                                                    nil
                                                    "Customized with use-package blah"))
        (with-mock
          (stub unload-feature)
          (up-doc-cleanup '(use-package blah :custom (my-custom-var 456)))
          (should (equal my-custom-var original-value))))
    (makunbound 'my-custom-var)))

(ert-deftest up-doc-cleanup/test-remove-after-load-symbol ()
  "Should remove entries from `after-load-alist'."
  (unwind-protect
      (with-mock
        (stub unload-feature)
        (eval-after-load 'blah
          (message "blah"))
        ;; should always be cleaned no matter the form
        (up-doc-cleanup '(use-package blah))
        (should-not (member 'blah after-load-alist)))
    (setq after-load-alist (assoc-delete-all 'blah after-load-alist))))

(ert-deftest up-doc-cleanup/test-remove-after-load-regexp ()
  "Should remove entries from `after-load-alist' which trigger off a regexp."
  (let ((original after-load-alist))
   (unwind-protect
       (with-mock
         (stub unload-feature)
         ;; this produces a binding to a regexp matching the file
         (eval-after-load "blah.el"
           (message "blah"))
         ;; should always be cleaned no matter the form
         (up-doc-cleanup '(use-package blah))
         (should-not (member 'blah after-load-alist)))
     (setq after-load-alist original))))

(ert-deftest up-doc-lint/test-load-warning ()
  "Should warn when not loaded."
  (should (equal '("fren is not currently loaded, some warnings may not apply.")
            (up-doc-lint '(use-package fren))))
  (with-mock
    (mock (featurep 'fren) => t)
    (should-not (up-doc-lint '(use-package fren)))))

;;; up-doc.test.el ends here
