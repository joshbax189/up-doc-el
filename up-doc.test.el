;;; up-doc.test..el -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'up-doc)

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
  ;; TODO perhaps it's better to normalise these somehow?
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
  ;; extended format single
  (should (equal (up-doc--normalize-mode-list '(("a" foo-mode t)) 'foo-mode)
                 '(("a" foo-mode t))))
  ;; extended format double nested
  (should (equal (up-doc--normalize-mode-list '((("a" foo-mode t) ("b" foo-mode t))) 'foo-mode)
                 '(("a" foo-mode t)
                   ("b" foo-mode t)))))

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
  (should-not (up-doc-lint '(use-package graphql-mode
                              :mode ("\\.gql\\'" "\\.graphql\\'" )))))

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
  "Tests linting."
  ;; hook rule should not trigger on this one
  (should-not (up-doc-lint '(use-package fren
                              :straight nil
                              :hook
                              ((a-hook b-hook c-hook) . fn)
                              (x-hook . fn))))
  (should (up-doc-lint '(use-package fren
                          :straight nil
                          :hook
                          ;; unnecessary nesting
                          ((a-hook . fn)
                           (x-hook . fn))))))

(ert-deftest up-doc-lint/test-5 ()
  "Should suggest adding :hook."
  (should (up-doc-lint '(use-package fren
                          :init
                          (add-hook fren-hook (lambda () (message "hi")))))))

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

;;; up-doc.test.el ends here
