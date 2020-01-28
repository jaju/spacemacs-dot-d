(defvar current-user
  (getenv "USER"))

(defvar spacemacs-d-directory
  (file-name-directory (symbol-file 'dotspacemacs/user-init)))
(defvar personal-packages-directory
  (concat spacemacs-d-directory "packages"))

(setq personal-packages-list
  '("ox-hugo"
    "easy-hugo"
    "org-reveal"))

(defun add-to-load-path (path)
  (add-to-list 'load-path (concat personal-packages-directory "/" path)))
(mapc #'add-to-load-path personal-packages-list)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 10000000) ; 10 MB

(require 'cl)
(require 'package)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq require-final-newline t)
(delete-selection-mode t)
(setq tab-always-indent 'complete)

(setq save-place-file ".saveplace")
(save-place-mode 1)

(load "aboveall")
(load "ui")
(load "custom")
(load "orgmode")
(load "hugo")
(load "clojure")
;(load "haskell")

; https://joaotavora.github.io/yasnippet/snippet-expansion.html
; For the comman:d and explanation of yas-maybe-expand
;(define-key yas-minor-mode-map (kbd "TAB") yas-maybe-expand)
