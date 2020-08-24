;;; clojure.el -- Clojure fun configuration
;;; Commentary:
;;; Code:

(defun cljs-node-repl ()
  "Legacy function to start the clojurescript node REPL."
  (interactive)
  (run-clojure "lein trampoline run -m clojure.main scripts/repl.clj"))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

(add-hook 'cider-repl-mode-hook
          '(lambda () (define-key cider-repl-mode-map (kbd "C-c M-b"))
                   'cider-repl-clear-buffer))

;;; Parinfer - for Clojure and other LISPs
;;; Picked from https://github.com/DogLooksGood/parinfer-mode
(use-package parinfer
  :ensure t
  :bind
  (("C-," . painfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults
             pretty-parens
             ;; evil
             ;; lispy
             paredit
             smart-tab
             smart-yank))
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(provide 'clojure)
;;; clojure.el ends here
