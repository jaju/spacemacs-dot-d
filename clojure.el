(defun cljs-node-repl ()
  (interactive)
  (run-clojure "lein trampoline run -m clojure.main scripts/repl.clj"))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

(add-hook 'cider-repl-mode-hook
      '(lambda () (define-key cider-repl-mode-map (kbd "C-c M-b")
            'cider-repl-clear-buffer)))
