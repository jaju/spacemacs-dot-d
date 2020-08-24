;;; hugo.el --- Hugo configuration
;;; Commentary:
;;; Code:
(require 'easy-hugo)
(setq easy-hugo-basedir "~/Projects/hugo-blog/")
(setq easy-hugo-url "https://blog.msync.org")
(setq easy-hugo-default-ext ".org")
(define-key global-map (kbd "C-c C-e") 'easy-hugo)
(provide 'hugo)
;;; hugo.el ends here
