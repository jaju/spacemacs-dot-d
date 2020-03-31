;; Copyright (C) 2015 Ravindra R. Jaju
;; Author: Ravindra Jaju - https:/msync.org/

(require 'org)
; Places where the agenda files exist.
(setq org-agenda-files '("~/.org/agenda" "~/.org/notes"))
(setq org-log-done t) ;; This sets timestamps on tasks when finished.
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-default-notes-file "~/.org/on-the-fly-notes.org")

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cr" 'remember)
(global-set-key "\C-cb" 'org-switchb)

;; BEGIN -- https://github.com/stuartsierra/dotfiles
;; Org-babel and Clojure
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)

; reveal.js setup
(require 'ox-reveal)
(setq org-reveal-external-plugins
      '((animate . "{src: '%splugin/animate/animate.js', async: true, condition: function() { return !!document.body.classList; }}")
        (anything . "{src: '%splugin/animate/anything.js', async: true, condition: function() { return true; }}")))

;; And Python, JS
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (clojure . t)))
(setq org-confirm-babel-evaluate nil)

;; More from http://fgiasson.com/blog/index.php/2016/04/05/using-clojure-in-org-mode-and-implementing-asynchronous-processing/
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)
;; END

(require 'ox-publish)
(with-eval-after-load 'ox
  (require 'ox-hugo))

(setq org-html-html5-fancy t)
(setq org-src-tab-acts-natively t)

(setq org-reveal-root "https://p.msync.org/reveal.js")
(define-skeleton org-presentation-skeleton
  "Inserts org directives for org buffers supposed to generate a slide-show presentation, using reveal.js"
  "Title: "
  "#+Title: " str "\n"
  "#+Author: Ravindra R. Jaju\n"
  "#+Email: first-name -dot- last-name -at- the-world's-email-domain\n"
  "#+FILETAGS: :sometag:\n"
  "#+SETUPFILE: settings.org\n"
  "\n"
  )

(define-skeleton org-slide-skeleton
  "Inserts org slide with directive for a tangled source-code"
  "Title: "
  "* " str
  "\n"
  @ -
  "#+BEGIN_SRC java :results silent\n"
  "\n"
  "#+END_SRC"
  "\n"
  "#+BEGIN_NOTES"
  "\n"
  "#+END_NOTES")

(setq hugo-blog-settings-file "~/.org/msync/blog-settings.org")
(define-skeleton org-post-skeleton
  "Inserts the right directives for hugo-orgmode blogging"
  "Title: "
  "+++\n"
  "title = \"" str "\"\n"
  "description = \"\"\n"
  "author = \"Ravindra R. Jaju\"\n"
  "date = " (now) "\n"
  "lastmod = " (now) "\n"
  "tags = []\n"
  "categories = []\n"
  "published = false\n"
  "+++\n")

(setq hugo-notes-settings-file "~/.org/msync/notes-settings.org")
(define-skeleton org-note-skeleton
  "Inserts the right directives for hugo-orgmode blogging"
  "Title: "
  "+++\n"
  "title = \"" str "\"\n"
  "description = \"\"\n"
  "author = \"Ravindra R. Jaju\"\n"
  "date = " (now) "\n"
  "lastmod = " (now) "\n"
  "tags = []\n"
  "categories = []\n"
  "published = false\n"
  "+++\n")

(setq org-publish-project-alist
      '(
        ("msync-posts"
         :base-directory "~/.org/msync/posts"
         :base-extension "org"
         :publishing-directory "~/Projects/hugo-blog/content/"
         :recursive t
         :publishing-function org-hugo-export-to-md
         :headline-levels 4
         :html-extension "html"
         :body-only t)

        ("msync-notes"
         :base-directory "~/.org/msync/notes"
         :base-extension "org"
         :recursive t
         :publishing-function org-hugo-export-to-md
         :publishing-directory "~/Projects/hugo-blog/content/")

        ("msync-notes-static"
         :base-directory "~/.org/msync/static"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Projects/hugo-blog/content/notes/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("msync"
         :components ("msync-posts" "msync-notes" "msync-notes-static"))

        ("msync-presentation"
         :base-directory "~/.org/presentations"
         :publishing-directory "~/Projects/hugo-blog/content/presentation"
         :recursive t
         :base-extension "org"
         :html-extension "html"
         :headline-levels 4
         :publishing-function org-reveal-export-to-html)))

(defun now ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%:z")))
(define-key global-map (kbd "\C-xt") 'now)

(define-key global-map (kbd "<f7>") (lambda () (interactive) (cd "~/Projects/hugo-blog/")))
(define-key global-map (kbd "<f9>") (lambda () (interactive) (org-hugo-export-to-md)))

(defun directory-files-recursive (directory match maxdepth)
  "List files in DIRECTORY and in its sub-directories. 
   Return files that match the regular expression MATCH. Recurse only 
   to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond 
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          (setq files-list (cons f files-list)))
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))     
          ;; recurse only if necessary
          (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1))))
          (setq files-list (cons f files-list)))
         (t)))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

(defun tangle-all ()
  "Tangle all the Org-mode files in the directory of the file of the current buffer
   recursively in child folders. Returns the list of tangled files"
  (mapcar (lambda (f)
            (when (not (file-directory-p f))
              (org-babel-tangle-file f)))
          (directory-files-recursive (file-name-directory (buffer-file-name)) "\\.org$" 20)))
