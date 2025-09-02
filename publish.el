;;; publish-simple.el --- Simple batch publishing script
;; -*- lexical-binding: t; -*-

(require 'ox-publish)
(require 'org)

;; Custom function to handle CSS paths based on file location
(defun my-org-html-head (info)
  "Generate HTML head with correct CSS path based on file location."
  (let* ((file-path (plist-get info :input-file))
         (relative-path (file-relative-name file-path "./notes"))
         (depth (length (split-string relative-path "/" t)))
         (css-path (if (> depth 1)
                       (concat (make-string (1- depth) ?.) "/assets/css/style.css")
                     "assets/css/style.css")))
    (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />
                     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                     <meta charset=\"utf-8\">" css-path)))





;; Configure HTML export settings
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/css/style.css\" />
                     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                     <meta charset=\"utf-8\">"
      org-html-htmlize-output-type 'css
      org-html-link-home "index.html"
      org-html-link-up "index.html")

(setq org-publish-project-alist
      (list
       (list "my-notes-org-root"
             :base-directory "./notes"
             :base-extension "org"
             :include '("index.org")
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./public"
             :with-author nil
             :with-creator t
             :with-toc t
             :section-numbers nil
             :time-stamp-file nil
             :html-preamble "<nav><a href=\"index.html\">Home</a> | <a href=\"index.html\">All Notes</a></nav>"
             :html-postamble "<footer><p>Last updated: %C</p></footer>")
       (list "my-notes-org-subdirs"
             :recursive t
             :base-directory "./notes"
             :base-extension "org"
             :exclude "index.org"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./public"
             :with-author nil
             :with-creator t
             :with-toc t
             :section-numbers nil
             :time-stamp-file nil
             :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/css/style.css\" />
                         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                         <meta charset=\"utf-8\">"
             :html-preamble "<nav><a href=\"../index.html\">Home</a> | <a href=\"../index.html\">All Notes</a></nav>"
             :html-postamble "<footer><p>Last updated: %C</p></footer>")
       (list "my-notes-static"
             :base-directory "./assets"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
             :publishing-directory "./public/assets"
             :recursive t
             :publishing-function 'org-publish-attachment)))

(defun simple-generate-index ()
  "Generate a simple index.org file."
  (let ((content "#+TITLE: My Digital Notes
#+AUTHOR: Roberto
#+EMAIL: roberto@example.com
#+OPTIONS: toc:nil num:nil

* Welcome to My Notes

This is my personal knowledge base where I collect thoughts, learnings, and insights on various topics. The notes are organized by category and automatically updated as I add new content.

* Emacs Notes

- [[file:emacs/emacs-basics.org][Emacs Basics]] :emacs: :editor: :basics: :navigation: (2024-01-15)
- [[file:emacs/org-mode.org][Org Mode Mastery]] :emacs: :org-mode: :productivity: :notes: (2024-01-16)
- [[file:emacs/emacs-configuration.org][Emacs Configuration Tips]] :emacs: :configuration: :customization: :setup: (2024-01-19)

* Ruby Notes

- [[file:ruby/ruby-basics.org][Ruby Programming Basics]] :ruby: :programming: :basics: :syntax: (2024-01-17)
- [[file:ruby/rails-intro.org][Ruby on Rails Introduction]] :ruby: :rails: :web-development: :framework: (2024-01-18)
"))
    (with-temp-file "./notes/index.org"
      (insert content))
    (message "Generated simple index.org")))

(defun batch-publish ()
  "Publish the notes in batch mode."
  (simple-generate-index)
  (org-publish-project "my-notes-org-root" t)
  (org-publish-project "my-notes-org-subdirs" t)
  (org-publish-project "my-notes-static" t))

(provide 'publish-simple)
;;; publish-simple.el ends here

(batch-publish)
