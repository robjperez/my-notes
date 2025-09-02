;;; publish-simple.el --- Simple batch publishing script
;; -*- lexical-binding: t; -*-

(require 'ox-publish)
(require 'org)

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
       (list "my-notes-org"
             :recursive t
             :base-directory "./notes"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./public"
             :with-author nil
             :with-creator t
             :with-toc t
             :section-numbers nil
             :time-stamp-file nil
             :html-preamble "<nav><a href=\"index.html\">Home</a> | <a href=\"index.html\">All Notes</a></nav>"
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
  (org-publish-all t))

(provide 'publish-simple)
;;; publish-simple.el ends here

(batch-publish)
