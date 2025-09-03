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

(defun my/generate-index ()
  "Genera el archivo index.org con enlaces a todas las notas por subcarpeta (categoría), usando rutas relativas."
  (let* ((base-dir "notes/")
         (index-file (concat base-dir "index.org"))
         (categories (seq-filter
                      (lambda (f)
                        (and (file-directory-p (concat base-dir f))
                             (not (string-match-p "^\\." f))))
                      (directory-files base-dir nil nil t))))
    (with-temp-file index-file
      (insert "#+TITLE: My Digital Notes
#+AUTHOR: Roberto
#+EMAIL: roberto@example.com
#+OPTIONS: toc:nil num:nil

* Welcome to My Notes

This is my personal knowledge base where I collect thoughts, learnings, and insights on various topics. The notes are organized by category and automatically updated as I add new content.

")
      (dolist (cat categories)
        (let ((cat-name (capitalize cat))
              (cat-dir (concat base-dir cat "/")))
          (insert (format "* %s Notes\n\n" cat-name))
          (dolist (file (directory-files cat-dir t "\\.org$"))
            (let ((fname (file-name-nondirectory file)))
              (unless (member fname '("index.org" "sitemap.org"))
                (let* ((title
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (if (re-search-forward "^#\\+TITLE: \\(.*\\)$" nil t)
                              (match-string 1)
                            fname)))
                       (tags
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (if (re-search-forward "^#\\+TAGS: \\(.*\\)$" nil t)
                              (match-string 1)
                            ""))))
                  (insert (format "- [[file:%s/%s][%s]]" cat fname title))
                  (when (not (string-empty-p tags))
                    (insert (format " :%s:" (replace-regexp-in-string ", " ":" tags))))
                  (insert "\n")))))
          (insert "\n")))
      (insert "** How This Works\n\n")
      (insert "These notes are:\n- Written in Emacs Org mode\n- Version controlled with Git\n- Automatically published to this website via GitHub Actions\n- Hosted on GitHub Pages\n\n")
      (insert "** Browse All Notes\n\n")
      (insert "Visit the [[file:sitemap.org][complete sitemap]] to see all notes organized by date.\n\n")
      (insert "*Note: The sitemap is automatically generated when the site is published.*\n"))
    (message "Generated dynamic index.org with all categories")))

(defun batch-publish ()
  "Publish the notes in batch mode."
  (my/generate-index)
  (org-publish-project "my-notes-org-root" t)
  (org-publish-project "my-notes-org-subdirs" t)
  (org-publish-project "my-notes-static" t))

(provide 'publish-simple)
;;; publish-simple.el ends here

(batch-publish)
