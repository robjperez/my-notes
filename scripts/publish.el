;;; publish-simple.el --- Simple batch publishing script
;; -*- lexical-binding: t; -*-

(require 'ox-publish)
(require 'org)

;; Custom function to handle CSS and JS paths based on file location
(defun my-org-html-head (info)
  "Generate HTML head with correct CSS and JS paths based on file location."
  (let* ((file-path (plist-get info :input-file))
         (relative-path (file-relative-name file-path "./notes"))
         (depth (length (split-string relative-path "/" t)))
         (css-path (if (> depth 1)
                       (concat (make-string (1- depth) ?.) "/assets/css/style.css")
                     "assets/css/style.css"))
         (js-path (if (> depth 1)
                      (concat (make-string (1- depth) ?.) "/assets/js/theme-toggle.js")
                    "assets/js/theme-toggle.js")))
    (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />
                     <script src=\"%s\" defer></script>
                     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                     <meta charset=\"utf-8\">" css-path js-path)))


;; Configure HTML export settings
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
                     <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
                     <link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap\" rel=\"stylesheet\">
                     <link rel=\"stylesheet\" type=\"text/css\" href=\"assets/css/style.css\" />
                     <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism.min.css\" />
                     <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-tomorrow.min.css\" media=\"(prefers-color-scheme: dark)\" />
                     <script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-core.min.js\"></script>
                     <script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/plugins/autoloader/prism-autoloader.min.js\"></script>
                     <script src=\"assets/js/prism-org-mode.js\" defer></script>
                     <script src=\"assets/js/theme-toggle.js\" defer></script>
                     <script src=\"assets/js/tag-styling.js\" defer></script>
                     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                     <meta charset=\"utf-8\">"
      org-html-htmlize-output-type nil
      org-html-link-home ""
      org-html-link-up "")

;; Auto-confirm code block execution during batch export
(setq org-confirm-babel-evaluate nil)

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
             :html-preamble "<nav><a href=\"index.html\">🏠 Home</a></nav>"
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
             :html-head "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
                         <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
                         <link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap\" rel=\"stylesheet\">
                         <link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/css/style.css\" />
                         <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism.min.css\" />
                         <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-tomorrow.min.css\" media=\"(prefers-color-scheme: dark)\" />
                         <script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-core.min.js\"></script>
                         <script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/plugins/autoloader/prism-autoloader.min.js\"></script>
                         <script src=\"../assets/js/prism-org-mode.js\" defer></script>
                         <script src=\"../assets/js/theme-toggle.js\" defer></script>
                         <script src=\"../assets/js/tag-styling.js\" defer></script>
                         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                         <meta charset=\"utf-8\">"
             :html-preamble "<nav><a href=\"../index.html\">🏠 Home</a></nav>"
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
              (cat-dir (concat base-dir cat "/"))
              (cat-icon (cond
                         ((string= cat "emacs") "📝")
                         ((string= cat "other") "📚")
                         ((string= cat "programming") "💻")
                         ((string= cat "web") "🌐")
                         ((string= cat "games") "🎮")
                         ((string= cat "tutorials") "📖")
                         ((string= cat "projects") "🚀")
                         (t "📄"))))
          (insert (format "* %s %s Notes\n\n" cat-icon cat-name))
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
                            "")))
                       (date
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (if (re-search-forward "^#\\+DATE: <\\([^>]+\\)>" nil t)
                              (match-string 1)
                            ""))))
                  (insert "- ")
                  (when (not (string-empty-p date))
                    (insert (format "@@html:<span class=\"date\">%s</span>@@ " date)))
                  (insert (format "[[file:%s/%s][%s]]" cat fname title))
                  (when (not (string-empty-p tags))
                    (let ((tag-list (split-string tags ", ")))
                      (dolist (tag tag-list)
                        (insert (format " @@html:<span class=\"tag tag-%s\">%s</span>@@" 
                                        (downcase (replace-regexp-in-string "[^a-zA-Z0-9]" "-" tag))
                                        tag)))))
                  (insert "\n")))))
          (insert "\n"))))
    (message "Generated dynamic index.org with all categories")))

(defun my/copy-assets ()
  "Copy CSS and JS assets to public directory."
  (let ((assets-dir "assets/")
        (public-dir "public/assets/"))
    ;; Ensure public assets directory exists
    (unless (file-directory-p public-dir)
      (make-directory public-dir t))
    
    ;; Copy CSS files
    (let ((css-files (directory-files (concat assets-dir "css/") t "\\.css$")))
      (dolist (css-file css-files)
        (let ((target-file (concat public-dir "css/" (file-name-nondirectory css-file))))
          (unless (file-directory-p (concat public-dir "css/"))
            (make-directory (concat public-dir "css/") t))
          (copy-file css-file target-file t)
          (message "Copied %s to %s" css-file target-file))))
    
    ;; Copy JS files
    (let ((js-files (directory-files (concat assets-dir "js/") t "\\.js$")))
      (dolist (js-file js-files)
        (let ((target-file (concat public-dir "js/" (file-name-nondirectory js-file))))
          (unless (file-directory-p (concat public-dir "js/"))
            (make-directory (concat public-dir "js/") t))
          (copy-file js-file target-file t)
          (message "Copied %s to %s" js-file target-file))))
    
    (message "Assets copied successfully")))

(defun batch-publish ()
  "Publish the notes in batch mode."
  (my/generate-index)
  (org-publish-project "my-notes-org-root" t)
  (org-publish-project "my-notes-org-subdirs" t)
  (org-publish-project "my-notes-static" t)
  (my/copy-assets))

(provide 'publish-simple)
;;; publish-simple.el ends here

(batch-publish)
