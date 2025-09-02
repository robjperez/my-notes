;;; publish.el --- Batch publishing script for GitHub Pages

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

;; Custom function to format tags in HTML
(defun my-org-html-format-tags (tags)
  "Format tags with custom CSS classes."
  (when tags
    (let ((tag-list (split-string tags ":" t)))
      (mapconcat (lambda (tag)
                   (format "<span class=\"tag\">%s</span>" tag))
                 tag-list ""))))

;; Override the default tag formatting
(defun my-org-html-format-list-item (contents type checkbox info &optional term-counter-id headline)
  "Custom list item formatting to handle tags."
  (let ((checkbox-str (org-html-checkbox checkbox info))
        (counter-str (org-html-list-counter info))
        (extra-str (or checkbox-str counter-str "")))
    (cond
     ((and checkbox-str (not counter-str)) (concat extra-str contents))
     (counter-str (concat counter-str " " contents))
     (t (concat extra-str contents)))))

;; Add custom HTML export options
(setq org-html-format-list-item-function 'my-org-html-format-list-item)

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

(defun generate-index-org ()
  "Generate index.org file automatically from all org files in subdirectories."
  (let ((notes-dir "./notes")
        (index-file "./notes/index.org")
        (categories '())
        (content ""))
    
    ;; Find all subdirectories in notes/
    (dolist (item (directory-files notes-dir t))
      (when (and (file-directory-p item)
                 (not (string-match-p "\\.$" item))
                 (not (string-equal item (expand-file-name notes-dir))))
        (let ((category (file-name-nondirectory item))
              (org-files '()))
          
          ;; Find all org files in this category
          (dolist (org-file (directory-files item t "\\.org$"))
            (when (not (string-equal (file-name-nondirectory org-file) "index.org"))
              (let ((title "")
                    (tags "")
                    (date "")
                    (relative-path (file-relative-name org-file notes-dir)))
                
                ;; Read org file to extract metadata
                (with-temp-buffer
                  (insert-file-contents org-file)
                  (goto-char (point-min))
                  
                  ;; Extract title
                  (when (re-search-forward "^#\\+TITLE:\s*\\(.*\\)" nil t)
                    (setq title (match-string 1)))
                  
                  ;; Extract tags
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+TAGS:\s*\\(.*\\)" nil t)
                    (setq tags (match-string 1)))
                  
                  ;; Extract date
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+DATE:\s*\\(.*\\)" nil t)
                    (setq date (match-string 1))))
                
                ;; Add to org-files list
                (push (list :title title
                           :tags tags
                           :date date
                           :path relative-path)
                      org-files)))
          
          ;; Sort org files by date (newest first)
          (setq org-files (sort org-files 
                               (lambda (a b)
                                 (string> (plist-get a :date)
                                         (plist-get b :date)))))
          
          ;; Add category to categories list
          (push (list :name category :files org-files) categories))))
    
    ;; Sort categories alphabetically
    (setq categories (sort categories 
                          (lambda (a b)
                            (string< (plist-get a :name)
                                    (plist-get b :name)))))
    
    ;; Generate content
    (setq content "#+TITLE: My Digital Notes
#+AUTHOR: Roberto
#+EMAIL: roberto@example.com
#+OPTIONS: toc:nil num:nil

* Welcome to My Notes

This is my personal knowledge base where I collect thoughts, learnings, and insights on various topics. The notes are organized by category and automatically updated as I add new content.

")
    
    ;; Add each category
    (dolist (category categories)
      (let ((category-name (plist-get category :name))
            (files (plist-get category :files)))
        (setq content (concat content 
                             (format "\n* %s Notes\n\n" 
                                     (capitalize category-name))))
        
        ;; Add each file in the category
        (dolist (file files)
          (let ((title (plist-get file :title))
                (tags (plist-get file :tags))
                (date (plist-get file :date))
                (path (plist-get file :path)))
            
            (setq content (concat content 
                                 (format "- [[file:%s][%s]]" path title)))
            
            ;; Add tags if they exist
            (when (and tags (not (string-empty-p tags)))
              (setq content (concat content 
                                   (format " :%s:" 
                                           (replace-regexp-in-string 
                                            ",\\| " ":" tags)))))
            
            ;; Add date if it exists
            (when (and date (not (string-empty-p date)))
              (setq content (concat content 
                                   (format " (%s)" date))))
            
            (setq content (concat content "\n")))))
    
    ;; Write the index file
    (with-temp-file index-file
      (insert content))
    
    (message "Generated index.org with %d categories" (length categories))))

(defun batch-publish ()
  "Publish the notes in batch mode."
  (generate-index-org)
  (org-publish-all t))

(provide 'publish)
;;; publish.el ends here

(batch-publish)
