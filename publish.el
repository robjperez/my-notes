;;; publish.el --- Batch publishing script

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(package-install 'htmlize)

(require 'ox-publish)

(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/style.css\" />
                     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                     <meta charset=\"utf-8\">"
      org-html-htmlize-output-type 'css)

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
             :auto-sitemap t
             :sitemap-filename "index.html"
             :sitemap-title "My Notes"
             :sitemap-sort-files 'anti-chronologically
             :html-preamble "<nav><a href=\"/\">Home</a> | <a href=\"/index.html\">All Notes</a></nav>"
             :html-postamble "<footer><p>Last updated: %C</p></footer>")
       (list "my-notes-static"
             :base-directory "./assets"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
             :publishing-directory "./public/assets"
             :recursive t
             :publishing-function 'org-publish-attachment)))

(defun batch-publish ()
  "Publish the notes in batch mode."
  (org-publish-all t))

(provide 'publish)
;;; publish.el ends here