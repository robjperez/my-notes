;; generate-index.el
;; Script para generar index.org automáticamente

(defun my/generate-index ()
  "Genera el archivo index.org con enlaces a todas las notas por categoría."
  (let ((base-dir "/workspaces/my-notes/notes/")
        (categories '(("Programming" "programming")
                      ("Books & Reading" "books")
                      ("Personal" "personal")))
        (index-file "/workspaces/my-notes/notes/index.org"))
    (with-temp-file index-file
      (insert "** Categories\n\n")
      (dolist (cat categories)
        (let ((cat-name (car cat))
              (cat-dir (concat base-dir (cadr cat))))
          (insert (format "*** %s\n" cat-name))
          (dolist (file (directory-files cat-dir t "\\.org$"))
            (let ((fname (file-name-nondirectory file)))
              (unless (member fname '("index.org" "sitemap.org"))
                (let ((title
                       (with-temp-buffer
                         (insert-file-contents file)
                         (goto-char (point-min))
                         (if (re-search-forward "^#\\+TITLE: \\(.*\\)$" nil t)
                             (match-string 1)
                           fname))))
                  (insert (format "- [[file:%s/%s][%s]]\n"
                                  (cadr cat) fname title))))))
          (insert "\n")))
      (insert "** How This Works\n\n")
      (insert "These notes are:\n- Written in Emacs Org mode\n- Version controlled with Git\n- Automatically published to this website via GitHub Actions\n- Hosted on GitHub Pages\n\n")
      (insert "** Browse All Notes\n\n")
      (insert "Visit the [[file:sitemap.org][complete sitemap]] to see all notes organized by date.\n\n")
      (insert "*Note: The sitemap is automatically generated when the site is published.*\n"))))

(my/generate-index)
