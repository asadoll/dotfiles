(defconst org-roam-packages
  '((org-roam :location
              (recipe :fetcher github :repo "jethrokuan/org-roam"))))

(defun org-roam/init-org-roam ()
  (use-package org-roam
    :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory "~/Dropbox/Personal/org/")
    (org-roam-db-location "/home/asad/org-roam.db")
    ;; :custom-face
    ;; (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
    :init
    (progn
      (spacemacs/declare-prefix "aor" "org-roam")
      (spacemacs/set-leader-keys
       "aorl" 'org-roam
       "aort" 'org-roam-today
       "aorf" 'org-roam-find-file
       "aorg" 'org-roam-show-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
                                                "rl" 'org-roam
                                                "rt" 'org-roam-today
                                                "rb" 'org-roam-switch-to-buffer
                                                "rf" 'org-roam-find-file
                                                "ri" 'org-roam-insert
                                                "rg" 'org-roam-show-graph))
    :config
    (require 'org-roam-protocol)
    (setq org-roam-capture-templates
          '(("d" "default" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "${slug}"
             :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
             :unnarrowed t)
            ("p" "private" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "private-${slug}"
             :head "#+TITLE: ${title}\n"
             :unnarrowed t)))
    (setq org-roam-ref-capture-templates
          '(("r" "ref" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "websites/${slug}"
             :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
             :unnarrowed t)))
    (defun asad/org-roam--backlinks-list (file)
      (if (org-roam--org-roam-file-p file)
          (--reduce-from
           (concat acc (format "- [[file:%s][%s]]\n"
                               (file-relative-name (car it) org-roam-directory)
                               (org-roam--get-title-or-slug (car it))))
           "" (org-roam-sql [:select [file-from]
                                     :from file-links
                                     :where (= file-to $s1)
                                     :and file-from :not :like $s2] file "%private%"))
        ""))
    (defun asad/org-export-preprocessor (_backend)
      (let ((links (asad/org-roam--backlinks-list (buffer-file-name))))
        (unless (string= links "")
          (save-excursion
            (goto-char (point-max))
            (insert (concat "\n* Backlinks\n" links))))))
    (add-hook 'org-export-before-processing-hook 'asad/org-export-preprocessor)
    ))
