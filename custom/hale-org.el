(straight-use-package '(helm-bibtex
                        :host github
                        :repo "tmalsburg/helm-bibtex"))
(straight-use-package '(bibtex-actions
                        :host github
                        :repo "bdarcus/bibtex-actions"))
(straight-use-package '(citeproc
                        :host github
                        :repo "andras-simonyi/citeproc-el"))

(use-package org
  :straight
  (org-contrib :type git
		       :repo "https://git.sr.ht/~bzg/org-contrib")
  :config
  (add-hook 'org-mode-hook (lambda () (org-overview)))
  (setq org-todo-keywords
        '((sequence "TODO" "WAIT" "|" "DONE" "CANCELED"))))

(use-package ox-latex
  :after org
  :config
  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass{report-CD}"
                 ;; "\\documentclass[lang=cn,11pt,a4paper]{elegntpaper}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-export-with-sub-superscripts nil)
  (setq org-latex-listings 'listings)
  (setq org-latex-tables-booktabs t)
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f")))

(use-package org-bullets
  :straight (org-buiilets :type git
			              :host github
			              :depth 1
			              :repo "sabof/org-bullets")
  ;; :hook (org-mode . org-bullets-mode)
  :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-bullets-bullet-list '( "◉" "○" "⚫" "⚪" )
        org-startup-indented t
        org-ellipsis "  "
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        ;; org-pretty-entities t
        ;; org-hide-emphasis-markers t
        ;; org-bullets-bullet-list '("› ")
        org-fontify-quote-and-verse-blocks t))

(use-package org-roam
  :straight (org-roam :host github
                      :repo "org-roam/org-roam" )
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-file-extensions '("org"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

(use-package org-appear
  :straight (org-appear :host github
                        :repo "awth13/org-appear")
  :after org
  :init
  (add-hook 'org-mode-hook 'org-appear-mode)
  :config
  (setq org-appear-autolinks 1))

(use-package org-fragtog
  :straight (org-fragtog :host github
                         :repo "io12/org-fragtog")
  :after org
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode))


(defvar my/bibs '("~/Dropbox/org-roam/references.bib"))
(use-package oc-bibtex-actions
  :straight (bibtex-actions
             :host github
             :repo "bdarcus/bibtex-actions")
  :bind (("C-c b" . org-cite-insert)
         ("M-o" . org-open-at-point)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset))
  :after (embark oc)
  :config
  (setq bibtex-actions-bibliography my/bibs
        org-cite-global-bibliography my/bibs
        org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions
        org-cite-activate-processor 'oc-bibtex-actions))

(setq bibtex-actions-at-point-function 'embark-act)

(provide 'hale-org)
;;; hale-org.el ends here
