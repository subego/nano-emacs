;;; my-journal.el --- 

;;; Commentary:

;; My daily diary, inspired by org-journal, but customized for my needs.

;;; Code:

(require 'org)
(require 'nano-base-colors)
(require 'nano-faces)

(defun month-headline() (format "* %s" (format-time-string "%B")))
(defun month-week-headline() (format "** %s" (format-time-string "%b W%W")))
(defun day-headline() (format "*** %s" (format-time-string "%Y-%m-%d %a W%W")))

(defun month-headline-p()
  (save-excursion
    (widen)
    (goto-char (point-max))
    (search-backward (month-headline) nil t)))

(defun month-week-headlin-p()
  (save-excursion
    (widen)
    (goto-char (point-max))
    (search-backward (month-week-headline) nil t)))

(defun day-headline-p()
  (save-excursion
    (widen)
    (goto-char (point-max))
    (search-backward (day-headline) nil t)))

(defun hale-insert-file-header()
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (if (looking-at-p "#\\+TITLE")
	(ignore)
      (insert (format-time-string "#+TITLE: %Y工作日志"))
      (newline)
      (insert "#+TAGS: Region1(1) Region2(2) 问题处理(w) 需求处理(x) 变更完成(b)")
      (newline))))

(defun hale-insert-headline(headline)
  (save-excursion
    (goto-char (point-max))
    (if (save-excursion
	  (beginning-of-line)
	  (looking-at-p "[[:space:]]*$"))
	(insert headline)
      (progn
	(newline)
	(insert headline)))))

(defun hale-journal-date-format()
  (interactive)
  (if (month-headline-p)
      (if (month-week-headlin-p)
	  (if (day-headline-p)
	      (progn
		(hale-insert-headline (format "**** ")))
	    (progn
	      (hale-insert-headline (day-headline))
	      (hale-journal-date-format)))
	(progn
	  (hale-insert-headline (month-week-headline))
	  (hale-journal-date-format)))
    (progn
      (hale-insert-headline (month-headline))
      (hale-journal-date-format))))

(defun hale-show-last-subtree()
  (save-excursion
    (widen)
    (goto-char (point-max))
    (search-backward (day-headline) nil t)
    (org-show-children)))


(defvar my-diary-directory "~/Dropbox/org-journal/")
(defvar my-diary-filename (expand-file-name (concat my-diary-directory "%Y-log.org")))

(defun hale-journal-new-entry()
  (interactive)
  (find-file (format-time-string my-diary-filename))
  (hale-insert-file-header)
  (hale-journal-date-format)
  (hale-show-last-subtree)
  (goto-char (point-max)))

(global-set-key (kbd "C-c j") 'hale-journal-new-entry)

;; (org-ql-query
;;   :from (format-time-string my-diary-filename)
;;   :where '(and (ancestors (heading "2021-04-15 Thu W15")) (tags "Region1") (tags "问题处理")))

(use-package org-ql
  :straight
  (org-ql :type git
	  :host github
	  :depth 1
	  :repo "alphapapa/org-ql"))

(defun hale-today-1()
  (interactive)
  (org-ql-sparse-tree 
    '(and (ancestors (heading (format-time-string "%Y-%m-%d %a W%W"))) (tags "Region1"))))

(defun hale-today-2()
  (interactive)
  (org-ql-sparse-tree 
    '(and (ancestors (heading (format-time-string "%Y-%m-%d %a W%W"))) (tags "Region2"))))

(defun hale-week-1()
  (interactive)
  (org-ql-sparse-tree 
    '(and (ancestors (heading (format-time-string "%b W%W"))) (tags "Region1"))))

(defun hale-week-2()
  (interactive)
  (org-ql-sparse-tree 
    '(and (ancestors (heading (format-time-string "%b W%W"))) (tags "Region2"))))

(use-package org-bullets
  :straight (org-buiilets :type git
			  :host github
			  :depth 1
			  :repo "sabof/org-bullets")
  ;; :hook (org-mode . org-bullets-mode)
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-bullets-bullet-list '( " " "  " "   " "    " "     ")
	org-startup-indented t
	org-ellipsis "  "
	org-agenda-block-separator ""
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	;; org-pretty-entities t
	;; org-hide-emphasis-markers t
	;; org-bullets-bullet-list '("› ")
	org-fontify-quote-and-verse-blocks t))

(defun journal-mode--num-format (numbering)
  "Alternative numbering format for org-num."
  (if (= (length numbering) 4)
      (propertize (concat
                   (mapconcat #'number-to-string
                              (cdddr numbering) " " ) ". ")
                  'face `(:family "Roboto Condensed"
                          :height 120
                          :foreground ,nano-color-faded))))

(define-derived-mode hale-journal-mode org-mode "work journal notes"
  "Record daily work issues."
  ;; faces
  (face-remap-add-relative 'org-level-1
                           ;; :underline nano-color-subtle
                           :family "Sarasa Mono Slab SC" :height 180)
  (face-remap-add-relative 'org-level-2
                           ;; :underline nano-color-subtle
                           :family "Sarasa Mono Slab SC" :height 160)
  (face-remap-add-relative 'org-level-3
                           :underline nano-color-subtle
                           :family "Sarasa Mono Slab SC" :height 140)
  (face-remap-add-relative 'org-level-4
                           :family "Sarasa Mono Slab SC" :height 130
                           :weight 'semi-light)
  (face-remap-add-relative 'org-document-info
                           :inherit 'nano-face-faded)
  (face-remap-add-relative 'org-document-title
                           :foreground nano-color-foreground
                           :family "Source Han Serif SC" 
                           :height 200
                           :weight 'semi-bold)
  ;; hide title / author ... keywords
  ;; (setq-local org-hidden-keywords '(title author date startup))

  ;; Header line
  ;; (setq header-line-format nil)
  
  ;; Layout
  (setq fill-column 72)
  (setq-default line-spacing 1)
  (setq-local truncate-lines nil)
  (org-overview)
  ;; ;; Indentation
  ;; (setq org-startup-folded t)  
  ;; ;; (org-indent-mode)
  ;; ;; (setq org-level-color-stars-only nil)
  ;; ;; (setq org-hide-leading-stars nil)
  ;; (advice-add 'org-indent--compute-prefixes :override
  ;;             #'writer-mode--compute-prefixes)

  ;; ;; Numbering
  (setq org-num-skip-unnumbered t)
  (setq org-num-skip-footnotes t)
  (setq org-num-max-level 4)
  (setq org-num-face nil)
  (org-num-mode)
  (setq org-num-format-function 'journal-mode--num-format))

(add-to-list 'auto-mode-alist
             '("\\/Users\\/hale\\/Dropbox\\/org-journal\\/.*\\.org"
               . hale-journal-mode))

(provide 'hale-journal)
;;; my-diary.el ends here
