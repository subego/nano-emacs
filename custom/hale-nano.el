(setq straight-base-dir
      (expand-file-name "~/nano-emacs"))
(defvar bootstrap-version)

(let ((bootstrap-file
       (format "%s/%s" straight-base-dir
               "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Hide the process buffer.
(setq straight-process-buffer " *straight-process*")
(setq straight-vc-git-default-clone-depth 1)
(straight-use-package 'use-package)

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil)
  (setq-default mac-option-modifier 'meta))

(defun hale-copy-line ()
  "Copy the whole line at point."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (kill-new (buffer-substring beg end)))
  (message "The whole line has been copied!"))

;; From https://emacs-china.org/t/topic/6601/4
;; ==============================================
(defun org-insert-image ()
  "Insert a image from clipboard."
  (interactive)
  (let* ((path (concat default-directory "img/"))
	     (image-file (concat
			          path
			          (buffer-name)
			          (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
	    (mkdir path))
    (do-applescript (concat
		             "set the_path to \"" image-file "\" \n"
		             "set png_data to the clipboard as «class PNGf» \n"
		             "set the_file to open for access (POSIX file the_path as string) with write permission \n"
		             "write png_data to the_file \n"
		             "close access the_file"))
    ;; (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil
		             (concat "file:" image-file)
		             "")
    (message image-file))
  ;; (org-display-inline-images)
  )

(use-package el-patch
  :straight t)

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :depth 1
                  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-show-candidate nil))

(use-package magit
  :straight (magit :host github
                   :repo "magit/magit"))

(use-package vertico
  :straight (vertico :host github
                     :repo "minad/vertico")
  :init (vertico-mode)
  :custom
  (vertico-count 5))

(use-package consult
  :straight (consult :host github
		     :repo "minad/consult")
  :bind
  (("C-s" . consult-line))
  (("C-x b" . consult-buffer))
  :config
  (setq consult-preview-key '(:debounce 0.5 any)))

(use-package embark
  :straight (embark :host github
		    :repo "oantolin/embark")
  :custom
  (embark-prompter 'embark-completing-read-prompter)
  :bind
  (("s-o" . embark-act))
  (("C-c C-c" . embark-export)))

(use-package embark-consult
  :straight t
  :after (embark consult))

(use-package orderless
  :straight (orderless :host github
                       :repo "oantolin/orderless")
  :custom (completion-styles '(substring orderless)))

;; ;; (use-package marginalia
;;   :straight t
;;   :init (marginalia-mode))

(use-package mini-frame
  :straight (mini-frame :host github
			:repo "muffinmad/emacs-mini-frame"))

(use-package wgrep
  :straight (wgrep :type git
		   :host github
		   :repo "mhayashi1120/Emacs-wgrep"))

(use-package auto-save
  :straight (auto-save :type git
		       :host github
		       :repo "manateelazycat/auto-save")
  :init
  :custom (auto-save-silent t)
  :config (auto-save-enable))

(use-package move-dup
  :straight (move-dup :type git
		      :host github
		      :depth 1
		      :repo "wyuenho/move-dup")
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)))

(use-package company
  :straight company
  :hook
  ((prog-mode . company-mode)
   (slime-repl-mode . company-mode))
  :config
  (setq company-idle-delay 0.1)
  (setq completion-ignore-case t)
  (setq company-minimum-prefix-length 1)
  (setq company-backends
	    '((company-elisp)
          (company-slime)
          (company-files)
          (company-dabbrev)
          (company-keywords)
          (company-capf))))

(use-package smartparens
  :straight smartparens)

(use-package aggressive-indent-mode
  :straight
  (aggressive-indent-mode :host github
                          :repo "Malabarba/aggressive-indent-mode")
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)
   (common-lisp-mode . aggressive-indent-mode)
   (lisp-interaction-mode . aggressive-indent-mode)))

(use-package yasnippet
  :straight (yasnippet :type git
                       :host github
                       :depth 1
                       :repo "joaotavora/yasnippet")
  ;; :hook (org-mode)
  :config
  (yas-reload-all)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/"))

(use-package visual-regexp
  :straight (visual-regexp :type git
                           :host github
                           :repo "benma/visual-regexp.el")
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

(use-package evil
  :straight (evil :host github
		          :repo "emacs-evil/evil")
  :config
  (define-key evil-normal-state-map (kbd "Y") 'hale-copy-line)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (setq-default evil-kill-on-visual-paste nil)
  (evil-mode)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs))

(use-package disable-mouse
  :straight (disable-mouse :host github
                           :repo "purcell/disable-mouse")
  :config
  (mapc #'disable-mouse-in-keymap
  (list evil-motion-state-map
        evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map)))

(use-package org
  :straight (:type git
		           :repo "https://code.orgmode.org/bzg/org-mode.git")
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

(use-package slime
  :straight slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package slime-company
  :straight
  (slime-company :host github
                 :repo "anwyn/slime-company")
  :after
  (slime company)
  :config
  (setq
   ;; slime-company-completion 'fuzzy
   slime-company-after-completion 'slime-company-just-one-space))

(setq frame-resize-pixelwise t)
(scroll-bar-mode -1)
(show-paren-mode 1)
(put 'narrow-to-region 'disabled nil)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

(add-to-list 'load-path "~/nano-emacs/custom")
(require 'hale-header-line)
(require 'hale-journal)
(require 'hale-command)

(provide 'hale-nano)
;;; hale-nano.el ends here
