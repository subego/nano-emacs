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

(setq-default mac-option-modifier 'meta)

(defun hale-copy-line ()
  "Copy the whole line at point."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (kill-new (buffer-substring beg end)))
  (message "The whole line has been copied!"))

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

;; (use-package smex
;;   :straight t)

;; (use-package ivy
;;   :straight t)

;; (use-package counsel
;;   :straight t
;;   :diminish ivy-mode counsel-mode
;;   :requires (smex)
;;   :custom (ivy-use-virtual-buffers t)
;;   :bind (("C-s" . swiper-isearch)))

(use-package vertico
  :straight (vertico :host github
                     :repo "minad/vertico")
  :init (vertico-mode)
  :custom
  (vertico-count 5))

;; (use-package selectrum
;;   :straight selectrum
;;   :init (selectrum-mode))

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
  :straight t
  :custom
  (completion-styles '(orderless)))

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
  :hook ((emacs-lisp-mode . company-mode))
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.1)
  (setq completion-ignore-case t)
  (setq company-minimum-prefix-length 2)
  (setq company-backends
	    '((company-capf)
          (company-slime))))

(use-package smartparens
  :straight smartparens)

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
  ;; (add-hook 'org-mode-hook (lambda () (org-fill-paragraph t)))
  ;; (add-hook 'org-mode-hook (lambda () (toggle-truncate-lines nil)))
  ;; (add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
  (add-hook 'org-mode-hook (lambda () (org-overview)))
  ;; (add-to-list 'org-emphasis-alist
  ;;              '("*" (:family "Operator Mono" :weight Bold)))
)

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

;; (use-package beacon
;;   :straight (beacon-mode :type git
;;                          :host github
;;                          :repo "Malabarba/beacon")
;;   :custom
;;   (beacon-blink-duration 0.4)
;;   (beacon-color nano-color-subtle)
;;   :init
;;   (add-hook 'emacs-startup-hook #'beacon-mode))

(use-package svg-lib
  :straight (svg-lib :host github
                     :repo "rougier/svg-lib"))

(use-package slime
  :straight slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package slime-company
  :straight (slime-company :host github
                           :repo "anwyn/slime-company")
  :after (slime company)
  :config (setq
           ;; slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(scroll-bar-mode -1)
(show-paren-mode 1)
(put 'narrow-to-region 'disabled nil)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(add-to-list 'load-path "~/nano-emacs/custom")
(require 'hale-header-line)
(require 'hale-journal)
(require 'hale-command)

(provide 'hale-nano)
;;; hale-nano.el ends here
