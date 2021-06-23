(defface evil-normal-tag-face
  '((t :foreground "#005f00"
       :background "#AFD700"
       :family     nano-font-family-monospaced
       :weight     semi-bold))
  "Face for evil normal state tag face")

(defface evil-insert-tag-face
  '((t :foreground "#005F5F"
       :background "#9DD9F2"
       :family     nano-font-family-monospaced
       :weight     semi-bold))
  "Face for evil insert state tag face")

(defface evil-visual-tag-face
  '((t :foreground "#870000"
       :background "#FF8700"
       :family     nano-font-family-monospaced
       :weight     semi-bold))
  "Face for evil visual state tag face")

(defface evil-emacs-tag-face
  '((t :foreground "#F5F5ED"
       :background "#7F5AB6"
       :family     nano-font-family-monospaced
       :weight     semi-bold))
  "Face for evil emacs state tag face")

(defface evil-motion-tag-face
  '((t :foreground "#BFD7ED"
       :background "#003B73"
       :family     nano-font-family-monospaced
       :weight     semi-bold))
  "Face for evil motion state tag face")

(defface evil-operator-tag-face
  '((t :foreground "#F5F5F5"
       :background "#D70000"
       :family     nano-font-family-monospaced
       :weight     semi-bold))
  "Face for evil operator state tag face")

;; -------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun nano-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))

;; ---------------------------------------------------------------------
(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))

;; -------------------------------------------------------------------
(defun hale-modeline-compose (tag status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	     (prefix (concat
                  (cond ((string= tag "normal")
                         (propertize " <N> " 'face 'evil-normal-tag-face))
                        ((string= tag "insert")
                         (propertize " <I> " 'face 'evil-insert-tag-face))
                        ((string= tag "visual")
                         (propertize " <V> " 'face 'evil-visual-tag-face))
                        ((string= tag "emacs")
                         (propertize " <E> " 'face 'evil-emacs-tag-face))
                        ((string= tag "motion")
                         (propertize " <M> " 'face 'evil-motion-tag-face))
                        ((string= tag "operator")
                         (propertize " <O> " 'face 'evil-operator-tag-face))
                        (t (propertize "     " 'face 'nano-face-header-default)))
                  (cond ((string= status "RO")
			             (propertize (if (window-dedicated-p)" -- " " RO ")
                                     'face 'nano-face-header-popout))
                        ((string= status "**")
			             (propertize (if (window-dedicated-p)" -- " " ** ")
                                     'face 'nano-face-header-critical))
                        ((string= status "RW")
			             (propertize (if (window-dedicated-p)" -- " " RW ")
                                     'face 'nano-face-header-faded))
                        (t (propertize status 'face 'nano-face-header-popout)))))
         (left (concat
                (propertize " "  'face 'nano-face-header-default
			                'display `(raise ,space-up))
                (propertize name 'face 'nano-face-header-strong)
                (propertize " "  'face 'nano-face-header-default
			                'display `(raise ,space-down))
		        (propertize primary 'face 'nano-face-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
			                 (length prefix) (length left) (length right)
			                 (/ (window-right-divider-width) char-width)))
	     (available-width (max 1 available-width)))
    (concat prefix
	        left
	        (propertize (make-string available-width ?\ )
                        'face 'nano-face-header-default)
	        (propertize right 'face `(:inherit nano-face-header-default
                                               :foreground ,nano-color-faded)))))

;; -------------------------------------------------------------------
(defun hale-header-line-default ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line " %l:%c")))
      (hale-modeline-compose
        (symbol-name evil-state)
        (nano-modeline-status)
        buffer-name
        (concat "(" mode-name
                (if (not (string= "" (rime-lighter)))
                    (concat ", RIME"))
                (if (buffer-narrowed-p)
                    (concat ", NARR" ))
                (if branch (concat ", "
                                   (propertize branch 'face 'italic)))
                ")" )
        position)))

;; -------------------------------------------------------------------
(setq-default header-line-format
              '((:eval (hale-header-line-default))))

;; -------------------------------------------------------------------
(provide 'hale-header-line)
;; file hale-header-line.el end here.
