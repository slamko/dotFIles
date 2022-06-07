;; package manager initialization

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq inhibit-startup-message t)

;; package imports

(use-package company
  :diminish company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package doom-themes)
(use-package smooth-scrolling)

(use-package ivy
  :diminish ivy-mode
  :bind ()
  :config
  (ivy-mode 1))

(use-package counsel)
(use-package magit)
(use-package neotree)
(use-package sh-script)
(use-package bash-completion)
(use-package vterm)
(use-package color-identifiers-mode)

(use-package multiple-cursors)

(use-package flycheck)
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (c-mode . lsp))
  :commands lsp)

(use-package move-text)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package preproc-font-lock)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; basic ui initialization

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(add-hook 'after-init-hook 'global-color-identifiers-mode)
(font-lock-add-keywords 'c-mode
                 '(("\\<\\([a-zA-Z_]*\\) *("  1 font-lock-keyword-face)))

(smooth-scrolling-mode 1)
(load-theme 'doom-dark+ t)
(display-time)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 'set-from-style t)
 '(c-default-style '((c-mode . "c") (awk-mode . "awk") (other . "gnu")) t)
 '(custom-safe-themes
   '("a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" default))
 '(package-selected-packages
   '(multiple-cursors rainbo-identifiers-mode color-identifiers-mode modus-themes preproc-font-lock move-text doom-modeline dap-mode lsp-mode vterm bash-completion doom-themes neotree magit company smooth-scrolling counsel ivy use-package))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq-default tab-width 2)
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000)

(setq initial-scratch-message nil)

(setq-default c-default-style "linux"
			c-basic-offset 4)

(setq-default tab-width 4)

(electric-pair-mode 1)

(delete-selection-mode)

(setq-default color-identifiers-mode 1)
;;(lsp-toggle)
;;(lsp-toggle-symbol-highlight)
(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

;; function definitions

(defun my-sh-completion-at-point ()
  (let ((end (point))
        (beg (save-excursion (sh-beginning-of-command))))
    (when (and beg (> end beg))
      (bash-completion-dynamic-complete-nocomint beg end t))))

(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position))))
(defun my-sh-hook ()
  (add-hook 'completion-at-point-functions #'my-sh-completion-at-point nil t))

(add-hook 'sh-mode-hook #'my-sh-hook)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;;(defun magit-stage-all ()
  ;;(interactive)
  ;;(magit-stage-modified)
  ;;(magit-stage-untracked))


(defun do-switch-buffer-file ()
  (switch-to-next-buffer)
  (if (string= "*" (substring (buffer-name (current-buffer)) 0 1))
	  (do-switch-buffer-file)))

(defun switch-to-next-file-buffer ()
  (interactive)
  (do-switch-buffer-file))

(defun toggle-comment-on-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; key bindings

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-<tab>") 'switch-to-next-file-buffer)
(global-set-key (kbd "C-<tab>") 'switch-to-last-buffer)
(global-set-key (kbd "C-x w q") 'delete-window)
(global-set-key (kbd "C-x w k") 'kill-buffer-and-window)
(global-set-key (kbd "C-S-<return>") 'eshell)
(global-set-key (kbd "C-<return>") 'other-window)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-p") 'yank)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-h") 'left-char)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-l") 'right-char)
(global-set-key (kbd "C-S-Q") 'kill-current-buffer)
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "M-k") 'move-text-up)
(global-set-key (kbd "M-j") 'move-text-down)
(global-set-key (kbd "C-S-w") 'copy-line)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "M-w") 'kill-region)
(global-set-key (kbd "C-.") 'replace-string)
(global-set-key (kbd "C-M-.") 'replace-regexp)
(global-set-key (kbd "C-:") 'goto-line)
(global-set-key (kbd "C-{") 'beginning-of-buffer)
(global-set-key (kbd "C-}") 'end-of-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<f12>") 'lsp-find-declaration)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-f") 'swiper-isearch)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-n") 'forward-char)
(global-set-key (kbd "M-n") 'forward-word)

;;(defbine-prefix-command 'pomichnyk)
;;(global-set-key (kbd "C-<menu>") 'pomichnyk)
;;(define-key pomichnyk (kbd "f") 'Helper-describe-function)
;;(define-key pomichnyk (kbd "v") 'Helper-describe-variable)

(define-prefix-command 'magit-map)
(global-set-key (kbd "C-'") 'magit-map)
(define-key magit-map (kbd "c") 'magit-commit)
(define-key magit-map (kbd "s") 'magit)
(define-key magit-map (kbd "m") 'magit-stage-modified)
(define-key magit-map (kbd "a") 'magit-stage-all)
(define-key magit-map (kbd "p") 'magit-push)
(define-key magit-map (kbd "f") 'magit-pull)





