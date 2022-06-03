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

;; basic ui initialization

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(smooth-scrolling-mode 1)
(load-theme 'doom-dark+)
(display-time)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" default))
 '(package-selected-packages
   '(move-text doom-modeline dap-mode lsp-mode vterm bash-completion doom-themes neotree magit company smooth-scrolling counsel ivy use-package))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000)
;; package imports

(use-package company
  :diminish company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

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

(use-package flycheck)
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp))
  :commands lsp)
(use-package move-text)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; function definitions

(defun my-sh-completion-at-point ()
  (let ((end (point))
        (beg (save-excursion (sh-beginning-of-command))))
    (when (and beg (> end beg))
      (bash-completion-dynamic-complete-nocomint beg end t))))

(defun my-sh-hook ()
  (add-hook 'completion-at-point-functions #'my-sh-completion-at-point nil t))

(add-hook 'sh-mode-hook #'my-sh-hook)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; key bindings

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<backspace>") 'switch-to-last-buffer)
(global-set-key (kbd "C-<tab>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-x w q") 'delete-window)
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
