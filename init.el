;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(evil
                      linum-relative
                      auto-complete
                      typescript-mode tss
                      markdown-mode
                      web-mode))
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (when (yes-or-no-p (format "Missing package '%s'! Install?" p))
      (package-install p))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(require 'evil)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))
(define-key evil-normal-state-map (kbd "C-j")
  (lambda ()
    (interactive)
    (evil-scroll-line-down 5)))
(define-key evil-normal-state-map (kbd "C-k")
  (lambda ()
    (interactive)
    (evil-scroll-line-up 5)))
(define-key evil-normal-state-map (kbd "RET")
  (lambda ()
    (interactive)
    (save-buffer)))
(define-key evil-normal-state-map (kbd "C-p")
  (lambda ()
    (interactive)
    (ido-switch-buffer)))
(define-key evil-normal-state-map (kbd "C-S-p")
  (lambda ()
    (interactive)
    (ido-switch-buffer-other-window)))
(setq evil-want-fine-undo nil)
(evil-mode)

(require 'linum-relative)
(global-linum-mode)

(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

(setq inhibit-startup-message t)

(show-paren-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(ido-everywhere)

(load-theme 'tango-dark)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(tool-bar-mode -1)

(setq apropos-sort-by-scores t)

(require 'auto-complete)
(ac-config-default)

(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(tss-config-default)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
