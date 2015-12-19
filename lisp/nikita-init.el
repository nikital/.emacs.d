(provide 'nikita-init)

;;;;; Bootstrap
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))

(require 'package)
(add-to-list 'package-archives melpa)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)


;;;;; Change defaults
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-step 1
      scroll-conservatively 5)

(set-language-environment "UTF-8")
(setq apropos-sort-by-scores t)

(setq-default indent-tabs-mode nil)


;;;;; Ido
(use-package ido
  :demand
  :init
  (setq ido-enable-flex-matching t
	ido-auto-merge-work-directories-length -1)
  :config
  (ido-mode t)
  (ido-everywhere)
  :bind
  ("C-x C-g" . ido-switch-buffer)
  ("C-x g" . ido-switch-buffer-other-window)
  ("C-x f" . ido-find-file-other-window)
  )


;;;;; Linum
(use-package linum-relative
  :ensure t
  :init
  (setq linum-relative-current-symbol "")
  :config
  (linum-relative-global-mode))


;;;;; Config editing
(bind-key "C-c i"
	  (lambda () (interactive)
	    (find-file-other-window "~/.emacs.d/lisp/nikita-init.el")))


;;;;; Evil

(defun nik--evil-c-u ()
  (interactive)
  (evil-delete (point-at-bol) (point)))

(defun nik--evil-scroll-up ()
  (interactive)
  (evil-scroll-line-up 5))
(defun nik--evil-scroll-down ()
  (interactive)
  (evil-scroll-line-down 5))

(use-package evil
  :ensure t
  :init
  (setq evil-want-fine-undo nil)
  :config
  (bind-key "RET" 'save-buffer evil-normal-state-map)
  (bind-key "C-u" 'nik--evil-c-u evil-insert-state-map)

  (evil-declare-not-repeat (bind-key "C-k" 'nik--evil-scroll-up evil-normal-state-map))
  (evil-declare-not-repeat (bind-key "C-j" 'nik--evil-scroll-down evil-normal-state-map))

  (evil-mode t))


;;;;; Graphics
(load-theme 'tango-dark)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(tool-bar-mode -1)
(show-paren-mode 1)


;;;;; OS X
(use-package exec-path-from-shell
  :ensure t
  :if (eq window-system 'ns)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))
