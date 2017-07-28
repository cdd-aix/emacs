;;; package --- Summary
;;; Commentary:
;;; Code:
;; -*- emacs-lisp -*-

(setq package-user-dir (concat "~/p/emacs/elpa" (number-to-string emacs-major-version))
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpy" . "http://jorgenscahefer.github.io/packages/")))

					; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
	(defvar use-package-always-ensure)
        (setq use-package-always-ensure t)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; real .emacs starts here

;; use-package for the case when init.el is byte-compiled
(use-package diminish)
(use-package bind-key)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package :commands use-package-autoload-keymap)
(bind-key "C-z" nil)

;; First: everything that is only setq
(setq custom-file "~/p/emacs/cf-own-custom.el")
(load "~/p/emacs/cf-own-custom")

;; Workaround for i3 focus issue on emacs exit if an emacs package
;; called x-focus-frame before.  The i3 guys refused fixing their shit
;; and instead decided to be ssholes and rude.
(setf (symbol-function 'x-focus-frame) #'ignore)

(set-default 'truncate-lines nil)
(set-default 'show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "pink")
(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n"
      unibyte-display-via-language-environment t
      column-number-mode t
      echo-keystrokes 0.1
      kill-whole-line t
      make-backup-files nil
      auto-save-timeout 10
      auto-save-file-name-transforms (progn
                                       (make-directory "~/.emacs.d/auto-save-files/" t)
                                       `((".*" "~/.emacs.d/auto-save-files/" t)))
      mouse-yank-at-point t      switch-to-buffer-preserve-window-point t
      select-enable-clipboard t
      select-enable-primary t)

(electric-pair-mode 1)
(subword-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Second: deferred packages, eval-after-loads and autoloads
;; beautiful hungarian letters in ps-print

(use-package ps-print
	     :defer t
	     :config
	     (require 'ps-mule)
	     (eval-when-compile (require 'ps-mule))
	     (setq ps-paper-type 'letter
		   ps-font-size 10))

(use-package elisp-format)
;; also-dependency-for-gnus!
(use-package bbdb
	     :defer t
	     :init (autoload 'bbdb "bbdb-com" nil t))

(use-package expand-region
	     :bind (("C-z e" . er/expand-region)
		    ("C-z C-e" . er/expand-region)))

(use-package compile
	     :bind (("C-z c" . compile)
		    ("C-z C-c" . compile))
	     :config (setq compilation-ask-about-save nil
			   compilation-read-command nil
			   compilation-scroll-output t
			   compile-command "make"))

(use-package rjsx-mode
	     :mode ("\\.js\\'" "\\.jsx\\'"))

;; look at nix within docker volumes for EDA tools

(use-package with-editor
	     :commands (with-editor-async-shell-command with-editor-export-editor)
	     :init
	     (progn
	       (define-key (current-global-map) [remap async-shell-command] 'with-editor-async-shell-command)
	       (define-key (current-global-map) [remap shell-command] 'with-editor-shell-command))
	     (with-eval-after-load 'shell
	       (add-hook 'shell-mode-hook 'with-editor-export-editor))
	     (with-eval-after-load 'term
	       (add-hook 'term-exec-hook 'with-editor-export-editor))
	     (with-eval-after-load 'eshell
	       (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(use-package whitespace)

(use-package markdown-mode
	     :mode ("\\.md\\'" "\\.markdown\\'")
	     :commands (gfm-mode))

(use-package dockerfile-mode
	     :functions (s-replace)
	     :mode ("Dockerfile\\'")
	     :config (add-hook 'dockerfile-mode-hook
			       '(lambda ()
				  (setq indent-tabs-mode nil
					tab-width 4)))
	     (add-hook 'dockerfile-mode-hook 'subword-mode))


(use-package flycheck
	     :commands (global-flycheck-mode)
	     :config (global-flycheck-mode))

(use-package flycheck-color-mode-line
	     :after flycheck
	     :config (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip)

(use-package flycheck-checkbashisms
	     :commands (flycheck-checkbashisms-setup)
	     :config (flycheck-checkbashisms-setup))

(use-package magit
	     :bind ("C-c g" . magit-status)
	     :commands (magit-define-popup-switch)
	     :config (magit-define-popup-switch 'magit-push-popup ?u
		       "Set upstream" "--set-upstream"))

;; (use-package magithub
;; 	     :if (> emacs-major-version 24))

;; (use-package multiple-cursors)

;; Find better keybindings
;; (use-package windmove
;;   :config (defun errge/other-window-back ()
;;             (interactive)
;;             (other-window -1))
;;   :bind* (("<S-left>" . windmove-left)
;;           ("<S-right>" . windmove-right)
;;           ("<S-up>" . windmove-up)
;;           ("<S-down>" . windmove-down)
;;           ("<f1>" . errge/other-window-back)
;;           ("<f2>" . other-window)))

(use-package ace-window
	     :bind (("C-z o" . ace-window))
	     :config (setq aw-scope 'frame
			   aw-dispatch-always t))

;; these work because of (describe-variable 'package--builtins)
(use-package calendar
	     :defer t
	     :config (setq calendar-week-start-day 6))

(use-package vc
	     :defer t
	     :config (setq vc-follow-symlinks t))

(use-package ediff
	     :defer t
	     :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package smex
	     :defer t)

(use-package neotree
	     :commands (neotree-toggle errge/neotree-project-dir)
	     :bind (("<f8>" . errge/neotree-project-dir))
	     :functions (neo-buffer--unlock-width neo-buffer--lock-width neo-global--window-exists-p neotree-dir neotree-find)
	     :config
	     ;; from https://www.emacswiki.org/emacs/NeoTree
	     (defun errge/neotree-project-dir ()
	       "Open NeoTree using the git root."
	       (interactive)
	       (let ((project-dir (projectile-project-root))
		     (file-name (buffer-file-name)))
		 (neotree-toggle)
		 (if project-dir
		     (if (neo-global--window-exists-p)
			 (progn
			   (neotree-dir project-dir)
			   (neotree-find file-name)))
		   (message "Could not find git project root.")))))

(use-package default-text-scale
	     :bind (("C-M-=" . default-text-scale-increase)
		    ("C-M--" . default-text-scale-decrease)))

;; Silver Searcher
;; (use-package ag)

(use-package elpy
	     :defer t
	     :commands (elpy-enable)
	     :config (elpy-enable))

(use-package diff-hl
	     :commands (global-diff-hl-mode)
	     :functions (diff-hl-margin-mode)
	     :config
	     (global-diff-hl-mode 1)
	     (require 'diff-hl-margin)
	     (diff-hl-margin-mode))

(with-eval-after-load 'vc-git
  (require 'diff-hl))

(use-package projectile
	     :functions (projectile-project-root))

(use-package helm-projectile
	     :diminish projectile-mode
	     :commands (helm-projectile-on)
	     :config
	     (setq projectile-completion-system 'helm
		   uniquify-buffer-name-style 'reverse)
	     (require 'uniquify)
	     (helm-projectile-on)
	     (projectile-mode)
	     )


(use-package midnight)

(use-package ansible)

(use-package ansible-doc)

(use-package ansible-vault)

(use-package coffee-mode)

(use-package hyde)

(use-package jekyll-modes)

;; Required for jinja2-mode
(use-package xml-rpc)

(use-package jinja2-mode)

(use-package jira)

(use-package json-mode)

(use-package json-reformat)

(use-package nginx-mode)

(use-package yaml-mode)

(use-package groovy-mode)

(use-package vhdl-mode)

(use-package verilog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third: the sad ones, nothing to defer on and they are slow, so we defer on time...
(use-package color-theme
	     :defer t
	     :commands (color-theme-initialize color-theme-clarity)
	     :defines (color-theme-is-global)
	     ;;	     :init (setq color-theme-is-global t)
	     :config (color-theme-initialize))

(use-package powerline
	     :config (powerline-default-theme))

(use-package smart-mode-line
	     ;;	     :defer 1
	     :functions (sml/setup)
	     :defines (sml/theme)
	     :config
	     (setq sml/theme 'respectful)
	     (color-theme-clarity)
	     (sml/setup)
	     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fourth: require, defuns and bind-keys that are evaluated right
;; here, right now, might be slow, test these first for slowness!

(use-package which-key
	     :diminish which-key-mode
	     :config (progn
		       (setq which-key-idle-secondary-delay 0.1
			     which-key-idle-delay 0.3)
		       (which-key-mode 1)))

(use-package ws-butler
	     :diminish ws-butler-mode
	     :config
	     (progn
	       (setq ws-butler-keep-whitespace-before-point nil)
	       (ws-butler-global-mode 1)))

(use-package dtrt-indent
	     :config
	     (dtrt-indent-mode 1))

;; start server, so we can connect anytime with emacsclient
(unless noninteractive
  (setq server-socket-dir (format "/tmp/emacs-%d-%s-%d"
                                  (user-uid)
                                  (format-time-string "%Y%m%d-%H%M%S")
                                  (emacs-pid)))
  (server-start)
  (add-hook 'kill-emacs-hook #'(lambda () (delete-directory server-socket-dir t))))

(provide 'init)
;;; init.el ends here
