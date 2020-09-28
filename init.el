;;; package -- summary
;; -*- elisp-format-split-subexp-keyword-except-list: (quote ("provide" "require" "loop" "throw" "featurep" "use-package")); -*-
;;; init.el intended for bytecompilation
;;; Commentary:
;;; Initial concept https://github.com/nilcons/emacs-use-package-fast
;;; Revised in 2019 following https://github.com/jwiegley/use-package#getting-started
;;; Code:
;; TODO: Test TODOS
(eval-when-compile (package-initialize)
		   (defvar default-package-check-signature package-check-signature)
		   (setq package-archives '(("melpa" . "https://melpa.org/packages/")
					    ("gnu" . "https://elpa.gnu.org/packages/")))
		   (unless (package-installed-p 'use-package)
		     (message "Installing use-package")
		     (package-refresh-contents)
		     (package-install 'use-package))
		   (require 'use-package)
		   (require 'use-package-ensure)
		   (setq use-package-always-ensure t)
		   (unless (package-installed-p 'gnu-elpa-keyring-update)
		     (message "Installing gnu-elpa-keyring-update")
		     (setq package-check-signature nil)
		     (package-refresh-contents)
		     (package-install 'gnu-elpa-keyring-update)
		     (setq package-check-signature default-package-check-signature)
		     (package-refresh-contents)))

;; And this is the bytecompile magic from nilcons
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add)
	  (add-to-list 'load-path add))
      (eval-when-compile (let ((package-user-dir-real (file-truename package-user-dir)))
			   ;; The reverse is necessary, because outside we mapc
			   ;; add-to-list element-by-element, which reverses.
			   (nreverse (apply #'nconc
					    ;; Only keep package.el provided loadpaths.
					    (mapcar #'(lambda (path)
							(if (string-prefix-p package-user-dir-real
									     path)
							    (list path) nil)) load-path))))))

;;;; keyboard and customization
;; disable annoying ctrl-z to minimize
(use-package bind-key
  :bind ("C-z" . nil))
;; Kludge to enable magit help
(use-package use-package)
;; Keep customizations out of ~/.emacs.d/init.el
(setq custom-file (concat user-emacs-directory "custom.el"))
;; It's okay if it's missing
(load custom-file t t)

;; per buffer overidable defaults
(set-default 'truncate-lines nil)
(set-default 'show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "pink")

;; Global preferences
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; ready\n\n")
(setq unibyte-display-via-language-environment t)
(setq column-number-mode t)
(setq echo-keystrokes 0.1)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq auto-save-timeout 10)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq mouse-yank-at-point t)
(setq switch-to-buffer-preserve-window-point t)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(electric-pair-mode 1)
(subword-mode +1)

;;;; mode minimization
(use-package delight)
(use-package diminish)

;;;; Navigation and appearance aids

(use-package ace-window
  :bind (("C-z o" . ace-window))
  :config (setq-default aw-scope 'frame)
  (setq-default aw-dispatch-always t))

(use-package color-theme-modern
  :config
  (load-theme 'clarity t t)
  (enable-theme 'clarity)
  ;; Kludge to put color-theme-modern on the custom-theme-load-path
  :init
  (require 'clarity-theme)
  )

(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
	 ("C-M--" . default-text-scale-decrease)))

(use-package expand-region
  :bind (("C-z e" . er/expand-region)
	 ("C-z C-e" . er/expand-region)))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package minions
  :commands (minions-mode)
  :init (setq minions-mode-line-lighter "[+]")
  (setq-default minions-direct '(flycheck-mode))
  (minions-mode))

(use-package move-text
  :commands (move-text-default-bindings) ;; Enable M-up M-down to move line or region
  :init (move-text-default-bindings))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smart-mode-line
  :commands (sml/setup)
  :config
  (setq-default sml/theme 'dark)
  (setq-default sml/shorten-modes t)
  (setq-default sml/no-confirm-load-theme t)
  (sml/setup)
  )
;; From https://github.com/DiegoVicen/my-emacs

(use-package whitespace)

;;;; Auto-sanity
(use-package dtrt-indent)

(use-package epm)

(use-package midnight
  :commands (midnight-delay-set)
  :config (midnight-delay-set 'midnight-delay "4:30am")
  :defer 30)

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-M-X" . execute-extended-command)))

(use-package which-key
  :commands (which-key-mode)
  :init (which-key-mode))

(use-package ws-butler
  :commands (ws-butler-global-mode)
  :config (setq-default ws-butler-keep-whitespace-before-point nil)
  :init (ws-butler-global-mode))

;;;; Project management
(use-package helm-projectile
  :bind (("s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map))
  :commands (helm-projectile-on projectile-mode)
  :config
  (require 'uniquify)
  (setq-default uniquify-buffer-name-style 'reverse)
  (projectile-mode +1)
  (helm-projectile-on)
  ;; :init
  )
;; Kludge cannot handle in (use-package delight)
(use-package projectile)
;;;; General development
(use-package compile
  :bind (("C-z c" . compile)
	 ("C-z C-c" . compile))) ;; https://www.emacswiki.org/emacs/CompileCommand save, read, scroll, default command

(use-package diff-hl
  :after (vc-git)
  :commands (global-diff-hl-mode diff-hl-margin-mode)
  :config (require 'diff-hl-margin)
  (diff-hl-margin-mode)
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh)
	 (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
	 (dired-mode-hook . diff-hl-dired-mode))
  :init (global-diff-hl-mode))

(use-package forge
  :after (magit))

(use-package magit
  :bind ("C-c g" . magit-status)
  :config (setq-default vc-follow-symlinks t))

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package magit-lfs)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :commands (global-flycheck-mode flycheck-add-mode flycheck-mode)
  :config (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-indication-mode 'left-fringe)
  )

(use-package flycheck-checkbashisms
  :hook (flycheck-mode . flycheck-checkbashisms-setup))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package flycheck-yamllint
  :hook (flycheck-mode . flycheck-yamllint-setup))

(use-package lsp-mode
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package treemacs)
;; https://github.com/Alexander-Miller/treemacs for suggestions on bindings

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package dap-mode)

(use-package yasnippet-snippets)
(use-package yasnippet
  :bind (:map yas-minor-mode-map
	      ("C-'" . yas-expand))
  :config (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  :functions yas-global-mode
  :init (yas-global-mode 1))

;;;; Language Specific

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))
(use-package coffee-mode)

(use-package docker-compose-mode
  :mode (".*docker-compose.*\\.yml\\'"))

(use-package dockerfile-mode
  :mode ("Dockerfile.*\\'")
  :hook ((dockerfile-mode . subword-mode)
	 (dockerfile-mode . (lambda()
			      (setq indent-tabs-mode nil tab-width 4)))))

(use-package elisp-format)

;; python
(use-package elpy
  :hook (python-mode . elpy-enable))

(use-package company-jedi
  :hook (python-mode . (lambda ()
			 (add-to-list 'company-backends 'company-jedi))))

(use-package importmagic
  :bind (("C-c i l" . importmagic-fix-imports)
	 ("C-c i f" . importmagic-fix-symbol-at-point)
	 ("C-c i s" . importmagic-fix-symbol)
	 ("C-c i u" . importmagic-update-index))
  :config (setq importmagic-python-interpreter "python")
  :hook (python-mode . importmagic-mode))

(use-package groovy-mode)

(use-package json-mode)

(use-package nginx-mode
  :mode (("/nginx/.*\\.conf\\'" . nginx-mode)
	 ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(use-package nim-mode
  :hook ((nim-mode . company-mode)))

(use-package poly-markdown)

(use-package powershell)

(use-package rjsx-mode)

(use-package rust-mode
  :hook (rust-mode . lsp-deferred))

(use-package toml-mode)

(use-package web-mode
  :mode ("\\.html\\'" "\\.htm\\'"))

;;;; Omitted as haven't used much
;; neotree
;; multiple-cursors
;; bbdb
;; calendar
;; org
;; org-bullets
;; writegood-mode
;; artbollocks-mode
;; apache-mode
;; emmet-mode (Consider when writing html)
;; smart-mode-line-powerline-theme
;; with-editor (magit provides... will need config in magit)
;; projectile (Grabbed with helm-projectile)
;; ps-print
;; jekyll-modes (Broken in 2019 by polymode changes and no upstream maintainer)
;; yaml-mode (poly-markdown wires it for us)
;; highlight-indentation (Provided by elpy)
;; python (Provided by elpy)
;; ediff (System package I think... not sure if we need)
;; importmagic will take some work... get elpy C-c C-r i working


(provide 'init)
;;; init.el ends here
