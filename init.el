;;; package -- summary
;;; init.el intended for bytecompilation
;;; Commentary:
;;; Initial concept https://github.com/nilcons/emacs-use-package-fast
;;; Revised in 2019 following https://github.com/jwiegley/use-package#getting-started
;;; Code:
(eval-when-compile
  (package-initialize)
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
    (package-refresh-contents))
  )

;; And this is the bytecompile magic from nilcons
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
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

;;;; keyboard and customization
;; disable annoying ctrl-z to minimize
(use-package bind-key
  :bind ("C-z" . nil))
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
(use-package delight :defer t)
(use-package diminish :defer t)

;;;; Navigation and appearance aids

(use-package ace-window
  :bind
  (("C-z o" . ace-window))
  :custom
  (aw-scope 'frame "https://github.com/abo-abo/ace-window#aw-scope")
  (aw-dispatch-always t "https://github.com/abo-abo/ace-window#aw-dispatch-always"))

(use-package color-theme-modern
  :init
  (require 'clarity-theme)
  (load-theme 'clarity t t)
  (enable-theme 'clarity))

(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
	 ("C-M--" . default-text-scale-decrease)))

(use-package expand-region
  :bind (("C-z e" . er/expand-region)
	 ("C-z C-e" . er/expand-region)))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package move-text
  :commands (move-text-default-bindings) ;; Enable M-up M-down to move line or region
  :init (move-text-default-bindings))

(use-package smart-mode-line
  :commands (sml/setup sml/appy-theme)
  :init (sml/setup))

(use-package whitespace)

;;;; Auto-sanity
(use-package dtrt-indent)

(use-package epm)

(use-package midnight
  :commands (midnight-delay-set)
  :config (midnight-delay-set 'midnight-delay "4:30am")
  :defer 30
  )

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-M-X" . execute-extended-command)))

(use-package which-key
  :commands (which-key-mode)
  :init (which-key-mode))

(use-package ws-butler
  :commands (ws-butler-global-mode)
  :custom (ws-butler-keep-whitespace-before-point nil)
  :delight
  :init (ws-butler-global-mode))

;;;; Project management
(use-package helm-projectile
  :bind (("s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map))
  :commands (helm-projectile-on projectile-mode)
  :config
  (require 'uniquify)
  :custom ((uniquify-buffer-name-style 'reverse))
  :delight
  :init
  (helm-projectile-on)
  (projectile-mode +1))

;;;; General development
(use-package compile
  :bind (("C-z c" . compile)
	 ("C-z C-c" . compile))) ;; https://www.emacswiki.org/emacs/CompileCommand save, read, scroll, default command

(use-package diff-hl
  :after (vc-git)
  :commands (global-diff-hl-mode diff-hl-margin-mode)
  :config
  (require 'diff-hl-margin)
  (diff-hl-margin-mode)
  :delight
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh)
	 (dired-mode-hook . diff-hl-dired-mode))
  :init (global-diff-hl-mode))

(use-package forge
  :after (magit))

(use-package magit
  :delight (magit magit-mode auto-revert-mode)
  :bind ("C-c g" . magit-status)
  :custom
  (vc-follow-symlinks t))

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package flycheck
  :commands (global-flycheck-mode flycheck-add-mode)
  :config (append flycheck-disabled-checkers '(javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :init (global-flycheck-mode))

(use-package flycheck-checkbashisms
  :hook (flycheck-mode . flycheck-checkbashisms-setup))

(use-package flycheck-yamllint
  :hook (flycheck-mode . flycheck-yamllint-setup))

;;;; Language Specific

(use-package coffee-mode)

(use-package docker-compose-mode
  :delight
  :mode (".*docker-compose.*\\.yml\\'")  )

(use-package dockerfile-mode
  :delight
  :mode ("Dockerfile.*\\'"))

(use-package elisp-format)

(use-package elpy
  :delight
  :hook (python-mode . elpy-enable))

(use-package groovy-mode)

(use-package json-mode
  :delight)

(use-package nginx-mode
  :mode (("/nginx/.*\\.conf\\'" . nginx-mode)
	 ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(use-package poly-markdown)

(use-package powershell)

(use-package rjsx-mode
  :delight)

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
