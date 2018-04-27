;;; package --- Summary
;;; Commentary:
;;; Code:
;; -*- emacs-lisp -*-

(defvar package-archives)
(defvar package--init-file-ensured)
(setq package-user-dir (concat "~/p/emacs/elpa" (number-to-string emacs-major-version))
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))

;; Many ideas mooched from https://github.com/nilcons/emacs-use-package-fast
;; Disable package initialize after us.  We either initialize it
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
(mapc #'(lambda (add)
	  (add-to-list 'load-path add))
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
					   (list path) nil)) load-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; real .emacs starts here

;;;; Do not remove, use-package uses these
(use-package bind-key)
(use-package delight)
(use-package diminish)
;; Required for things like flycheck to find in path.
(use-package exec-path-from-shell)
;; Required projectile and flycheck
(use-package pkg-info
	     :functions (pkg-info)
	     :defer t)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package
	     :commands use-package-autoload-keymap
	     ;; :defines use-package-handler/:bind
	     )

;;;; keybinding overrides and setqish things
;; No ctrl-Z to minimize
(bind-key "C-z" nil)

(setq custom-file "~/p/emacs/cf-own-custom.el")
(load custom-file t t)

;; Workaround for i3 focus issue on emacs exit if an emacs package
;; called x-focus-frame before.  The i3 guys refused fixing their shit
;; and instead decided to be ssholes and rude.
(setf (symbol-function 'x-focus-frame) #'ignore)

(set-default 'truncate-lines nil)
(set-default 'show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "pink")
(setq inhibit-startup-screen t initial-scratch-message ";; ready\n\n"
      unibyte-display-via-language-environment t column-number-mode t echo-keystrokes 0.1
      kill-whole-line t make-backup-files nil auto-save-timeout 10 auto-save-file-name-transforms
      (progn (make-directory "~/.emacs.d/auto-save-files/" t)
	     `((".*" "~/.emacs.d/auto-save-files/" t))) mouse-yank-at-point t
	     switch-to-buffer-preserve-window-point t select-enable-clipboard t
	     select-enable-primary t)

(electric-pair-mode 1)
(subword-mode +1)

(defvar cdd/writing-map (make-sparse-keymap) "For writing aids.")

;;;; cursor movement and selection and visualization
(use-package ace-window
	     :bind (("C-z o" . ace-window))
	     :config (setq aw-scope 'frame aw-dispatch-always t))

;; sort-words and sort-symbols for normalizing order in source code
(use-package cddsort
	     :load-path "~/p/emacs/lisp")

(use-package default-text-scale
	     :bind (("C-M-=" . default-text-scale-increase)
		    ("C-M--" . default-text-scale-decrease)))

(use-package dtrt-indent
	     :commands (dtrt-indent-mode)
	     :init (dtrt-indent-mode 1))

(use-package expand-region
	     :bind (("C-z e" . er/expand-region)
		    ("C-z C-e" . er/expand-region)))

(use-package highlight-indentation
	     :commands (highlight-indentation-mode)
	     :delight)

;; M-up M-down on region
(use-package move-text
	     :commands (move-text-default-bindings)
	     :init (move-text-default-bindings))

(use-package multiple-cursors
	     ;; look at https://github.com/Schnouki/dotfiles/blob/master/emacs/init-60-multiple-cursors.el
	     )

;; Smart meta-X
(use-package smex
	     :bind (("M-x" . smex)
		    ("M-X" . smex-major-mode-commands)
		    ("C-M-X" . execute-extended-command)))

(use-package which-key
	     :defines (which-key-idle-secondary-delay which-key-idle-delay)
	     :commands (which-key-mode)
	     :delight which-key-mode
	     :init (which-key-mode)
	     :config (setq which-key-idle-secondary-delay 0.1 which-key-idle-delay 0.3))

;; whitespace visualization
(use-package whitespace)

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

;; use current emacs for $EDITOR
(use-package with-editor
	     :commands (with-editor-async-shell-command with-editor-export-editor)
	     :delight
	     :init (progn (define-key (current-global-map)
			    [remap async-shell-command] 'with-editor-async-shell-command)
			  (define-key (current-global-map)
			    [remap shell-command] 'with-editor-shell-command))
	     :hook ((shell-mode . with-editor-export-editor)
		    (term-exec . with-editor-export-editor)
		    (eshell-mode . with-editor-export-editor)))

(use-package ws-butler
	     :commands (ws-butler-global-mode)
	     :defines (ws-butler-keep-whitespace-before-point)
	     :delight ws-butler-mode
	     :init (ws-butler-global-mode)
	     :config (setq ws-butler-keep-whitespace-before-point nil))


;;;; project and management
(use-package neotree
	     :commands (neotree-toggle neo-global--window-exists-p)
	     :bind (("<f8>" . errge/neotree-project-dir))
	     :functions (neo-buffer--unlock-width neo-buffer--lock-width neotree-dir neotree-find
						  projectile-project-root)
	     :defines (projectile-switch-project-action)
	     :config
	     ;; from https://www.emacswiki.org/emacs/NeoTree
	     (when
		 (require 'projectile nil 'noerror)
	       (setq projectile-switch-project-action 'neotree-projectile-action))
	     :init (defun errge/neotree-project-dir ()
		     "Open NeoTree using the git root."
		     (interactive)
		     (let ((project-dir (projectile-project-root))
			   (file-name (buffer-file-name)))
		       (neotree-toggle)
		       (if project-dir (if (neo-global--window-exists-p)
					   (progn (neotree-dir project-dir)
						  (neotree-find file-name)))
			 (message "Could not find git project root.")))))

(use-package projectile
	     :commands (projectile-mode)
	     :delight)

(use-package helm-projectile
	     :commands (helm-projectile-on)
	     :init (helm-projectile-on)
	     :config (setq projectile-completion-system 'helm uniquify-buffer-name-style 'reverse)
	     (require 'uniquify)
	     (projectile-mode))


;;;; output and publishing
;; ;; For managing jekyll blogs... don't forget to include a .hyde.el in them.
;; ;; Consider revisiting and integrating with magit.
;; (use-package hyde)
;; Printing including foreign characters
(use-package ps-print
	     ;; :defer t
	     :config (require 'ps-mule)
	     (eval-when-compile
	       (require 'ps-mule))
	     (setq ps-paper-type 'letter ps-font-size 10))

;;;; misc
;; also-dependency-for-gnus!
(use-package bbdb
	     :init (autoload 'bbdb "bbdb-com" nil t))
(use-package calendar
	     :defer t
	     :config (setq calendar-week-start-day 6))

;; run actions every midnight
(use-package midnight)

;; org
(use-package org
	     :mode ("\\.org\\'" . org-mode)
	     :bind (("C-c l" . org-store-link)
		    ("C-c c" . org-capture)
		    ("C-c a" . org-agenda)
		    ("C-c b" . org-iswitchb)
		    ("C-c C-w" . org-refile)
		    ("C-c j". org-clock-goto)
		    ("C-c C-x C-o" . org-clock-out)
		    ("C-c C-x t" . org-inlinetask-insert-task)
		    )
	     :defines (org-capture-templates org-agenda-custom-commands org-clock-persist)
	     :config
	     (progn
	       ;; Mooched from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
	       ;; Which mooched from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
	       (setq org-directory "~/org")
	       (setq org-agenda-files
		     (mapcar (lambda (path) (concat org-directory path))
			     '("/org.org"
			       "/gtd/gtd.org"
			       "/gtd/inbox.org"
			       "/gtd/ticker.org")))
	       (setq org-log-done 'time)
	       (setq org-src-fontify-natively t)
	       (setq org-use-speed-commands t)
	       (setq org-capture-templates
		     '(("t" "Todo [inbox]" entry
			(file+headline "~/org/gtd/inbox.org" "Tasks")
			"* TODO %i%?")
		       ("T" "Tickler" entry
			(file+headline "~/org/gtd/tickler.org" "Tickler")
			"* %i%? \n %^t")
		       ("J" "Journal" entry
			(file+headline "~/org/journal.org" "Journal")
			)))
	       (setq org-refile-targets
		     '(("~/org/gtd/gtd.org" :maxlevel . 3)
		       ("~/org/gtd/someday.org" :level . 1)
		       ("~/org/gtd/tickler.org" :maxlevel . 2)))
	       (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
	       (setq org-agenda-custom-commands
		     '(("@" "Contexts"
			((tags-todo "@email"
				    ((org-agenda-overriding-header "Emails")))
			 (tags-todo "@phone"
				    ((org-agenda-overriding-header "Phone")))))))
	       (setq org-clock-persist t)
	       (org-clock-persistence-insinuate)
	       (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
	       (require 'org-inlinetask)))

;; (use-package org-inlinetask
;; 	     :bind (:map org-mode-map
;; 	     :after (org)
;; 	     :commands (org-inlinetask-insert-task))

(use-package org-bullets
	     :commands (org-bullets-mode)
	     :hook
	     (org-mode . (lambda() (org-bullets-mode 1))))


;; Improve writing
(use-package writegood-mode
	     :commands (writegood-grade-level
			writegood-reading-ease
			writegood-mode)
	     :delight
	     :bind (:map cdd/writing-map
			 ("C-c C-w C-w" . writegood-mode))
	     :bind-keymap
	     ("C-c C-w" . cdd/writing-map)
	     )

(use-package artbollocks-mode
	     :commands (artbollocks-mode)
	     :delight
	     :bind (:map cdd/writing-map
			 ("C-c C-w C-a" . artbollocks-mode)))

;;;; General development
(use-package compile
	     :bind (("C-z c" . compile)
		    ("C-z C-c" . compile))
	     :config (setq compilation-ask-about-save nil compilation-read-command nil
			   compilation-scroll-output t compile-command "make"))
(use-package elisp-format
	     :commands (elisp-format-region elisp-format-buffer elisp-format-file
					    elisp-format-directory elisp-format-dired-mark-files
					    elisp-format-library))

(use-package dockerfile-mode
	     :functions (s-replace)
	     :delight
	     :mode ("Dockerfile.*\\'")
	     :hook ((dockerfile-mode . subword-mode)
		    (dockerfile-mode . (lambda()
					 (setq indent-tabs-mode nil tab-width 4)))))

(use-package docker-compose-mode
	     :delight
	     :mode (".*docker-compose.*\\.yml\\'"))

;;;; Web and web development related
(use-package apache-mode
	     :mode
	     ("\\.htaccess\\'" . apache-mode)
	     ("srm\\.conf\\'" . apache-mode)
	     ("access\\.conf\\'" . apache-mode)
	     ;; ("sites-\\(available\\|enabled\\)/" . apache-mode)
	     ("httpd\.conf\\'" . apache-mode))
(use-package nginx-mode
	     :mode
	     ("/nginx/.*\\.conf\\'" . nginx-mode)
	     ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(use-package emmet-mode
	     :delight
	     :commands (emmet-mode)
	     :bind
	     ;; Default bindings clash with webmode indent.
	     ("M-s-<left>" . emmet-prev-edit-point)
	     ("M-s-<right>" . emmet-next-edit-point)
	     :hook
	     (sgml-mode . emmet-mode)
	     (web-mode . emmet-mode)
	     (css-mode . emmet-mode)
	     )
(use-package json-mode
	     :mode ("\\.json\\'"))
;; xml syntax to javascript
(use-package rjsx-mode
	     :mode ("\\.js\\'" "\\.jsx\\'"))
(use-package web-mode
	     :mode ("\\.html\\'"))
(use-package yaml-mode
	     :mode ("\\.yaml\\'" "\\.yml\\'"))

;;;; Markup languages
(use-package adoc-mode
	     :mode ("\\.adoc\\'"))
(use-package jekyll-modes
	     :commands (jekyll-markdown-mode jekyll-html-mode))
(use-package markdown-mode
	     :delight
	     (markdown-mode)
	     (gfm-mode)
	     :commands (gfm-mode markdown-mode)
	     :mode (("\\.md\\'" . gfm-mode)
		    ("\\.markdown\\'" . markdown-mode))
	     :config
	     (setq markdown-command "multimarkdown")
	     (setq markdown-gfm-uppercase-checkbox t)
	     )

;; (use-package sgml-mode)


;;;; Syntax checking
(use-package flycheck
	     :commands (global-flycheck-mode flycheck-add-mode)
	     :defines (flycheck-disabled-checkers)
	     :config (append flycheck-disabled-checkers '(javascript-jshint))
	     (flycheck-add-mode 'javascript-eslint 'web-mode)
	     :init (global-flycheck-mode))

(use-package flycheck-color-mode-line
	     :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-checkbashisms
	     :commands (flycheck-checkbashisms-setup)
	     :hook (flycheck-mode . flycheck-checkbashisms-setup))

;;;; Source Control
;; magit uses auto-revert
(use-package autorevert
	     :commands (auto-revert-mode)
	     :delight auto-revert-mode)

(use-package diff-hl
	     :commands (global-diff-hl-mode)
	     :functions (diff-hl-margin-mode)
	     :config (global-diff-hl-mode 1)
	     (require 'diff-hl-margin)
	     (diff-hl-margin-mode)
	     :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
	     :init (with-eval-after-load 'vc-git
		     (require 'diff-hl)))

(use-package ediff
	     :defer t
	     :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
	     :commands (magit-define-popup-switch)
	     :bind ("C-c g" . magit-status)
	     :config (magit-define-popup-switch 'magit-push-popup ?u "Set upstream"
		       "--set-upstream"))

(use-package magit-gitflow
	     :commands (turn-on-magit-gitflow)
	     :hook (magit-mode . turn-on-magit-gitflow))

(use-package vc
	     :defer t
	     :config (setq vc-follow-symlinks t))

;;;; Python
;; elpy removed importmagic so...
(use-package importmagic
	     :commands importmagic-fix-imports
	     importmagic-fix-symbol importmagic-fix-symbol-at-point importmagic-update-index
	     importmagic-mode
	     :defines importmagic-mode-map
	     :bind (:map importmagic-mode-map
			 ("C-c C-f" . nil)
			 ("C-c C-l" . nil)
			 ("C-c i i" . 'importmagic-fix-symbol-at-point)
			 ("C-c i l" . 'importmagic-fix-imports)
			 ("C-c i u" . 'importmagic-update-index)
			 ("C-c i s" . 'importmagic-fix-symbol))
	     :bind-keymap ("C-c i" . importmagic-mode-map)
	     :hook (python-mode . importmagic-mode)
	     :delight)
;; elpy uses company
(use-package company
	     :defer t
	     :delight)
;; elpy uses yasnippet
(use-package yasnippet-snippets
	     :commands (yasnippet-snippets-initialize)
	     :delight)
(use-package yasnippet
	     :commands (yas-minor-mode)
	     :config (yasnippet-snippets-initialize)
	     :delight)
(use-package elpy
	     :commands (elpy-enable)
	     :defines (elpy-rpc-backend)
	     :config (setq elpy-rpc-backend "jedi")
	     :delight)

(use-package python
	     :delight
	     :functions (python-mode)
	     :mode ("\\.py" . python-mode)
	     :interpreter ("python" . python-mode)
	     :config (elpy-enable))


;;;; Other languages for reference
;; (use-package ansible
;; 	     :defer t)

;; (use-package ansible-doc)

;; (use-package ansible-vault)

;; (use-package coffee-mode)

(use-package groovy-mode
	     :mode ("\\.groovy\\'" "\\.groovy.override\\'"))

;; (use-package indium)
;;;; https://indium.readthedocs.io/en/latest/setup.html

;; (use-package jinja2-mode)

;; (use-pacakge tide)
;;;; https://github.com/ananthakumaran/tide

;; (use-package vhdl-mode)

;; (use-package verilog-mode)

;;;; Slow theme crud
(use-package smart-mode-line-powerline-theme
	     :after smart-mode-line)
(use-package smart-mode-line
	     :commands (sml/setup sml/apply-theme)
	     :defines (sml/theme)
	     :init (sml/setup)
	     (sml/apply-theme 'powerline))

(use-package color-theme-modern
	     :init (require 'clarity-theme) ;; color-theme-el should have a (when (load-file-name) ....)
	     (load-theme 'clarity t t)
	     (enable-theme 'clarity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fourth: require, defuns and bind-keys that are evaluated right
;; here, right now, might be slow, test these first for slowness!

;; start server, so we can connect anytime with emacsclient
(unless noninteractive
  (setq server-socket-dir (format "/tmp/emacs-%d-%s-%d" (user-uid)
				  (format-time-string "%Y%m%d-%H%M%S")
				  (emacs-pid)))
  (server-start)
  (add-hook 'kill-emacs-hook #'(lambda ()
				 (delete-directory server-socket-dir t))))

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
