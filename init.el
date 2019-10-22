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
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  (unless (package-installed-p 'gnu-elpa-keyring-update)
    (setq package-check-signature nil)
    (package-refresh-contents)
    (package-install 'gnu-elpa-keyring-update))
  (setq package-check-signature default-package-check-signature)
  (package-refresh-contents))

;; ;;;; niceities for use-package
;; (use-package diminish)
;; (use-package delight)
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

;;;; movement, selection, visualization

(use-package ace-window
  :bind
  (("C-z o" . ace-window))
  :custom
  (aw-scope 'frame) ;; https://github.com/abo-abo/ace-window#aw-scope
  (aw-dispatch-always t)) ;; https://github.com/abo-abo/ace-window#aw-dispatch-always


(use-package highlight-indentation
  :delight)
(provide 'init)
;;; init.el ends here
