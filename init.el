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
(use-package gnu-elpa-keyring-update)
(use-package diminish)
(provide 'init)
;;; init.el ends here
