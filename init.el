;;; package -- summary
;;; init.el intended for bytecompilation
;;; Commentary:
;;; Initial concept https://github.com/nilcons/emacs-use-package-fast
;;; Revised in 2019 following https://github.com/jwiegley/use-package#getting-started
;;; Code:
(eval-when-compile
  (package-initialize)
  (defvar default-package-check-signature package-check-signature)
  (setq package-check-signature nil
	package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "https://elpa.gnu.org/packages/")))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (unless (package-installed-p 'gnu-elpa-keyring-update)
    (package-install 'gnu-elpa-keyring-update))
  (setq package-check-signature default-package-check-signature)
  (package-initialize)
  (package-refresh-contents)
  )
(use-package gnu-elpa-keyring-update)
(provide 'init)
;;; init.el ends here
