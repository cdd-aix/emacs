;;; package -- Summary
;;; Commentary:
(require 'package)
;;; Code:
(package-initialize)
(setq package-user-dir (expand-file-name "~/p/emacs/elpa"))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents)
(package-install 'gnu-elpa-keyring-update)
(provide 'bootstrapkeyring)
;;; bootstrapkeyring.el ends here
