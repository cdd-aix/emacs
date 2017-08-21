;;; package --- Summary
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
		  (company-dabbrev-code company-gtags company-etags company-keywords)
		  company-oddmuse company-dabbrev)))
 '(company-files-exclusions nil)
 '(company-idle-delay 0)
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`/p")))
 '(package-selected-packages
   (quote
    (powerline dtrt-indent ws-butler which-key smart-mode-line elpy default-text-scale neotree smex rjsx-mode expand-region bbdb yaml-mode xml-rpc use-package rainbow-mode nginx-mode markdown-mode magithub json-reformat json-mode jira jinja2-mode jekyll-modes hyde helm-projectile groovy-mode flycheck-pos-tip flycheck-color-mode-line flycheck-checkbashisms dockerfile-mode color-theme coffee-mode ansible-vault ansible-doc ansible))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; Commentary:
(provide 'cf-own-custom)
;;; cf-own-custom.el ends here
