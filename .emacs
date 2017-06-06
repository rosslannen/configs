;; User info
(setq user-full-name "Ross Lannen")
(setq user-mail-address "ross.lannen@gmail.com")

(package-initialize)

;; init-use-package.el
;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))


;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use package
(eval-when-compile
 (require 'use-package))

(setq use-package-always-ensure t)
;; init-use-package.el ends here

;; Set encoding to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Delete trailing whitespaces upon exiting
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; Accept "y" for yes and "n" for no
(defalias 'yes-or-no-p 'y-or-no-p)

;; sets better defaults
(use-package better-defaults)

;; Evil mode
(use-package evil
  :config
  (progn
    (evil-mode 1)))

;; Sets line numbers
(global-linum-mode t)

;; Org mode
(use-package org)

;; Evil org mode - org evil integration and keybindings
(use-package evil-org)

;; Splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      )

;; Set smooth 1 line scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Helm incremental narrowing search framework
(use-package helm)

;; Company auto-completion
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm))
  (add-hook 'elm-mode-hook #'elm-oracle-setup-copletion))

 ;; YASnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Magit
(use-package magit)

;; Elm-mode
(use-package elm-mode
  :config
  '(elm-format-on-save t)
  '(elm-sort-imports-on-save t))

;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Flycheck-Elm
(use-package flycheck-elm
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" default)))
 ; '(elm-format-on-save t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (company company-mode flycheck-elm flycheck use-package solarized-theme org evil)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Alect-black theme
(use-package alect-themes
  :config
  (load-theme 'alect-black))
