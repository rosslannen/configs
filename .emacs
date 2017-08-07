;;; .emacs --- Global Settings

;;; Commentary:

;;; Code:


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

;; sets better defaults
(use-package better-defaults)

;; Evil mode
(use-package evil
  :config
  (progn
    (evil-mode 1))
  (define-key evil-window-map (kbd "C-j") 'evil-window-down)
  (define-key evil-window-map (kbd "C-k") 'evil-window-up)
  (define-key evil-window-map (kbd "C-h") 'evil-window-left)
  (define-key evil-window-map (kbd "C-l") 'evil-window-right)
  (define-key evil-window-map (kbd "C-j") 'evil-window-next))

;; Sets line numbers
(global-linum-mode t)

;; Org mode
(use-package org)

;; Evil org mode - org evil integration and keybindings
(use-package evil-org)

;; Splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Set smooth 1 line scrolling
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Helm incremental narrowing search framework
(use-package helm)

;; Company auto-completion
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm)
    (add-to-list 'company-backends 'company-anaconda)))

;; Company Quickhelp
(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

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

;; Anaconda mode for Python IDE
(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; Anaconda backend for company
(use-package company-anaconda)

;; Multi-Term for supporting multiple terminals
(use-package multi-term
  :config
  (evil-define-key 'normal term-raw-map
    "p" 'term-paste)
  (evil-define-key 'normal term-raw-map
    "P" 'term-paste))

;; TRAMP settings for remote hosts
(setq tramp-default-method "ssh")

;; Mode line fun
(use-package telephone-line
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                      telephone-line-erc-modified-channels-segment
                      telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                      telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (setq telephone-line-subseparator-faces '())
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-right
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-right
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24)
  (telephone-line-mode t))

;; Dockerfile syntax highlighting
(use-package dockerfile-mode)

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
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
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


;; Material theme
(use-package material-theme
  :config
  (load-theme 'material))

;; Rainbow Delimiters are pretty!
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide '.emacs)
;;; .emacs end here
