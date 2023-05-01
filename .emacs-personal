;;; .emacs --- Global Settings

;;; Commentary:

;;; Code:


;; User info
(setq user-full-name "Ross Lannen")
(setq user-mail-address "ross.lannen@gmail.com")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; init-use-package.el
;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use package
(eval-when-compile
 (require 'use-package))

(setq use-package-always-ensure t)
;; init-use-package.el ends here

;; Enables some garbage collection magic
(use-package gcmh)

;; Basic defaults
;; Set encoding to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package better-defaults
  :custom
  (inhibit-splash-screen t)
  (initial-scratch-message nil)
  (tab-width 4))


;; Line numbers
(if (version<= "26.0.50" emacs-version )
    (use-package display-line-numbers
      :ensure nil
      :init
      ;; Uncomment to set relative line numbers
      ;; (setq display-line-numbers-type 'relative)
      :config
      (global-display-line-numbers-mode t))
  (use-package linum
    :ensure nil
    :config
    (global-linum-mode t)))

;; TRAMP settings for remote hosts
(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh"))


;; Utility to restart Emacs from Emacs
(use-package restart-emacs)


;; Dired customizations
(use-package dired
  :ensure nil
  :init
  (setq dired-auto-revert-buffer t))

(use-package dired-aux
  :ensure nil)

(use-package dired-x
  :ensure nil
  :config
  (let ((cmd "xdg-open"))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(:?mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(when (executable-find "fd")
  (use-package fd-dired))

(use-package dired-git-info
  ;; One of these 2 should be set
  :init
  (setq dgi-auto-hide-details-p nil)
  ;; :config
  ;; (define-key dired-mode-map ")" 'dired-git-info-mode)
  :hook (dired-after-readin . dired-git-info-auto-enable))

(use-package diredfl
  :config (diredfl-global-mode))


;; Improves and replaces the built-in help menus
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))
;; These should be uncommented if Ivy is installed
  ;; :config
  ;; (setq counsel-describe-function-function #'helpful-callable)
  ;; (setq counsel-describe-variable-function #'helpful-variable))


;; Aggressive Indentation
(use-package aggressive-indent
  :hook ((clojure-mode cider-repl-mode slime-mode) . aggressive-indent-mode))


;; Improved Commenting
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))


;; Evil mode
(use-package evil
  :config
  (progn
    (evil-mode 1))
  (setq evil-ex-substitute-global t)
  (define-key evil-normal-state-map
    (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-normal-state-map
    (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (define-key evil-motion-state-map
    (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-motion-state-map
    (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (add-to-list 'evil-motion-state-modes 'special-mode))


;; Org mode
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)))
  :hook (org-mode . visual-line-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))


;; Helm incremental narrowing search framework
(use-package helm)

;; (use-package helm-gtags
;;   :hook (dired-mode eshell-mode c-mode c++-mode asm-mode))


;; Key-binding help
(use-package which-key
  :config (which-key-mode))


;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-idle-delay 0)
  (setq company-idle-delay 0))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))


;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.4))

(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))


;; LSP
(use-package lsp-mode
  :hook (elm-mode (scala-mode . lsp)))

(use-package etags
  :ensure nil
  :init
  (setq tags-revert-without-query t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :init
  (push 'company-lsp company-backends))


;; YASnippet
(use-package yasnippet)
;  :config
;  (yas-global-mode 1))


;; Smartparens
(use-package smartparens)


;; Multi-Term
;; adds support for multiple terminals
(use-package multi-term
  :config
  (evil-define-key 'normal term-raw-map
    "p" 'term-paste)
  (evil-define-key 'normal term-raw-map
    "P" 'term-paste))


;; Projectile
(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;; Git
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package gitignore-mode)


;; Elm
(use-package elm-mode)

(use-package flycheck-elm
  :after (flycheck)
  :hook (flycheck-mode . flycheck-elm-setup))


;; Python
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(setq python-shell-interpreter "python3")

(defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)

(use-package company-anaconda
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda))

;(use-package py-autopep8
;  :hook (python-mode . py-autopep8-enable-on-save)
;  :config
;  (setq py-autopep8-options '("--ignore E501")))


;; c, c++, Obj-c
(use-package cc-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(use-package cc-mode
  :config
  (add-to-list 'c-doc-comment-style '(c++-mode . javadoc))
  (add-to-list 'c-doc-comment-style '(c-mode . javadoc)))

(use-package irony
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (global-set-key (kbd "C-c b") 'clang-format-buffer))

(use-package company-irony
  :after (company)
  :hook (irony-mode . company-irony-setup-begin-commands)
  :config
  (setq company-backends (delete 'company-semantic company-backends)))

(use-package company-irony-c-headers
  :after (company)
  :config
  (add-to-list
   'company-backends '(company-irony-c-headers company-irony)))

(use-package flycheck-irony
  :after (flycheck)
  :hook ((flycheck-mode . flycheck-irony-setup)
         (c++-mode . (lambda () (setq flycheck-gcc-language-standard "gnu++17")))
         (c++-mode . (lambda () (setq flycheck-clang-language-standard "gnu++17")))))

(use-package irony-eldoc
  :hook (irony))

(use-package clang-format
  :custom
  (clang-format-style-option "google")
  (clang-format-on-save t))


;; Dockerfiles
(use-package dockerfile-mode)


;; .NET
(use-package omnisharp
  :after (company)
  :hook ((csharp-mode . omnisharp-mode)
         (fsharp-mode . omnisharp-mode))
  :config
  (add-to-list 'company-backends 'company-omnishap))

(use-package csharp-mode)

(use-package fsharp-mode)


;; Web Mode
(use-package web-mode
  :mode "\\.\\([jt]s[x]?\\|html?\\)\\'"
  :init
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("angular" . "\\.component\\.html\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  :config
  (when (and (featurep 'flycheck) (string-match "html?" (file-name-extension buffer-file-name)))
    (flycheck-add-mode 'html-tidy 'web-mode)))

(use-package prettier-js
  :hook (web-mode . prettier-js-mode))

(use-package add-node-modules-path
  :hook (web-mode lsp-mode))

(defun setup-tide-mode ()
  "Set up tide mode with required minor modes."
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :hook (web-mode . (lambda ()
                      (when (string-match "[jt]s[x]?" (file-name-extension buffer-file-name))
                        (setup-tide-mode)
                        (when (featurep 'flycheck)
                        (flycheck-add-mode 'typescript-tslint 'web-mode)
                        (flycheck-add-next-checker 'typescript-tslint 'jsx-tide 'append))))))


;; Clojure
(use-package clojure-mode
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)))

(use-package cider
  :hook (cider-repl-mode . rainbow-delimiters-mode))


;; Paredit
(use-package paredit
  :hook ((clojure-mode cider-repl-mode slime-mode) . enable-paredit-mode))


;; Scala
(use-package scala-mode)
; :mode "\\.s\\(cala\\|bt\\)$"

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))


;; Powershell
(use-package powershell)


;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command "multimarkdown"))


;; Assembly
(use-package asm-mode
  :ensure nil
  :init
  (setq asm-comment-char ?\#))


;; Haskell
(use-package haskell-mode)

(use-package company-ghci
  :after (company)
  :config
  (add-to-list 'company-backends 'company-ghci))


;; Latex
(require 'ox-latex)
(add-hook 'latex-mode 'visual-line-mode)

; (setq org-latex-listings 'minted)

; (add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "xelatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "xelatex -interaction nonstopmode -shell-escape -output-directory %o %f"))


;; Elixir
(use-package elixir-mode)

(use-package alchemist)


;; Rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package flycheck-rust
  :after (rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))


;; Javascript
(use-package js2-mode)

(use-package xref-js2)

(use-package rjsx-mode)

(use-package company-tern)


;; Css
(use-package css-mode
  :config
  (setq css-indent-offset 2))


;; JSON
(use-package json-mode
  :config
  (setq js-indent-level 2))


;; Yaml
(use-package yaml-mode
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;; TOML
(use-package toml-mode)


;; Protocol Buffers
(use-package protobuf-mode
  :hook (protobuf-mode . rainbow-delimiters-mode))


;; Golang
(use-package go-mode
  :init
  (setq-default tab-width 4)
  :config
  (setq exec-path (append '("/usr/local/go/bin") exec-path))
  (setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :hook (go-mode))

(use-package company-go)

(use-package flycheck-gometalinter)


;; Groovy (Jenkinsfiles)
(use-package groovy-mode
  :mode "\\Jenkinsfile\\'")


;; SLIME
(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/clisp")
  (setq slime-contribs '(slime-fancy)))


;; Nginx Config Files
(use-package nginx-mode
  :config
  (add-to-list 'auto-mode-alist '("/etc/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))


;; Systemd Unit Files
(use-package systemd)


;; Bitbake Integration
(use-package bitbake)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auto-package-update-delete-old-versions t)
 '(auto-package-update-hide-results t)
 '(blink-cursor-mode nil)
 '(clang-format-on-save t t)
 '(clang-format-style-option "google" t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" default)))
 '(elm-format-command "elm-format")
 '(elm-format-on-save t)
 '(elm-indent-look-past-empty-line nil)
 '(elm-interactive-command (quote ("elm" "repl")))
 '(elm-package-command (quote ("elm" "package")))
 '(elm-package-json "elm.json")
 '(elm-reactor-command (quote ("elm" "reactor")))
 '(elm-sort-imports-on-save t)
 '(elm-tags-on-save t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(inhibit-splash-screen t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (gnu-elpa-keyring-update highlight-numbers all-the-icons-dired all-the-icons flycheck-rust alchemist elixir-mode color-theme-sanityinc-tomorrow color-theme-tomorrow clang-format aggressive-indent csharp-mode markdown-mode powershell cider fsharp-mode rainbow-delimiters company company-mode flycheck-elm flycheck use-package solarized-theme org evil)))
 '(safe-local-variable-values
   (quote
    ((engine . liquid)
     (web-mode-engines-alist
      ("liquid" . "\\.html\\'"))
     (web-mode-engines-alist
      ("django" . "\\.html\\'"))
     (engine . django)
     (irony-additional-clang-options "--define=PRODUCT_MPU_V3")
     (irony-cdb-search-directory-list "MPU V3 Debug")
     (irony-cdb-compilation-databases irony-cdb-clang-complete)
     (irony-cdb-search-directory "MPU V3 Debug")
     (irony-cdb-search-directory . "MPU V3 Debug")
     (eval progn
           (add-to-list
            (quote exec-path)
            (concat
             (locate-dominating-file default-directory ".dir-locals.el")
             "node_modules/.bin/")))
     (flycheck-cppcheck-language-standard . "c++11")
     (flycheck-disabled-checkers quote
                                 (c/c++-clang)))))
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Asthetics Below Here

;; Icons
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
  ;; This was found in centaur emacs, not sure what it does...
  ;; :config
  ;; (with-no-warnings
  ;;   (defun my-all-the-icons-dired--display ()
  ;;     "Display the icons of files without colors in a dired buffer."
  ;;     (when dired-subdir-alist
  ;;       (let ((inhibit-read-only t))
  ;;         (save-excursion
  ;;           ;; TRICK: Use TAB to align icons
  ;;           (setq-local tab-width 1)
  ;;           (goto-char (point-min))
  ;;           (while (not (eobp))
  ;;             (when (dired-move-to-filename nil)
  ;;               (insert " ")
  ;;               (let ((file (dired-get-filename 'verbatim t)))
  ;;                 (unless (member file '("." ".."))
  ;;                   (let ((filename (dired-get-filename nil t)))
  ;;                     (if (file-directory-p filename)
  ;;                         (insert (all-the-icons-icon-for-dir filename nil ""))
  ;;                       (insert (all-the-icons-icon-for-file file :v-adjust -0.05))))
  ;;                   ;; Align and keep one space for refeshing after some operations
  ;;                   (insert "\t "))))
  ;;             (forward-line 1))))))
  ;;   (advice-add #'all-the-icons-dired--display
  ;;               :override #'my-all-the-icons-dired--display)))


;; Mode line
;; These functions define some mode line icon helpers
(defun custom-modeline-modified ()
  "This snippet displays an icon depending on if the file is modified.
It shows a chain icon when the current file is saved,
a broken chain when it is modified and a pad lock when
the file is read only."
    (let* ((config-alist
             '(("*"
                all-the-icons-faicon-family all-the-icons-faicon "chain-broken"
                :height 1.2 :v-adjust -0.0)
               ("-"
                all-the-icons-faicon-family all-the-icons-faicon "link"
                :height 1.2 :v-adjust -0.0)
               ("%"
                all-the-icons-octicon-family all-the-icons-octicon "lock"
                :height 1.2 :v-adjust 0.1)))
            (result (cdr (assoc (format-mode-line "%*") config-alist))))
       (propertize (apply (cadr result) (cddr result))
                   'face `(:family ,(funcall (car result))))))

(use-package telephone-line
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-major-mode-segment))
          (nil    . (telephone-line-buffer-segment
                     ;; (custom-modeline-modified)
                     telephone-line-minor-mode-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (setq telephone-line-subseparator-faces '())
  (setq telephone-line-primary-left-separator 'telephone-line-gradient
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-gradient
        telephone-line-secondary-right-separator 'telephone-line-gradient)
  (setq telephone-line-height 18)
  (telephone-line-mode t))

;; Doom themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;; Highlight Numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))


;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (latex-mode . rainbow-delimiters-mode)))


(provide '.emacs)

;;; .emacs ends here
