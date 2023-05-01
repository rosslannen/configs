;;; .emacs --- Global settings

;;; Commentary:

;;; Code:

;; User info
(setq user-full-name "Ross Lannen")
(setq user-mail-address "ross.lannen@gmail.com")

(package-initialize)

;; init-use-package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
;; init-use-package.el ends here


;; Enables some garbage collection magic
(use-package gcmh)


;; Basic defaults
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package better-defaults
  :custom
  (inhibit-splash-screen t)
  (inhibit-scratch-message nil)
  (tab-width 4))


;; Put auto saves in a separate directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))


;; Line numbers
(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-type 'relative)
  :config
  (global-display-line-numbers-mode t))


;; Utility to restart emacs from emacs
(use-package restart-emacs)


;; Dired customizations
(use-package dired
  :ensure nil
  :init
  (setq dired-auto-revert-buffer t))


;; Improved commenting
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; Evil mode
(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  :config
  (progn (evil-mode 1))
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
    (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (add-to-list 'evil-motion-state-modes 'special-mode))


;; Org mode
(use-package org)


;; Key-binding help
(use-package which-key
  :config (which-key-mode))


;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  ;; (add-to-list 'company-dabbrev-code-modes 'web-mode)
  (setq company-idle-delay 0.1))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))


;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.3))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))


;; YASnippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


;; Smartparens
(use-package smartparens)


;; Projectile
(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;; Git
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package gitignore-mode)


;; Python

;; (use-package elpy
;;   :init
;;   (elpy-enable)
;;   :config
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(setq python-shell-interpreter "python3")

(defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)

(use-package company-anaconda
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda))


;; C, C++, Obj-C
(use-package cc-mode
  :ensure nil
  :init
  (setq c-doc-comment-style
        '((c-mode . javadoc)
          (c++-mode . javadoc)))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :bind
  (:map c-mode-map ("C-c b" . clang-format-buffer) :map c++-mode-map ("C-c b" . clang-format-buffer)))

(use-package irony
  :hook ((c++-mode . irony-mode)
	 (c-mode-hook . irony-mode)
	 (irony-mode . irony-cdb-autosetup-compile-options)))

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
	 (c++-mode . (lambda () (setq flycheck-gcc-language-standard "std++17")))
	 (c++-mode . (lambda () (setq flycheck-clang-language-standard "std++17")))))

(use-package clang-format
  :custom
  (clang-format-style-option "google")
  (clang-format-on-save t))


;; LSP
(use-package lsp-mode
  :commands lsp
  :custom
  ;; Can also use clippy, but this is way too much noise.
  (lsp-rust-analyzer-cargo-watch-command "check")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.4)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-diagnostics-disabled
   '("unresolved-macro-call" ;; See https://github.com/rust-analyzer/rust-analyzer/issues/6835
     "unresolved-import" ;; See https://github.com/rust-analyzer/rust-analyzer/issues/6038
     ))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; Rust
(use-package rustic
  :init
  (add-to-list 'exec-path "/home/ross/.cargo/bin")
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c b" . rustic-format-buffer)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)

  ;; Uncomment to enable rustfmt on save
  ;; (setq rustic-format-on-save t)

  (add-hook 'rustic-mode-hook (lambda ()
                                (setq-local buffer-save-without-query t))))


;; Golang
(use-package go-mode)


;; Typescript
;;
;; This is the more basic package. For full dev, use tide instead.
(use-package typescript-mode)

(use-package tide
  :init
  (add-to-list 'exec-path "/home/ross/.nvm/versions/node/v16.14.2/bin")
  (setenv "PATH" (concat "/home/ross/.nvm/versions/node/v16.14.2/bin:" (getenv "PATH")))
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))
         ;; (before-save . tide-format-before-save)))


;; Swift
(use-package swift-mode)


;; Dart/Flutter
(use-package lsp-dart
  :hook (dart-mode . lsp))


;; Web Mode
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))


;; Dockerfiles
(use-package dockerfile-mode)


;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command "multimarkdown"))


;; JSON
(use-package json-mode
  :config
  (setq js-indent-level 2))


;; Yaml
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;; Protocol Buffers
(use-package protobuf-mode
  :hook (protobuf-mode . rainbow-delimiters-mode))


;; Systemd unit files
(use-package systemd)


;; Bitbake files
(use-package bitbake)



;; Asthetics

;; Icons
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

;; Mode line
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
  ;; Global defaults
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode line on errors
  (doom-themes-visual-bell-config)

  ;; Org mode stuff
  (doom-themes-org-config))


;; Highlight Numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))


;; Raindow delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))


(provide '.emacs)

;;; .emacs ends here
