;;; notnarb/init.el --- My emacs configuration
;;; Commentary:
;;; Code:

;; benchmark init via https://github.com/dholm/benchmark-init-el
;; (add-to-list 'load-path "~/vendor/benchmark-init-el")
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(defconst emacs-start-time (current-time))

(defconst notnarb/default-cons-threshold 800000)
;; Turn off GC during initialization: appears to reduce startup time by almost 50%
;; (1.7s => .9s, .75s => .5s)
(setq gc-cons-threshold most-positive-fixnum)

;; Manually configure package because the new Cask maintainers seem to not
;; really want to support this use case anymore :(
;; https://github.com/cask/cask/issues/463#issuecomment-912496111
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; Add all cask folders to the load path.
(let ((default-directory (format "~/.emacs.d/.cask/%s/elpa" emacs-version)))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)

(setq inhibit-startup-message t)

(defun notnarb/add-keywords ()
  "Highlight TODO FIXME XXX BUG."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
								 1 font-lock-warning-face prepend))))
(add-hook 'prog-mode-hook 'notnarb/add-keywords)

(require 'use-package)

(use-package ace-jump-mode
  :defer t
  :bind ("C-o" . ace-jump-mode))

(use-package company
  :diminish company-mode
  :commands company-mode)

(use-package css-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'company-mode))

(use-package elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package helm
  :bind ("M-x" . helm-M-x))

(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package neotree
  :bind ("<f8>" . neotree-toggle)
  :config (setq neo-window-width 45))

(defun notnarb/setup-tide-mode ()
  "Add hooks and modes for tide-mode."
  (interactive)
  (tide-setup)
  ;; use local versions of tslint rather than global
  (setq-local flycheck-typescript-tslint-executable
			  (concat (locate-dominating-file "tslint.json" "tslint.json")
					  "node_modules/.bin/tslint"))
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  ;; Add these two lines to support tslint in web-mode
  ;; https://github.com/ananthakumaran/tide/issues/95
  (flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (eldoc-mode +1)
  (local-set-key (kbd "C-c C-r") 'tide-rename-symbol)
  (company-mode +1))

(use-package typescript-mode
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)
		 ("\\.tsx\\'" . web-mode))
  :pin melpa-stable
  :init
  (use-package tide
	:diminish tide-mode
	:defer t
	:pin melpa-stable)
  (add-hook 'typescript-mode-hook #'notnarb/setup-tide-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
							 (when (string-equal "tsx" (file-name-extension buffer-file-name))
							   (notnarb/setup-tide-mode)))))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package flycheck
  :commands flycheck-mode
  :pin melpa
  :init (progn (add-hook 'sh-mode-hook 'flycheck-mode))
  :config (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 2
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package org
  :mode ("\\.org'\\' . org-mode")
  :bind (("C-c c" . org-capture)
		 ("C-c a" . org-agenda))
  :config
  (add-hook 'org-mode-hook (lambda () (setq org-src-preserve-indentation t)))
  (use-package ox-jira)
  (use-package ox-gfm)
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
								 (file+headline "~/org/inbox.org" "Tasks")
								 "* TODO %i%?")
								("j" "TIL [journal]" item
								 (file+datetree "~/org/TIL.org")
								 "%i%?")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
	 (shell . t)				 ;allow bash scripts to be executed in org mode (could be redundant)
	 (plantuml . t)
	 (python . t)
	 )))

(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar")))

(use-package ag
  :config
  (setq ag-reuse-buffers 't))

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package multiple-cursors
  :bind (("C-c m m" . mc/edit-lines)
		 ("C-c m n" . mc/mark-next-like-this-word)))

(use-package lsp-mode
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode)

(defun notnarb/setup-go-mode ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (local-set-key (kbd "C-c C-r") 'lsp-rename)
  )

(use-package go-mode
  :init
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'notnarb/setup-go-mode)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(defun notnarb/init-projectile-with-c-p ()
  "Start projectile-global-mode and enter (kbd 'C-c p') key sequence."
  (interactive)
  (projectile-global-mode)
  ;; Needed as of 2.0.0
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq unread-command-events (listify-key-sequence (kbd "C-c p"))))

(use-package projectile
  ;; Start projectile mode on first use of "C-c p" since projectile mode doesn't
  ;; work well in some scenarios (e.g. sshfs mounts)
  :pin melpa-stable
  :bind("C-c p" . notnarb/init-projectile-with-c-p))

(use-package alchemist
  :mode ("\\.exs?\\'" . elixir-mode)
  :init
  (progn
	(add-hook 'elixir-mode-hook 'flycheck-mode)
	(add-hook 'elixir-mode-hook 'alchemist-mode)
	(add-hook 'alchemist-mode-hook 'company-mode)))

(use-package anaconda-mode
  :defer t
  :init
  (eval-after-load "company"
	'(add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
  ;; (use-package flycheck-mypy) ; currently unused
  )

(use-package pytest
  :defer t
  :init
  (add-hook 'python-mode-hook (lambda ()
								(local-set-key "\C-ca" 'pytest-all))))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(custom-safe-themes
   '("cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default))
 '(enable-local-variables nil)
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(ispell-personal-dictionary "~/.emacs.d/aspell.en.pws")
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files '("~/org/inbox.org"))
 '(package-selected-packages
   '(hcl-mode systemd bazel-mode go-mode company-terraform terraform-mode py-yapf flycheck-mypy pytest pyenv-mode company-anaconda lsp-python company-lsp zenburn-theme yaml-mode web-mode use-package tt-mode tide smart-tabs-mode restclient projectile perl-completion org-plus-contrib nginx-mode neotree markdown-mode magit less-css-mode json-mode jinja2-mode htmlize helm groovy-mode evil editorconfig dumb-jump dockerfile-mode anything alchemist ag ace-jump-mode))
 '(tab-width 4)
 '(tide-tsserver-executable "node_modules/typescript/bin/tsserver")
 '(warning-minimum-level :error)
 '(warning-suppress-log-types
   '(((editorconfig editorconfig--advice-find-file-noselect))
	 (comp)))
 '(warning-suppress-types '((comp))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
;; /autosave

(load-theme 'zenburn t)
(setq tab-width 4)
;; (smart-tabs-advice js2-indent-line js2-basic-offset)

;; http://stackoverflow.com/questions/915985/in-emacs-how-to-line-up-equals-signs-in-a-series-of-initialization-statements
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
	ad-do-it))
(ad-activate 'align-regexp)

(global-set-key (kbd "C-c .") 'align-regexp)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

;; stolen from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(add-hook 'after-init-hook
		  `(lambda ()
			 (let ((elapsed (float-time (time-subtract (current-time)
													   emacs-start-time))))
			   (message "Loading %s...done (%.3fs) [after-init]"
						,load-file-name elapsed))))
;; set gc threshold back to original value
(setq gc-cons-threshold notnarb/default-cons-threshold)
;; (provide init.el)
;;; init.el ends here
