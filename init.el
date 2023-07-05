
(require 'server)
(unless (server-running-p)
  (server-start))

(set-frame-font "CaskaydiaCove NF" nil t)

;;no, thank you
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

					;ERROR handling

(setq byte-compile-warnings '(cl-functions))
;;(setq visible-bell nil)


					;BOOTSTRAPPING STRAIGHT.EL

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-path "~/.emacs.d/straight/")


					;GENERAL + binding free ups

(use-package general
  :straight t
  :config
  (general-unbind
    "C-M-o" ; free up for org-roam *Notes*
    "C-M-r" ; free up for remote ops))
    "C-s" ; for super bindings))
    ))


					;DASHBOARD

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (general-define-key
   "C-c h" (lambda () (interactive)(view-buffer "*dashboard*")))
  (setq
   initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
   dashboard-center-content t
   dashboard-startup-banner 'logo
   ))

					;UTITLITIES
;;toggle background to a specified hex
(defun toggle-background ()
  (interactive)
  (defvar background-state 0)
  (defvar last-background nil)
  (defvar default-preference "#ffffff")
  (cond ( (= background-state 0) (progn
				   (setq last-background (background-color-at-point))
				   (set-background-color (let ((choice (read-string "background hex?:")))
							   (if (string= choice "") default-preference choice)))
				   (setq background-state 1)))
	( t (progn
	      (set-background-color last-background)
	      (setq background-state 0)))))

(general-define-key "C-c C-t C-b" #'toggle-background)

					;EVIL ENV

(use-package evil
  :straight t
  :init
  (use-package undo-fu
    :straight t
    :config
    (general-define-key
     :states 'normal
     "u" #'undo-fu-only-undo
     "\C-r" #'undo-fu-only-redo)
    (setq evil-undo-system 'undo-fu))
  (setq
   evil-want-keybinding nil
   evil-want-integration t)
  :config 
  (evil-mode 1))


(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :straight t
  :after org
  :config
  (general-add-hook 'org-mode
		    (list #'evil-org-mode))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

					;HELM

(use-package helm
  :straight t
  :config
  (general-define-key
   "M-x" #'helm-M-x
   "C-h a" #'helm-apropos
   "C-/" #'helm-occur
   "C-x C-f" #'helm-find-files)
  (helm-mode 1))

					;PROJECTILE
(use-package projectile
  :init
  :straight t
  :config
  (projectile-mode 1)
  (use-package helm-projectile
    :straight t
    :config
    (general-define-key
     :prefix "C-c p"
     "f f" #'helm-projectile-find-file
     "f d" #'helm-projectile-find-dir
     "a" #'helm-projectile-ack)))


					;FILE MANAGEMENT : DIRED-X

(setq find-file-visit-truename t)
(general-add-hook 'dired-load-hook
		  (list (lambda ()
			  (load "dired-x")
			  ;; Set dired-x global variables here.  For example:
			  ;; (setq dired-guess-shell-gnutar "gtar")
			  ;; (setq dired-x-hands-off-my-keys nil)
			  )))

					;AESTHETICS

(use-package doom-themes
  :straight t)
(use-package darkroom
  :straight t)
(use-package nimbus-theme
  :straight t
  :config
  (load-theme 'nimbus t))
(use-package beacon
  :straight t
  :init
  (setq beacon-size 60 
	beacon-blink-when-focused t
	beacon-blink-when-point-moves-vertically 5
	beacon-blink-when-window-changes t
	beacon-color "#4444bb"
	beacon-blink-delay 0.2
	beacon-blink-duration 0.2)
  :config
  (beacon-mode 1))

					;MODELINE AND ICONS

(use-package doom-modeline
  :straight t
  :init
  (use-package all-the-icons :straight t)
  (use-package minions :straight t)
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-continuous-word-count-modes '(org-mode))
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-mu4e t)
  (setq doom-modeline-minor-modes (featurep 'minions))
  (general-add-hook 'after-init-hook
		    (list #'doom-modeline-mode
			  #'minions-mode)))

  
(use-package nlinum-relative
  :straight t
  :config
  (nlinum-relative-setup-evil)                    
  (general-add-hook 'prog-mode-hook
		    (list #'nlinum-relative-mode))
  (setq nlinum-relative-redisplay-delay 0)      
  (setq nlinum-relative-current-symbol "->")      
  (setq nlinum-relative-offset 0)) 

					;QUICK CONFIG

(defun edit-init ()
  (interactive)
  (message (concat "editing user-init-file @ " user-init-file))
  (find-file user-init-file))
(general-define-key
 :prefix "C-c"
 "e" #'edit-init)

					;Window management

(use-package window-purpose
  :straight t
  :config
;;(add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;;(add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;;(add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;;(purpose-compile-user-configuration))
;;(purpose-mode 1))
  (purpose-compile-user-configuration)
  (general-define-key
   :prefix "C-c ,"
   "s" #'purpose-set-window-purpose))

					;Tabs
(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode t))

					;buffer managment
(general-define-key "C-c i" #'ibuffer)


					;WHICH-KEY

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (general-define-key "C-h C-k" #'which-key-show-top-level)
  (which-key-setup-side-window-bottom))

					;BROWSING

(setq browse-url-browser-function 'eww-browse-url)
(setq shr-max-image-proportion 0.6)
(general-define-key
 "C-c C-e C-c" #'eww-copy-page-url
 "C-c t" #'toggle-truncate-lines)

					;PDF INTEROP

(use-package pdf-tools
  :straight (pdf-tools :host github
		       :repo "vedang/pdf-tools")
  :config
  (pdf-loader-install))


					;MAGIT

(use-package magit :straight t)

					;ORG-OPS

(use-package org
  :straight t
  :config
  (general-define-key
   :prefix "C-c"
   "l" #'org-store-link
   "a" #'org-agenda
   "c" #'org-capture
   "!" #'org-time-stamp-inactive)
  (setq org-directory (file-truename "~/links/source/org")
	org-default-notes-file (concat org-directory "/gtd/GTD_HQ.org")
	org-startup-with-inline-images t
	org-startup-truncated nil)
  (general-add-hook 'org-mode-hook
		    (list #'toggle-word-wrap
			  #'flyspell-mode
			  #'nlinum-relative-mode))
  (defun update-org-latex-fragments ()
    (org-latex-preview '(64))
    (plist-put org-format-latex-options :scale (* 2 text-scale-mode-amount))
    (org-latex-preview '(16)))
  (general-add-hook 'text-scale-mode-hook
		    (list #'update-org-latex-fragments))
  (setq org-latex-packages-alist '(("margin=2cm" "geometry")))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (general-define-key
   :prefix "C-c"
   "r" #'org-refile
   "C-x C-g" #'org-clock-goto))
					;ORG-BABEL
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
;;(mermaid . t)))

(setq org-babel-python-command "/home/rajp152k/miniconda3/bin/python")

;;(use-package ob-mermaid
;;  :straight t
;;  :config
;;  (setq ob-mermaid-cli-path "/home/rajp152k/node_modules/.bin/mmdc"))

(use-package org-bullets
  :straight t
  :config
  (general-add-hook
   'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 5)))

(setq org-capture-templates
      '(("n" "Next Action" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "NA")
         "* TODO %?\n  %i\n  %a")
	("e" "Event" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "Events")
         "* %?\nSCHEDULED: %T\n  %i")
        ("i" "IN" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "INQ")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("c" "consolidate" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "Consolidate")
         "* %?\nEntered on %U\n  %i\n  %a")
	("t" "Tickler" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "Tickler")
	 "* %?\nDEFER THOUGHT TO: %T\n %i"))) 

					;GTD

(defun gtd()
  "open the GTD workspace"
  (interactive)
  (let ((gtd-dir (concat org-directory "/gtd/GTD_HQ.org")))
    (message (concat "opening GTD workspace @ " gtd-dir))
    (find-file gtd-dir)
    (flyspell-mode-off)))

(general-define-key
 :prefix "C-c"
 "g" #'gtd)

					;ORG-ROAM

(use-package org-roam
  :straight (org-roam :host github
		      :repo "org-roam/org-roam"
		      :branch "v2")
  :config
  (setq org-id-method 'ts)
  (setq org-roam-directory (file-truename "/mnt/c/Users/Raj Patil/source/org/journal/"))
  (setq org-roam-file-extensions '("org"))
  (org-roam-setup)
  (general-define-key
   :prefix "C-M-o"
   "f" #'org-roam-node-find
   "i" #'org-roam-node-insert
   "c" #'org-roam-capture
   "h" #'(lambda ()
	   (interactive)
	   (find-file (concat org-roam-directory "Index.org")))
   "d s" #'org-roam-db-sync
   "t a" #'org-roam-tag-add
   "t d" #'org-roam-tag-remove
   "r" #'org-roam-buffer-toggle
   "a a" #'org-roam-alias-add
   "a d" #'org-roam-alias-remove)
  (add-to-list 'display-buffer-alist
	       '(; org-roam buffer toggle config
		 (".org-roam.*"
		  (display-buffer-in-side-window)
		  (window-width . 0.25)
		  (side . left)
		  (slot 0)))))


					;COMPANY

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-capf)
  (setq company-ignore-case t)
  (global-company-mode 1))

(use-package company-box
  :straight t
  :config
  (general-add-hook 'company-mode-hook
		    (list 'company-box-mode)))

(use-package company-lsp
  :straight t
  :requires company
  :config
  (push 'company-lsp company-backends)
  ;;disable client side cache as LSP does is better
  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil))

					;PYTHON
(use-package pyvenv
  :straight t
  :config
  (setenv "WORKON_HOME" "/home/rajp152k/miniconda3/envs/")
  (pyvenv-mode 1)
  (pyvenv-workon "emacs"))

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

					;YAML
(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))




					; LSP

(use-package lsp-mode
  :straight t
  :config
  (setq lsp-prefer-flymake nil)
  (general-define-key "C-M-l" (general-simulate-key "s-l")) ;; no super key
  (general-add-hook
   (list 'c++-mode-hook
	 'python-mode-hook
	 'racket-mode-hook)
   (list #'lsp))
  (general-add-hook
   'lsp-mode-hook
   (list #'lsp-enable-which-key-integration))
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" )
	lsp-clients-clangd-executable "clangd"))

;;c++ compile_flags.txt auto place

(defun clangd-lsp-setup ()
  (interactive)
  ;;check if database already exists
  (let* ((dir default-directory)
	 (include-path-1 "/usr/include/c++/")
	 (include-path-2 "/usr/include/x86_64-linux-gnu/c++/")
	 (ver (caddr (directory-files include-path-1)))
	 (includes-str (concat "-I" (concat include-path-1 ver) "/\n"
			       "-I" (concat include-path-2 ver) "/\n"))
	 (compilation-db (concat dir "compile_flags.txt")))
    (if (file-exists-p compilation-db)
	(message "compilation database already exists")
      (progn (message "placing a new compilation database")
	     (write-region includes-str nil compilation-db)))))

(general-add-hook 'c++-mode-hook
		  (list#'clangd-lsp-setup))

;;(use-package lsp-pyright
;;  :straight t
;;  :config
;;  (general-add-hook 'python-mode #'(lambda () (lsp)) ))

(use-package lsp-racket
  :straight '(lsp-racket
	      :type git
	      :host github
	      :repo "mullikine/lsp-racket-el"))

(use-package dap-mode
  :straight t)

(use-package lsp-ui
  :straight t
  :requires lsp-mode flycheck
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-use-childframe t
	lsp-ui-doc-position 'top
	lsp-ui-doc-include-signature t
	lsp-ui-sidline-enable t
	lsp-ui-flycheck-list-position 'right
	lsp-ui-flycheck-live-reporting t
	lsp-ui-peek-enable t
	lsp-ui-peek-list-width 60
	lsp-ui-peek-peek-height 40)
  (general-add-hook 'lsp-mode-hook (list 'lsp-ui-mode)))

					; remote ops

(defun remote-shell-specifics ()
  (when (and (fboundp 'company-mode)
	     (file-remote-p default-directory))
    (company-mode -1)))
(general-add-hook 'shell-mode-hook #'remote-shell-specifics)

					;LISP ADD ONS

(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode t))
(use-package rainbow-delimiters :straight t)

					;SLIME
(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))


					;RACKET

(use-package racket-mode
  :straight t
  :config
  (setq racket-documentation-search-location 'local
	racket-images-inline t)
  (general-add-hook (list 'racket-mode-hook 'racket-repl-mode-hook)
		    (list #'rainbow-delimiters-mode)))


					;ELISP

(general-add-hook 'emacs-lisp-mode-hook
		  (list 'smartparens-mode ;;use (kbd `C-q '`) for single quoting
			'rainbow-delimiters-mode))

					;BLOG


(general-define-key
 "C-c b" 'blog)
(use-package easy-hugo
  :straight t
  :config
  (setq easy-hugo-basedir "/mnt/c/Users/Raj Patil/source/rajpatil.dev/")
  (setq easy-hugo-url "https://rajpatil.dev")
  (setq easy-hugo-root "/usr/local/bin")
  (defun blog ()
    "Open blogging workspace"
    (interactive)
    (let ((blog-dir "~/links/source/rajpatil.dev"))
      (message (concat "opening blogging workspace @ " blog-dir))
      (find-file blog-dir)))
  (general-define-key
   :prefix "C-c"
   "b" 'easy-hugo-newpost
   "C-b" #'blog ) )


					;DICTIONARY
(use-package define-word
  :straight t
  :config
  (general-define-key
   :prefix "C-c"
   "d" #'define-word-at-point
   "D" #'define-word))

					;LATEX

;;install auctex from package-list-packages
;;don't use the github mirror via straight
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(general-add-hook 'TeX-after-compilation-finished-functions
		  #'TeX-revert-document-buffer)

(setq-default TeX-master nil)
(general-add-hook 'Latex-mode-hook
		  (list #'LaTeX-math-mode
			#'turn-on-reftex
			#'flyspell-mode))

					;MARKDOWN

(use-package markdown-mode
  :straight t
  :config 
  (set-fill-column 40)
  (general-add-hook 'markdown-mode-hook
		    (list #'nlinum-relative-mode
			  #'auto-fill-mode
			  #'flyspell-mode)))


;;self appends
;; custom-set-vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5379937b99998e0510bd37ae072c7f57e26da7a11e9fb7bced8b94ccc766c804" "a3bdcbd7c991abd07e48ad32f71e6219d55694056c0c15b4144f370175273d16" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" default))
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(lsp-ui-doc-position 'at-point)
 '(org-agenda-files
   '("/mnt/c/Users/Raj Patil/source/org/gtd/GTD_base.org" "/mnt/c/Users/Raj Patil/source/org/gtd/GTD_HQ.org"))
 '(warning-suppress-types '((comp))))
   
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
