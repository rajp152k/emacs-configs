(set-frame-font "CaskaydiaCove NF" nil t)
;;no, thank you
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

					;CL WARNINGS

(setq byte-compile-warnings '(cl-functions))

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


					;GENERAL

(use-package general
  :straight t)


					;DASHBOARD

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t))

					;EVIL ENV

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
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
  "M-o" #'helm-occur
  "C-x C-f" #'helm-find-files)
  (helm-mode 1))

					;FILE MANAGEMENT : DIRED-X

(setq find-file-visit-truename t)
(general-add-hook 'dired-load-hook
		(list (lambda ()
			(load "dired-x")
			;; Set dired-x global variables here.  For example:
			;; (setq dired-guess-shell-gnutar "gtar")
			;; (setq dired-x-hands-off-my-keys nil)
			)))

					; TRAMP

(setq tramp-default-method "ssh")

					;AESTHETICS

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-challenger-deep))
(use-package darkroom
  :straight t)

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

					;Winner mode
(use-package winner
  :straight t
  :config
  (winner-mode 1)
  (general-define-key
   "<C-M-left>" #'winner-undo
   "<C-M-right>" #'winner-redo))

					;ACE-JUMP

(use-package ace-jump-mode
  :straight t
  :config
  (general-define-key
   "C-M-j" 'ace-jump-mode))


					;WHICH-KEY

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;;browsing
;;(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-generic-program "/mnt/c/Program\ Files\ \(x86\)/Microsoft/Edge/Application/msedge.exe")
(general-define-key
 "C-c t" #'toggle-truncate-lines)

					;PDF INTEROP

(use-package pdf-tools
  :straight t
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
   "c" #'org-capture)
  (setq org-directory "~/links/source/org")
  (setq org-default-notes-file (concat org-directory "/gtd/GTD_HQ.org"))
  (setq org-startup-with-inline-images t)
  (setq org-startup-truncated nil)
  (general-add-hook 'org-mode-hook
		    (list #'toggle-word-wrap
			  #'nlinum-relative-mode))
  (defun update-org-latex-fragments ()
    (org-latex-preview '(64))
    (plist-put org-format-latex-options :scale (* 1.5 text-scale-mode-amount))
    (org-latex-preview '(16)))
  (general-add-hook 'text-scale-mode-hook
		  (list #'update-org-latex-fragments))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (general-define-key
   :prefix "C-c C-x"
   "C-g" 'org-clock-goto))

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

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 10)))

(setq org-capture-templates
      '(("n" "Next Action" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "NA")
         "* TODO %?\n  %i\n  %a")
        ("i" "IN" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "INQ")
         "* %?\nEntered on %U\n  %i\n  %a")))




(require 'server)
(unless (server-running-p)
  (server-start))
;(use-package org-protocol
;  :straight t
;  :config
;  (add-to-list 'org-capture-templates
;               '("p" "Protocol" entry (file "")
;                 "* %?[[%:link][%:description]] %U\n%i\n" :prepend t))
;  (add-to-list 'org-capture-templates
;               '("L" "Protocol Link" entry (file "")
;                 "* %?[[%:link][%:description]] %U\n" :prepend t)))

					;GTD

(defun gtd()
  "open the GTD workspace"
  (interactive)
  (let ((gtd-dir (concat org-directory "/gtd/GTD_HQ.org")))
    (message (concat "opening GTD workspace @ " gtd-dir))
    (find-file gtd-dir)))

(general-define-key
 :prefix "C-c"
 "g" #'gtd)



					;ORG-ROAM


(use-package org-roam
  :straight t
  :config
  (setq org-roam-directory "~/links/source/org/org-roam")
  (general-define-key
   :prefix "C-c n"
   "l" #'org-roam
   "f" #'org-roam-find-file
   "g" #'org-roam-graph
   "i" #'org-roam-insert
   "I" #'org-roam-insert-immediate
   "c" #'org-roam-capture
   "r" #'org-roam-buffer-toggle-display)
  (executable-find "sqlite3")
  (general-add-hook 'after-init-hook 'org-roam-mode)
  (setq org-roam-index-file
	(concat org-roam-directory "/Index.org"))
  (setq org-roam-db-update-method 'immediate)
  (setq org-roam-tag-sources '(prop vanilla all-directories))
  (setq org-roam-link-use-custom-faces 'everywhere)
  (setq org-roam-buffer-position 'right)
  (setq org-roam-buffer-window-parameters
	'((no-delete-other-windows . t))))

(require 'org-roam-protocol)

(use-package org-roam-server
  :straight t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8081
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package org-roam-bibtex
  :straight t 
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (general-add-hook 'org-roam-mode 'org-roam-bibtex-mode)
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

					;COMPANY

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.3)
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
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log-error")))

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
	lsp-ui-peek-enable t
	lsp-ui-peek-list-width 60
	lsp-ui-peek-peek-height 25)
  (general-add-hook 'lsp-mode-hook (list 'lsp-ui-mode)))

					;LISP ADD ONES

(use-package smartparens :straight t)
(use-package rainbow-delimiters :straight t)

					;RACKET

(use-package racket-mode
  :straight t
  :config
  (general-add-hook 'racket-mode-hook
	  	  (list 'smartparens-mode ;;use (kbd `C-q '`) for single quoting
			'rainbow-delimiters-mode)))


					;ELISP

(general-add-hook 'emacs-lisp-mode-hook
		  (list 'smartparens-mode ;;use (kbd `C-c '`) for single quoting
			'rainbow-delimiters-mode))

					;BLOG

(defun blog ()
  "Open blogging workspace"
  (interactive)
  (let ((blog-dir "~/links/source/blog/rajp152k.github.io"))
    (message (concat "opening blogging workspace @ " blog-dir))
    (find-file blog-dir)))

(general-define-key
 "C-c b" 'blog)

					;DICTIONARY
(use-package define-word
  :straight t
  :config
  (general-define-key
   :prefix "C-c"
   "d" #'define-word-at-point
   "D" #'define-word))


					;MARKDOWN

(use-package markdown-mode
  :straight t
  :config 
  (general-add-hook 'markdown-mode-hook
		  (list 'nlinum-relative-mode)))

					;MAIL



;;self appends
;; custom-set-vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" default))
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files
   '("/mnt/c/Users/Raj Patil/source/org/gtd/GTD_HQ.org" "/mnt/c/Users/Raj Patil/source/org/gtd/events.org")))
   
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
