
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
    "C-s" ; for super bindings
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

(use-package golden-ratio
    :straight t
    :config
    (setq golden-ratio-extra-commands
	  (append golden-ratio-extra-commands
		  '(evil-window-left
		    evil-window-right
		    evil-window-up
		    evil-window-down
		    buf-move-left
		    buf-move-right
		    buf-move-up
		    buf-move-down
		    window-number-select
		    ace-jump-char-mode
		    racket-repl
		    select-window
		    select-window-1
		    select-window-2
		    select-window-3
		    select-window-4
		    select-window-5
		    select-window-6
		    select-window-7
		    select-window-8
		    select-window-9)))

    (golden-ratio-mode 1))



					;PRETTIFY W/ LIGATURES





(global-prettify-symbols-mode 1)
(general-add-hook
 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("def" .      #x2131)
           ("not" .      #x2757)
           ("in" .       #x2208)
           ("not in" .   #x2209)
           ("return" .   #x27fc)
           ("yield" .    #x27fb)
           ("for" .      #x2200)
           ;; Base Types
           ("int" .      #x2124)
           ("float" .    #x211d)
           ("str" .      #x1d54a)
           ("True" .     #x1d54b)
           ("False" .    #x1d53d)
           ;; Mypy
           ("Dict" .     #x1d507)
           ("List" .     #x2112)
           ("Tuple" .    #x2a02)
           ("Set" .      #x2126)
           ("Iterable" . #x1d50a)
           ("Any" .      #x2754)
           ("Union" .    #x22c3)))))


(use-package pretty-mode
  :straight t
  :config
  (global-pretty-mode t)
  (pretty-deactivate-groups
   '(:equality :ordering :ordering-double :ordering-triple
	       :arrows :arrows-twoheaded :punctuation
	       :logic :sets))
  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic-nary)))

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
  ;(add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
  ;(add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
  ;(add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
  ;(purpose-compile-user-configuration))
  ;(purpose-mode 1))
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

					;ACE-JUMP

(use-package ace-jump-mode
  :straight t
  :config
  (general-define-key
   "C-M-j" 'ace-jump-char-mode))


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
   :prefix "C-c"
   "r" #'org-refile
   "C-x C-g" #'org-clock-goto))

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
	("e" "Event" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "INQ")
         "* %?\nSCHEDULED: %T\n  %i")
        ("i" "IN" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "INQ")
         "* %?\nEntered on %U\n  %i\n  %a")
	("t" "Tickler" entry (file+headline "~/links/source/org/gtd/GTD_HQ.org" "Tickler")
	 "* %?\nDEFER THOUGHT TO: %T\n %i"))) 




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
  :straight (org-roam :host github
		      :repo "org-roam/org-roam"
		      :branch "v2")
  :config
  (setq org-id-method 'ts)
  (setq org-roam-directory (file-truename "/mnt/c/Users/Raj Patil/source/org/org-roam/"))
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
   "o a" #'orb-note-actions
   "b" #'helm-bibtex
   "o i" #'orb-insert-link
   "t d" #'org-roam-tag-remove
   "r" #'org-roam-buffer-toggle
   "a a" #'org-roam-alias-add
   "a d" #'org-roam-alias-remove
   "n o" #'org-noter)
  (add-to-list 'display-buffer-alist
	       '(; org-roam buffer toggle config
		 (".org-roam.*"
		  (display-buffer-in-side-window)
		  (window-width . 0.25)
		  (side . left)
		  (slot 0)))))


;(require 'org-roam-protocol)

;(use-package org-roam-server
;  :straight t
;  :config
;  (general-define-key
;   "C-c n s" #'org-roam-server-mode)
;  (setq org-roam-server-host "127.0.0.1"
;        org-roam-server-port 8081
;        org-roam-server-authenticate nil
;        org-roam-server-export-inline-images t
;        org-roam-server-serve-files nil
;        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;        org-roam-server-network-poll t
;        org-roam-server-network-arrows nil
;        org-roam-server-network-label-truncate t
;        org-roam-server-network-label-truncate-length 60
;        org-roam-server-network-label-wrap-length 20))


					;Research-workflow
(setq zot_bib (file-truename "~/Zotero/My Library.bib")
      zot_store (file-truename "~/Zotero/storage"))

(use-package helm-bibtex
  :straight t
  :config
  (setq
   bibtex-completion-notes-path org-roam-directory
   bibtex-completion-bibliography zot_bib
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n"
    )))

(use-package org-ref
  :straight t
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography (list zot_bib)
   org-ref-bibliography-notes (concat org-roam-directory "bibnotes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory org-roam-directory
   org-ref-notes-function 'orb-edit-notes
   ))

(use-package org-roam-bibtex
  :straight '(org-roam-bibtex
	      :type git
	      :host github
	      :repo "org-roam/org-roam-bibtex"
	      :branch "org-roam-v2") 
  :after org-roam
  :config
  (general-add-hook 'org-roam-mode #'org-roam-bibtex-mode)
  (setq org-roam-bibtex-preformat-keywords
	'("=key=" "title" "url" "file" "author-or-editor" "keywords" ))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

(use-package org-noter
  :straight t
  :config
  (setq
   org-noter-always-create-frame nil
   org-noter-notes-window-location 'other-buffer
   org-noter-hide-other nil
   org-noter-notes-search-path (list zot_store org-roam-directory)))
   
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
  (setq lsp-clients-clangd-args '("-j=4" "-background-index")
	lsp-clients-clangd-executable "clangd"))

					;(use-package lsp-pyright
					;  :straight t
					;  :config
					;  (general-add-hook 'python-mode #'(lambda () (lsp)) ))

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
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-include-signature t
	lsp-ui-sidline-enable t
	lsp-ui-flycheck-list-position 'right
	lsp-ui-flycheck-live-reporting t
	lsp-ui-peek-enable t
	lsp-ui-peek-enable t
	lsp-ui-peek-list-width 60
	lsp-ui-peek-peek-height 25)
  (general-add-hook 'lsp-mode-hook (list 'lsp-ui-mode)))

					; remote ops

(defun remote-shell-specifics ()
  (when (and (fboundp 'company-mode)
	     (file-remote-p default-directory))
    (company-mode -1)))
(general-add-hook 'shell-mode-hook #'remote-shell-specifics)

(use-package tramp
  :straight t
  :config
  (setq tramp-default-method "ssh"
	vc-ignore-dir-regexp (format "%s\\|%s"
				     vc-ignore-dir-regexp
				     tramp-file-name-regexp)
	tramp-verbose 4)

  (defun gpu_dgx_50.93 ()
    "ssh into the .50.93 DGX station"
    (interactive)
    (add-to-list 'tramp-remote-path "/raid/cs18btech11039/anaconda3/bin")
    (find-file "/ssh:cs18btech11039@192.168.50.93:/home/cs18btech11039"))

  (defun gpu_v100_209.54 ()
    "ssh into the .209.54 v100 device"
    (interactive)
    (add-to-list 'tramp-remote-path "/home/cs18btech11039/miniconda3/bin")
    (find-file "/ssh:cs18btech11039@192.168.209.54:/home/cs18btech11039"))

  ;;Remote python lsp tramp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
		    :major-modes '(python-mode)
		    :remote? t
		    :server-id 'pyls-remote)))

					;LISP ADD ONS

(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode t))
(use-package rainbow-delimiters :straight t)

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
  (set-fill-column 60)
  (general-add-hook 'markdown-mode-hook
		    (list #'nlinum-relative-mode
			  #'auto-fill-mode
			  #'flyspell-mode)))

					;MAIL



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
