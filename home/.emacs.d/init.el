(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
	    (shell-command-to-string "source $HOME/.zshenv && printf $PATH")))

(setq exec-path (split-string (getenv "PATH") ":"))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(defun ensure-package-installed (&rest packages)
    "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       ;; (package-installed-p 'evil)
       (if (package-installed-p package)
	   nil
	 (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	     (package-install package)
	   package)))
     packages))

(package-initialize)

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed
 'ack
 'expand-region
 'haskell-mode
 'helm
 'helm-projectile
 'helm-ag
 'magit
 'markdown-mode
 'markdown-mode+
 'markdown-toc
 'projectile
 'rust-mode
 'solarized-theme
 'web-mode
 'yaml-mode
 'yasnippet
 )

(package-initialize)

(add-to-list 'load-path "/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/share/emacs/site-lisp/gnus")
(load-file "~/elisp/ProofGeneral-4.2/generic/proof-site.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-command "ack-grep --noenv")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(magit-use-overlays nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ditaa . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(proof-assistants (quote (coq pghaskell)))
 '(send-mail-function (quote smtpmail-send-it))
 '(tool-bar-mode nil)
 '(web-mode-enable-auto-quoting nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)

;; clean up the screen
(tool-bar-mode -1)

(eval-after-load "haskell-mode"
  '(progn
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-set-key (kbd "<f12>") 'magit-status)

(load-theme 'solarized-dark)

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(display-battery-mode 1)
(display-time-mode 1)

(setq gnus-treat-fill-long-lines nil)

(add-hook 'message-setup-hook
  (lambda ()
    (setq
     truncate-lines nil
     word-wrap t
     use-hard-newlines t)))

(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)

(setenv "ACK_PAGER_COLOR" "")

(defun jec/markdown-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'markdown-mode-hook 'jec/markdown-hook)

(desktop-save-mode)

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun jec/web-mode-hook ()
  (interactive)
  (setq tab-width 8))

(add-hook 'web-mode-hook 'jec/web-mode-hook)
