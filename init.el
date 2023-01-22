;; -----------------------------------------
;; general stuff
;; -----------------------------------------

;; backups in single directory

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".saves"))))

;; keybinding

(global-set-key  [f2] 'shell)
(global-set-key  [f5 f5] 'revert-buffer)
(global-set-key  "\C-cr" 'recentf-open-files)
(global-set-key  [?\C-\'] 'goto-line)
(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs)

;; variables

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq sentence-end-double-space nil
      dired-dwim-target t
      completion-ignore-case t
      display-line-numbers-type 'relative
      read-buffer-completion-ignore-case t
      delete-by-moving-to-trash t
      completion-styles '(basic partial-completion substring flex emacs22))

;; minor modes, hooks

(recentf-mode 1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; search

(setq lazy-highlight-initial-delay 0.10
      isearch-lazy-count t
      lazy-count-suffix-format " {%s/%s}"
      lazy-count-prefix-format nil)

;; garbage collect

(setq gc-cons-threshold #x4000000)
(defvar k-gc-timer (run-with-idle-timer 15 t (lambda nil (garbage-collect))))

;; ui
(menu-bar-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1)
(setq default-frame-alist (append `((background-color . "black")
                                    (foreground-color . "peach puff")
                                    (cursor-color . "pink"))
                                  default-frame-alist))
;; (load-theme 'cherry-blossom t)


;; functions

(defun gogo (filename program)
  (interactive (list (file-relative-name (read-file-name "what file: "))
                     (read-string "what program: " nil nil "xdg-open")))
  (call-process-shell-command
   (concat "nohup " program " \"" filename "\" &") nil 0))
(global-set-key "\C-co" 'gogo)

;; -----------------------------------------
;; packages
;; -----------------------------------------

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; jump to words

(autoload 'ace-jump-mode "ace-jump-mode" nil t)
(global-set-key [?\C-\;] 'ace-jump-mode)

;; minibuffer show stuff vertical

(require 'vertico)
(vertico-mode 1)

;; template

(require 'tim)
(tim-global-mode)

;; ;; melpa
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (package-refresh-contents)
;; (package-install 'dumb-jump)
;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


;; -----------------------------------------
;; C, Java
;; -----------------------------------------

(setq-default c-basic-offset 4)
(add-hook 'java-mode-hook 'subword-mode)

;; -----------------------------------------
;; Python
;; -----------------------------------------

(setq python-shell-interpreter "python3")

;; -----------------------------------------
;; Latex
;; -----------------------------------------

(add-hook 'tex-mode-hook 'latex-electric-env-pair-mode)
(add-hook 'tex-mode-hook 'display-line-numbers-mode)

;; get pdf

(setq pdfify "pdflatex") ; can specify with buffer-local variable

(defun go-latexmk nil
  (interactive)
  (async-shell-command (concat "latexmk -pdf -pvc " (buffer-file-name))
                       (generate-new-buffer "latexmk")))

(defun go-pdfify nil
  (interactive)
  (async-shell-command (concat "yes \"\" | " pdfify " " (buffer-file-name))))

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*" display-buffer-no-window))

(require 'tex-mode)
(define-key tex-mode-map "\C-c\C-l" 'go-pdfify)

;; latex templates

(require 'tim)
(add-to-list
 'tim-temps
 '(latex-mode
   .
   (
    ("fr"   . ("\\frac{" 1 "}{" 2 "}" 0))
    ("lr"   . ("\\left" 1 " " 0 " \\right" 2))
    ("choo" . ("{" 1 " \\choose " 2 "}" 0))
    ("fp"   . ("\\frac{\\partial" 1 "}{\\partial" 2 "}" 0))
    ("eq"   . ("\\[\n" 0 "\n\\]"))
    ("inl"  . ("\\(\n" 0 "\n\\)"))
    ("te"   . ("\\text{" 0 "}"))
    ("eqv"  . ("\\equiv "))
    ("mbf"  . ("\\mathbf "))
    ("mb"   . ("\\mathbb "))
    ("ms"   . ("\\mathscr "))
    ("mc"   . ("\\mathcal "))
    ("qq"   . ("\\quad\\quad"))
    ("ra"   . ("\\rightarrow "))
    ("sm"   . ("\\setminus "))
    ("se"   . ("\\subseteq "))
    ("sb"   . ("\\subset "))
    ("xx"   . ("\\cdot "))
    )))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(markdown-mode lua-mode dumb-jump)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
