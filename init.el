;;; ----- etc folder -----
(defun etc (str) (concat user-emacs-directory "etc/" str))
(setq custom-file                (etc "custom.el")
      auto-save-list-file-prefix (etc "auto-save-list/")
      backup-directory-alist     `(("." . ,(etc ".saves/")))
      recentf-save-file          (etc "recentf"))

;;; ----- keys -----
(global-set-key  (kbd "<f2>")      'shell)
(global-set-key  (kbd "<f5> <f5>") 'revert-buffer)
(global-set-key  (kbd "<f5> <f6>") 'eval-buffer)
(global-set-key  (kbd "C-c r")     'recentf-open-files)
(global-set-key  (kbd "C-'")       'goto-line)
(global-set-key  (kbd "M-j")       (lambda () (interactive) (join-line -1)))
(global-set-key  (kbd "C-c f")     'grep-find)
(global-set-key  (kbd "C-x C-c")   'save-buffers-kill-emacs)


;;; ----- settings -----
(setq-default indent-tabs-mode           nil)  ; 2 spaces for indent
(setq-default tab-width                  2)    ;
(setq scroll-step                        8)    ; scrolling
(setq sentence-end-double-space          nil)  ;
(setq dired-dwim-target                  t)    ; ez move file between folders
(setq completion-ignore-case             t)    ; caseless minibuffer completion
(setq read-buffer-completion-ignore-case t)    ; ... except filenames
(setq display-line-numbers-type          t)    ; global line numbers
(setq delete-by-moving-to-trash          t)    ; recycle bin
(setq completion-styles                        ;
      '(basic partial-completion substring flex emacs22))


;;; ----- garbage collection -----
(setq gc-cons-threshold #x4000000)
(defvar k-gc-timer (run-with-idle-timer 15 t (lambda nil (garbage-collect))))


;;; ----- searching -----
(setq lazy-highlight-initial-delay 0.10
      isearch-lazy-count t
      lazy-count-suffix-format " {%s/%s}"
      lazy-count-prefix-format nil)

;;; ----- turn on off -----
(recentf-mode 1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(if (>= emacs-major-version 26)
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'linum-mode))
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;;; ----- ui -----
(menu-bar-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1)
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp"))
;; (load-theme 'cherry-blossom t)
(package-install 'nyan-mode)
(nyan-mode)(nyan-start-animation)(nyan-toggle-wavy-trail)

(setq fancy-splash-image (expand-file-name "~/.emacs.d/nadeko.jpg"))

(setq zone-timer (run-with-idle-timer
                  1800 t
                  (lambda nil (interactive) (when (= 0 (random 3)) (zone)))))


;;; ----- useful functions -----
;; open file in external program
(defun hh-open (filename program)
  (interactive (list (file-relative-name (read-file-name "what file: "))
                     (read-string "what program: " nil nil "xdg-open")))
  (call-process-shell-command
   (concat "nohup " program " \"" filename "\" &") nil 0))
(global-set-key "\C-co" 'hh-open)


;; get recursive folder size in dired file browser
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))


;;; ----- load elisp -----
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (package-refresh-contents)

;; jump in windows
(autoload 'ace-jump-mode "ace-jump-mode" nil t)
(global-set-key [?\C-\;] 'ace-jump-mode)

;; template
(require 'tem)
(tem-global-mode)

;; multiple cursor
(package-install 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; vertical minibuffer
(when (>= emacs-major-version 27)
  (package-install 'vertico)
  (vertico-mode)
  (package-install 'consult)
  (global-set-key "\C-cf" 'consult-grep)
  )

;; dumb jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


;;; ----- languages / misc berk -----

;; C
(setq-default c-basic-offset 4)

;; Java
(add-hook 'java-mode-hook 'subword-mode)

;; Python
(setq python-shell-interpreter "python3")

;; Julia
(add-to-list 'load-path  "/home/zeng/.juliaup/bin")
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(setq julia-repl-executable-records '((default "julia")                  ; in the executable path
                                       (master ,(expand-file-name "~/.juliaup/bin/julia"))))

;; Latex
(add-hook 'tex-mode-hook 'latex-electric-env-pair-mode)
(add-hook 'tex-mode-hook 'display-line-numbers-mode)

(setq pdfify "pdflatex") ; can specify with buffer-local variable

(defun hh-pdfify nil
  (interactive)
  (async-shell-command (concat "yes \"\" | " pdfify " " (buffer-file-name))))

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*" display-buffer-no-window))

(require 'tex-mode)
(define-key tex-mode-map "\C-c\C-l" 'hh-pdfify)

;; templates
(require 'tem)
(add-to-list
 'tem-temps
 '(latex-mode
   .
   (
    ("fr"   . ("\\frac{" 1 "}{" 2 "}" 0))
    ("lr"   . ("\\left" 1 " " 0 " \\right" 2))
    ("choo" . ("{" 1 " \\choose " 2 "}" 0))
    ("ee"   . ("\\[\n" 0 "\n\\]"))
    ("eq"   . ("\\begin{equation}\n" 0 "\n\\end{equation}"))
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

(add-to-list
 'tem-temps
 '(emacs-lisp-mode
   .
   (
    ("yo"   . (";;; ----- " 1 " -----" 0))

    )))
