;; insert simple templates with

;;   :::::::::::       :::::::::::         :::   ::: 
;;      :+:               :+:            :+:+: :+:+: 
;;     +:+               +:+           +:+ +:+:+ +:+ 
;;    +#+               +#+           +#+  +:+  +#+  
;;   +#+               +#+           +#+       +#+   
;;  #+#               #+#           #+#       #+#    
;; ###           ###########       ###       ###     


;; OVERVIEW

;; Set global or major-mode-specific templates via setting "tim-temps",
;; an alist which should look like
;; -- ((global . (template template template)) (poggers-mode . (template ... )) ...)


;; Templates are cons cells which look like
;; -- ("fr" . ("\\frac{" 1 "}{" 2 "}" 0))
;; -- ("givetime" . ("heya it is " (insert (current-time-string)) 0))
;;
;; So, it will look for 1, 2, etc. and running out it will go to 0.
;; Check out "tim-next-part" and the last part in "tim-insert" for clarification.


;; If there are any numbers, 0 must be one of them
;; You leave tim-insert-mode by reaching 0 or typing C-c C-c or undoing lots
;; Text inserted from template gets indented.
;; For mirroring, look into text-clone-create.

;; Nested Expansion
;;
;; A complication comes from the fact that while expanding one
;; template, one can start expanding another. In this situation, only
;; the most recently expanded template is active. The "paused in the
;; middle of expansion" templates have their state stored in a
;; "stack": the list "tim-nested-state".


;; CODE


;; User options.
;; Options involving key bindings should be set before running this file.

(defvar tim-expand-key (kbd "TAB")
  "The key used for template expansion.")

(defvar tim-next-key (kbd "TAB")
  "The key used for moving to the next field in a template.")

(defvar tim-prev-key (kbd "<backtab>")
  "The key used for moving to the previous field in a template.")

(defvar tim-temps nil
  "Alist of alist of templates to expand. first key: a major
mode (or the symbol \"global\"). second key: word to trigger
expansion of template. value: template.")

;; Store some state

(setq tim-current-pos nil)  ; the marker we're at
(setq tim-jump-markers nil) ; list of entries "(index . marker)", and maybe
(setq tim-nested-state nil) ; a stack of current-pos and jump-markers for nested templates

;; ------- Insert template ----------------------------

(defun tim-insert (temp)
  "Insert the template TEMP."

  ;; If already in insert-mode, deal with nested templates.
  ;; Else, make sure tim-nested-state = nil.

  (if tim-insert-mode
      (push `(,tim-current-pos . ,tim-jump-markers) tim-nested-state)
    (setq tim-nested-state nil))

  ;; Undo behavior: undo past here --> finish this template.
  ;; Slightly confusing.

  (push `(apply (lambda nil (if tim-insert-mode (tim-finish))))
        buffer-undo-list)


  ;; Set jump-markers to nil

  (setq tim-jump-markers nil)

  ;; Insert template.

  (let ((text) (a) (next) (old-loc))
    (while temp
      (setq text (car temp)
            temp (cdr temp))
      (cond
       ;; possibility 1: string
       ((stringp text)
        (progn (setq old-loc (point))
               (insert text)
               (indent-region old-loc (point))))
       ;; possibility 2: field for input (marker)
       ((integerp text)
        (progn (setq a (make-marker))
               (set-marker a (point))
               (push (cons text a) tim-jump-markers)))
       ;; possibility 3: something to evaluate
       (eval text))
      )

    ;; Setup for fields. Call tim-finish if no marker 0.
    ;; Else try going to marker 1, if there is none go to marker 0.
    ;; Call tim-insert-mode if going to 1, tim-finish if going to 0.

    (if (assq 0 tim-jump-markers)
        (if (setq next (assq 1 tim-jump-markers))
            (progn (tim-insert-mode 1)
                   (setq tim-current-pos 1)
                   (goto-char (cdr next)))
          (progn (goto-char (cdr (assq 0 tim-jump-markers)))
                 (tim-finish)))
      (tim-finish))))

;; ----- Finish expanding current template ------------------

(defun tim-finish nil
  (if tim-nested-state
      (let ((old-state (pop tim-nested-state)))
        (setq tim-current-pos  (car old-state)
              tim-jump-markers (cdr old-state)))
    (tim-insert-mode -1)))

;;; ----- Move between fields ------------------------------

(defun tim-next-part (arg)
  (interactive "p")
  ;; if going indeed to the "next" fields
  (if (> arg 0)
      (let* ((next-pos (+ tim-current-pos 1))
             (next (assq next-pos tim-jump-markers)))
        (if next
            (progn (setq tim-current-pos next-pos)
                   (if (= arg 1)
                       (goto-char (cdr next))
                     (tim-next-part (- arg 1))))
          (progn (setq tim-current-pos 0)
                 (goto-char (cdr (assq 0 tim-jump-markers)))
                 (tim-finish))))
    )
  ;; if instead going backwards
  (if (< arg 0)
      (let ((next-pos (max (- tim-current-pos 1) 1))) ;; if at 1, stay at 1
        ;; next-pos is guaranteed to have a marker
        (setq tim-current-pos next-pos)
        (if (= arg -1)
            (goto-char (cdr (assq next-pos tim-jump-markers)))
          (tim-next-part (+ arg 1)))))
  )

(defun tim-prev-part (arg)
  (interactive "p")
  (tim-next-part (- arg)))

;;; ----- Expand template for word at point ----------------

(defun tim-try-expand ()
  "Should be called via `tim-expand-key'."
  (interactive)
  (or (tim-try-expand-point-in-mode major-mode)
      (tim-try-expand-point-in-mode 'global)
      ;; else do the normal keybind for tim-expand-key
      (let* ((tim-mode nil)
             (func (key-binding tim-expand-key)))
        (if func (call-interactively func)
          (execute-kbd-macro tim-expand-key) ; a (less good?) alternative
          ))
      ))

(defun tim-try-expand-point-in-mode (mode)
  "Try to expand word at point in MODE. Returns true if successful, nil if not."
  (let ((word (thing-at-point 'word 'no-properties)) mode-temps temp)
     (if (and (setq mode-temps (assoc mode tim-temps))
              (setq temp (assoc word (cdr mode-temps))))
         (progn (tim-helper/kill-thing-at-point 'word)
                (tim-insert (cdr temp))
                t)
       nil)
    ))

(defun tim-helper/kill-thing-at-point (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds (kill-region (car bounds) (cdr bounds)))))

;;; ----- Minor modes --------------------------------

;; We can leave tim-insert-mode by tabbing through fields, undoing
;; enough times, or by typing C-c C-c

(define-minor-mode tim-insert-mode
  "Minor mode activated when using fields in tim templates."
  :lighter (:eval (propertize (format " %s tem" tim-depth)
                              'font-lock-face '(:foreground "red")))
  :keymap `((,tim-next-key . tim-next-part)
            (,tim-prev-key . tim-prev-part)
            (,(kbd "C-c C-c") . ,(lambda nil (interactive)
                                   (tim-insert-mode -1)))
            ))

;; should be defined after tim-insert-mode, we want bindings here to take precendence
(define-minor-mode tim-mode
  "Minor mode for inserting templates."
  :lighter " tim"
  :keymap `((,tim-expand-key . tim-try-expand))
  )

(define-globalized-minor-mode tim-global-mode
  tim-mode
  (lambda nil (tim-mode 1)))

(provide 'tim)
