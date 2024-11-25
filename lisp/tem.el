;; insert templates

;; USAGE

;; Set global or major-mode-specific templates via setting "tem-temps",
;; an alist which should look like
;; -- ((global . (template template template)) (poggers-mode . (template ... )) ...)

;; Templates are cons cells which look like
;; -- ("fr" . ("\\frac{" 1 "}{" 2 "}" 0))
;; -- ("time" . ("heya it is " (insert (current-time-string)) 0))
;;
;; So, it will look for 1, 2, etc. and running out it will go to 0.
;; Check out "tem-next-part" and the last part in "tem-insert" for clarification.

;; If there are any numbers, 0 must be one of them
;; You leave tem-insert-mode by reaching 0 or typing C-c C-c or undoing lots
;; Text inserted from template gets indented.

;; Nested Expansion
;;
;; A complication comes from the fact that while expanding one
;; template, one can start expanding another. In this situation, only
;; the most recently expanded template is active. The "paused in the
;; middle of expansion" templates have their state stored in a
;; "stack": the list "tem-nested-state".



;; User options.

(defvar tem-expand-key (kbd "TAB")
  "The key used for template expansion.")

(defvar tem-next-key (kbd "TAB")
  "The key used for moving to the next field in a template.")

(defvar tem-prev-key (kbd "<backtab>")
  "The key used for moving to the previous field in a template.")

(defvar tem-temps nil
  "Alist of alist of templates to expand. first key: a major
mode (or the symbol \"global\"). second key: word to trigger
expansion of template. value: template.")

;; Store some state

(setq tem-current-pos nil)  ; the marker we're at
(setq tem-jump-markers nil) ; list of entries "(index . marker)", and maybe
(setq tem-nested-state nil) ; a stack of current-pos and jump-markers for nested templates

;; ------- Insert template ----------------------------

(defun tem-insert (temp)
  "Insert the template TEMP."

  ;; If already in insert-mode, deal with nested templates.
  ;; Else, make sure tem-nested-state = nil.

  (if tem-insert-mode
      (push (cons tem-current-pos tem-jump-markers) tem-nested-state)
    (setq tem-nested-state nil))

  ;; Undo behavior: undo past here --> finish this template.
  ;; Slightly confusing.

  (push '(apply (lambda nil (if tem-insert-mode (tem-finish))))
        buffer-undo-list)

  ;; Set jump-markers to nil

  (setq tem-jump-markers nil)

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
               (push (cons text a) tem-jump-markers)))
       ;; possibility 3: something to evaluate
       (eval text))
      )

    ;; Setup for fields. Call tem-finish if no marker 0.
    ;; Else try going to marker 1, if there is none go to marker 0.
    ;; Call tem-insert-mode if going to 1, tem-finish if going to 0.

    (if (assq 0 tem-jump-markers)
        (if (setq next (assq 1 tem-jump-markers))
            (progn (tem-insert-mode 1)
                   (setq tem-current-pos 1)
                   (goto-char (cdr next)))
          (progn (goto-char (cdr (assq 0 tem-jump-markers)))
                 (tem-finish)))
      (tem-finish))))

;; ----- Finish expanding current template ------------------

(defun tem-finish nil
  (if tem-nested-state
      (let ((old-state (pop tem-nested-state)))
        (setq tem-current-pos  (car old-state)
              tem-jump-markers (cdr old-state)))
    (tem-insert-mode -1)))

;;; ----- Move between fields ------------------------------

(defun tem-next-part (arg)
  (interactive "p")
  ;; if going indeed to the "next" fields
  (if (> arg 0)
      (let* ((next-pos (+ tem-current-pos 1))
             (next (assq next-pos tem-jump-markers)))
        (if next
            (progn (setq tem-current-pos next-pos)
                   (if (= arg 1)
                       (goto-char (cdr next))
                     (tem-next-part (- arg 1))))
          (progn (setq tem-current-pos 0)
                 (goto-char (cdr (assq 0 tem-jump-markers)))
                 (tem-finish))))
    )
  ;; if instead going backwards
  (if (< arg 0)
      (let ((next-pos (max (- tem-current-pos 1) 1))) ;; if at 1, stay at 1
        ;; next-pos is guaranteed to have a marker
        (setq tem-current-pos next-pos)
        (if (= arg -1)
            (goto-char (cdr (assq next-pos tem-jump-markers)))
          (tem-next-part (+ arg 1)))))
  )

(defun tem-prev-part (arg)
  (interactive "p")
  (tem-next-part (- arg)))

;;; ----- Expand template for word at point ----------------

(defun tem-try-expand ()
  "Should be called via `tem-expand-key'."
  (interactive)
  (or (tem-try-expand-point-in-mode major-mode)
      (tem-try-expand-point-in-mode 'global)
      ;; else do the normal keybind for tem-expand-key
      ;; bit fishy
      (let ((tem-mode nil)
             (func (key-binding tem-expand-key)))
        (if func (call-interactively func)
          (execute-kbd-macro tem-expand-key) ; a (less good?) alternative
          ))
      ))

(defun tem-try-expand-point-in-mode (mode)
  "Try to expand word at point in MODE. Returns true if successful, nil if not."
  (let ((word (thing-at-point 'word 'no-properties)) mode-temps temp)
     (if (and (setq mode-temps (assoc mode tem-temps))
              (setq temp (assoc word (cdr mode-temps))))
         (progn (tem-helper/kill-thing-at-point 'word)
                (tem-insert (cdr temp))
                t)
       nil)
    ))

(defun tem-helper/kill-thing-at-point (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds (kill-region (car bounds) (cdr bounds)))))

;;; ----- Minor modes --------------------------------

;; We can leave tem-insert-mode by tabbing through fields, undoing
;; enough temes, or by typing C-c C-c

;; We should only enter tem-insert-mode through tem-insert (template expand)

(define-minor-mode tem-insert-mode
  "Minor mode activated when using fields in tem templates."
  :lighter (:eval (propertize " tem"
                              'font-lock-face '(:foreground "#F94FA0")))
  :keymap `((,tem-next-key . tem-next-part)
            (,tem-prev-key . tem-prev-part)
            (,(kbd "C-c C-c") . ,(lambda nil (interactive)
                                   (tem-insert-mode -1)))
            ))

;; should be defined after tem-insert-mode, we want bindings here to take precendence
(define-minor-mode tem-mode
  "Minor mode for inserting templates."
  :lighter " tem"
  :keymap `((,tem-expand-key . tem-try-expand))
  )

(define-globalized-minor-mode tem-global-mode
  tem-mode
  (lambda nil (tem-mode 1)))

(provide 'tem)
