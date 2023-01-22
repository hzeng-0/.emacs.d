;;;;; show definitions from jisho.org  -*- lexical-binding: t; -*-

(defvar jisho-org-display nil "whether or not to use org-mode for showing defs, if org is alr loaded")

(defun jisho-insert-word-def (word)
  "Takes in alist WORD, which should come from the
jisho.org api json, and inserts formatted definition stuff into buffer.
The format is that of org-mode but you don't have to use org-mode."
  (let-alist word
    ;;; insert word + readings
    (when (not (seq-empty-p .japanese))
      (let-alist (elt .japanese 0)
        (insert "* ")
        (cond ((and .word .reading) (insert .word " | " .reading))
              (.word                (insert .word))
              (.reading             (insert .reading)))))

    ;;; insert tags
    (insert " ")
    (setq tagstr "")
    ;; if "common" is true
    (when .is_common 
      (setq tagstr (concat tagstr ":common")))
    ;; "jlpt" level
    (when (and .jlpt (not (seq-empty-p .jlpt)))
      (setq tagstr
            (concat tagstr ":" (elt .jlpt 0))))
    ;; all things in "tags"
    (when (not (seq-empty-p .tags))
      (seq-doseq (tag .tags)
        (setq tagstr (concat tagstr ":" tag))))
    (insert (if tagstr (replace-regexp-in-string "-" "_" (concat tagstr ":")) ""))

    ;;; insert definitions
    (setq tagstr "")
    (seq-doseq (def .senses)
      (let-alist def
        (insert "\n** ")
        (seq-doseq (eng-def .english_definitions)
          (insert eng-def "; "))
        (seq-doseq (tag .parts_of_speech)
          (setq tagstr (concat tagstr ":" tag)))
        (insert (if tagstr (concat (replace-regexp-in-string " " "_" tagstr) ":") "")))
      )
    (insert "\n\n")
    ;; can add more
    ))

(defun jisho-get-defs (query)
  ;; get json data
  (setq results nil)
  (with-temp-buffer
    (url-insert-file-contents
     (concat "https://jisho.org/api/v1/search/words?keyword=" query))
    (setq results (json-parse-buffer :object-type 'alist)))

  ;; insert formatted definitions to new buffer
  (set-buffer
   (setq def-buff (generate-new-buffer (concat query " - jisho.org definition"))))
  (insert "#+TITLE: jisho results for \"" query "\"\n")
  (seq-doseq (word (cdadr results))
    (jisho-insert-word-def word))

  ;; display
  (when (and jisho-org-display (featurep 'org))
    (org-mode)
    (org-global-cycle 2)
    (org-align-all-tags))

  (hl-line-mode)
  (view-mode)

  (beginning-of-buffer)
  (next-line)
  (move-beginning-of-line nil)

  (select-window (display-buffer def-buff))
  )

(defun jisho-input-for-definition (query)
  (interactive "swhat word? ")
  (jisho-get-defs query))

(defun jisho-define-at-point nil
  (interactive)
  (if (region-active-p)
      (jisho-get-defs (buffer-substring (region-beginning) (region-end)))
    (jisho-get-defs (thing-at-point 'word)))
  )

(provide 'jisho)
