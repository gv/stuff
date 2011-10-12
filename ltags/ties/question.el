(defun question-complete (what check type) 
  (let ((text (shell-command-to-string 
			   (format "qn --complete 1 0 \"%s\"" what)
			   ))
		(words))
	(setq words (split-string text "\n"))
	(cond ((eq type nil)
		   (try-completion what words check))
		  ((eq type t)
		   (all-completions what words check))
		  ((eq type 'lambda)
		   nil))))



(defun question-jump-to-single ()
  
  )


(defun question-display-results-page (query)
  (compilation-start (concat "qn " query)
					 'grep-mode))


(local-set-key [M-.] 'question-eponimous)
(local-set-key [M-\\] 'question-eponimous)
(global-set-key [M-.] 'question-eponimous)
(global-set-key [M-\\] 'question-eponimous)
(global-set-key [M-right] 'question-eponimous)
(local-set-key [M-right] 'question-eponimous)
(defun question-eponimous ()  (interactive)
  (let (query)
	(define-key minibuffer-local-completion-map " " nil)
	(setq query (completing-read "Question: " 'question-complete))
	(question-display-results-page query)))


(defun question-here () (interactive)
  (message (shell-command-to-string 
			(format "qn -l %s %d" (buffer-file-name) (point))))
)

(defun gtags-match-string (n)
  (buffer-substring (match-beginning n) (match-end n)))

;; Return a default tag to search for, based on the text at point.
(defconst gtags-symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")
(defconst gtags-definition-regexp "#[ \t]*define[ \t]+\\|ENTRY(\\|ALTENTRY("
  "Regexp matching tag definition name.")
(defun gtags-current-token ()
  (cond
   ((looking-at "[0-9A-Za-z_]")
    (while (looking-at "[0-9A-Za-z_]")
      (forward-char -1))
    (forward-char 1))
   (t
    (while (looking-at "[ \t]")
      (forward-char 1))))
  (if (and (bolp) (looking-at gtags-definition-regexp))
      (goto-char (match-end 0)))
  (if (looking-at gtags-symbol-regexp)
      (gtags-match-string 0) nil))

(global-set-key [M-next] 'question-printdef)
(local-set-key [M-next] 'question-printdef)
(defun question-printdef () (interactive)
  (let ((token (gtags-current-token))
		(m))
	(setq m (shell-command-to-string (format "qn d:%s" token)))
	(setq m (replace-regexp-in-string "[ \r\n]+$" "" m))
	(if (zerop (length m))
		(message (format "%s: no definitions found" token))
	  (message m)
	  )))


(global-set-key [M-prior] 'question-search-pointed-word)
(local-set-key [M-prior] 'question-search-pointed-word)
(defun question-search-pointed-word () (interactive)
  (let ((token (gtags-current-token)))
	(question-display-results-page token)))
