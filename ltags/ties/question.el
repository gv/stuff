(defun question-complete (what check type) 
  (let ((text (shell-command-to-string 
			   (message (format "qn --complete %d 0 %s" (length (split-string what)) what))
			   ))
		(words))
	(setq words (split-string text))
	(cond ((eq type nil)
		   (try-completion what words check))
		  ((eq type t)
		   (all-completions what words check))
		  ((eq type 'lambda)
		   nil))
	)
  )

(defun question-jump-to-single ()
  
  )

(defun question-eponimous ()  (interactive)
  (let (query)
	(define-key minibuffer-local-completion-map " " nil)
	(setq query (completing-read "Question: " 'question-complete))
	(compilation-start (concat "qn " query)
					   'grep-mode)
	)
  )

(global-set-key [M-.] 'question-eponimous)
(global-set-key [M-\\] 'question-eponimous)
(global-set-key [M-right] 'question-eponimous)
(local-set-key [M-.] 'question-eponimous)
(local-set-key [M-\\] 'question-eponimous)
(local-set-key [M-right] 'question-eponimous)

(defun question-here () (interactive)
  (message (shell-command-to-string 
			(format "qn -l %s %d" (buffer-file-name) (point))))
)

