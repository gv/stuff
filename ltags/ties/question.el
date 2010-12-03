(defun question-complete (what check type) 
  )
  

(defun question-eponimous ()  (interactive)
  (let (query)
	(setq query (completing-read "Question:" 'question-complete))
	(compilation-start (concat "ltags " query)
					   'grep-mode)
	)
  )

(global-set-key [M-.] 'question-eponimous)
(global-set-key [M-\\] 'question-eponimous)
(local-set-key [M-.] 'question-eponimous)
(local-set-key [M-\\] 'question-eponimous)
(global-set-key [M-right] 'question-eponimous)
(local-set-key [M-right] 'question-eponimous)
