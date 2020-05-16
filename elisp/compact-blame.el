;;; compact-blame.el -*-lexical-binding: t-*-
;; A minor emacs mode for showing "git blame" data with text in an
;; unobtrusive way. (At the end of line like this) <|2018-11 vg|

(defvar compact-blame-mode nil)
(defvar compact-blame-format "%# %Y-%m %.")
(defvar compact-blame-process nil)
(defvar compact-blame-overlays nil)

(defun compact-blame-make-line-pattern (&rest parts)
  (format "^\\(?:%s\\)\n" (mapconcat 'identity parts "\\|")))

(defun compact-blame-pattern-for-space-separated-tokens (&rest parts)
  (mapconcat 'identity parts "[ \t]+"))

(defun compact-blame--update-overlay (ov length time author)
  (let ((str (format-time-string compact-blame-format time))
		(b "#808080") (f "white"))
	(setq str (replace-regexp-in-string "%#" length str))
	(setq
	 str (replace-regexp-in-string "%." (or author "...") str))
	;;(message "str='%s'" str)
	(setq str
		  (concat
		   (propertize
			" \x25c4" 'face (list :background f :foreground b))
		   (propertize
			str 'face (list :background b :foreground f :height 0.85))))
	(overlay-put ov 'before-string str)
	))

(defun compact-blame--find-pos (b n)
  (with-current-buffer b
	(let ((lines (split-string (buffer-string) "\n")))
	  (- (length (buffer-string))
		 (length (mapconcat 'identity (nthcdr n lines) " ")))
	  ))
  )

(defun compact-blame--spawn-local (name &rest cmd)
  (message "Running %s" cmd)
  ;; Only in Aquamacs 3.4 
  ;; (make-process
  ;;  :command command :buffer nil
  ;;  :filter
  (set (make-local-variable name)
	   (apply 'start-process (symbol-name name) nil cmd)))

(defun compact-blame--filter-lines (process b pattern cb)
  (let ((ac "") consumed) 
	(defun real-filter (proc str)
	  (if (not (live-buffer-name b))
		  (progn
			(message "Buffer '%s' gone, killing process '%s'" b proc)
			(delete-process proc))
		(setq ac (concat ac str))
		(while (string-match pattern ac)
		  (setq consumed (match-end 0))
		  (funcall cb ac)
		  (setq ac (substring ac consumed)))))
	(set-process-filter process 'real-filter)))
								
(defconst compact-blame--pattern
  (compact-blame-make-line-pattern
   "\\(?1:[0-9a-fA-F]+\\) [0-9]+ \\(?2:[0-9]+\\) \\(?3:[0-9]+\\)"
   "\\(?99:[0-9a-fA-F]+\\) [0-9]+ \\(?2:[0-9]+\\)"
   "author-mail <\\(?4:.+?\\)[@>].*"
   "author-time \\(?5:.+\\)"
   "\\(?99:[a-zA-Z0-9_-]+\\) .*"
   "\t\\(?99:.*\\)"
   "fatal:\\(?6:.+?\\)"
   "\\(?98:.*?\\)"))

(defun compact-blame--create-process ()
  (compact-blame--cleanup)
  (let* ((take-off (float-time)) (b (current-buffer))
		 ov number author length id new-author new-time pos)
	(defun filter (ac)
	  ;;(message "a='%s' m=%s" (match-string 0 ac) (match-data))
	  (when (setq id (match-string 1 ac))
		(setq number (match-string 2 ac) length (match-string 3 ac))
		(setq pos (compact-blame--find-pos b (string-to-number number)))
		(with-current-buffer b
		  (push (setq ov (make-overlay pos pos b t t))
				compact-blame-overlays))
		(update 'time (setq author nil)))
	  (when (setq new-time (match-string 5 ac))
		;;(message "new-time='%s'" new-time)
		;;(setq time (string-to-number new-time))
		(update 'time (seconds-to-time (string-to-number new-time))))
	  ;;(when (setq unimportant (match-string 101 ac))
	  ;;(message "unimportant='%s'" unimportant))
	  (when (setq new-author (match-string 4 ac))
		;;(message "new-author='%s'" new-author)
		(setq author new-author)
		(update 'author new-author))
	  (when (setq fatal (match-string 6 ac))
		(message "fatal='%s'" fatal)) 
	  (when (setq unparsed (match-string 98 ac))
		(message "unparsed='%s'" unparsed)))
	(defun update (&rest args)
	  (apply 'set args)
	  (compact-blame--update-overlay ov length time author))
	(defun sentinel (process event)
	  (setq event (car (split-string event)))
	  (message
	   "event=%s time=%dms" event (* 1000 (- (float-time) take-off))))
	(set (make-local-variable 'compact-blame-overlays) nil)
	(compact-blame--spawn-local
	 'compact-blame-process
	 "git" "blame" "--line-porcelain" (buffer-file-name))
	(compact-blame--filter-lines
	 compact-blame-process b compact-blame--pattern 'filter)
	(set-process-sentinel compact-blame-process 'sentinel)))
	  

(defun compact-blame--cleanup ()
  (if compact-blame-process (delete-process compact-blame-process))
  ;;(backtrace)
  (message
   "#=%d cbm=%s buf=%s" (length compact-blame-overlays) compact-blame-mode (current-buffer))
  (mapc 'delete-overlay compact-blame-overlays)
  (setq compact-blame-overlays nil)
  )

(defconst compact-blame--keymap (make-sparse-keymap))
(define-key compact-blame--keymap (kbd "RET") 'compact-blame-mode)
		
(define-minor-mode compact-blame-mode "TODO Git blame view"
  :lighter ""
  :keymap compact-blame--keymap
  (let* ((path (buffer-file-name)))
	(if (not (buffer-file-name))
		(message "Buffer %s is not a file" (current-buffer))
	  (if compact-blame-mode
		  (progn
			(set (make-local-variable 'compact-blame-saved-readonly)
				 buffer-read-only)
			(setq buffer-read-only t)
			(compact-blame--create-process))
		(compact-blame--cleanup)
		(setq buffer-read-only compact-blame-saved-readonly)
		))))

(defun test ()
  (interactive)
  (message
   "#=%d cbm=%s buf=%s" (length compact-blame-overlays) compact-blame-mode (current-buffer)))

  
