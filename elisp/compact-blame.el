;;; compact-blame.el -*-lexical-binding: t-*-
;; A minor emacs mode for showing "git blame" data  in an
;; unobtrusive way. (At the end of a line like this) <|2018|11|vg|

(defvar compact-blame-mode nil)
(defvar compact-blame-format "%Y%m%.%#")

;; revert-buffer erases all buffer-local vars except marked. We must
;; keep them to clean up process and overlays
(defvar compact-blame-process nil)
(put 'compact-blame-process 'permanent-local t)
(defvar compact-blame-overlays nil)
(put 'compact-blame-overlays 'permanent-local t)
(defvar compact-blame--line-info nil)
(defvar compact-blame--total-lines 0)

(defun compact-blame-make-line-pattern (&rest parts)
  (format "^\\(?:%s\\)\n" (mapconcat 'identity parts "\\|")))

(defun compact-blame-pattern-for-space-separated-tokens (&rest parts)
  (mapconcat 'identity parts "[ \t]+"))

(setq lisp-indent-offset 1)

(defun compact-blame--update-overlay (ov length time author)
 (let ((str compact-blame-format)
	   (b "#E0FFE0") (b2 "#FFFFC0") (f "#303030") (f2 "#111111")
	   (defprops '(:height 0.85)))
  (setq length (if (< (length length) 2) "" (concat "\x2193" length)))
  (setq str (replace-regexp-in-string "%#" length str))
  (setq str (replace-regexp-in-string "%[.]" (or author "...") str))
  (setq str
   (replace-regexp-in-string "%Y" (format-time-string "%Y" time) str))
  (setq str (propertize
			 str 'face (apply 'list :background b :foreground f defprops)))
  (setq str
   (replace-regexp-in-string "%m"
	(propertize (format-time-string "%m" time) 'face
	 (apply 'list :background b2 :foreground f2	:box t defprops)) str))
  (setq str (replace-regexp-in-string "^\s+\\|\s+$" "" str))
  (setq str
   (concat (propertize " \x25c4" 'face (list :foreground b)) str))
  (overlay-put ov 'before-string str)
  ))

(defun compact-blame--make-status ()
 (set (make-local-variable 'compact-blame--total-lines)
  (count-lines (point-min) (point-max)))
 (let ((pos (compact-blame--find-pos (current-buffer) 1)))
  (push (make-overlay pos pos (current-buffer) t t)
   compact-blame-overlays)
  (compact-blame--update-status (current-buffer) t 0)))

(defun compact-blame--update-status (b show line-number)
 (with-current-buffer b
  (let ((ov (car (last compact-blame-overlays)))
		(str "Loading 'git blame' data %d/%d (%d%%)...")
		(b "#404040") (f "#FFFFFF"))
   (setq str (format str line-number compact-blame--total-lines
			  (/ (* 100 line-number) compact-blame--total-lines)))
   (if show
	(overlay-put ov 'before-string
	 (propertize str 'face (list :foreground f :background b)))
	(delete-overlay ov)
	)
   )))

(setq compact-blame--saved-pos 0)
(setq compact-blame--saved-pos-ln 0)

(defun compact-blame--find-pos (b n)
 (with-current-buffer b
  (save-excursion
   (goto-char 0)
   (forward-line n)
   (1- (point)))
   ;; ---
   ;;(goto-char compact-blame--saved-pos)
   ;;(forward-line (- n compact-blame--saved-pos-ln))
   ;;(set (make-local-variable 'compact-blame--saved-pos-ln) n)
   ;;(1- (set (make-local-variable 'compact-blame--saved-pos) (point))))
   ;; ---
   ;;(let ((lines (split-string (buffer-string) "\n")))
   ;; (- (length (buffer-string))
   ;;	(length (mapconcat 'identity (nthcdr n lines) " ")))
   ;; )
 ))

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
  (set-process-filter process
   (lambda (proc str)
	(if (not (buffer-live-p b))
	 (progn
	  (message "Buffer '%s' gone, killing process '%s'" b proc)
	  (delete-process proc))
	 (setq ac (concat ac str))
	 (while (string-match pattern ac)
	  (setq consumed (match-end 0))
	  (funcall cb ac)
	  (setq ac (substring ac consumed))))))))
								
(defconst compact-blame--pattern
  (compact-blame-make-line-pattern
   "\\(?1:[0-9a-fA-F]+\\) [0-9]+ \\(?2:[0-9]+\\) \\(?3:[0-9]+\\)"
   "\\(?99:[0-9a-fA-F]+\\) [0-9]+ \\(?2:[0-9]+\\)"
   "author-mail <\\(?4:.+?\\)[@>].*"
   "author-time \\(?5:.+\\)"
   "\\(?99:[a-zA-Z0-9_-]+\\) .*"
   "\\(?99:[a-zA-Z0-9_-]+\\)"
   "\t\\(?99:.*\\)"
   "fatal:\\(?6:.+?\\)"
   "\\(?98:.*?\\)"))

;; (eval-buffer)

(defun compact-blame--create-process ()
 (compact-blame--cleanup)
 (let* ((take-off (float-time)) (b (current-buffer))
		ov number author length id new-author new-time pos update)
  (setq update
   (lambda (&rest args)
	(apply 'set args)
	(compact-blame--update-overlay ov length time author)))
  (set (make-local-variable 'compact-blame-overlays) nil)
  (compact-blame--make-status)
  (compact-blame--spawn-local 'compact-blame-process
   "nice" "git" "blame" "-w" "--line-porcelain" (buffer-file-name))
  (compact-blame--filter-lines
   compact-blame-process b compact-blame--pattern
   (lambda (ac)
	;;(message "a='%s' m=%s" (match-string 0 ac) (match-data))
	(when (setq id (match-string 1 ac))
	 (setq number (string-to-number (match-string 2 ac)))
	 (setq length (match-string 3 ac))
	 (setq pos (compact-blame--find-pos b number))
	 (compact-blame--update-status b t number)
	 (with-current-buffer b
	  (push (setq ov (make-overlay pos pos b t t))
	   compact-blame-overlays))
	 (overlay-put ov 'compact-blame--rev id)
	 (funcall update 'time (setq author nil)))
	(when (setq new-time (match-string 5 ac))
	 ;;(message "new-time='%s'" new-time)
	 ;;(setq time (string-to-number new-time))
	 (funcall update 'time (seconds-to-time (string-to-number new-time))))
	;;(when (setq unimportant (match-string 101 ac))
	;;(message "unimportant='%s'" unimportant))
	(when (setq new-author (match-string 4 ac))
	 ;;(message "new-author='%s'" new-author)
	 ;; TODO "update" args don't work
	 (setq author new-author)
	 (funcall update 'author new-author))
	(when (setq fatal (match-string 6 ac))
	 (message "fatal='%s'" fatal)) 
	(when (setq unparsed (match-string 98 ac))
	 (message "unparsed='%s'" unparsed))
	))
  (set-process-sentinel compact-blame-process
   (lambda (process event)
	(setq event (car (split-string event)))
	(compact-blame--update-status b nil 100)
	(message
	 "event=%s time=%dms" event (* 1000 (- (float-time) take-off)))))))

(defun compact-blame--cleanup ()
 (if compact-blame-process (delete-process compact-blame-process))
 ;;(backtrace)
 (message
  "#=%d cbm=%s buf=%s" (length compact-blame-overlays) compact-blame-mode
  (current-buffer))
 (mapc 'delete-overlay compact-blame-overlays)
 (setq compact-blame-overlays nil)
 )

(defun compact-blame--show-diff (all-files)
 (require 'vc)
 (let*
  ((p (save-excursion
	   (skip-chars-forward "^\n")
	   (point)))
   (ovs (overlays-in p p))
   (get-id (lambda (ov)
			(list (overlay-get ov 'compact-blame--rev))))
   (ids (mapcan get-id ovs)))
  (cond
   ((not ovs)
	(message "No overlays at pos %d" p))
   ((not ids)
	(message "Commit id not found in %d overlays" (length ovs)))
   (t
	(message "ids=%s" ids)
	(if all-files
	 (compact-blame--show-commit (car ids))
	 (vc-diff-internal t ;; async
	  (list 'git (if all-files nil (list (buffer-file-name))))
	  ;; TODO: fix situation with root commit
	  (format "%s^" (car ids))
	  (car ids)
	  t ;; verbose
	  )))
   )))

(defun compact-blame--show-commit (id)
 (let* ((bn (format "*Commit %s*" id)) proc)
  (with-current-buffer (get-buffer-create bn)
   (setq buffer-read-only nil)
   (erase-buffer)
   (insert " ")
   (setq buffer-read-only t)
   (diff-mode)
   (setq proc (start-process bn (current-buffer) "git" "show" id))
   (goto-char 1)
   (set-marker (process-mark proc) (point-max) (current-buffer))
   (pop-to-buffer bn)
   )))

(defun compact-blame-show-diff ()
 (interactive) (compact-blame--show-diff nil))

(defun compact-blame-show-commit () (interactive)
 (compact-blame--show-diff t))

(defconst compact-blame--keymap (make-sparse-keymap))
(define-key compact-blame--keymap (kbd "RET") 'compact-blame-mode)
(define-key compact-blame--keymap "=" 'compact-blame-show-diff)
(define-key compact-blame--keymap "/" 'compact-blame-show-commit)
		
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
