;;;  -*- lexical-binding: t; lisp-indent-offset: 1 -*-
;; A minor emacs mode for showing "git blame" data  in an
;; unobtrusive way. (At the end of a line like this) <|2018|11|vg|

;; Config

(defvar compact-blame-format "%Y%m%.%#")
(defvar compact-blame-separators-enabled nil)
(setq compact-blame-bg1 "#E0FFE0")
(setq compact-blame-bg1 "rainbow")
(setq compact-blame-bg2 "#FFFFC0")
(setq compact-blame-bg2 "rainbow")
(defvar compact-blame-light-coeff 650)

;; End of config

(defvar compact-blame-mode nil)
;; revert-buffer erases all buffer-local vars except marked. We must
;; keep them to clean up process and overlays
(defvar-local compact-blame/process nil)
(put 'compact-blame/process 'permanent-local t)
(defvar-local compact-blame/overlays nil)
(put 'compact-blame/overlays 'permanent-local t)
(defvar-local compact-blame/separators nil)
(defvar-local compact-blame/file-info nil)
(defvar-local compact-blame/total-lines 0)

;; (eval-buffer)
;; (emacs-lisp-byte-compile-and-load)

(defun compact-blame-make-line-pattern (&rest parts)
  (format "^\\(?:%s\\)\n" (mapconcat 'identity parts "\\|")))

(defun compact-blame-pattern-for-space-separated-tokens (&rest parts)
 (mapconcat 'identity parts "[ \t]+"))

(defun compact-blame/get-light-coeff () "TODO")

(defun compact-blame--get-bg-color (id config)
 (if (not (string-equal "rainbow" config))
  config (compact-blame--bg-color-from-id id)))

(defun compact-blame--bg-color-from-id (id)
 (let* ((r (string-to-number (substring id 0 2) 16))
		(g (string-to-number (substring id 2 4) 16))
		(b (string-to-number (substring id 4 6) 16))
		;; 600 is more contrast but darker...
		(lc (max 1 (/ (- (min 765 compact-blame-light-coeff) (+ r g b)) 50)))
		(up (lambda (x) (- 255 (/ (- 255 x) lc)))))
  (apply 'format "#%02x%02x%02x" (mapcar up (list r g b)))))

;; I think this is needed to use it in macros
(eval-when-compile
 (defconst compact-blame/commit-vars '(time author id))
 (defconst compact-blame/ov-vars '(ov number length))
 )

;; This has to be a macro, because I can't put lexical
;; environment through eval
(defmacro compact-blame-+commit-vars (&rest prefix)
 (append prefix compact-blame/commit-vars))

(modify-syntax-entry ?@ ".")

(defun compact-blame--propertize-face_ (str &rest props)
 (propertize str 'face
  (cons :height (cons 0.85 props))))

(define-inline compact-blame--propertize-face (str &rest props)
 (propertize str 'face (cons :height (cons 0.85 props))))

(defun compact-blame/propertize-face_ (s_ &rest props)
 (list 'propertize s_ ''face
  `(list :height 0.85 ,@props)))

(eval
 `(defun compact-blame--update-overlay
   (,@compact-blame/ov-vars ,@compact-blame/commit-vars)
   (let* ((str compact-blame-format)
		  (id (overlay-get ov 'compact-blame--rev))
		  (b (compact-blame--get-bg-color id compact-blame-bg1))
		  (b2 (compact-blame--get-bg-color
			   (substring id 6) compact-blame-bg2))
		  (f "#303030") (f2 "#111111"))
	(setq length-indication
	 (if (< (length length) 2) "" (concat "\x2193" length)))
	(setq str (replace-regexp-in-string "%#" length-indication str))
	(setq str (replace-regexp-in-string "%[.]" (or author "...") str))
	(setq str
	 (replace-regexp-in-string "%Y" (format-time-string "%Y" time) str))
	;;(setq str (propertize
	;;		 str 'face (apply 'list :background b :foreground f defprops)))
	(setq str
	 (compact-blame--propertize-face str :background b :foreground f))
	(setq str
	 (replace-regexp-in-string "%m"
	  (compact-blame--propertize-face (format-time-string "%m" time)
	   :background b2 :foreground f2 :box t) str))
	;;(propertize (format-time-string "%m" time) 'face
	;; (apply 'list :background b2 :foreground f2	:box t defprops)) str))
	(setq str (replace-regexp-in-string "^\s+\\|\s+$" "" str))
	(setq str
	 (concat (propertize " \x25c4" 'face (list :foreground b)) str))
	(overlay-put ov 'before-string str)
	(overlay-put ov 'compact-blame/ov-data
	 (list ,@compact-blame/ov-vars))
	(puthash id
	 (list ,@compact-blame/commit-vars) compact-blame/file-info))))
;;(format "---\n\n%s" (symbol-function 'compact-blame--update-overlay))
(byte-compile 'compact-blame--update-overlay)

(defun compact-blame--make-status ()
 (set (make-local-variable 'compact-blame/total-lines)
  (count-lines (point-min) (point-max)))
 (let ((pos (compact-blame--find-pos (current-buffer) 1)))
  (push (make-overlay pos pos (current-buffer) t t)
   compact-blame/overlays)
  (compact-blame--update-status (current-buffer) t 0)))

(defun compact-blame--get-status-ov-local ()
 (car (last compact-blame/overlays)))

(defun compact-blame--update-status (b show line-number)
 (with-current-buffer b
  (let ((str "Loading 'git blame' data %d/%d (%d%%)...")
		(b "#404040") (f "#FFFFFF"))
   (setq str (format str line-number compact-blame/total-lines
			  (/ (* 100 line-number) compact-blame/total-lines)))
	(overlay-put (compact-blame--get-status-ov-local) 'after-string
	 (if show
	  (compact-blame--propertize-face str :foreground f :background b)
	  "")))))

(defvar-local compact-blame--saved-pos 0)
(defvar-local compact-blame--saved-pos-ln 0)

(defun compact-blame--find-pos (b n)
 (with-current-buffer b
  (save-excursion
   (goto-char 0)
   (forward-line n)
   (1- (point)))
  ;; apparently the second is equally fast...
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

(defun compact-blame--get-overlay-local (line-number id)
 (let (ov)
  (setq ov
   (if (= line-number 1)
	(compact-blame--get-status-ov-local)
	(let* ((pos (compact-blame--find-pos (current-buffer) line-number))
		   (ov (make-overlay pos pos (current-buffer) t t)))
	 (push ov compact-blame/overlays)
	 ov)))
  (overlay-put ov 'compact-blame--rev id)
  ov))

(defun compact-blame--get-body-ov-local (line-number id)
 (let* ((start (compact-blame--find-start-local line-number)) ov
		(end (+ start 4)))
  ;; (setq end (min end 
  (setq ov (make-overlay start end))
  (push ov compact-blame/separators)
  (push ov compact-blame/overlays)
  (overlay-put ov 'face (list :overline compact-blame-separators-enabled))
  ))

(defun compact-blame--find-start-local (line-number)
 (save-excursion
  (goto-char 0)
  (forward-line (1- line-number))
  (point)))

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

(defconst compact-blame/update-call-code
 (append '(compact-blame--update-overlay)
  compact-blame/ov-vars compact-blame/commit-vars))

(eval
 `(defun compact-blame/install-output-handler ()
   (let* ((b (current-buffer)) n commit-data (count 0)
		  ,@compact-blame/ov-vars ,@compact-blame/commit-vars)
	(compact-blame--filter-lines
	 compact-blame/process b compact-blame--pattern
	 (lambda (ac)
	  ;;(message "a='%s' m=%s" (match-string 0 ac) (match-data))
	  (cond
	   ((setq n (match-string 1 ac))
		(setq number (string-to-number (match-string 2 ac)))
		(setq length (match-string 3 ac) id n)
		(compact-blame--update-status b t
		 (setq count (+ count (string-to-number length))))
		(with-current-buffer b
		 (setq ov (compact-blame--get-overlay-local number id))
		 (compact-blame--get-body-ov-local number id)
		 (if (setq commit-data (gethash id compact-blame/file-info))
		  (progn
		   (apply 'compact-blame--update-overlay
			,@compact-blame/ov-vars commit-data))
		  (setq time nil author nil)
		  ,compact-blame/update-call-code)))
	   ((setq n (match-string 5 ac))
		;;(message "new-time='%s'" n)
		(setq time (seconds-to-time (string-to-number n)))
		,compact-blame/update-call-code)
	   ;;(when (setq unimportant (match-string 101 ac))
	   ;;(message "unimportant='%s'" unimportant))
	   ((setq n (match-string 4 ac))
		;;(message "new-author='%s'" n)
		(setq author n)
		,compact-blame/update-call-code)
	   ((setq fatal (match-string 6 ac))
		(message "fatal='%s'" fatal)) 
	   ((setq unparsed (match-string 98 ac))
		(message "unparsed='%s'" unparsed))
	   ))))) t)
(format "----\n\n%s" (symbol-function 'compact-blame/install-output-handler))
(byte-compile 'compact-blame/install-output-handler)

(defun compact-blame--create-process ()
 (compact-blame--cleanup)
 (let* ((take-off (float-time)) (b (current-buffer)))
  (setq compact-blame/overlays nil
   compact-blame/file-info (make-hash-table :test 'equal))
  (compact-blame--make-status)
  (compact-blame--spawn-local 'compact-blame/process
   "nice" "git" "blame" "-w" "--incremental" (buffer-file-name))
  (compact-blame/install-output-handler)
  (set-process-sentinel compact-blame/process
   (lambda (process event)
	(setq event (car (split-string event)))
	(compact-blame--update-status b nil 100)
	(message
	 "event=%s time=%dms" event (* 1000 (- (float-time) take-off)))))))


(defun compact-blame--cleanup ()
 (if compact-blame/process (delete-process compact-blame/process))
 ;;(backtrace)
 (message
  "#=%d cbm=%s buf=%s" (length compact-blame/overlays) compact-blame-mode
  (current-buffer))
 (mapc 'delete-overlay compact-blame/overlays)
 (setq compact-blame/overlays nil)
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
   (ids (delq nil (mapcan get-id ovs))))
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

(defun compact-blame-toggle-separators () (interactive)
 (set (make-local-variable 'compact-blame-separators-enabled)
  (not compact-blame-separators-enabled))
 (mapc
  (lambda (ov)
   (overlay-put ov 'face
	(list :overline compact-blame-separators-enabled)))
  compact-blame/separators
  ))

(defun compact-blame-light-up () (interactive)
 (compact-blame/light-adjust 20))

(defun compact-blame-light-down () (interactive)
 (compact-blame/light-adjust -20))

(defun compact-blame/light-adjust (amount)
 (setq compact-blame-light-coeff
  (min 765 (+ compact-blame-light-coeff amount)))
 ;; Refresh
 (mapc
  (lambda (ov)
   (let* (args (id (overlay-get ov 'compact-blame--rev))
		  (commit-data (gethash id compact-blame/file-info)))
	(when id
	 (setq args (append (overlay-get ov 'compact-blame/ov-data) commit-data))
	 ;;(message "id=%s args=%s" id args)
	 (apply 'compact-blame--update-overlay args))))
  compact-blame/overlays)
 (message "coeff=%s" compact-blame-light-coeff))

(defconst compact-blame--keymap (make-sparse-keymap))
(define-key compact-blame--keymap (kbd "RET") 'compact-blame-mode)
(define-key compact-blame--keymap "=" 'compact-blame-show-diff)
(define-key compact-blame--keymap "/" 'compact-blame-show-commit)
(define-key compact-blame--keymap "s" 'compact-blame-toggle-separators)
(define-key compact-blame--keymap "-" 'compact-blame-light-up)
(define-key compact-blame--keymap "+" 'compact-blame-light-down)

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
