(message "Trying to load init.el by vg...")

;;
;;     BEHAVIOR
;;     ````````
;;

(setq default-major-mode 'text-mode)
(setq transient-mark-mode '1)
;;загружается молча
(setq inhibit-startup-message t)
;; Scratch buffer settings. Очищаем его.
(setq initial-scratch-message nil)
;;гладкий скроллинг с полями
(setq scroll-conservatively 100 scroll-preserve-screen-position 't
	  scroll-margin 0)
;; show column & line numbers in status bar
(setq column-number-mode t line-number-mode t)

;; Start off in "C:/home" dir.
(cd "~/")
(setq my-author-name (getenv "USER") user-full-name (getenv "USER"))
;; Shut off message buffer. Note - if you need to debug emacs,
;; comment these out so you can see what's going on.
; (setq message-log-max nil)
; (kill-buffer "*Messages*")
(recentf-mode 1); Recent files in menu
;;
;;Создание резервных копий редактируемых файлов (Backup)
;;
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist '(("." . "~/backup"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

;;
;;мышка...
;;
;; Scroll Bar gets dragged by mouse butn 1
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)
;; Paste at point NOT at cursor
(setq mouse-yank-at-point 't)
;;колесо мышки
(mouse-wheel-mode 1)
;;
;;Настройка поведения редактора "как в Windows"
;;
;;настройка клавиатуры как в Windows
;;
;;Delete (and its variants) delete forward instead of backward.
;;C-Backspace kills backward a word (as C-Delete normally would).
;;M-Backspace does undo.
;;Home and End move to beginning and end of line
;;C-Home and C-End move to beginning and end of buffer.
;;C-Escape does list-buffers." 
(if (fboundp 'pc-bindings-mode) (pc-bindings-mode))
;;Настройка выделения "как в Windows"
(if (fboundp 'pc-selection-mode) (pc-selection-mode))
;(delete-selection-mode nil)
;;
;;Установка режима CUA
;;поддержка Ctr-c,v,x,d как в windows
;;
(require 'cua-base)
(cua-mode t)
;;установка режимов работы курсора через CUA
(setq cua-normal-cursor-color "black")
(setq cua-overwrite-cursor-color "red")
(setq cua-read-only-cursor-color "green") 
;; always end a file with a newline
(setq require-final-newline t)
(delete-selection-mode t) ; <del> удаляет выделенный текст

;;
;;     GENERAL KEYS
;;     ``````` ````

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
; Workaround for windows remote terminals
(global-set-key [select] 'end-of-line)
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)
;;удаляем строку целиком
;(setq kill-whole-line t) удаляет ОТ позиции курсора до конца строки
(global-set-key [(control y)] 
  '(lambda () 
     (interactive)
     (beginning-of-line)
     (kill-line)))
;; setting some f[1-12] keys
(global-set-key [f2]    'save-buffer)
(global-set-key [f4]    'ispell-buffer)
(global-set-key [M-f4]  'save-buffers-kill-emacs)
(global-set-key [C-f]  'isearch-forward)
(global-set-key [M-f7]  'find-name-dired)
(global-set-key [C-tab]  'other-window) 
;(global-set-key [M-left] 'pop-tag-mark)

;;
;;    APPEARANCE
;;    ``````````
;;

(setq-default tab-width 4)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode 1) ; for syntax highlighting


;; Выделение парных скобок
(show-paren-mode 1)
(setq show-paren-style 'expression);выделять все выражение в скобках
(tool-bar-mode -1)
(set-default-font 
 (if (equal window-system 'x)
    ; "Bitstream Vera Sans Mono-9"
	 "Monospace-9"
   "Courier New 9")
 )

;(set-cursor-color "red")
(blink-cursor-mode nil)

;; 
;;    КОДИРОВКИ
;;    `````````
;;

;;Используем Windows 1251
;(set-language-environment "Russian")
;(define-coding-system-alias 'windows-1251 'cp1251)
;(set-buffer-file-coding-system 'windows-1251-dos)
;(set-default-coding-systems 'windows-1251-dos)
;(set-terminal-coding-system 'windows-1251-dos)
;(set-selection-coding-system 'windows-1251-dos)
;(set-clipboard-coding-system 'windows-1251-dos)
;;
;; Использовать окружение UTF-8
(set-language-environment 'UTF-8)
(set-buffer-file-coding-system 'utf-8-dos)
(set-default-coding-systems 'utf-8-dos)
(set-terminal-coding-system 'utf-8-dos)
(set-selection-coding-system 'utf-8-dos)
(prefer-coding-system 'koi8-r-dos)
(prefer-coding-system 'cp866-dos)
(prefer-coding-system 'windows-1251-dos)
(prefer-coding-system 'utf-8-dos)

;; 
;;     PROGRAMMING
;;     ```````````
;;

; Загрузим другие программы 
(autoload 'php-mode "php-mode.el" "XXX" t)
(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)

;; for ViewSourceWith Firefox extension
;;(add-to-list 'auto-mode-alist '("index.\\.*" . wikipedia-mode))

;(load "../ltags/ties/question")
;(global-set-key [M-up] 'question-here)
;(global-set-key (kbd "ESC <up>") 'question-here)
(global-set-key [M-up] 'previous-error)
(global-set-key (kbd "ESC <up>") 'previous-error)
(global-set-key [M-down] 'next-error)
;(global-set-key [M-.] 'question-eponimous)

(defun vg-tune-c ()
  (setq c-basic-offset 4
		tab-width 4
		indent-tabs-mode t
		case-fold-search nil
		case-replace nil)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-close 0)
  (setq js-indent-level 2)
  ;(local-set-key [M-.] 'question-eponimous)
  ;(local-set-key [M-up] 'question-here)
  )

(add-hook 'c-mode-common-hook 'vg-tune-c)
(add-hook 'js-mode-hook 'vg-tune-c)

(add-hook 'python-mode-hook 
		  '(lambda ()
			 (setq c-basic-offset 2
				   tab-width 2
				   indent-tabs-mode t
				   case-fold-search nil
				   case-replace nil)))

(if window-system
	(global-set-key (kbd "M-[") 'gtags-find-rtag)
)

; Set file types.
(add-to-list 'auto-mode-alist '("\\.ks\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . java-mode))
(make-face-bold 'font-lock-keyword-face)
(make-face-italic 'font-lock-string-face)



(defun utf () "Reload this buffer as utf-8" (interactive) 
  (let ((coding-system-for-read 'utf-8))
    (revert-buffer nil t t)))

(defun dos () "Reload this buffer as dos linebreaked text" (interactive) 
  (let ((coding-system-for-read 'windows-1251-dos))
    (revert-buffer nil t t)))

;;установка цветов экрана
;;(set-background-color "black")
;;(set-foreground-color "white")

(defun bw () "Black on white" (interactive) 
  (set-background-color "white")
  (set-foreground-color "black"))

(setq tramp-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'c 'compile)


(load "haskell-mode-2.8.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(setenv "GREP_OPTIONS" "--recursive --binary-files=without-match")

(setq dabbrev-case-fold-search nil)

(message "init.el by vg loaded OK.")

