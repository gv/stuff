;;Файл конфигурации для emacs версии 22.0.50.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(message "Trying to load init.el by vg...")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Установка режимов работы Emacs
;;
(setq default-major-mode 'text-mode)
; Turn on auto-fill mode
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(setq auto-fill-mode t)
(setq fill-column 75)
;; Show marked text
(setq transient-mark-mode '1)
(setq font-lock-maximum-decoration t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Установка правил поведения редактора
;;
;;загружается молча
(setq inhibit-startup-message t)
;; Scratch buffer settings. Очищаем его.
(setq initial-scratch-message nil)
;;гладкий скроллинг с полями
;(setq scroll-conservatively 50)
;(setq scroll-preserve-screen-position 't)
;(setq scroll-margin 10)
;; show column & line numbers in status bar
(setq column-number-mode t)
(setq line-number-mode t)
;; hour format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq calendar-date-display-form (quote ((format "%04s-%02d-%02d" year (string-to-int month) (string-to-int day)))))
(setq calendar-time-display-form (quote (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))))
(setq calendar-week-start-day 1)
(setq european-calendar-style t)
;;Табулятор
(setq-default tab-width 4)
;; Start off in "C:/home" dir.
(cd "~/")
(setq my-author-name (getenv "USER"))
(setq user-full-name (getenv "USER"))
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
   backup-directory-alist
    '(("." . "~/backup"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
;(setq version-control t);нумерованный бэкап - 2 первых и 2 последних
;(setq delete-old-versions t);удаление промежуточных бэкапов
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
(pc-bindings-mode)
;;Настройка выделения "как в Windows"
(pc-selection-mode)
(delete-selection-mode nil)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Настройка внешнего вида редактора
;;
;;установка размеров экрана
;(set-frame-height (selected-frame) 55)
;(set-frame-width (selected-frame) 100)
;;установка левого верхнего угла фрейма 
;(set-frame-position (selected-frame) 60 0)
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Установка значений клавиш
;;
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
(global-set-key [f1]    'help)
(global-set-key [f2]    'save-buffer)
(global-set-key [f4]    'ispell-buffer)
(global-set-key [M-f4]  'save-buffers-kill-emacs)
(global-set-key [C-f]  'isearch-forward)
(global-set-key [M-f7]  'find-name-dired)
(global-set-key [C-tab]  'other-window) 
(global-set-key [M-left] 'pop-tag-mark)

; for syntax highlighting
(global-font-lock-mode 1)

;; Выделение парных скобок
(show-paren-mode 1)
(setq show-paren-style 'expression);выделять все выражение в скобках

;;Установка кодировки текста
;;Используем Windows 1251
(set-language-environment "Russian")
(define-coding-system-alias 'windows-1251 'cp1251)
(set-buffer-file-coding-system 'cp1251-dos)
(set-default-coding-systems 'cp1251-dos)
(set-terminal-coding-system 'cp1251-dos)
(set-selection-coding-system 'cp1251-dos)
(prefer-coding-system 'cp1251-dos)
;;
;; Использовать окружение UTF-8
;(set-language-environment 'UTF-8)
;(set-buffer-file-coding-system 'utf-8-dos)
;(set-default-coding-systems 'utf-8-dos)
;(set-terminal-coding-system 'utf-8-dos)
;(set-selection-coding-system 'utf-8-dos)
;; Установки автоопределения кодировок. Первой будет определяться utf-8-dos
(prefer-coding-system 'cp866-dos)
(prefer-coding-system 'koi8-r-dos)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'windows-1251-dos)


; Загрузим другие программы 
(autoload 'php-mode "php-mode.el" "XXX" t)
(autoload 'wikipedia-mode "wikipedia-mode.el"
"Major mode for editing documents in Wikipedia markup." t)
; for ViewSourceWith Firefox extension
(add-to-list 'auto-mode-alist '("index.\\.*" . wikipedia-mode))

(global-set-key (kbd "M-[") 'gtags-find-rtag)
(global-set-key (kbd "M-]") 'gtags-find-symbol)
(global-set-key [M-.] 'gtags-find-tag)
(global-set-key [M-left] 'gtags-pop-stack)

(defun vg-tune-c ()
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (setq indent-tabs-mode t)
  (setq case-fold-search nil)
  (setq case-replace nil)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-close 0)
  (gtags-mode 1)
  (setq js-indent-level 2)
  )

(add-hook 'c-mode-common-hook 'vg-tune-c)

(add-hook 'js-mode-hook 'vg-tune-c)

(defun etags () 
  "Switch from global to etags, revert to go back | doesn't work"
  (interactive)
  (local-set-key (kbd "M-[") 'gtags-find-rtag)
  (local-set-key (kbd "M-]") 'gtags-find-pattern)
  (local-set-key [M-.] 'find-tag)
  (local-set-key [M-left] 'pop-tag-mark)
  )

  
(add-hook 'gtags-select-mode-hook
		  '(lambda ()
			 (local-set-key (kbd "M-[") 'gtags-find-rtag)
			 (local-set-key (kbd "M-]") 'gtags-find-pattern)
			 (local-set-key [M-.] 'gtags-find-tag)
			 (local-set-key [M-left] 'gtags-pop-stack)
			 ))
			 

(add-hook 'python-mode-hook 
		  '(lambda ()
			 (setq c-basic-offset 2)
			 (setq tab-width 2)
			 (setq indent-tabs-mode t)
			 (setq case-fold-search nil)
			 (setq case-replace nil)))


; Set file types.
(add-to-list 'auto-mode-alist '("\\.ks\\'" . java-mode))
;(setq javascript-mode 'java-mode)
;(add-to-list 'auto-mode-alist '("\\.js\\'" . java-mode))

; Hide toolbar
(tool-bar-mode -1)

;
(set-default-font 
 (if (equal window-system 'x)
    ; "Bitstream Vera Sans Mono-9"
	 "Monospace-9"
   "Courier New 9")
 )

(make-face-bold 'font-lock-keyword-face)
(make-face-italic 'font-lock-string-face)

;;установка режимов работы курсора
(set-cursor-color "red")
;(setq blink-matching-delay 0.1)
(blink-cursor-mode nil);курсор не мигает!
;;
; because javascript-mode still doesn't work
;(font-lock-add-keywords 'java-mode '("\\<\\(function)\\>"))
         
;(defvar js-font-lock-keywords
  ;'(;("\\<\\(function\\|constructor\\|prototype\\)\\>" . font-lock-keyword-face)
;	("\\<\\(four\\|five\\|six\\)\\>" . font-lock-type-face))
;  "Default expressions to highlight in Foo mode.")

;(add-hook 'java-mode-hook
;		  (lambda ()
;			(set (make-local-variable 'font-lock-defaults)
;				 '(js-font-lock-keywords t))))




(defun utf () "Reload this buffer as utf-8" (interactive) 
  (let ((coding-system-for-read 'utf-8))
    (revert-buffer nil t t)))

(defun dos () "Reload this buffer as dos linebreaked text" (interactive) 
  (let ((coding-system-for-read 'cp1251-dos))
    (revert-buffer nil t t)))

;;установка цветов экрана
;;(set-background-color "black")
;;(set-foreground-color "white")

(defun bw () "Black on white" (interactive) 
  (set-background-color "white")
  (set-foreground-color "black"))

(setq tramp-mode nil)

(defun cock () (interactive)
  (message "test")
)

(require 'gtags)
(fset 'find-tag (symbol-function 'gtags-find-tag))
;;
(message "init.el by vg loaded OK.")
