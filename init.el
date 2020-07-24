;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Build - DONE
;; 2. TODO highlight - DONE
;; 3. Move back - forward
;; 4. Replace in region - DONE
;; 5. Select function's body for C++ code only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)

(require 'package
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

; (package-refresh-contents)

(defun lost-install-package (package-name)
	"Install package if not installed"
	(unless (package-installed-p package-name)
		(package-install package-name)))

;;;;;;;;;;;;

(lost-install-package 'helm)
(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;

(lost-install-package 'projectile)
(projectile-global-mode)
(setq projectile-project-search-path '("~/.emacs.d/projects/" "~/.emacs.d/work/"))

(lost-install-package 'helm-projectile)
(require 'helm-projectile)

;;;;;;;;;;;;
; Don't user any of those for now
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (lost-install-package 'irony)																	  ;;
;; 																																  ;;
;; (lost-install-package 'irony-eldoc)														  ;;
;; 																																  ;;
;; (lost-install-package 'flycheck-irony)													  ;;
;; (add-hook 'after-init-hook #'global-flycheck-mode)							  ;;
;; (global-flycheck-mode)																					  ;;
;; 																																  ;;
;; (lost-install-package 'company-irony)													  ;;
;; (add-hook 'after-init-hook 'global-company-mode)								  ;;
;; (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)	  ;;
;; (setq company-idle-delay 0)																		  ;;
;; 																																  ;;
;; (add-hook 'c++-mode-hook 'irony-mode)													  ;;
;; (add-hook 'c-mode-hook 'irony-mode)														  ;;
;; 																																  ;;
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common settings section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)

(toggle-frame-maximized)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

(setq-default frame-title-format '("%f [%m]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun file-path-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun open-theme-file ()
  "Open the theme file."
  (interactive)
  (find-file (concat "~/.emacs.d/themes/" user-theme-name ".el")))

(defun quit-emacs ()
  "Close emacs with saving changes in files and without creating backup files"
  (interactive)
  (mapcar 'save-buffer (buffer-list))
  (delete-other-windows)
  (kill-emacs))

(defun kill-all-buffers ()
	"Closes all opened buffers"
	(interactive)
	(mapc 'kill-buffer (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookmarks extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lost-last-bookmark nil
	"Last bookmark")

(defun lost-update-last-bookmark (r)
	"Update last bookmark index"
	(if (eq bookmark-alist nil)
			(setq lost-last-bookmark nil)
		(if (eq lost-last-bookmark nil)
				(setq lost-last-bookmark 0)
			(let ((count (length bookmark-alist)))
			(setq lost-last-bookmark (+ lost-last-bookmark r))
			(if (>= lost-last-bookmark count)
					(setq lost-last-bookmark 0))
			(if (< lost-last-bookmark 0)
					(setq lost-last-bookmark (- count 1))))
		)))

(defun lost-go-to-last-bookmark ()
	"Go to last bookmark selected"
	(interactive)
	(message "Go to bookmark index = %d" lost-last-bookmark)
	(bookmark-jump (nth lost-last-bookmark bookmark-alist)))

(defun lost-set-bookmark ()
	"Sets bookmark at cursor position"
	(interactive)
	(message "Isn't implemented yet!"))

(defun lost-jump-next-bookmark ()
	"Jumps to next bookmark"
	(interactive)
	(lost-update-last-bookmark 1)		
	(lost-go-to-last-bookmark))

(defun lost-jump-prev-bookmark ()
	"Jumps to prev bookmark"
	(interactive)
	(lost-update-last-bookmark -1)
	(lost-go-to-last-bookmark))

(defun lost-kill-all-bookmarks ()
	"Kill all bookmars"
	(interactive)	
	(setq bookmark-alist nil)
	(delete-file "~/.emacs.d/bookmarks"))

(defun lost-jump-to-current-bookmark()
	"Jump to last selected bookmark"
	(interactive)
	(lost-update-last-bookmark 0)
	(lost-go-to-last-bookmark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text replacement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lost-replace-in-region ()
	"Replace string in selected region"
	(interactive)
	(let ((beg (mark))
				(end (point))
				(string-to-replace (read-from-minibuffer "String to replace: "))
				(new-string (read-from-minibuffer "New string: ")))
		(replace-string string-to-replace new-string 1 beg end nil)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom text highlight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
			fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lost-call-cmake ()
	"Tries to call cmake build script"
	(interactive)
	(let (
				(root (projectile-project-root))
				(command nil)
				)
		(setq command (format "cd %s && make" root))
		(shell-command command)
		))

(global-set-key (kbd "M-m") 'lost-call-cmake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New keybings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-u") nil)
(global-set-key (kbd "C-c u") 'uncomment-region)

(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "C-t s") 'bookmark-set)
(global-set-key (kbd "C-t j") 'bookmark-jump)
(global-set-key (kbd "C-t l") 'helm-bookmarks)
(global-set-key (kbd "C-t d") 'bookmark-delete)

(global-set-key (kbd "C-t n") 'lost-jump-next-bookmark)
(global-set-key (kbd "C-t p") 'lost-jump-prev-bookmark)
(global-set-key (kbd "C-t c") 'lost-jump-to-current-bookmark)

(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-a") 'back-to-indentation)

(global-set-key (kbd "C-r") nil)
(global-set-key (kbd "C-r r") 'replace-rectangle)
(global-set-key (kbd "C-r k") 'kill-rectangle)

; This keybinding isn't good
(global-set-key (kbd "C-r e") 'lost-replace-in-region)

(global-set-key (kbd "M-<f4>") 'quit-emacs)

; (global-set-key (kbd "C-x <escape>") 'kill-emacs)

(global-set-key (kbd "C-, f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-, p") 'helm-projectile-switch-project)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors, visual appearence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Leave all of them for now, maybe i'll use them ()_ but i am not sure.
;(defvar user-theme-name "lost-ray-theme"
;  "Path to user's color theme")

;(add-to-list 'load-path "~/.emacs.d/themes")

;(unless (package-installed-p 'doom-themes)
;	(package-install 'doom-themes))

;(require 'doom-themes)

;Initialize text color settings
(defun init-text-color-settings()
  "Sets my appearence for the text"
  (setq-default tab-width 2)
	(set-cursor-color "#00FF00"))

(unless (package-installed-p 'gruvbox-theme)
	(package-install 'gruvbox-theme))

(require 'gruvbox-theme)

(defun init-select-font ()
	(if (string= system-type "gnu/linux")
			(set-default-font "Source Code Pro 11" nil t))
	(if (string= system-type "windows-nt")
		  (set-default-font "Lucida Console 13" nil t)))

(defun lost-init-hook ()
  (init-text-color-settings)
  (init-select-font))

(add-hook 'after-init-hook 'lost-init-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(irony-eldoc helm-projectile gruvbox-theme doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 16777215)) (:background "#282828" :foreground "#fdf4c1")) (((class color) (min-colors 255)) (:background "#262626" :foreground "#ffffaf")))))
