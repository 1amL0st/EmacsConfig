;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;(package-refresh-contents)

;;;;;;;;;;;;

(unless (package-installed-p 'helm)
	(package-install 'helm))

(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(projectile-mode +1)
(setq projectile-project-search-path '("~/.emacs.d/projects/" "~/.emacs.d/work/"))

(unless (package-installed-p 'helm-projectile)
	(package-install 'helm-projectile))
(require 'helm-projectile)

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

(defun show-take-a-break-notification ()
	  (x-popup-dialog t
       '("Well it's seems you need to take a break for a while"
				("Take a break" . "This"))
			  nil)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New keybings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-<f4>") 'quit-emacs)

(global-set-key (kbd "C-x <escape>") 'kill-emacs)

(global-set-key (kbd "C-, f") 'helm-projectile-find-file)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
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
  (setq-default tab-width 2))

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

(custom-set-variables)
