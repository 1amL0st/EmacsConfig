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

(package-refresh-contents)

;;;;;;;;;;;;

(unless (package-installed-p 'helm)
	(package-install 'helm))

(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;

(unless (package-installed-p 'rtags)
	(package-install 'rtags))

(unless (package-installed-p 'helm-rtags)
  (package-install 'helm-rtags))

(require 'helm-rtags)
(setq rtags-use-helm t)

(unless (package-installed-p 'company-rtags)
	(package-install 'company-rtags))

(global-company-mode)
(setq company-idle-delay 0)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

;;;;;;;;;;;;

(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

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

(defun close-all-buffers ()
(interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New keybings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-t") nil)

(global-set-key (kbd "C-t r") 'rtags-find-references)
(global-set-key (kbd "C-t f") 'rtags-find-file)
(global-set-key (kbd "C-t d") 'rtags-find-symbol)

(global-set-key (kbd "C-r") nil)
(global-set-key (kbd "C-r k") 'kill-rectangle)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company-rtags rtags gruvbox-theme helm-projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 16777215)) (:background "#282828" :foreground "#fdf4c1")) (((class color) (min-colors 255)) (:background "#262626" :foreground "#ffffaf")))))
