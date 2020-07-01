;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common settings section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq make-backup-files nil)

; Use this because kill-emacs triggers creation of backupfiles i don't know why
 (setq backup-directory-alist '(("." . "d:/emacs-backups")))

; Don't know what it is
(toggle-frame-maximized)


; Turning off that bell sound
(setq ring-bell-function 'ignore)

; Turning off the tool bar with icons
(tool-bar-mode -1)

; Hiding scrollbars
(set-scroll-bar-mode nil)

; Settings for pattern matching while enetering buffer name...
; !!!Read about this more!
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; Change emac's title
(setq-default frame-title-format '("%f [%m]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (package-refresh-contents)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For working with 'workspaces'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(defun lost-projectile-init ()
	(projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-m") 'projectile-command-map)
  (setq projectile-project-search-path '("~/.emacs.d/projects/" "~/.emacs.d/work/")))

(lost-projectile-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

;Replacing buffer with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-<f4>") 'quit-emacs)

(global-set-key (kbd "C-x <escape>") 'kill-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color scheme section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar user-theme-name "lost-ray-theme"
  "Path to user's color theme")

(add-to-list 'load-path "~/.emacs.d/themes")

;Initialize text color settings
(defun init-text-color-settings()
  "Sets my appearence for the text"
  (setq-default tab-width 2)
)

(defun init-select-font ()
	(if (string= system-type "gnu/linux")
			(set-default-font "Liberation Mono 11" nil t))
	(if (string= system-type "windows-nt")
		  (set-default-font "Lucida Console 13" nil t)))

(defun lost-init-hook ()
  (require 'lost-ray-theme)
  (init-select-font)
  (init-text-color-settings)
)

(add-hook 'after-init-hook 'lost-init-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

