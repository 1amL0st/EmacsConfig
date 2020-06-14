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

; (desktop-change-dir "d:/emacs-backups/desktop/")
;(setq desktop-path '("d:/emacs-backups/desktop/"))
(desktop-save-mode 1)

; Don't know what it is
(setq default-frame-alist initial-frame-alist)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
;; For working with emacs desktops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(message "commad line args: %s" command-line-args)

;(defvar startup-directory	(nth 0 command-line-args)
;	"This is documentation")
;(message "startup emacs directory: %s" startup-directory)
;(message "%s" (nth 0 command-line-args))
;(defvar startup-destkop-file (concat startup-directory ".emacs.desktop"))

;(desktop-load)

; For now i'll try to use desktop-save and desktop-change-dir functions to work with a numbers of desktops
; Just don't forget to run emacs with --no-desktop flag!!!

(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(append-to-list 'desktop-path '("d:/emacs-backups/desktops/"))

(defun print-desktop-path ()
	(message "List of desktop paths directories:")
	(dolist (el desktop-path)
		(message "%s" el))
	(message "\n\n"))

(defun print-desktop-files ()
	(message "List of desktop files:")
	(dolist (folder desktop-path)
		(if (equal (file-exists-p folder) t)
				(dolist (file (directory-files folder))
					(if (not (equal (cl-search ".desktop" file) nil))
							(message "%s" file))
					))
	))
	
(print-desktop-files)
(print-desktop-path)

(message "%s"desktop-dirname)

(defun save-desktop-before-close ()
	"Saves current (if open) desktop"
	(interactive)
	(if (not (equal desktop-dirname nil))
			(desktop-save desktop-dirname))
	)


(defun kill-emacs-and-save-desktop ()
	"Kill emacs and save workspace in the directory"
	(interactive)
	(save-desktop-before-close)
	(kill-emacs))

(global-set-key (kbd "C-x <escape>") 'kill-emacs-and-save-desktop)

;; (load-file "~/.emacs.d/workgroups.el")

;; (require 'workgroups)

; (desktop-save-in-desktop-dir "Pass path here")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; TODO add nickname generation command

; TODO add (or see) how i can implement saving all destkops and check them in one list to
; choose what one i would like to open now
; Make something similiar to projects from Visual studio
; This could be very useful

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

(defun take-a-break-notification ()
  "Take a break notification"
	(run-with-timer 0 5 'show-take-a-break-notification)
	(message "This is my message"))

(defvar take-a-break-delay (* 30 60)
	"Take a break delay")

(run-with-timer take-a-break-delay take-a-break-delay 'take-a-break-notification)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New keybings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Replacing buffer with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-<f4>") 'quit-emacs)



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

(defun lost-init-hook ()
  (require 'lost-ray-theme)
  (set-default-font "Lucida Console 13" nil t)
  (init-text-color-settings)
)

(add-hook 'after-init-hook 'lost-init-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'package
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))
; For projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c s") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
 '(default ((t (:background "#161A1F" :foreground "#DEDEDE")))))
