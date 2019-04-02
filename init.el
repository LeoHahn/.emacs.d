(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
(package-initialize)

;;------------------------------------------
;; Use VIM emulatioon
;;------------------------------------------
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

;;------------------------------------------
;; Use general for managing key bindings
;;------------------------------------------
(use-package general
  :ensure t
  :init
  (general-def :states '(normal motion emacs) "SPC" nil)
  )

(general-create-definer leader-definer
  :prefix "SPC")

(general-create-definer leader-files-definer
  :prefix "SPC f")

(general-create-definer leader-window-definer
  :prefix "SPC w")

(general-create-definer leader-buffer-definer
  :prefix "SPC b")

(general-create-definer leader-project-definer
  :prefix "SPC p")

(general-create-definer leader-config-definer
  :prefix "SPC f e")

(general-create-definer leader-git-definer
  :prefix "SPC g")

(general-create-definer leader-jump-definer
  :prefix "SPC j")

(general-create-definer leader-comment-definer
  :prefix "SPC c")

;;------------------------------------------
;; Read PATH from shell
;;------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;------------------------------------------
;; Install better defauls
;;------------------------------------------
(use-package better-defaults
  :ensure t
  :config
  (setq ring-bell-function 'ignore))

(use-package smooth-scrolling
  :ensure t
  :init
  (smooth-scrolling-mode 1))

(recentf-mode 1)
(setq inhibit-startup-screen t)

(general-define-key
 :states '(normal visual)
 "TAB" 'indent-for-tab-command)

;;------------------------------------------
;; Better searching with Ivy
;;------------------------------------------
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (global-set-key "\C-s" 'swiper)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-h" (kbd "DEL")))

(use-package counsel
  :ensure t
  :init
  (counsel-mode 1)
  (leader-definer
    :states 'motion
    "SPC" 'counsel-M-x)
  (leader-files-definer
    :states 'motion
    "g" 'counsel-ag
    "r" 'counsel-recentf
    "s" 'save-buffer
    "f" 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x))

;;------------------------------------------
;; Project management with Projectile
;;------------------------------------------
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  (leader-project-definer
   :states 'motion
   "f" 'projectile-find-file
   "c" 'projectile-compile-project
   "r" 'projectile-run-project)
  :config
  (setq projectile-project-compilation-cmd "cmake --build build"))

;; searching with deadgrep (ripgrep)
(use-package deadgrep
  :ensure t
  :init
  (leader-definer
    :states 'motion
    "/" 'deadgrep))

;;------------------------------------------
;; Git integration with Magit
;;------------------------------------------
(use-package magit
  :ensure t
  :init
  (leader-git-definer
   :states 'normal
   "s" 'magit-status))

(use-package evil-magit :ensure t)

;;------------------------------------------
;; Window management
;;------------------------------------------
(leader-window-definer
  :states '(motion visual)
  "v" 'split-window-right
  "-" 'split-window-below
  "l" 'evil-window-right
  "h" 'evil-window-left
  "j" 'evil-window-bottom
  "k" 'evil-window-up
  "d" 'evil-window-delete
  "m" 'delete-other-windows)

;;------------------------------------------
;; Buffers management
;;------------------------------------------
(leader-buffer-definer
  :states 'motion
  "b" 'switch-to-buffer
  "d" 'kill-this-buffer)

;; jump to the last buffer
(leader-definer
  :states 'motion
  "TAB" 'mode-line-other-buffer)

;;------------------------------------------
;; Flycheck
;;------------------------------------------
(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

;;------------------------------------------
;; Language Server Protocol
;;------------------------------------------
(use-package lsp-mode
  :ensure t 
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil))

(use-package company-lsp :ensure t :commands company-lsp)

(leader-jump-definer
  :states 'normal
  :keymaps 'lsp-ui-mode-map
  "d" 'lsp-ui-peek-find-definitions
  "r" 'lsp-ui-peek-find-references)

;;------------------------------------------
;; Config files
;;------------------------------------------
(defun edit-dotfile ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(leader-config-definer
  :states 'motion
  "d" 'edit-dotfile)

;;------------------------------------------
;; Improve editing
;;------------------------------------------
(use-package evil-nerd-commenter
  :ensure t
  :init
  (leader-comment-definer
    :states '(normal visual)
    "l" 'evilnc-comment-or-uncomment-lines))

;;------------------------------------------
;; C++/C
;;------------------------------------------
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable "/usr/local/bin/ccls"))

(use-package cmake-mode :ensure t)

;;------------------------------------------
;; Modeline
;;------------------------------------------
(use-package spaceline :ensure t)
(use-package spaceline-config
  ;:init
  ;(custom-set-faces '(mode-line-buffer-id ((t nil)))) ;; blend well with tango-dark
  :config
  (spaceline-toggle-buffer-size-on)             ;; buffer size
  (spaceline-toggle-buffer-position-on)        ;; buffer position (top)
  (spaceline-toggle-hud-on)                    ;; visual cue for buffer position
  (spaceline-toggle-buffer-encoding-abbrev-on)  ;; line ending convention (unix, dos or mac)
  (spaceline-spacemacs-theme))                   ;; Load spaceline theme

;;------------------------------------------
;; Load default theme
;;------------------------------------------
(use-package cyberpunk-theme :ensure t)
(use-package tao-theme :ensure t)
(use-package soothe-theme :ensure t)
(use-package darkburn-theme :ensure t)

;(load-theme 'minimal t)
(load-theme 'darkburn t)

;; Set default font
(set-face-attribute 'default nil
                    :family "IBM Plex Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; FIREPLACE
(use-package fireplace :ensure t)

;;-------------------------------------------
;; Custom variables (DO NOT EDIT)
;;-------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8572fed7a217affc4d91c12a808a5228b0858113602dd2e577e0bbd3a4994034" default)))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages (quote (doom-modeline evil use-package)))
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "cmake --build cpp-lib/build")
     (projectile-project-run-cmd . "cd cpp-lib/build && ./main")
     (projectile-project-compilation-cmd . "cmake --build cpplib/build")
     (projectile-project-run-cmd . "cd cpplib/build && ./main")
     (projectile-project-run-cmd . "cd build && ./main")
     (projectile-project-compilation-cmd . "cmake --build build")
     (projectile-project-run-cmd . "cd build && ./blocks"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
