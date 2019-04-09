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

;;------------------------------------------
;; Special evil keybindings
;;------------------------------------------
(use-package evil-collection
  :after (evil helm)
  :ensure t
  :init
  ;(setq evil-collection-setup-minibuffer t)
  :config
  ;(setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

;;------------------------------------------
;; Use general for managing key bindings
;;------------------------------------------
(use-package general
  :ensure t
  :init
  (general-def :states '(normal motion emacs) "SPC" nil))

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

(general-create-definer leader-compile-comments-definer
  :prefix "SPC c")

(general-create-definer leader-errors-definer
  :prefix "SPC e")

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

;; better scrolling
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10
      scroll-preserve-screen-position 1)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

(recentf-mode 1)
(setq inhibit-startup-screen t)

;; Change questions of 'yes/no' to 'y/n'
(fset 'yes-or-no-p 'y-or-n-p)

(general-define-key
 :states '(normal visual)
 "TAB" 'indent-for-tab-command)

;;------------------------------------------
;; Better searching with Helm and FZF
;;------------------------------------------
(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  (helm-mode 1)
  (use-package helm-files)
  ;; (general-define-key
  ;;  :states '(insert normal)
  ;;  :keymaps 'helm-map
  ;;  "C-j" 'helm-next-line
  ;;  "C-k" 'helm-previous-line)
  (leader-definer
    :states 'motion
    "SPC" 'helm-M-x)
  (leader-files-definer
    :states 'motion
    "g" 'helm-ag
    "r" 'helm-mini
    "f" 'helm-find-files
    "s" 'save-buffer)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)
  (define-key helm-map (kbd "C-S-h") 'describe-key)
  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (define-key helm-map [escape] 'helm-keyboard-quit)
  (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
    (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
    (define-key keymap (kbd "C-S-h") 'describe-key))

  :config
  (add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4))))

(use-package fzf
  :ensure t
  :init
  (leader-definer
    :states 'motion
    "s" 'fzf))

;;------------------------------------------
;; Project management with Projectile
;;------------------------------------------
(defun lh/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

;; make so that emacs always follows the compilation buffer
;; and stops at the first error if there is one.
(setq compilation-scroll-output 'first-error)

(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  (leader-project-definer
   :states 'motion
   "c" 'projectile-compile-project
   "r" 'projectile-run-project
   "C" 'projectile-test-project)
  :config
  (setq projectile-project-compilation-cmd "cmake --build build")
  (setq projectile-enable-caching t))

;; searching with deadgrep (ripgrep)
(use-package deadgrep
  :ensure t
  :init
  (leader-definer
    :states 'motion
    "/" 'deadgrep))

(use-package helm-projectile
  :ensure t
  :init
  (leader-project-definer
    :states 'motion
    "f" 'helm-projectile-find-file
    "/" 'helm-projectile-rg
    "a" 'helm-projectile-find-other-file))

(leader-compile-comments-definer
 :states 'motion
 "d" 'lh/close-compilation-window)
  
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

;; Fix issue of reverting buffers
(defun _revert-all-buffers (buffer-predicate)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (funcall buffer-predicate buf))
        (condition-case e
            (revert-buffer t t t)
          (error
           (message "%s" e))))))
  (message "Refreshed open files."))

(defun revert-all-unmodified-buffers-in-git-repo ()
  "Refreshes all open modified buffers in current buffer's Git repo
 from their files."
  (interactive)
  (_revert-all-buffers (lambda (b)
                         (and (not (buffer-modified-p b))
                              (magit-auto-revert-repository-buffer-p b)))))

(with-eval-after-load 'magit-autorevert
  (magit-auto-revert-mode 0)
  (defalias 'magit-auto-revert-buffers
    'revert-all-unmodified-buffers-in-git-repo))

;;------------------------------------------
;; Markdown support
;;------------------------------------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("CHANGELOG\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "marked"))

;;------------------------------------------
;; Configure windows with Shackle
;;------------------------------------------
(setq helm-display-function 'pop-to-buffer) ; make helm play nice

(use-package shackle
  :ensure t
  :config
  (progn (shackle-mode t)
         (setq shackle-rules '((compilation-mode :align 'bottom :size 0.4)
                               ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4))
               shackle-default-rule '(:select t))))

;;------------------------------------------
;; Buffers management
;;------------------------------------------
(defun lh/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(leader-buffer-definer
  :states 'motion
  "b" 'helm-buffers-list
  "d" 'lh/kill-this-buffer)

;; jump to the last buffer
(leader-definer
  :states 'motion
  "TAB" 'mode-line-other-buffer)

;;------------------------------------------
;; Autocompletion with Company mode
;;------------------------------------------
(use-package company
  :diminish ""
  :bind (:map company-active-map
              ("C-j" . company-select-next)
              ("<backtab>" . company-complete-common-or-cycle)
              ("C-k" . company-select-previous))
  :custom
  (company-idle-delay 0.3)
  :config
  (global-company-mode))

;;------------------------------------------
;; Snippets
;;------------------------------------------
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;;------------------------------------------
;; Flycheck
;;------------------------------------------
(use-package flycheck
  :ensure t
  :init
  (leader-errors-definer
   :states 'motion
   "n" 'flycheck-next-error
   "N" 'flycheck-previous-error)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

;;------------------------------------------
;; Language Server Protocol
;;------------------------------------------
(use-package lsp-mode
  :ensure t 
  :commands lsp
  :init
  (leader-files-definer
    :states 'normal
    "F" 'lsp-format-buffer)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil company-lsp-async t))

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
  (leader-compile-comments-definer
    :states '(normal visual)
    "l" 'evilnc-comment-or-uncomment-lines))

(electric-pair-mode 1)
(setq tab-width 4)

;; Use a TODO highlighter
(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

;;------------------------------------------
;; Window management
;;------------------------------------------
(leader-window-definer
  :states '(motion visual normal)
  :keymaps '(magit-mode-map prog-mode-map compilation-mode-map global)
  "v" 'split-window-right
  "-" 'split-window-below
  "l" 'evil-window-right
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "d" 'evil-window-delete
  "m" 'delete-other-windows)

;;------------------------------------------
;; Treemacs
;;------------------------------------------
(use-package treemacs
  :ensure t
  :init
  (leader-files-definer
    :states 'motion
    "t" 'treemacs))

(use-package treemacs-projectile
  :ensure t
  :init
  (leader-project-definer
    :states 'motion
    "t" 'treemacs-projectile))

(use-package treemacs-evil :ensure t)

;;------------------------------------------
;; C++/C
;;------------------------------------------
(use-package ccls
  :defer t
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable "/usr/local/bin/ccls"))
(use-package cmake-mode :ensure t)
(use-package glsl-mode :ensure t)

(c-add-style "work"
             '("stroustrup"
               (c-basic-offset . 4)
               (c-indent-level . 4)
               (tab-width . 4)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((substatement-open . 0)
                                   (case-label . 0)
                                   (brace-list-open . 0)
                                   (innamespace . 0)
                                   (inline-open . 0)
                                   (substatement-open . 0)
                                   (inlambda . 0) ; no extra indent for lambda
                                   (block-open . 0) ; no space before {
                                   (knr-argdecl-intro . -)))))

(setq c-default-style "work")

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
;; (use-package apropospriate-theme :ensure t)
;; (use-package grayscale-theme :ensure t)
(use-package zenburn-theme :ensure t)
(load-theme 'zenburn t)
;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-grayscale-dark t))

;; (load-theme 'minimal t)
;;(load-theme 'grayscale t)
;; (load-theme 'darkburn t)
;; (load-theme 'apropospriate-dark t)

;; Set default font
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(set-cursor-color "IndianRed")

;;-------------------------------------------
;; Custom variables (DO NOT EDIT)
;;-------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.3)
 '(package-selected-packages
   '(shackle zenburn-theme yasnippet use-package treemacs-projectile treemacs-evil tao-theme spaceline soothe-theme smooth-scrolling parrot lsp-ui hl-todo helm-projectile grayscale-theme grandshell-theme glsl-mode general fzf flycheck fireplace exec-path-from-shell evil-nerd-commenter evil-magit evil-collection doom-modeline deadgrep darkburn-theme cyberpunk-theme counsel-projectile company-lsp cmake-mode ccls better-defaults base16-theme apropospriate-theme))
 '(safe-local-variable-values
   '((projectile-project-test-cmd . "cd cpp-lib/build && ./tests")
     (projectile-project-compilation-cmd . "cmake --build cpp-lib/build")
     (projectile-project-run-cmd . "cd cpp-lib/build && ./main")
     (projectile-project-compilation-cmd . "cmake --build build")
     (projectile-project-run-cmd . "cd build && ./blocks")))
 '(shackle-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
