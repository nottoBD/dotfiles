;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "JetBrains Mono" :size 22))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-modern-table-vertical 1)
(setq org-modern-table t)
(add-hook 'org-mode-hook #'hl-todo-mode)
(custom-theme-set-faces!
  'doom-one
  '(org-level-8 :inherit outline-3 :height 1.0)
  '(org-level-7 :inherit outline-3 :height 1.0)
  '(org-level-6 :inherit outline-3 :height 1.1)
  '(org-level-5 :inherit outline-3 :height 1.2)
  '(org-level-4 :inherit outline-3 :height 1.3)
  '(org-level-3 :inherit outline-3 :height 1.4)
  '(org-level-2 :inherit outline-2 :height 1.5)
  '(org-level-1 :inherit outline-1 :height 1.6)
  '(org-document-title  :height 1.8 :bold t :underline nil))

(setq display-line-numbers-type t)   ;; Turn line numbers on
(setq confirm-kill-emacs nil)        ;; Don't confirm on exit


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; In ~/.doom.d/config.el
(use-package! exec-path-from-shell
  :if (memq window-system '(x pgtk))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Add Go bin directory to path
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/go/bin")))

;; Go compilation setup
(defun setup-go-compilation ()
  "Set up compilation command for Go projects."
  (when (derived-mode-p 'go-mode)
    (setq compile-command "go build -v -ldflags=\"-w -s\"")))

(add-hook 'go-mode-hook #'setup-go-compilation)

;; Enable GPG support
(setq epg-gpg-program "gpg")

;; Configure for loopback pinentry in Emacs
(setq epg-pinentry-mode 'loopback)

;; Ensure Emacs can find the GPG agent
(when (memq window-system '(x pgtk))
  (setenv "GPG_AGENT_INFO" nil))

(after! vterm
  ;; Bind Ctrl-Shift-V to paste from the kill-ring/clipboard into vterm
  (define-key vterm-mode-map (kbd "C-S-v") #'vterm-yank)

  ;; Bind Ctrl-Shift-C to toggle copy mode (enter to select/copy, exit when done)
  (define-key vterm-mode-map (kbd "C-S-c") #'vterm-copy-mode))


;; Enable global word wrapping
(global-visual-line-mode t)


(setq vterm-shell "/usr/bin/fish")

;; pdf remember
(save-place-mode 1)


(setq select-enable-clipboard t)   ;; sync Emacs kill-ring ↔ system clipboard
(setq select-enable-primary t)     ;; also sync PRIMARY selection (mouse middle-click paste)


(defun my/pdf-view-auto-copy (beg end)
  "Automatically copy PDF text selection to clipboard."
  (interactive "r")
  (pdf-view-kill-ring-save beg end)
  (message "📋 Copied selection to clipboard"))

(setq my/notes-root "~/org")

;; Keybindings for note creation
(map! :leader
      :desc "New page" "n n"
      (cmd! (let ((denote-directory my/pages-dir))
              (call-interactively #'denote)))
      :desc "New journal entry" "n j"
      (cmd! (let ((denote-directory my/journals-dir)
                  (denote-prompts '(title keywords date)))
              (call-interactively #'denote)))
      :desc "Search notes" "n s" #'consult-notes
      :desc "Backlinks" "n b" #'denote-backlinks)

;; Org-roam integration
(after! org-roam
  (setq org-roam-directory (expand-file-name "org" my/notes-root))
  (org-roam-db-autosync-mode))

;; Markdown settings
(after! markdown-mode
  (setq markdown-fontify-code-blocks-natively t))

(use-package! typescript-mode
  :mode ("\\.tsx\\'" . tsx-ts-mode))  ; Use built-in tsx-ts-mode for .tsx

(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))

;; Optional: Enable highlighting and hooks
(add-hook 'tsx-ts-mode-hook #'tree-sitter-hl-mode)



;; Optional: Disable automatic sync between Emacs kill-ring and system clipboard
;; to prevent unintended overwrites (e.g., from deletions). Uncomment if desired.
;; (setq select-enable-clipboard nil)

;; Bind Ctrl+Shift+C to copy selected region to system clipboard
(map! "S-C-c" #'clipboard-kill-ring-save)

;; Bind Ctrl+Shift+V to paste from system clipboard
(map! "S-C-v" #'clipboard-yank)


(after! projectile
  (setq projectile-auto-discover nil)
  (projectile-add-known-project "~/org")
  (projectile-add-known-project "~/.config/doom/")
  (setq projectile-project-search-path '("~/.projects/" "~/org/"))
  (setq projectile-require-project-root nil)
  )

(after! pdf-tools
  (require 'saveplace-pdf-view))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("⊙" "◉" "◎" "○" "❍" "◍" "◦")))
