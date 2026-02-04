(general-auto-unbind-keys)

(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "JetBrains Mono" :size 22))
(setq display-line-numbers-type t)


(setq org-directory "~/org/"  ; Base dir for all Org files
      org-agenda-files (list "~/org/study/")  ; Agenda scans here—add more paths as needed
      org-noter-notes-search-path '("~/org/study/notes/")  ; Where PDF notes live
      org-default-notes-file "~/org/study/inbox.org"  ; Quick capture lands here
      org-log-done 'time  ; Timestamp when you complete tasks
      org-startup-folded t)  ; Start files folded for clean overviews

(setq org-modern-table-vertical 1)
(setq org-modern-table t)
(add-hook 'org-mode-hook #'hl-todo-mode)


(setq display-line-numbers-type t)   ;; Turn line numbers on
(setq confirm-kill-emacs nil)        ;; Don't confirm on exit

(use-package! exec-path-from-shell
  :if (memq window-system '(x pgtk))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

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
(setq my/pages-dir (expand-file-name "pages" my/notes-root))
(setq my/journals-dir (expand-file-name "journals" my/notes-root))


;; Bind Ctrl+Shift+C to copy selected region to system clipboard
(map! "S-C-c" #'clipboard-kill-ring-save)

;; Bind Ctrl+Shift+V to paste from system clipboard
(map! "S-C-v" #'clipboard-yank)

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
(after! markdown
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

(after! projectile
  (setq projectile-auto-discover nil)
  (projectile-add-known-project "~/org")
  (projectile-add-known-project "~/.config/doom/")
  (setq projectile-project-search-path '("~/.projects/" "~/org/"))
  (setq projectile-require-project-root nil))

(after! pdf-tools
  (require 'saveplace-pdf-view))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("⊙" "◉" "◎" "○" "❍" "◍" "◦")))

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due today"
                                   :deadline today)
                                  (:name "Important"
                                         priority "A")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)
                                  (:name "Big Outcomes"
                                   :tag "bo")))
  :config
  (org-super-agenda-mode))
;; (after! org
;;   (map! :map org-mode-map
;;         :n "z" #'org-cycle
;;         :n "Z" #'org-shifttab
;;         :leader
;;         :desc "Org Noter" "m n" #'org-noter))

(setq org-agenda-custom-commands
      '(("n" "My Custom Agenda" ;; Example custom command; customize as needed
         ((agenda "" ((org-agenda-span 7)))
          (todo "TODO" ((org-agenda-overriding-header "Tasks")))))))

(setq org-capture-templates
      '(("s" "Study Note" entry (file+headline "~/org/study/inbox.org" "Notes")
         "* %^{Title}\n%?")))

(after! spell-fu  ; Doom's spell module uses spell-fu under the hood
  (setq ispell-program-name "hunspell")  ; Switch to hunspell
  (setq ispell-dictionary "en_US")       ; Default dict; add multiples like "en_US,fr_FR"
  (setq ispell-personal-dictionary "~/.config/doom/hunspell_personal.dict")  ; Custom words file
  (setq ispell-alternate-dictionary "/usr/share/dict/words"))  ; Fallback plain list


(after! org
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (let ((file (expand-file-name path)))
                                 (if (file-exists-p file)
                                     (progn
                                       (find-file-other-window file)  ; opens in vertical split by default
                                       (pdf-view-mode))           ; ensure mode activates
                                   (message "File not found: %s" file)))))
  ;; Optional: make RET / C-c C-o always split vertically
  (setq org-link-frame-setup '((file . find-file-other-window))))


(setq split-height-threshold nil)          ; prefer vertical over horizontal

(after! pdf-view
  (add-hook 'pdf-view-mode-hook #'evil-emacs-state))
