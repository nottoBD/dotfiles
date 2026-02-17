(general-auto-unbind-keys)

(+global-word-wrap-mode +1)

(setq doom-theme 'doom-one)
(setq evil-respect-visual-line-mode t)
(setq doom-font (font-spec :family "JetBrains Mono" :size 22))
(setq display-line-numbers-type t)


(setq org-directory "~/org/"  ; Base dir for all Org files
      org-agenda-files (list "~/org/school/")  ; Agenda scans here—add more paths as needed
      org-noter-notes-search-path '("~/org/school/")  ; Where PDF notes live
      org-default-notes-file "~/org/school/inbox.org"  ; Quick capture lands here
      org-log-done 'time  ; Timestamp when you complete tasks
      org-startup-folded t)  ; Start files folded for clean overviews

(setq org-modern-table-vertical 1)
(setq org-modern-table t)
(add-hook 'org-mode-hook #'hl-todo-mode)

(setq doc-view-continuous t)

(after! pdf-tools
  (pdf-tools-install :no-query))  ; Auto-build epdfinfo silently if missing

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

(setq org-modern-star nil)  ; let org-superstar handle bullets only

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
  :mode ("\\.tsx\\'" . tsx-ts-mode))


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

(use-package! org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⊙" "◉" "◎" "○" "❍" "◍" "◦"))
  ;; Completely hide any remaining * (clean: just bullet + headline text)
  (setq org-superstar-remove-leading-stars t)
  ;; Add 3 spaces before each bullet → headlines no longer touch the left edge
  (setq org-superstar-leading-bullet "       ")
  ;; Fallback if needed: (setq org-superstar-leading-fallback ?\s)
  )

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
      '(("s" "Study Note" entry (file+headline "~/org/school/inbox.org" "Notes")
         "* %^{Title}\n%?")))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  (require 'org-noter-pdftools))

(add-hook 'buffer-list-update-hook
          (lambda ()
            (when (derived-mode-p 'text-mode 'org-mode)
              (setq truncate-lines nil)
              (visual-line-mode 1))))


(after! spell-fu  ; Doom's spell module uses spell-fu under the hood
  (setq ispell-program-name "hunspell")  ; Switch to hunspell
  (setq ispell-dictionary "en_US")       ; Default dict; add multiples like "en_US,fr_FR"
  (setq ispell-personal-dictionary "~/.config/doom/hunspell_personal.dict")  ; Custom words file
  (setq ispell-alternate-dictionary "/usr/share/dict/words"))  ; Fallback plain list

(map! :leader
      (:prefix "b"
       :desc "Dashboard" "h" #'+doom-dashboard/open))

(setq split-height-threshold nil)          ; prefer vertical over horizontal

(after! pdf-view
  (add-hook 'pdf-view-mode-hook #'evil-emacs-state))

(after! org-noter
  (defun my/org-noter-pdf-left ()
    "Start org-noter session with PDF on the left, notes on the right."
    (interactive)
    (org-noter)
    (when (= 2 (count-windows))
      (window-swap-states (frame-first-window) (cadr (window-list nil 'no-minibuf)))))

  ;; Optional: make PDF window wider (60% of frame)
  (setq org-noter-doc-split-fraction '(0.6 . 0.5))

  ;; Ensure horizontal (side-by-side) split
  (setq org-noter-notes-window-location 'horizontal-split))

(after! pdf-view
  (map! :map pdf-view-mode-map
        :localleader
        :desc "Highlight text" "h" #'pdf-annot-add-highlight-markup-annotation))
(after! org-noter
  ;; Auto-highlight selected text when inserting note
  (setq org-noter-highlight-selected-text t))

(add-hook 'buffer-list-update-hook
          (lambda ()
            (when (or (derived-mode-p 'text-mode 'org-mode)
                      (and (boundp 'org-noter-notes-mode) org-noter-notes-mode))
              (setq truncate-lines nil)
              (visual-line-mode 1)
              (setq word-wrap t))))

(after! org
  ;; Consolidated settings for reliability

  ;; Priorities (unchanged)
  (setq org-priority-faces
        '((?A . (:height 1.30 :weight bold   :foreground "#ff6c6b"))
          (?B . (:height 1.20 :weight bold   :foreground "#ecbe7b"))
          (?C . (:height 1.10 :weight normal :foreground "#a0c980"))
          (?D . (:height 1.00 :weight normal :foreground "#7daea3"))
          (?E . (:height 1.00 :weight normal :foreground "#6d8dad"))))

  ;; Headline scaling (unchanged)
  (dolist (pair '((org-level-1 . 1.40)
                  (org-level-2 . 1.30)
                  (org-level-3 . 1.20)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.10)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.00)
                  (org-level-8 . 0.90)))
    (set-face-attribute (car pair) nil
                        :height (cdr pair)
                        :weight 'semi-bold))

  ;; Breathing room (vertical + modern tweaks – unchanged)
  (setq line-spacing 0.15)
  (setq org-modern-block-fringe nil
        org-modern-keyword nil)

  ;; Soft wrapping: reinforce global default
  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; Indentation activation: enable fully
  (setq org-hide-leading-stars t)                ; critical for virtual indent to work
  (setq org-startup-indented t)                  ; files open indented
  (add-hook 'org-mode-hook #'org-indent-mode)    ; always on in Org buffers

  ;; Stronger per-level space: bump to 6 for obvious change with font size 22
  (setq org-indent-indentation-per-level 6)      ; try 4-8; adjust as needed

  ;; Other Org settings (e.g., file-apps, maps – unchanged; add more here if scattered)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps '("\\.pdf::[0-9]+\\'" . emacs))
  (map! :map org-mode-map
        :n "RET" #'org-open-at-point
        :n "gf"  #'org-open-at-point)
  (map! :map org-mode-map
        :localleader
        :desc "Org-noter (PDF right)" "n" #'org-noter
        :desc "Org-noter (PDF left)" "N" #'my/org-noter-pdf-left))

(after! pdf-tools
  ;; Finer zoom steps (default ~1.25 is jumpy; 1.1 or 1.05 feels smoother)
  (setq pdf-view-resize-factor 1.1)

  ;; Default display size: fit-width is most readable/centered for notes
  (setq pdf-view-display-size 'fit-width)

  ;; Auto-fit to width on every PDF open (fixes alignment when org-noter splits windows)
  (add-hook 'pdf-view-mode-hook #'pdf-view-fit-width-to-window)
  (after! org-noter
    (add-hook 'org-noter-notes-mode-hook
              (lambda ()
                (visual-line-mode 1)
                (setq-local truncate-lines nil)
                (setq-local word-wrap t)
                (+word-wrap-mode 1)))  ; Ensure Doom's wrap mode
    ;; Optional: Enhance with visual-fill-column for centered/margin wraps
    (use-package! visual-fill-column
      :hook (org-noter-notes-mode . (lambda ()
                                      (visual-fill-column-mode 1)
                                      (setq visual-fill-column-width 80
                                            visual-fill-column-center-text t)))))
  ;; Optional: Dark mode (easier on eyes, especially with slides)
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
  (setq pdf-view-midnight-colors '("#ffffff" . "#1e1e1e"))  ; White text on dark bg; tweak as needed

  ;; Optional: Prune off-screen pages for smoother scrolling in long docs
  ;; (press C-c C-p to toggle manually if you don't want always-on)
  ;; (add-hook 'pdf-view-mode-hook #'pdf-view-prune-non-facing-pages)
  )

(after! org
  (add-hook 'org-mode-hook #'org-fragtog-mode))
(after! org
  ;; Force modern SVG backend — eliminates dvipng entirely
  (setq org-latex-preview-default-process 'dvisvgm)
  (setq org-latex-preview-process 'dvisvgm);; Optional: nicer scaling and transparency
  (setq org-latex-preview-options
        '(:foreground "default"
          :background "transparent"
          :scale 1.3
          :zoom 1.1));; Ensure clean overlays
  (setq org-latex-preview-appearance-options
        '(:page-width 0.9)))
