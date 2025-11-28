;;; iota-tutorial.el --- Interactive tutorial for IOTA modal editing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: tutorial, modal, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (modalka "0.1.5"))

;;; Commentary:

;; ι • ο • τ • α
;; Interactive Tutorial for IOTA Modal Editing
;;
;; This file is part of I O T Λ.
;;
;; Provides an interactive tutorial for learning Iota's modalka-based
;; key bindings through progressive exercises.
;;
;; Unlike the standard Emacs tutorial (C-h t), which focuses on modifier
;; chords, the Iota tutorial emphasizes modal workflow and semantic
;; translation.
;;
;; Usage:
;;   M-x iota-tutorial
;;   Or via leader: , h t

;;; Code:

(require 'cl-lib)
(require 'iota-box)
(require 'iota-widgets)
(require 'iota-modal)

;;; Customization

(defgroup iota-tutorial nil
  "IOTA interactive tutorial configuration."
  :group 'iota
  :prefix "iota-tutorial-")

(defcustom iota-tutorial-progress-file
  (expand-file-name "iota-tutorial-progress" user-emacs-directory)
  "File to store tutorial progress."
  :type 'file
  :group 'iota-tutorial)

;;; Tutorial State

(defvar iota-tutorial--current-section 0
  "Current section index in the tutorial.")

(defvar iota-tutorial--section-completed nil
  "List of completed section indices.")

(defvar iota-tutorial--buffer-name "*I O T Λ Tutorial*"
  "Name of the tutorial buffer.")

;;; Tutorial Content

(defconst iota-tutorial--sections
  '((:title "Welcome to IOTA"
     :content iota-tutorial--section-welcome)

    (:title "Understanding Modes"
     :content iota-tutorial--section-modes)

    (:title "Basic Navigation"
     :content iota-tutorial--section-navigation)

    (:title "Line Navigation"
     :content iota-tutorial--section-line-navigation)

    (:title "Buffer Navigation"
     :content iota-tutorial--section-buffer-navigation)

    (:title "Editing Basics"
     :content iota-tutorial--section-editing)

    (:title "Copy, Paste, Cut"
     :content iota-tutorial--section-copy-paste)

    (:title "Selection with Mark"
     :content iota-tutorial--section-mark)

    (:title "Search"
     :content iota-tutorial--section-search)

    (:title "Prefix Delegation"
     :content iota-tutorial--section-prefixes)

    (:title "Leader Key Framework"
     :content iota-tutorial--section-leader)

    (:title "Congratulations!"
     :content iota-tutorial--section-complete))
  "List of tutorial sections with content generators.")

;;; Section Content Generators

(defun iota-tutorial--separator (&optional style)
  "Generate a separator line spanning the full window width.
STYLE defaults to `single'."
  (let* ((style (or style 'single))
         (width (max 60 (- (window-body-width) 2))))
    (iota-box-horizontal-line width style 'iota-muted-face)))

(defun iota-tutorial--separator-double ()
  "Generate a double separator line spanning the full window width."
  (let ((width (max 60 (- (window-body-width) 2))))
    (iota-box-horizontal-line width 'double 'iota-muted-face)))

(defun iota-tutorial--section-welcome ()
  "Generate welcome section content."
  (concat
   (iota-tutorial--heading "WELCOME TO IOTA")
   "\n\n"
   (propertize "ι • ο • τ • α" 'face 'iota-accent-face)
   " — Not one iota more than needed.\n\n"
   (iota-tutorial--separator)
   "\n\n"
   "This tutorial will teach you IOTA's native semantic modal editing.\n\n"
   "IOTA is different from other modal systems:\n\n"
   "  • " (propertize "Native Semantics" 'face '(:weight bold)) " — Keys map to Emacs commands directly\n"
   "      " (propertize "n" 'face 'iota-accent-face) " → C-n (next-line), not some other action\n\n"
   "  • " (propertize "No Emulation" 'face '(:weight bold)) " — Not Vi, not Vim, just ergonomic Emacs\n"
   "      " (propertize "w" 'face 'iota-accent-face) " → M-w (copy), " (propertize "y" 'face 'iota-accent-face) " → C-y (paste)\n\n"
   "  • " (propertize "RSI Prevention" 'face '(:weight bold)) " — No more modifier chord gymnastics\n"
   "      Single keys in COMMAND mode replace Control/Meta chords\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "How to use this tutorial:\n\n" 'face '(:weight bold))
   "  This buffer is " (propertize "EDITABLE" 'face 'iota-success-face) " - practice the commands directly!\n\n"
   "  " (propertize "n/p" 'face 'iota-accent-face) "    — Move to next/previous line\n"
   "  " (propertize "v/V" 'face 'iota-accent-face) "    — Scroll down/up\n"
   "  " (propertize "i" 'face 'iota-accent-face) "      — Enter INSERT mode (type text)\n"
   "  " (propertize "C-]" 'face 'iota-accent-face) "    — Return to COMMAND mode\n"
   "  " (propertize "w/y" 'face 'iota-accent-face) "    — Copy/paste text\n"))

(defun iota-tutorial--section-modes ()
  "Generate modes section content."
  (concat
   (iota-tutorial--heading "UNDERSTANDING MODES")
   "\n\n"
   "IOTA has two modes:\n\n"
   (iota-tutorial--mode-box "COMMAND" "box" "Keys trigger commands" 'command)
   "\n\n"
   (iota-tutorial--mode-box "INSERT" "bar" "Keys insert text" 'insert)
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Visual Feedback:\n\n" 'face '(:weight bold))
   "  " (propertize "Cursor Shape" 'face '(:weight bold)) "\n"
   "    COMMAND: █ (box) — 'Stopping' power, keys are commands\n"
   "    INSERT:  | (bar) — 'Insertion' point, keys type text\n\n"
   "  " (propertize "Modeline Indicator" 'face '(:weight bold)) "\n"
   "    " (propertize "● COMMAND" 'face 'iota-success-face) " — Green, prominent\n"
   "    " (propertize "○ INSERT" 'face 'iota-muted-face) "  — Gray, subtle\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Mode Switching:\n\n" 'face '(:weight bold))
   "  " (propertize "C-]" 'face 'iota-accent-face) "  → Enter COMMAND mode (or ESC in GUI Emacs)\n"
   "  " (propertize "i" 'face 'iota-accent-face) "    → Enter INSERT mode (to type text)\n\n"
   (propertize "Try it now! Press 'i' to enter INSERT mode and type something.\n" 'face 'iota-muted-face)
   (propertize "Then press C-] to return to COMMAND mode.\n" 'face 'iota-muted-face)))

(defun iota-tutorial--section-navigation ()
  "Generate navigation section content."
  (concat
   (iota-tutorial--heading "BASIC NAVIGATION")
   "\n\n"
   "In COMMAND mode, navigate without modifier keys:\n\n"
   (iota-tutorial--key-table
    '(("n" "C-n" "next-line" "Move down one line")
      ("p" "C-p" "previous-line" "Move up one line")
      ("f" "C-f" "forward-char" "Move right one character")
      ("b" "C-b" "backward-char" "Move left one character")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Word Movement:\n\n" 'face '(:weight bold))
   (iota-tutorial--key-table
    '(("F" "M-f" "forward-word" "Move to next word")
      ("B" "M-b" "backward-word" "Move to previous word")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Practice Area:\n\n" 'face '(:weight bold))
   "  Try moving around this text using n, p, f, b.\n"
   "  Use F and B to jump between words.\n\n"
   "  The quick brown fox jumps over the lazy dog.\n"
   "  IOTA makes Emacs ergonomic and efficient.\n"
   "  Modal editing preserves your muscle memory.\n"))

(defun iota-tutorial--section-line-navigation ()
  "Generate line navigation section content."
  (concat
   (iota-tutorial--heading "LINE NAVIGATION")
   "\n\n"
   "Move to line boundaries and recenter:\n\n"
   (iota-tutorial--key-table
    '(("a" "C-a" "beginning-of-line" "Jump to line start")
      ("e" "C-e" "end-of-line" "Jump to line end")
      ("l" "C-l" "recenter" "Center line in window")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Common Pattern:\n\n" 'face '(:weight bold))
   "  To select an entire line:\n"
   "    1. " (propertize "a" 'face 'iota-accent-face) "   — Go to line start\n"
   "    2. " (propertize "SPC" 'face 'iota-accent-face) " — Set mark\n"
   "    3. " (propertize "e" 'face 'iota-accent-face) "   — Go to line end\n"
   "    4. " (propertize "w" 'face 'iota-accent-face) "   — Copy (or W to cut)\n\n"
   (propertize "Practice:\n\n" 'face '(:weight bold))
   "  Use 'a' and 'e' on these lines:\n\n"
   "  This line has some text in it.\n"
   "  Another line for practicing navigation.\n"
   "  The cursor should jump to start and end.\n"))

(defun iota-tutorial--section-buffer-navigation ()
  "Generate buffer navigation section content."
  (concat
   (iota-tutorial--heading "BUFFER NAVIGATION")
   "\n\n"
   "Navigate through the buffer efficiently:\n\n"
   (iota-tutorial--key-table
    '(("v" "C-v" "scroll-down" "Page down (view next screen)")
      ("V" "M-v" "scroll-up" "Page up (view previous screen)")
      ("<" "M-<" "beginning-of-buffer" "Jump to buffer start")
      (">" "M->" "end-of-buffer" "Jump to buffer end")
      ("{" "M-{" "backward-paragraph" "Jump to previous paragraph")
      ("}" "M-}" "forward-paragraph" "Jump to next paragraph")
      ("[" "C-M-b" "backward-sexp" "Jump to previous sexp")
      ("]" "C-M-f" "forward-sexp" "Jump to next sexp")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Paragraph & Sexp Motions:\n\n" 'face '(:weight bold))
   "  " (propertize "{" 'face 'iota-accent-face) " / " (propertize "}" 'face 'iota-accent-face) " — Move by paragraph (blank-line separated blocks)\n"
   "  " (propertize "[" 'face 'iota-accent-face) " / " (propertize "]" 'face 'iota-accent-face) " — Move by sexp (balanced expressions: parens, brackets)\n\n"
   (propertize "Note:\n" 'face '(:weight bold))
   "  In Vi/Vim, 'v' enters Visual mode for selection.\n"
   "  In Iota, selection uses the mark (SPC = C-SPC).\n"
   "  This frees 'v' for the 'View next page' mnemonic.\n\n"
   (propertize "Practice:\n\n" 'face '(:weight bold))
   "  1. Press " (propertize ">" 'face 'iota-accent-face) " to go to the end of this buffer\n"
   "  2. Press " (propertize "<" 'face 'iota-accent-face) " to come back to the beginning\n"
   "  3. Use " (propertize "{" 'face 'iota-accent-face) " and " (propertize "}" 'face 'iota-accent-face) " to jump between sections\n"))

(defun iota-tutorial--section-editing ()
  "Generate editing basics section content."
  (concat
   (iota-tutorial--heading "EDITING BASICS")
   "\n\n"
   "Editing commands in COMMAND mode:\n\n"
   (iota-tutorial--key-table
    '(("d" "C-d" "delete-char" "Delete character at point")
      ("D" "M-d" "kill-word" "Kill word forward")
      ("k" "C-k" "kill-line" "Kill to end of line")
      ("u" "C-/" "undo" "Undo last change")
      ("U" "C-?" "undo-redo" "Redo (reverse undo)")
      ("/" "M-/" "dabbrev-expand" "Complete word (dynamic abbrev)")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Undo/Redo:\n\n" 'face '(:weight bold))
   "  " (propertize "u" 'face 'iota-accent-face) " — Undo last change\n"
   "  " (propertize "U" 'face 'iota-accent-face) " — Redo (reverse the undo)\n\n"
   (propertize "Kill vs Delete:\n\n" 'face '(:weight bold))
   "  Emacs distinguishes between:\n"
   "    • " (propertize "Delete" 'face '(:weight bold)) " — Character gone, not saved\n"
   "    • " (propertize "Kill" 'face '(:weight bold)) "   — Text saved to kill ring (can paste later)\n\n"
   "  " (propertize "d" 'face 'iota-accent-face) " deletes character (C-d)\n"
   "  " (propertize "k" 'face 'iota-accent-face) " kills line (C-k) — can yank back with y\n\n"
   (propertize "Transpose:\n\n" 'face '(:weight bold))
   "  " (propertize "t" 'face 'iota-accent-face) " → C-t — Swap two characters (fix typos quickly)\n"))

(defun iota-tutorial--section-copy-paste ()
  "Generate copy/paste section content."
  (concat
   (iota-tutorial--heading "COPY, PASTE, CUT")
   "\n\n"
   (propertize "THE CORE OF IOTA\n\n" 'face 'iota-success-face)
   "These bindings preserve Emacs semantics:\n\n"
   (iota-tutorial--key-table
    '(("w" "M-w" "kill-ring-save" "COPY — save region to kill ring")
      ("y" "C-y" "yank" "PASTE — insert from kill ring")
      ("W" "C-w" "kill-region" "CUT — remove region, save to ring")
      ("Y" "M-y" "yank-pop" "Cycle through kill ring")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Why these keys?\n\n" 'face '(:weight bold))
   "  " (propertize "w" 'face 'iota-accent-face) " for M-w (kill-ring-save) — 'W'rite to ring (copy)\n"
   "  " (propertize "y" 'face 'iota-accent-face) " for C-y (yank) — 'Y'ank from ring (paste)\n"
   "  " (propertize "W" 'face 'iota-accent-face) " for C-w (kill-region) — Capital for destructive (cut)\n\n"
   (propertize "Note on Vi/Vim:\n" 'face '(:weight bold))
   "  In Vi, 'y' means 'yank' (copy) and 'p' means 'put' (paste).\n"
   "  In Emacs, 'yank' means paste (pull from ring), and 'kill' is cut.\n"
   "  Iota keeps Emacs semantics: " (propertize "y = paste" 'face 'iota-accent-face) ", not copy!\n"))

(defun iota-tutorial--section-mark ()
  "Generate mark/selection section content."
  (concat
   (iota-tutorial--heading "SELECTION WITH MARK")
   "\n\n"
   "Emacs uses the 'mark' for text selection:\n\n"
   (iota-tutorial--key-table
    '(("SPC" "C-SPC" "set-mark" "Set mark at point")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Selection Workflow:\n\n" 'face '(:weight bold))
   "  1. Move to selection start\n"
   "  2. Press " (propertize "SPC" 'face 'iota-accent-face) " to set mark (C-SPC)\n"
   "  3. Move to selection end (region highlights)\n"
   "  4. Operate: " (propertize "w" 'face 'iota-accent-face) " (copy), " (propertize "W" 'face 'iota-accent-face) " (cut), or other command\n\n"
   (propertize "Example — Copy 3 lines:\n\n" 'face '(:weight bold))
   "  " (propertize "SPC n n n w" 'face 'iota-accent-face) " — Set mark, move down 3 lines, copy\n\n"
   "  Compare to standard Emacs:\n"
   "  C-SPC C-n C-n C-n M-w — Many modifier chords!\n\n"
   (propertize "Try it:\n\n" 'face '(:weight bold))
   "  Select and copy this text: IOTA is ergonomic!\n\n"
   "  Steps: position cursor, SPC, move to end, w (copy)\n"))

(defun iota-tutorial--section-search ()
  "Generate search section content."
  (concat
   (iota-tutorial--heading "SEARCH")
   "\n\n"
   "Incremental search in COMMAND mode:\n\n"
   (iota-tutorial--key-table
    '(("s" "C-s" "isearch-forward" "Search forward")
      ("r" "C-r" "isearch-backward" "Search backward")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Search Workflow:\n\n" 'face '(:weight bold))
   "  1. Press " (propertize "s" 'face 'iota-accent-face) " to start searching\n"
   "  2. Type your search term\n"
   "  3. Press " (propertize "s" 'face 'iota-accent-face) " again to find next match\n"
   "  4. Press " (propertize "RET" 'face 'iota-accent-face) " to exit at current match\n"
   "  5. Press " (propertize "g" 'face 'iota-accent-face) " (C-g) to cancel and return\n\n"
   (propertize "Note:\n" 'face '(:weight bold))
   "  Once search begins, you're in isearch-mode.\n"
   "  Modalka doesn't interfere — use standard isearch keys.\n\n"
   (propertize "Practice:\n\n" 'face '(:weight bold))
   "  Find the word 'IOTA' in this buffer using 's'.\n"))

(defun iota-tutorial--section-prefixes ()
  "Generate prefix delegation section content."
  (concat
   (iota-tutorial--heading "PREFIX DELEGATION")
   "\n\n"
   "Access all Emacs commands via prefix keys:\n\n"
   (iota-tutorial--key-table
    '(("x" "C-x" "prefix" "Global command prefix")
      ("c" "C-c" "prefix" "Mode-specific prefix")
      ("h" "C-h" "prefix" "Help prefix")
      ("g" "C-g" "keyboard-quit" "Cancel/quit")))
   "\n\n"
   (iota-tutorial--separator)
   "\n\n"
   (propertize "How It Works:\n\n" 'face '(:weight bold))
   "  Pressing " (propertize "x" 'face 'iota-accent-face) " translates to C-x, then Emacs waits for next key.\n\n"
   "  " (propertize "x f" 'face 'iota-accent-face) "   → C-x C-f (find-file) — because 'f' → C-f\n"
   "  " (propertize "x s" 'face 'iota-accent-face) "   → C-x C-s (save-buffer) — because 's' → C-s\n"
   "  " (propertize "x k" 'face 'iota-accent-face) "   → C-x k (kill-buffer) — 'k' isn't mapped to C-k here!\n\n"
   (propertize "Important Distinction:\n" 'face '(:weight bold))
   "  After 'x', if the next key IS mapped (like 's' → C-s),\n"
   "  you get C-x C-s. If NOT mapped (like 'k'), you get C-x k.\n\n"
   (propertize "Common Sequences:\n\n" 'face '(:weight bold))
   "  " (propertize "x f" 'face 'iota-accent-face) "   — Find file\n"
   "  " (propertize "x s" 'face 'iota-accent-face) "   — Save file\n"
   "  " (propertize "x b" 'face 'iota-accent-face) "   — Switch buffer\n"
   "  " (propertize "h f" 'face 'iota-accent-face) "   — Describe function\n"
   "  " (propertize "h k" 'face 'iota-accent-face) "   — Describe key\n"))

(defun iota-tutorial--section-leader ()
  "Generate leader key section content."
  (concat
   (iota-tutorial--heading "LEADER KEY FRAMEWORK")
   "\n\n"
   "The leader key provides organized command access:\n\n"
   (propertize "Leader Key: , (comma)\n\n" 'face 'iota-success-face)
   (iota-tutorial--separator)
   "\n\n"
   (propertize "Leader Hierarchy:\n\n" 'face '(:weight bold))
   "  " (propertize ", f" 'face 'iota-accent-face) "  — " (propertize "Files" 'face '(:weight bold)) "\n"
   "       , f f  Find file\n"
   "       , f s  Save file\n"
   "       , f r  Recent files\n\n"
   "  " (propertize ", b" 'face 'iota-accent-face) "  — " (propertize "Buffers" 'face '(:weight bold)) "\n"
   "       , b b  Switch buffer\n"
   "       , b k  Kill buffer\n"
   "       , b l  List buffers\n\n"
   "  " (propertize ", w" 'face 'iota-accent-face) "  — " (propertize "Windows" 'face '(:weight bold)) "\n"
   "       , w /  Split right\n"
   "       , w -  Split below\n"
   "       , w d  Delete window\n\n"
   "  " (propertize ", g" 'face 'iota-accent-face) "  — " (propertize "Git" 'face '(:weight bold)) " (if magit installed)\n"
   "       , g g  Magit status\n\n"
   "  " (propertize ", h" 'face 'iota-accent-face) "  — " (propertize "Help" 'face '(:weight bold)) "\n"
   "       , h f  Describe function\n"
   "       , h k  Describe key\n"
   "       , h t  This tutorial!\n\n"
   "  " (propertize ", q" 'face 'iota-accent-face) "  — " (propertize "Quit" 'face '(:weight bold)) "\n"
   "       , q q  Save and quit\n\n"
   (propertize "Tip: " 'face '(:weight bold))
   "Install " (propertize "which-key" 'face 'iota-accent-face) " for interactive menus!\n"))

(defun iota-tutorial--section-complete ()
  "Generate completion section content."
  (concat
   (iota-tutorial--heading "CONGRATULATIONS!")
   "\n\n"
   (propertize "You've completed the IOTA tutorial!\n\n" 'face 'iota-success-face)
   (iota-tutorial--separator-double)
   "\n\n"
   (propertize "Quick Reference Card:\n\n" 'face '(:weight bold))
   (iota-tutorial--reference-card)
   "\n"
   (iota-tutorial--separator-double)
   "\n\n"
   (propertize "Next Steps:\n\n" 'face '(:weight bold))
   "  1. Practice in your daily editing\n"
   "  2. Run " (propertize "M-x iota-demo" 'face 'iota-accent-face) " to see all features\n"
   "  3. Customize bindings in your init.el\n"
   "  4. Explore the leader key (,) menu\n\n"
   (propertize "Get Help:\n\n" 'face '(:weight bold))
   "  " (propertize "h f" 'face 'iota-accent-face) "  — Describe any function\n"
   "  " (propertize "h k" 'face 'iota-accent-face) "  — Describe any key\n"
   "  " (propertize ", h t" 'face 'iota-accent-face) " — Return to this tutorial\n\n"
   (propertize "ι • ο • τ • α — Not one iota more than needed.\n\n" 'face 'iota-muted-face)))

;;; Helper Functions

(defun iota-tutorial--heading (text)
  "Format TEXT as a section heading."
  (concat "\n"
          (propertize (concat "  " text) 'face 'iota-highlight-face)
          "\n"))

(defun iota-tutorial--mode-box (name cursor-shape description mode-type)
  "Create a mode description box for NAME with CURSOR-SHAPE and DESCRIPTION.
MODE-TYPE is either 'command or 'insert to derive the color from theme."
  (let* ((cursor-char (if (string= cursor-shape "box") "█" "|"))
         (color (pcase mode-type
                  ('command (or (and (fboundp 'iota-theme-get-success-color)
                                     (iota-theme-get-success-color))
                                "#50fa7b"))
                  ('insert (or (and (fboundp 'iota-theme-get-muted-color)
                                    (iota-theme-get-muted-color))
                               "#6272a4"))
                  (_ "#888888")))
         ;; Total width is 45. Line structure:
         ;; "  │ " (4) + cursor (1) + "  " (2) + name (36) + " │" (2) = 45
         ;; "  │     " (8) + desc (35) + " │" (2) = 45
         (name-padded (format "%-36s" name))
         (desc-padded (format "%-35s" description)))
    (concat "  ╭─────────────────────────────────────────╮\n"
            "  │ "
            (propertize cursor-char 'face `(:foreground ,color))
            "  "
            (propertize name-padded 'face `(:foreground ,color :weight bold))
            " │\n"
            "  │     " desc-padded " │\n"
            "  ╰─────────────────────────────────────────╯")))

(defun iota-tutorial--key-table (rows)
  "Format ROWS as a key binding table.
Each row is (key emacs-key command description)."
  (mapconcat
   (lambda (row)
     (let ((key (nth 0 row))
           (emacs-key (nth 1 row))
           (description (nth 3 row)))
       (format "  %s → %-6s  %s"
               (propertize (format "%-3s" key) 'face 'iota-accent-face)
               emacs-key
               description)))
   rows
   "\n"))

(defun iota-tutorial--reference-card ()
  "Generate a quick reference card."
  "  ╭──────────────────┬──────────────────────╮
  │ NAVIGATION       │ EDITING              │
  ├──────────────────┼──────────────────────┤
  │ n/p  lines       │ w    copy            │
  │ f/b  chars       │ y    paste           │
  │ F/B  words       │ W    cut             │
  │ a/e  line ends   │ k    kill line       │
  │ v/V  pages       │ d    delete char     │
  │ </>  buffer ends │ /    undo            │
  ├──────────────────┼──────────────────────┤
  │ PREFIXES         │ MODE SWITCH          │
  ├──────────────────┼──────────────────────┤
  │ x    C-x         │ ESC  → COMMAND       │
  │ c    C-c         │ i    → INSERT        │
  │ h    C-h         │ SPC  set mark        │
  ├──────────────────┼──────────────────────┤
  │ SEARCH           │ LEADER (,)           │
  ├──────────────────┼──────────────────────┤
  │ s    forward     │ ,f   files           │
  │ r    backward    │ ,b   buffers         │
  │                  │ ,w   windows         │
  │                  │ ,h   help            │
  ╰──────────────────┴──────────────────────╯")

;;; Tutorial Buffer Management

(defun iota-tutorial--render-all ()
  "Render all tutorial sections in one continuous buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Render each section sequentially
    (dolist (section iota-tutorial--sections)
      (let ((content-fn (plist-get section :content)))
        (insert (funcall content-fn))
        (insert "\n\n")))
    (goto-char (point-min))))

;;; Tutorial Commands

(defun iota-tutorial-quit ()
  "Quit the tutorial."
  (interactive)
  (when (y-or-n-p "Quit IOTA tutorial? ")
    (kill-buffer iota-tutorial--buffer-name)))

;;; Entry Point

;;;###autoload
(defun iota-tutorial ()
  "Start the IOTA interactive tutorial.

This tutorial teaches Iota's native semantic modal editing
through progressive exercises. Unlike standard Emacs tutorial,
it focuses on modal workflow and key translations.

The tutorial is EDITABLE - practice the editing commands directly!

Navigation (in COMMAND mode):
  n/p      — Next/previous line
  v/V      — Scroll down/up
  </>      — Beginning/end of buffer
  i        — Enter INSERT mode to type
  C-]      — Return to COMMAND mode (ESC in GUI)
  q        — Quit tutorial"
  (interactive)
  ;; Create and switch to tutorial buffer
  (let ((buf (get-buffer-create iota-tutorial--buffer-name)))
    (switch-to-buffer buf)
    ;; SINGLE WINDOW - delete all other windows
    (delete-other-windows)
    ;; Render ALL sections in one buffer
    (erase-buffer)
    (dolist (section iota-tutorial--sections)
      (let ((content-fn (plist-get section :content)))
        (insert (funcall content-fn))
        (insert "\n\n")))
    (goto-char (point-min))
    ;; Buffer is EDITABLE for practice!
    (setq buffer-read-only nil)
    ;; Set up local q binding for quit (only in COMMAND mode via modalka)
    ;; No local-set-key needed - 'q' will be handled by modalka or just type 'q'
    ;; Enable dynamic separators
    (when (fboundp 'iota-box--enable-dynamic-separators)
      (iota-box--enable-dynamic-separators))
    ;; Enable iota-modal-mode globally if not already active
    (unless (bound-and-true-p iota-modal-mode)
      (when (fboundp 'iota-modal-mode)
        (iota-modal-mode 1)))
    ;; Enter COMMAND mode for the tutorial
    (when (fboundp 'modalka-mode)
      (modalka-mode 1))
    ;; Force modeline update
    (force-mode-line-update t)
    (message "IOTA Tutorial - Practice editing! Press i for INSERT, C-] for COMMAND mode.")))

(provide 'iota-tutorial)
;;; iota-tutorial.el ends here
