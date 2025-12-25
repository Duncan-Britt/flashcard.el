;;; srs.el --- Spaced repetition in plain text -*- lexical-binding: t -*-

;; Author: Duncan Britt
;; Contact: https://github.com/Duncan-Britt/srs/issues
;; URL: https://github.com/Duncan-Britt/srs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: hypermedia, srs, memory

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; ┌─────────┐
;; │ Summary │
;; └─────────┘
;; This package implements a Spaced Repetition System (SRS) for
;; creating and reviewing flashcards.  Flashcards can be embedded
;; among your notes, or any text file, so long as you tell
;; `srs.el' where to look for them--by setting
;; `srs-path-list'.

;; ┌──────────────┐
;; │ Installation │
;; └──────────────┘
;; Example Elpaca + use-package instalation
;;
;;  (use-package srs
;;    :ensure (:host github :repo "Duncan-Britt/srs.el")
;;    :config
;;    (add-to-list 'srs-path-list (expand-file-name "~/notes/*.org")))

;; ┌────────────────────────────┐
;; │ Usage -- Making Flashcards │
;; └────────────────────────────┘
;; You can embed flashcards in any text file.  A typical flashcard
;; looks like this:
;; ┌──────────────────────────────────────────┐
;; │ in some text file...                     │
;; │                                          │
;; │ FC: 1F933760-D6B3-4A59-A9E2-09EC19A500CB │
;; │ What is the powerhouse of the cell?      │
;; │                                          │
;; │ Mitochondria.                            │
;; │                                          │
;; │ file continues...                        │
;; └──────────────────────────────────────────┘
;; The empty line between the question and answer marks the end of the
;; question, and the empty line after the answer marks the end of the
;; answer.

;; This is a "cloze" (fill in the blank) flashcard:
;; ┌─────────────────────────────────────────────────────┐
;; │ ...whatever comes before                            │
;; │                                                     │
;; │ FC: 4D55B42A-1389-45CF-B242-E8EBFE7E0784            │
;; │ The {{mitochondria}} is the powerhouse of the cell. │
;; │                                                     │
;; │ rest of the file...                                 │
;; └─────────────────────────────────────────────────────┘
;; Again, the empty line afterward marks the end of the flashcard.

;; To create flash cards like the above, first write your question and
;; answer, or cloze statement:
;; ┌─────────────────────────────────────┐
;; │ ...                                 │
;; │ What is the powerhouse of the cell? │
;; │                                     │
;; │ Mitochondria.                       │
;; │                                     │
;; │ ...                                 │
;; └─────────────────────────────────────┘

;; Then, with your cursor on the question, invoke
;;
;;   M-x srs-card-make-at-point
;;
;; Now you should see an id
;; ┌──────────────────────────────────────────┐
;; │ ...                                      │
;; │ FC: 1F933760-D6B3-4A59-A9E2-09EC19A500CB │
;; │ What is the powerhouse of the cell?      │
;; │                                          │
;; │ Mitochondria.                            │
;; │ ...                                      │
;; └──────────────────────────────────────────┘

;; The flash card is made.  The metadata associated with the flashcard
;; is saved in `srs-history-file' (by default,
;; "<user-emacs-directory>/srs-history.org").

;; BUT--in order to review your newly created flashcard, make sure the
;; file in which you wrote your flashcard is among those specified by
;; `srs-path-list'.  This is how `srs.el' knows where to
;; look for flashcards.

;; Some examples:
;; (add-to-list 'srs-path-list (expand-file-name "~/path/to/your/notes/*"))
;; (add-to-list 'srs-path-list (expand-file-name "~/only/org/files/*.org"))
;; (add-to-list 'srs-path-list (expand-file-name "~/a/specific/file.txt"))
;; (add-to-list 'srs-path-list (expand-file-name "~/even/source/code.el"))

;; ┌───────────────────────────────┐
;; │ Usage -- Reviewing Flashcards │
;; └───────────────────────────────┘
;;  M-x srs-review
;;
;; This is how you can review flashcards which are "due".

;; ┌─────────────────────────────┐
;; │ Usage -- Editing Flashcards │
;; └─────────────────────────────┘
;; Just revise the text of your card where you wrote it.  You don't
;; need to run any additional commands.  As long as the id is still
;; present above your card, all is well.

;; ┌──────────────────────────────┐
;; │ Usage -- Deleting Flashcards │
;; └──────────────────────────────┘
;;  M-x srs-card-delete-at-point
;;
;; Run this command with your cursor is over the line with <DESIGNATOR>: <ID>

;; ┌─────────────────────────┐
;; │ Usage -- Transient Menu │
;; └─────────────────────────┘
;; M-x srs-menu provides a transient menu for accessing
;; srs commands.

;; ┌───────────────┐
;; │ Related works │
;; └───────────────┘
;; https://elpa.nongnu.org/nongnu/doc/gnosis.html
;; https://github.com/abo-abo/pamparam
;; https://github.com/l3kn/org-fc
;; https://github.com/bohonghuang/org-srs
;; https://orgmode.org/worg/org-contrib/org-drill.html
;; https://eding.sourceforge.net/
;; https://github.com/anki-editor/anki-editor
;; https://github.com/chenyanming/anki.el
;; https://github.com/eyeinsky/org-anki
;; https://github.com/taksatou/flashcard.el
;; http://salvi.chaosnet.org/snippets/flashcard-old.html
;; https://github.com/open-spaced-repetition/lisp-fsrs

;;; Code:
(require 'transient)
;; ┌────────┐
;; │ Custom │
;; └────────┘
(defgroup srs nil
  "Spaced repetition in plain text."
  :group 'applications)

(defcustom srs-path-list nil
  "A list of locations in which to search for flashcards.
Examples:
- file.txt       -- includes specific file
- *.log          -- includes all =.log= files
- notes/         -- includes entire directory
- notes/**/*.org -- includes org files within notes or any subdirectories"
  :group 'srs)

(defcustom srs-designator "FC:"
  "Designator for flashcards.

For example, with the designator \"FC:\", a flashcard could be denoted
as \"FC: <id>\n<Question>\n\n<Answer>\"."
  :group 'srs)

(defcustom srs-history-file (expand-file-name (concat user-emacs-directory "srs-history.org"))
  "A file in which to store users saved flashcard review history.

Essential for effective spaced repetition."
  :group 'srs)

(defcustom srs-indicator-duration 1.0
  "Seconds to display success indicator."
  :group 'srs)
;; ┌────────────┐
;; │ End custom │
;; └────────────┘
(defvar-local srs--current-id nil)
(defvar-local srs--current-type nil)
(defvar-local srs--answer nil)

(defvar srs--review-queue nil "Queue of flashcards to review.")
(defvar srs--window-config-before-review nil "Saved window configuration to be restored after reviewing flashcards.")
(defvar srs--current-file nil "Source file of flashcard during reviewing.")
(defvar srs--current-line nil "Source line of flashcard during reviewing.")
(defvar srs--is-cramming nil "Cramming? Otherwise, rate cards.")

(defconst srs--id-regexp "[0-9A-F]\\{8\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{12\\}")

(defun srs--show-indicator (position)
  "Display success indicator overlay at POSITION.

Shows checkmark for `srs-indicator-duration' seconds.
Displays fetch timestamp in echo area if available.

Reuses existing indicator overlay if present, extending its timer.
This prevents overlay accumulation during rapid refreshes.

Called by `srs-card-make-at-point' and `srs-card-delete-at-point'."
  (when (> srs-indicator-duration 0)
    (let* ((beg position)
           (end (save-excursion
                  (goto-char beg)
                  (line-end-position)))
           ;; Check for existing indicator overlay
           (existing-ov
            (seq-find
             (lambda (ov)
               (overlay-get ov 'srs-indicator))
             (overlays-in beg end)))
           (ov (or existing-ov (make-overlay beg end))))

      ;; Cancel existing timer if overlay was reused
      (when existing-ov
        (when-let ((timer (overlay-get ov 'srs-timer)))
          (cancel-timer timer)))

      ;; Set overlay properties (idempotent if reusing)
      (overlay-put ov 'before-string
                   (propertize "☑ " 'face '(:foreground "green" :weight bold)))
      (overlay-put ov 'srs-indicator t)

      ;; Create new timer and store it on overlay
      (let ((timer (run-at-time srs-indicator-duration
                                nil
                                (lambda (overlay)
                                  (when (overlay-buffer overlay)
                                    (delete-overlay overlay)))
                                ov)))
        (overlay-put ov 'srs-timer timer)))))

(defun srs-card-make-at-point ()
  "Make flashcard starting from paragraph(s) at point.

Treats paragraph as the beginning of a flashcard question or
fill-in-the-blank.  Reformats text to include flashcard designator and id
before question, and inserts flashcard into persistant storage."
  (interactive)
  (save-excursion
    (open-line 1)
    (unless (bolp)
      (newline))
    (insert (srs--comment-marker))
    (insert srs-designator)
    (insert " ")
    (let ((card-id (srs--store-new)))
      (insert card-id)
      (move-beginning-of-line 1)
      (save-buffer)
      (message "Created new flashcard: %s" card-id)
      (pcase-let* ((`(,history-file . ,position) (org-id-find card-id))
                   (file-was-open-p (get-file-buffer history-file))
                   (hist-buffer (find-file-noselect history-file)))
        (with-current-buffer hist-buffer
          (org-mode)
          (let* ((new-tags (completing-read-multiple "Add tags: " (srs--all-known-tags)))
                 (tags-string (string-join new-tags ",")))
            (goto-char position)
            (org-set-property "srs-tags" tags-string)
            (save-buffer)))
        (unless file-was-open-p
          (kill-buffer hist-buffer)))
      (srs--show-indicator (point))
      (message "Edited tags for flashcard: %s" card-id))
    (unless (member buffer-file-name (srs--card-file-paths))
      (display-warning 'srs
                       (format "Created flashcard in file not found among `srs-path-list'.\nUse (add-to-list 'srs-path-list \"%s\")" buffer-file-name)
                       :warning))))

(defun srs-card-edit-tags ()
  "Edit tags of flashcard at point."
  (interactive)
  (save-excursion
    (let ((id nil)
          (line-start (line-beginning-position))
          (line-end (line-end-position))
          (completed-p))
      (goto-char line-start)
      (if (re-search-forward (concat (regexp-quote srs-designator)
                                     "[[:space:]]*\\("
                                     srs--id-regexp
                                     "\\)")
                             line-end t)
          (progn
            (setq id (match-string 1))
            (let ((history-entry (org-id-find id)))
              (if history-entry
                  (progn
                    (pcase-let* ((`(,history-file . ,position) history-entry)
                                 (file-was-open-p (get-file-buffer history-file))
                                 (hist-buffer (find-file-noselect history-file)))
                      (with-current-buffer hist-buffer
                        (org-mode)
                        (let* ((initial-tags-str (org-entry-get position "srs-tags"))
                               (initial-tags (when initial-tags-str
                                               (mapcar #'string-trim
                                                       (split-string initial-tags-str "," t)))))
                          (let* ((operation (completing-read (format "(Tags: %s)\nOperation: " (string-join initial-tags ", "))
                                                             '("add" "remove" "replace") nil t))
                                 (new-tags (pcase-exhaustive operation
                                             ("add" (let ((available-tags (seq-difference (srs--all-known-tags)
                                                                                          initial-tags #'string=)))
                                                      (seq-uniq (append initial-tags
                                                                        (completing-read-multiple
                                                                         "Add tags: " available-tags)))))
                                             ("remove" (seq-difference initial-tags (completing-read-multiple
                                                                                     "Remove tags: " initial-tags nil t)))
                                             ("replace" (completing-read-multiple
                                                         "Replace with: " (srs--all-known-tags)))))
                                 (tags-string (string-join new-tags ",")))
                            (goto-char position)
                            (org-set-property "srs-tags" tags-string)
                            (save-buffer)
                            (setq completed-p t))))
                      (unless file-was-open-p
                        (kill-buffer hist-buffer)))
                    (when completed-p
                      (move-beginning-of-line 1)
                      (srs--show-indicator (point)))
                    (message "Edited tags for flashcard: %s" id))
                ;; else
                (user-error "ID: %s not found in %s" id srs-history-file))))
        ;; else
        (user-error "No flashcard %s <ID> on this line" srs-designator)))))

(defun srs--all-known-tags ()
  "Return list of all unique tags used across all flashcards."
  (let ((tags (make-hash-table :test 'equal))
        (files (srs--card-file-paths)))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward (concat "^.*" (regexp-quote srs-designator)) nil t)
          (skip-chars-forward " \t\n\r\f")
          (let ((id (srs--id-at-point)))
            (when id
              (pcase-let ((`(,history-file . ,position) (org-id-find id)))
                (with-current-buffer (find-file-noselect history-file)
                  (let ((tags-str (org-entry-get position "srs-tags")))
                    (when tags-str
                      (dolist (tag (mapcar #'string-trim
                                           (split-string tags-str "," t)))
                        (puthash tag t tags)))))))))))
    (hash-table-keys tags)))

(defun srs-card-delete-at-point ()
  "Delete flashcard at point from file and `srs-history-file'."
  (interactive)
  (save-excursion
    (let ((id nil)
          (line-start (line-beginning-position))
          (line-end (line-end-position)))
      (goto-char line-start)
      (if (re-search-forward (concat (regexp-quote srs-designator)
                                     "[[:space:]]*\\("
                                     srs--id-regexp
                                     "\\)")
                             line-end t)
          (progn
            (setq id (match-string 1))
            (let ((history-entry (org-id-find id)))
              (if (and history-entry (y-or-n-p (format "Delete flashcard %s? " id)))
                  (progn
                    (pcase-let* ((`(,history-file . ,position) history-entry)
                                 (file-was-open-p (get-file-buffer history-file))
                                 (hist-buffer (find-file-noselect history-file)))
                      (with-current-buffer hist-buffer
                        (org-mode)
                        (goto-char position)
                        (org-mark-subtree)
                        (delete-region (region-beginning) (region-end))
                        (save-buffer))
                      (unless file-was-open-p
                        (kill-buffer hist-buffer)))
                    (delete-region line-start line-end)
                    (delete-blank-lines)
                    (srs--show-indicator (point))
                    (save-buffer)
                    (message "Deleted flashcard: %s" id))
                ;; else
                (unless history-entry
                  (user-error "ID: %s not found in %s" id srs-history-file)))))
        ;; else
        (user-error "No flashcard %s <ID> on this line" srs-designator)))))

(defun srs-review (&optional prefix-arg)
  "Review flashcards which are due.
When FILTER-BY-TAGS-P is non nil, such as when invoked with a prefix
arugment, prompt the user for tags by which to filter the flashcards."
  (interactive "P")

  (let ((filter-by-tags-p (if transient-current-prefix
                              (let ((args (transient-args transient-current-command)))
                                (transient-arg-value "--filter" args))
                            prefix-arg)))
    (setq srs--is-cramming nil)
    (setq srs--window-config-before-review (current-window-configuration))
    (let ((tags)
          (any-or-all-case))
      (when filter-by-tags-p
        (pcase-let ((`(,x ,y) (srs--prompt-read-tags)))
          (setq tags x)
          (setq any-or-all-case y)))
      (setq srs--review-queue (srs--due-for-review tags any-or-all-case))
      (if srs--review-queue
          (srs--review-next-card)
        (message "No flashcards due today")))))

(transient-define-suffix srs-cram (&optional prefix-arg)
  "Cram flashcards.
Like `srs-review', but doesn't update flashcard review history.  When
FILTER-BY-TAGS-P is non nil, such as when invoked with a prefix
arugment, prompt the user for tags by which to filter the flashcards."
  (interactive "P")
  (let ((filter-by-tags-p (if transient-current-prefix
                              (let ((args (transient-args transient-current-command)))
                                (transient-arg-value "--filter" args))
                            prefix-arg)))
    (setq srs--is-cramming t)
    (setq srs--window-config-before-review (current-window-configuration))
    (let ((tags)
          (any-or-all-case))
      (when filter-by-tags-p
        (pcase-let ((`(,x ,y) (srs--prompt-read-tags)))
          (setq tags x)
          (setq any-or-all-case y)))
      (setq srs--review-queue (srs--due-for-review tags any-or-all-case))
      (if srs--review-queue
          (srs--review-next-card)
        (message "Found 0 flashcards")))))

(defun srs--prompt-read-tags ()
  "Prompt the user for tags and return selection."
  (let ((any-or-all-case (completing-read "Filter by tags: require [any] match or [all] matches? "
                               '("any" "all") nil t))
        (tags (completing-read-multiple
                          "Tags: " (srs--all-known-tags))))
    (list tags any-or-all-case)))

(transient-define-suffix srs-browse (&optional prefix-arg)
  "Browse all flashcards in an occur-like buffer.
FILTER-BY-TAGS-P, (which can be set to non-NIL by using the prefix
argument when called interactively), when non-NIL, will prompt the user
for tags by which to filter the results."
  (interactive "P")
  (let ((filter-by-tags-p (if transient-current-prefix
                              (let ((args (transient-args transient-current-command)))
                                (transient-arg-value "--filter" args))
                            prefix-arg))
        (tags)
        (any-or-all-case))
    (when filter-by-tags-p
      (pcase-let ((`(,x ,y) (srs--prompt-read-tags)))
        (setq tags x)
        (setq any-or-all-case y)))
    (let* ((srs--is-cramming t) ;; <- don't filter cards not due in call to `srs--due-for-review'
           (cards (srs--due-for-review tags any-or-all-case)))
      (if cards
          (progn
            (when-let ((buf (get-buffer "*Srs Browse*")))
              (kill-buffer buf))
            (with-current-buffer (get-buffer-create "*Srs Browse*")
              (insert (format "%d cards in %s\n" (length cards) srs-path-list))
              (dolist (card cards)
                (pcase-exhaustive card
                  (`(,_ ,file ,line ,__ question ,question ,___)
                   (insert (format "%s:%d: %s\n" file line (string-replace "\n" "⮐ " question))))
                  (`(,_ ,file ,line ,__ cloze ,cloze-str)
                   (insert (format "%s:%d: %s\n" file line (string-replace "\n" "⮐ " (srs--format-cloze cloze-str)))))))
              (compilation-mode)
              (setq-local compilation-directory default-directory)
              (goto-char (point-min))
              (pop-to-buffer (current-buffer))))
        (message "No flashcards found")))))

(defun srs--matches-tag-p (card-id tags any-or-all)
  "Return non-NIL if card with CARD-ID matches TAGS.
If ANY-OR-ALL is \"any\", card only needs to match at least one tag.
Otherwise it must match all tags."
  (let ((card-tags (srs--tags card-id)))
    (pcase-exhaustive any-or-all
      ("any" (seq-intersection tags card-tags #'string=))
      ("all" (seq-every-p (lambda (tag) (member tag card-tags)) tags)))))

(defun srs--review-next-card ()
  "Review the next card in the queue."
  (when srs--review-queue
    (let ((card (pop srs--review-queue)))
      (srs--review-card card))))

(defun srs--format-cloze (cloze-str type)
  "Format CLOZE-STR.

TYPE is either 'HIDE or 'REVEAL."
  (pcase-exhaustive type
    ('hide
     (replace-regexp-in-string "{{[^}]*}}"
                               "[...]"
                               cloze-str))
    ('reveal
     (replace-regexp-in-string "{{\\([^}]*\\)}}"
                               "\\1"
                               cloze-str))))

(defun srs--tags (card-id)
  "Return list of tags for card with ID."
  (let ((history-entry (org-id-find card-id)))
    (when history-entry
      (pcase-let ((`(,history-file . ,position) history-entry))
        (let ((file-was-open-p (get-file-buffer history-file))
              (buffer (find-file-noselect history-file)))
          (unwind-protect
              (with-current-buffer buffer
                (let ((tags-str (org-entry-get position "srs-tags")))
                  (when tags-str
                    (mapcar #'string-trim
                            (split-string tags-str "," t)))))
            (unless file-was-open-p
              (kill-buffer buffer))))))))

(defun srs--review-card (card)
  "Review CARD."
  (delete-other-windows)
  (let* ((buffer-name "*srs*")
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (pcase card
      (`(,id ,file ,line ,mode cloze ,cloze)
       (funcall mode)
       (srs--question-menu)
       (setq-local srs--current-id id)
       (setq srs--current-file file)
       (setq srs--current-line line)
       (setq-local srs--current-type 'cloze)
       (setq-local srs--current-cloze cloze)
       (when-let ((tags (srs--tags id)))
         (insert "Tags: " (string-join tags ", ") "\n\n"))
       (insert (srs--format-cloze cloze 'hide)))
      (`(,id ,file ,line ,mode question ,question ,answer)
       (funcall mode)
       (srs--question-menu)
       (setq-local srs--answer answer)
       (setq-local srs--current-id id)
       (setq srs--current-file file)
       (setq srs--current-line line)
       (setq-local srs--current-type 'question)
       (when-let ((tags (srs--tags id)))
         (insert "Tags: " (string-join tags ", ") "\n\n"))
       (insert question))
      (_ (error "Unrecognized flashcard format: %s" card)))))

(transient-define-suffix srs-rate (&optional args)
  "Rate the just-revealed card.
Then continue."
  (interactive (list (transient-args transient-current-command)))
  (unless srs--is-cramming
    (let ((grade (pcase-exhaustive (this-command-keys)
                   ("e" :easy)
                   ("g" :good)
                   ("h" :hard)
                   ("f" :forgot))))
      (srs--update-review-history srs--current-id grade)))
  (let ((visit-source-p (transient-arg-value "--visit-source" args)))
    (if (and srs--review-queue
             (not visit-source-p))
        (srs--review-next-card)
      (srs-quit-review)
      (when visit-source-p
        (srs--visit-source)))))

(defun srs--visit-source ()
  "Visit source file of current flashcard."
  (find-file srs--current-file)
  (goto-line srs--current-line))

(transient-define-suffix srs-quit-review (&optional args)
  "Quit the current review session."
  (interactive (list (transient-args transient-current-command)))
  (setq srs--is-cramming nil)
  (kill-buffer "*srs*")
  (when (get-buffer-window "*srs*")
    (delete-window (get-buffer-window "*srs*")))
  (set-window-configuration srs--window-config-before-review)
  (when (and transient-current-prefix (transient-arg-value "--visit-source" args))
    (srs--visit-source)))

(transient-define-prefix srs-menu ()
  "Transient menu for srs.el."
  :refresh-suffixes t
  [["Review"
    ("r" "Review due cards" srs-review)
    ("c" "Cram cards" srs-cram)]
   ["Edit"
    ("m" "Make flashcard at point" srs-card-make-at-point)
    ("d" "Delete flashcard at point" srs-card-delete-at-point)
    ("t" "Edit tags for flashcard at point" srs-card-edit-tags)
    ("b" "Browse flashcards" srs-browse)]]
  ["Options"
   ("-f" "Filter by tag(s)" "--filter")])

(transient-define-prefix srs--rate-menu ()
  "Menu for flashcards once revealed."
  :refresh-suffixes t
  [["Rating"
    ("e" "Easy" srs-rate)
    ("g" "Good" srs-rate)
    ("h" "Hard" srs-rate)
    ("f" "Forgot" srs-rate)]
   ["Abort"
    ("q" "Quit without rating card" srs-quit-review)]]
  ["After rating or abort"
   ("-s" "Visit source (quit reviewing)" "--visit-source")])

(transient-define-prefix srs--cram-reveal-menu ()
  "Menu for flashcards once revealed."
  :refresh-suffixes t
  [["Continue"
    ("n" "Next card" srs-rate)]
   ["Exit"
    ("q" "Quit" srs-quit-review)
    ("s" "Visit source (quit reviewing)" (lambda ()
                                           (interactive)
                                           (srs-quit-review)
                                           (srs--visit-source)))]])

(defun srs--update-review-history (id grade)
  "Update review history of card with ID.

GRADE is used to calculate the next review deadline according to the
FSRS algorithm."
  (save-excursion
    (pcase-let* ((`(,history-file . ,position) (org-id-find id))
                 (buffer (find-file-noselect history-file)))
      (with-current-buffer buffer
        (org-mode)
        (let ((difficulty-str (org-entry-get position "difficulty"))
              (stability-str (org-entry-get position "stability"))
              (last-review-timestamp (org-entry-get position "last-review-timestamp"))
              (current-timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
          (if last-review-timestamp
              (let* ((difficulty-num (string-to-number difficulty-str))
                     (stability-num (string-to-number stability-str))
                     (days-since-last-review (srs--days-since-timestamp last-review-timestamp))
                     (retrievability (srs--retrievability days-since-last-review stability-num))
                     (stability (srs--stability difficulty-num stability-num retrievability grade))
                     (difficulty (srs--difficulty difficulty-num grade))
                     (days-til-due (srs--days-til-next-review 0.9 stability)))
                (goto-char position)
                (org-set-property "stability" (number-to-string stability))
                (org-set-property "difficulty" (number-to-string difficulty))
                (org-set-property "last-review-timestamp" current-timestamp)
                (org-set-property "next-review-deadline"
                                  (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                      (srs--time-add-days (current-time) days-til-due))))
            ;; else (initial review)
            (let ((initial-stability (srs--stability-initial grade))
                  (initial-difficulty (srs--difficulty-initial grade)))
              (goto-char position)
              (org-set-property "stability" (number-to-string initial-stability))
              (org-set-property "difficulty" (number-to-string initial-difficulty))
              (org-set-property "last-review-timestamp" current-timestamp)
              (org-set-property "next-review-deadline"
                                (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                    (srs--time-add-days (current-time)
                                                                              (srs--days-til-next-review 0.9
                                                                                                               initial-stability))))
              (list initial-stability initial-difficulty )))
          (save-buffer)))
      (kill-buffer buffer))))

(defun srs--days-since-timestamp (timestamp)
  "Days (float) since TIMESTAMP."
  (let ((time (encode-time (parse-time-string timestamp))))
    (/ (float-time (time-subtract (current-time) time))
       86400.0)))

(defun srs--time-add-days (time days)
  "Add DAYS (a decimal number) to TIME."
  (time-add time (seconds-to-time (* days 86400))))

(defun srs--due-for-review (tags any-or-all)
  "Return list of flashcard locations matching DESIGNATOR.
Filtered by those due for review.

Each location is a list of the form
`(,ID ,FILE ,LINE-NUMBER ,MAJOR-MODE ,@CONTENT), where CONTENT takes the
form `(question ,QUESTION ,ANSWER) or `(cloze ,FILL-IN-THE-BLANK).
Argument TAGS is used to filter flashcards.
Argument ANY-OR-ALL determines whether flashcards should match any or all provided tags, if tags are provided."
  (let ((cards (cond
                ((and (fboundp 'rg) (executable-find "rg"))
                 (srs--due-ripgrep))
                ((executable-find "grep")
                 (srs--due-grep))
                (t
                 (srs--due-native)))))
    (if tags
        (seq-filter (lambda (card)
                      (srs--matches-tag-p (car card) tags any-or-all))
                    cards)
      cards)))

(defun srs--due-ripgrep ()
  "Helper for `srs--due-for-review' using ripgrep."
  (save-excursion
    (let ((locations (srs--search-ripgrep))
          (results nil))
      (dolist (location locations results)
        (pcase-let ((`(,file ,line ,id) location))
          (pcase-let ((`(,history-file . ,position) (org-id-find id)))
            ;; Only collect cards due for review
            (when (or srs--is-cramming
                      (with-temp-buffer
                        (org-mode)
                        (insert-file-contents history-file)
                        (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                          (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                       (current-time)))))
              (with-temp-buffer
                (insert-file-contents file)
                (setq buffer-file-name file)
                (set-auto-mode)
                (set-buffer-modified-p nil)
                (goto-line line)
                (move-end-of-line 1)
                (push (append (list id file line major-mode)
                              (srs--parse-question-or-cloze-str-at-point))
                      results)))))))))

(defun srs--due-grep ()
  "Helper for `srs--due-for-review' using grep."
  (save-excursion
    (let ((locations (srs--search-grep))
          (results nil))
      (dolist (location locations results)
        (pcase-let ((`(,file ,line ,id) location))
          (pcase-let ((`(,history-file . ,position) (org-id-find id)))
            ;; Only collect cards due for review
            (when (or srs--is-cramming
                      (with-temp-buffer
                        (org-mode)
                        (insert-file-contents history-file)
                        (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                          (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                       (current-time)))))
              (with-temp-buffer
                (insert-file-contents file)
                (setq buffer-file-name file)
                (set-auto-mode)
                (set-buffer-modified-p nil)
                (goto-line line)
                (move-end-of-line 1)
                (push (append (list id file line major-mode)
                              (srs--parse-question-or-cloze-str-at-point))
                      results)))))))))

(defun srs--search-grep ()
  "Use grep to find flashcard locations."
  (let ((files (srs--card-file-paths))
        (results))
    (with-temp-buffer
      (apply #'call-process "grep" nil t nil
             "--with-filename" "-n" "-e" srs-designator
             files)
      (goto-char (point-min))
      (thing-at-point 'number)

      (while (re-search-forward (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote srs-designator) "[[:space:]]*\\(" srs--id-regexp "\\)\\s-*") nil t)
        (let* ((file (match-string 1))
               (line (string-to-number (match-string 2)))
               (id (match-string 3)))
          (push (list file
                      line
                      id)
                results))))
    (nreverse results)))

(defun srs--search-ripgrep ()
  "Use ripgrep to find flashcard locations."
  (let ((files (srs--card-file-paths))
        (results))
    (with-temp-buffer
      (apply #'call-process "rg" nil t nil
             "--with-filename" "-n" "-e" srs-designator
             files)
      (goto-char (point-min))
      (thing-at-point 'number)

      (while (re-search-forward (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote srs-designator) "[[:space:]]*\\(" srs--id-regexp "\\)\\s-*") nil t)
        (let* ((file (match-string 1))
               (line (string-to-number (match-string 2)))
               (id (match-string 3)))
          (push (list file
                      line
                      id)
                results))))
    (nreverse results)))

(defun srs--id-at-point ()
  "Grab id from buffer at point if it matches UUID format."
  (let ((id (buffer-substring-no-properties
             (point)
             (save-excursion
               (skip-chars-forward "^ \t\n")
               (point)))))
    (when (string-match-p srs--id-regexp id)
      id)))

(defun srs--due-native ()
  "Gather cards due for review.

Uses native Emacs search through files."
  (save-excursion
    (let ((files (srs--card-file-paths))
          results)
      (dolist (file files results)
        (let ((file-was-open-p (get-file-buffer file))
              (buf (find-file-noselect file))
              (saved-major-mode nil))
          (unwind-protect
              (with-current-buffer buf
                (setq saved-major-mode major-mode))
            (unless file-was-open-p
              (kill-buffer buf)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward (concat "^.*" (regexp-quote srs-designator)) nil t)
              (skip-chars-forward " \t\n\r\f")
              (let ((id (srs--id-at-point))
                    (line (line-number-at-pos)))
                (when id
                  (pcase-let ((`(,history-file . ,position) (org-id-find id)))
                    ;; Only collect cards due for review
                    (when (or srs--is-cramming
                              (with-current-buffer (find-file-noselect history-file)
                                (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                                  (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                               (current-time)))))
                      (move-end-of-line 1)
                      (push (append (list id file line saved-major-mode)
                                    (srs--parse-question-or-cloze-str-at-point))
                            results))))))))))))

(defun srs--store-new ()
  "Store new flashcard in persistent storage."
  (save-excursion
    (write-region "\n* Card" nil srs-history-file t)
    (let ((file-was-open-p (get-file-buffer srs-history-file))
          (buf (find-file-noselect srs-history-file)))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-max))
            (let ((card-id (org-id-get-create))
                  (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
              (org-set-property "created" timestamp)
              (org-set-property "difficulty" "nil")
              (org-set-property "stability" "nil")
              (org-set-property "last-review-timestamp" "nil")
              (org-set-property "next-review-deadline" timestamp)
              (save-buffer)
              card-id))
        (unless file-was-open-p
          (kill-buffer buf))))))

(defun srs--parse-question-or-cloze-str-at-point ()
  "Return `(question ,question ,answer) or `(cloze ,cloze).
Look ahead to find question beginning at nearest nonwhitespace character."
  (save-excursion
    (skip-chars-forward " \t\n\r\f")
    (let ((begin-question (point)))
      ;; Find end of question (empty line or EOF)
      (let ((end-question (progn
                            (re-search-forward "\n[ \t]*\n\\|\\'")
                            (match-beginning 0))))
        (let ((question-or-cloze-str (buffer-substring-no-properties begin-question end-question)))
          (if (string-match-p "{{\\([^}]*\\|[^}]*}[^}]*\\)}}" question-or-cloze-str)
              (list 'cloze question-or-cloze-str)
            ;; For question/answer cards, find answer
            (goto-char end-question)
            (skip-chars-forward " \t\n\r\f")
            (let ((begin-answer (point)))
              ;; Find end of answer (empty line or EOF)
              (re-search-forward "\n[ \t]*\n\\|\\'")
              (let* ((end-answer (match-beginning 0))
                     (answer (buffer-substring-no-properties begin-answer end-answer)))
                (list 'question question-or-cloze-str answer)))))))))

(defun srs--mappend (fn the-list)
  "Apply FN to each element of THE-LIST and append the results."
  (apply #'append (mapcar fn the-list)))

(defun srs--card-file-paths ()
  "Return list of file paths specified by `srs-path-list'."
  (seq-filter #'file-regular-p
              (srs--mappend #'file-expand-wildcards srs-path-list)))

(defun srs--comment-marker ()
  "Return comment marker for the current mode, or \"\"."
  (if comment-start
      (cond ((and (= (length (string-trim comment-start)) 1)
                  (not (string-match-p " $" comment-start)))
             (concat comment-start comment-start " "))
            (t (if (string-match-p " $" comment-start)
                   comment-start
                 (concat comment-start " "))))
    ""))

(transient-define-prefix srs--question-menu ()
  "Menu for displaying flashcards (before reveal)."
  :refresh-suffixes t
  [("r" "Reveal card" (lambda ()
                        (interactive)
                        (cond
                         ((eq srs--current-type 'cloze)
                          (erase-buffer)
                          (insert (srs--format-cloze srs--current-cloze 'reveal)))
                         ((eq srs--current-type 'question)
                          (insert "\n\n---\n\n")
                          (insert srs--answer)))
                        (if srs--is-cramming
                            (srs--cram-reveal-menu)
                          (srs--rate-menu))))
   ("q" "Quit reviewing" srs-quit-review)])

;; ┌──────────────────────────────────────────────────────────────────────────────────┐
;; │ FSRS algorithm implementation for flashcard.el                                   │
;; │ ==============================================                                   │
;; │ retrievability R in [0,1] - probability of recall, computed dynamically          │
;; │ stability S in [0, inf] - time in days for R to go form 1 to 0.9, stored in card │
;; │ difficulty D in [1,10] - how hard it is to recall, stored in card                │
;; │                                                                                  │
;; │ User's self-rating is called the `grade':                                        │
;; │ 1 <-> :forgot                                                                    │
;; │ 2 <-> :hard                                                                      │
;; │ 3 <-> :good                                                                      │
;; │ 4 <-> :easy                                                                      │
;; │                                                                                  │
;; │ (2025-12-17) This implementation was based off                                   │
;; │ https://borretti.me/article/implementing-fsrs-in-100-lines                       │
;; └──────────────────────────────────────────────────────────────────────────────────┘
;; ┌───────────┐
;; │ Constants │
;; └───────────┘
(defconst srs-F (/ 19.0 81.0))
(defconst srs-C -0.5)
(defconst srs-W [0.40255 1.18385 3.173 15.69105 7.1949 0.5345 1.4604 0.0046 1.54575 0.1192
                          1.01925 1.9395 0.11 0.29605 2.2698 0.2315 2.9898 0.51655 0.6621])
(defmacro srs--w (i) "Convenience wrapper for accessing weights via I, the index into `srs-W' vector." `(aref ,srs-W ,i))

;; ┌──────────────────────┐
;; │ Days til next review │
;; └──────────────────────┘
(defun srs--days-til-next-review (desired-retention stability-of-card)
  "Days until next review as function of DESIRED-RETENTION and STABILITY-OF-CARD.
This function is based on the `srs--retrievability' function, but
manipulated algebraically."
  (* (/ stability-of-card
        srs-F)
     (- (expt desired-retention
              (/ 1.0 srs-C))
        1)))

;; ┌────────────────┐
;; │ Retrievability │
;; └────────────────┘
(defun srs--retrievability (days-since-last-review stability-of-card)
  "Retrievability R in [0,1] - probability of recall.

DAYS-SINCE-LAST-REVIEW and STABILITY-OF-CARD are floats."
  (expt (+ 1.0
           (* srs-F (/ days-since-last-review
                        stability-of-card)) )
        srs-C))

;; ┌───────────┐
;; │ Stability │
;; └───────────┘
(defun srs--stability-initial (grade)
  "Initial stability of flashcard given first GRADE."
  (srs--w (pcase grade
         (:forgot 0)
         (:hard 1)
         (:good 2)
         (:easy 3))))

(defun srs--stability-on-success (difficulty stability retrievability grade)
  "Return new STABILITY of flashcard upon success.
DIFFICULTY, STABILITY, and RETRIEVABILITY are floats.  GRADE is one of
{:forgot, :hard, :good, :easy}"
  (let ((t-d (- 11.0 difficulty))
        (t-s (expt stability (- (srs--w 9))))
        (t-r (- (exp (* (srs--w 10)
                        (- 1.0 retrievability)))
                1))
        (h (pcase grade
             (:hard (srs--w 15))
             (_ 1.0)))
        (b (pcase grade
             (:easy (srs--w 16))
             (_ 1.0)))
        (c (exp (srs--w 8))))
    (let ((α (+ 1.0 (* t-d t-s t-r h b c))))
      (* stability α))))

(defun srs--stability-on-failure (difficulty stability retrievability)
  "Return new stability of flashcard upon failure.
DIFFICULTY, STABILITY and RETRIEVABILITY are floats."
  (let ((d-f (expt difficulty
                   (- (srs--w 12))))
        (s-f (- (expt (+ stability 1.0)
                      (srs--w 13))
                1.0))
        (r-f (exp (* (srs--w 14)
                     (- 1.0 retrievability))))
        (c-f (srs--w 11)))
    (min stability
         (* d-f s-f r-f c-f))))

(defun srs--stability (difficulty stability retrievability grade)
  "Return updated stability of flashcard after review.
DIFFICULTY, STABILITY and RETRIEVABILITY are floats.  GRADE is one of
{:forgot, :hard, :good, :easy}"
  (pcase grade
    (:forgot (srs--stability-on-failure difficulty stability retrievability))
    (_ (srs--stability-on-success difficulty stability retrievability grade))))

;; ┌────────────┐
;; │ Difficulty │
;; └────────────┘
(defun srs--grade-num (grade)
  "Convert user's self-rating (GRADE) from keyword to number.

:forgot => 1
:hard   => 2
:good   => 3
:easy   => 4"
  (pcase grade
    (:forgot 1)
    (:hard 2)
    (:good 3)
    (:easy 4)))

(defun srs--difficulty-initial (grade)
  "Initial difficulty after first review.
GRADE is a keyword in {:forgot, :hard, :good, :easy}"
  (1+ (- (srs--w 4)
         (exp (* (srs--w 5)
                 (- (srs--grade-num grade)
                    1))))))

(defun srs--clamp-d (d)
  "Clamp D between 1.0 and 10.0."
  (max 1.0 (min d 10.0)))

(defun srs--difficulty (difficulty grade)
  "Return updated DIFFICULTY based on GRADE after review."
  (srs--clamp-d
   (+ (* (srs--w 7)
         (srs--difficulty-initial :easy))
      (* (- 1.0 (srs--w 7))
         (srs--dp difficulty grade)))))

(defun srs--dp (difficulty grade)
  "Helper for `srs--difficulty'.
DIFFICULTY is a float.  GRADE is one of {:forgot, :hard, :good :easy}."
  (+ difficulty (* (srs--delta-d grade)
                   (/ (- 10.0 difficulty)
                      9.0))))

(defun srs--delta-d (grade)
  "Helper for `srs--difficulty'.
GRADE is one of {:forgot, :hard, :good :easy}."
  (* (- (srs--w 6))
     (- (srs--grade-num grade)
        3.0)))

(provide 'srs)
;;; srs.el ends here
