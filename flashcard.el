;;; flashcard.el --- Spaced repetition in plain text -*- lexical-binding: t -*-

;; Author: Duncan Britt
;; Contact: https://github.com/Duncan-Britt/flashcard/issues
;; URL: https://github.com/Duncan-Britt/flashcard
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
;; creating and reviewing flashcards.  Flashcards can be embedded among
;; your notes, or any text file, so long as you tell `flashcard.el'
;; where to look for them--by setting `flashcard-path-list'.

;; Use case: You're in the midst of taking notes when it occurs to you
;; that what you've just written would be worth committing to
;; memory.  You write a question above it, put your cursor over the
;; question, run M-x flashcard-make-at-point and resume notetaking.

;; ┌──────────┐
;; │ Features │
;; └──────────┘
;; - Flashcards can be embedded in any text file (e.g. your notes).
;; - No databases--flashcards and metadata are stored in text files.
;; - Implements Free Spaced Repetition Scheduler (FSRS) algorithm.
;;   https://github.com/open-spaced-repetition/fsrs4anki/wiki/ABC-of-FSRS
;; - No external dependencies

;; ┌──────────────┐
;; │ Installation │
;; └──────────────┘
;; Example Elpaca + use-package instalation
;;
;;  (use-package flashcard
;;    :ensure (:host github :repo "Duncan-Britt/flashcard.el")
;;    :config
;;    (add-to-list 'flashcard-path-list (expand-file-name "~/notes/*.org")))

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
;; │ The {{mitochondria}} is the powerhouse of the cell.  │
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
;; ┌─────────────────────────────┐
;; │ M-x flashcard-make-at-point │
;; └─────────────────────────────┘
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
;; is saved in `flashcard-history-file' (by default,
;; "<user-emacs-directory>/flashcard-history.org").

;; BUT--in order to review your newly created flashcard, make sure the
;; file in which you wrote your flashcard is among those specified by
;; `flashcard-path-list'.  This is how `flashcard.el' knows where to
;; look for flashcards.

;; Some examples:
;; (add-to-list 'flashcard-path-list (expand-file-name "~/path/to/your/notes/*"))
;; (add-to-list 'flashcard-path-list (expand-file-name "~/only/org/files/*.org"))
;; (add-to-list 'flashcard-path-list (expand-file-name "~/a/specific/file.txt"))
;; (add-to-list 'flashcard-path-list (expand-file-name "~/even/source/code.el"))

;; ┌───────────────────────────────┐
;; │ Usage -- Reviewing Flashcards │
;; └───────────────────────────────┘
;;  M-x flashcard-review
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
;;  M-x flashcard-delete-at-point
;;
;; Run this command with your cursor is over the line with <DESIGNATOR>: <ID>

;;; Code:
(require 'transient)
;; ┌────────┐
;; │ Custom │
;; └────────┘
(defgroup flashcard nil
  "Spaced repetition in plain text."
  :group 'applications)

(defcustom flashcard-path-list nil
  "A list of locations in which to search for flashcards.
Examples:
- file.txt       -- includes specific file
- *.log          -- includes all =.log= files
- notes/         -- includes entire directory
- notes/**/*.org -- includes org files within notes or any subdirectories"
  :group 'flashcard)

(defcustom flashcard-designator "FC:"
  "Designator for flashcards.

For example, with the designator \"FC:\", a flashcard could be denoted
as \"FC: <flashcard-id>\n<Question>\n\n<Answer>\"."
  :group 'flashcard)

(defcustom flashcard-history-file (expand-file-name (concat user-emacs-directory "flashcard-history.org"))
  "A file in which to store users saved flashcard review history.

Essential for effective spaced repetition."
  :group 'flashcard)

(defcustom flashcard-indicator-duration 1.0
  "Seconds to display success indicator."
  :group 'flashcard)
;; ┌────────────┐
;; │ End custom │
;; └────────────┘
(defvar-local flashcard--current-id nil)
(defvar-local flashcard--current-type nil)
(defvar-local flashcard--answer nil)

(defvar flashcard--review-queue nil "Queue of flashcards to review.")
(defvar flashcard--window-config-before-review nil "Saved window configuration to be restored after reviewing flashcards.")
(defvar flashcard--current-file nil "Source file of flashcard during reviewing.")
(defvar flashcard--current-line nil "Source line of flashcard during reviewing.")
(defvar flashcard--is-cramming nil "Cramming? Otherwise, rate cards.")

(defconst flashcard--id-regexp "[0-9A-F]\\{8\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{12\\}")

(defun flashcard--show-indicator (position)
  "Display success indicator overlay at POSITION.

Shows checkmark for `flashcard-indicator-duration' seconds.
Displays fetch timestamp in echo area if available.

Reuses existing indicator overlay if present, extending its timer.
This prevents overlay accumulation during rapid refreshes.

Called by `flashcard-make-at-point' and `flashcard-delete-at-point'."
  (when (> flashcard-indicator-duration 0)
    (let* ((beg position)
           (end (save-excursion
                  (goto-char beg)
                  (line-end-position)))
           ;; Check for existing indicator overlay
           (existing-ov
            (seq-find
             (lambda (ov)
               (overlay-get ov 'flashcard-indicator))
             (overlays-in beg end)))
           (ov (or existing-ov (make-overlay beg end))))

      ;; Cancel existing timer if overlay was reused
      (when existing-ov
        (when-let ((timer (overlay-get ov 'flashcard-timer)))
          (cancel-timer timer)))

      ;; Set overlay properties (idempotent if reusing)
      (overlay-put ov 'before-string
                   (propertize "☑ " 'face '(:foreground "green" :weight bold)))
      (overlay-put ov 'flashcard-indicator t)

      ;; Create new timer and store it on overlay
      (let ((timer (run-at-time flashcard-indicator-duration
                                nil
                                (lambda (overlay)
                                  (when (overlay-buffer overlay)
                                    (delete-overlay overlay)))
                                ov)))
        (overlay-put ov 'flashcard-timer timer)))))

(defun flashcard-make-at-point ()
  "Make flashcard starting from paragraph(s) at point.

Treats paragraph as the beginning of a flashcard question or
fill-in-the-blank.  Reformats text to include flashcard designator and id
before question, and inserts flashcard into persistant storage."
  (interactive)
  (save-excursion
    (open-line 1)
    (unless (bolp)
      (newline))
    (insert (flashcard--comment-marker))
    (insert flashcard-designator)
    (insert " ")
    (let ((flashcard-id (flashcard--store-new)))
      (insert flashcard-id)
      (move-beginning-of-line 1)
      (flashcard--show-indicator (point))
      (save-buffer)
      (message "Created new flashcard: %s" flashcard-id))
    (unless (member buffer-file-name (flashcard--get-all-flashcard-file-paths))
      (display-warning 'flashcard
                       (format "Created flashcard in file not found among `flashcard-path-list'.\nUse (add-to-list 'flashcard-path-list \"%s\")" buffer-file-name)
                       :warning))))

(defun flashcard-edit-tags-at-point ()
  "Edit tags of flashcard at point."
  (interactive)
  (save-excursion
    (let ((id nil)
          (line-start (line-beginning-position))
          (line-end (line-end-position))
          (completed-p))
      (goto-char line-start)
      (if (re-search-forward (concat (regexp-quote flashcard-designator)
                                     "[[:space:]]*\\("
                                     flashcard--id-regexp
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
                        (let* ((initial-tags-str (org-entry-get position "flashcard-tags"))
                               (initial-tags (when initial-tags-str
                                               (mapcar #'string-trim
                                                       (split-string initial-tags-str "," t)))))
                          (let* ((operation (completing-read (format "(Tags: %s)\nOperation: " (string-join initial-tags ", "))
                                                             '("add" "remove" "replace") nil t))
                                 (new-tags (pcase-exhaustive operation
                                             ("add" (let ((available-tags (seq-difference (flashcard--all-known-tags)
                                                                                          initial-tags #'string=)))
                                                      (seq-uniq (append initial-tags
                                                                        (completing-read-multiple
                                                                         "Add tags: " available-tags)))))
                                             ("remove" (seq-difference initial-tags (completing-read-multiple
                                                                                     "Remove tags: " initial-tags nil t)))
                                             ("replace" (completing-read-multiple
                                                         "Replace with: " (flashcard--all-known-tags)))))
                                 (tags-string (string-join new-tags ",")))
                            (goto-char position)
                            (org-set-property "flashcard-tags" tags-string)
                            (save-buffer)
                            (setq completed-p t))))
                      (unless file-was-open-p
                        (kill-buffer hist-buffer)))
                    (when completed-p
                      (move-beginning-of-line 1)
                      (flashcard--show-indicator (point)))
                    (message "Edited tags for flashcard: %s" id))
                ;; else
                (user-error "ID: %s not found in %s" id flashcard-history-file))))
        ;; else
        (user-error "No flashcard %s <ID> on this line." flashcard-designator)))))

(defun flashcard--all-known-tags ()
  "Return list of all unique tags used across all flashcards."
  (let ((tags (make-hash-table :test 'equal))
        (files (flashcard--get-all-flashcard-file-paths)))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward (concat "^.*" (regexp-quote flashcard-designator)) nil t)
          (skip-chars-forward " \t\n\r\f")
          (let ((id (flashcard--id-at-point)))
            (when id
              (pcase-let ((`(,history-file . ,position) (org-id-find id)))
                (with-current-buffer (find-file-noselect history-file)
                  (let ((tags-str (org-entry-get position "flashcard-tags")))
                    (when tags-str
                      (dolist (tag (mapcar #'string-trim
                                           (split-string tags-str "," t)))
                        (puthash tag t tags)))))))))))
    (hash-table-keys tags)))

(defun flashcard-delete-at-point ()
  "Delete flashcard at point from file and `flashcard-history-file'."
  (interactive)
  (save-excursion
    (let ((id nil)
          (line-start (line-beginning-position))
          (line-end (line-end-position)))
      (goto-char line-start)
      (if (re-search-forward (concat (regexp-quote flashcard-designator)
                                     "[[:space:]]*\\("
                                     flashcard--id-regexp
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
                    (flashcard--show-indicator (point))
                    (save-buffer)
                    (message "Deleted flashcard: %s" id))
                ;; else
                (unless history-entry
                  (user-error "ID: %s not found in %s" id flashcard-history-file)))))
        ;; else
        (user-error "No flashcard %s <ID> on this line." flashcard-designator)))))

(defun flashcard-review (&optional filter-by-tags-p)
  "Review flashcards which are due."
  (interactive "P")
  (setq flashcard--is-cramming nil)
  (setq flashcard--window-config-before-review (current-window-configuration))
  (let ((tags)
        (any-or-all-case))
    (when filter-by-tags-p
      (pcase-let ((`(,x ,y) (flashcard--prompt-read-tags)))
        (setq tags x)
        (setq any-or-all-case y)))
    (setq flashcard--review-queue (flashcard--due-for-review tags any-or-all-case))
    (if flashcard--review-queue
        (flashcard--review-next-card)
      (message "No flashcards due today"))))

(defun flashcard-cram (&optional filter-by-tags-p)
  "Cram flashcards.
Doesn't update flashcard review history."
  (interactive "P")
  (setq flashcard--is-cramming t)
  (setq flashcard--window-config-before-review (current-window-configuration))
  (let ((tags)
        (any-or-all-case))
    (when filter-by-tags-p
      (pcase-let ((`(,x ,y) (flashcard--prompt-read-tags)))
        (setq tags x)
        (setq any-or-all-case y)))
    (setq flashcard--review-queue (flashcard--due-for-review tags any-or-all-case))
    (if flashcard--review-queue
        (flashcard--review-next-card)
      (message "Found 0 flashcards"))))

(defun flashcard--prompt-read-tags ()
  "Prompt the user for tags and return selection."
  (let ((any-or-all-case (completing-read "Filter by tags: require [any] match or [all] matches? "
                               '("any" "all") nil t))
        (tags (completing-read-multiple
                          "Tags: " (flashcard--all-known-tags))))
    (list tags any-or-all-case)))

(defun flashcard-browse (&optional filter-by-tags-p)
  "Browse all flashcards in an occur-like buffer.
FILTER-BY-TAGS-P, (which can be set to non-NIL by using the prefix
argument when called interactively), when non-NIL, will prompt the user
for tags by which to filter the results."
  (interactive "P")
  (let ((tags)
        (any-or-all-case))
    (when filter-by-tags-p
      (pcase-let ((`(,x ,y) (flashcard--prompt-read-tags)))
        (setq tags x)
        (setq any-or-all-case y)))
    (cond
     ;; ((and (fboundp 'rg) (executable-find "rg"))
     ;;  (flashcard--browse-ripgrep tags any-or-all-case)) TODO implement someday
     ((executable-find "grep")
      (flashcard--browse-grep tags any-or-all-case))
     (t
      (flashcard--browse-native tags any-or-all-case)))))

(defun flashcard--browse-native (&optional tags any-or-all)
  "Helper for flashcard-browse using compilation-mode.
When TAGS is non-NIL, filter results. ANY-or-ALL specifies whether to
gather flashcards matching all TAGS or any TAGS."
  (when-let ((buf (get-buffer "*Flashcard Browse*")))
    (kill-buffer buf))
  (let ((files (flashcard--get-all-flashcard-file-paths))
        (results nil))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward (concat "^.*" (regexp-quote flashcard-designator)
                                          "\\s-*\\(" flashcard--id-regexp "\\)")
                                  nil t)
          (let ((line (line-number-at-pos))
                (id (match-string 1)))
            (when (or (null tags)
                      (flashcard--matches-tag-p id tags any-or-all))
              (push (format "%s:%d: %s" file line
                            (buffer-substring (line-beginning-position)
                                              (line-end-position)))
                    results))))))
    (if results
        (with-current-buffer (get-buffer-create "*Flashcard Browse*")
          (insert (mapconcat #'identity (nreverse results) "\n"))
          (compilation-mode)
          (setq-local compilation-directory default-directory)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))
      (message "No flashcards found"))))

(defun flashcard--browse-grep (&optional tags any-or-all)
  "Helper for flashcard-browse using grep.
When TAGS is non-NIL, filter results. ANY-or-ALL specifies whether to
gather flashcards matching all TAGS or any TAGS."
  (let ((files (flashcard--get-all-flashcard-file-paths))
        (pattern (concat (regexp-quote flashcard-designator) "\\s-*" flashcard--id-regexp)))
    (if (null files)
        (message "No flashcard files found in =flashcard-path-list'")
      ;; else
      (let ((grep-cmd (format "grep -n -H -e \"%s\" %s"
                              pattern
                              (mapconcat #'shell-quote-argument files " "))))
        (grep grep-cmd)
        ;; Add sentinel to filter after grep completes
        (let ((proc (get-buffer-process "*grep*")))
          (when proc
            (set-process-sentinel
             proc
             (let ((captured-tags tags)
                   (captured-any-or-all any-or-all))
               (lambda (process event)
                 (when (string-match-p "finished\\|exited" event)
                   (let ((grep-buffer (get-buffer "*grep*")))
                     (when (buffer-live-p grep-buffer)
                       (with-current-buffer grep-buffer
                         (rename-buffer "*Flashcard Browse*" t)
                         (when captured-tags
                           (flashcard--filter-grep-buffer
                            captured-tags
                            captured-any-or-all)))))))))))))))

(defun flashcard--filter-occur-buffer (tags any-or-all)
  "Remove results not matching TAGS.
ANY-OR-ALL determines whether cards must match any tag, or all TAGS."
  (read-only-mode -1)
  (unwind-protect
      (while (flashcard--occur-buffer-advance-card)
        (let ((card-id (match-string 1)))
          (unless (flashcard--matches-tag-p card-id tags any-or-all)
            (flashcard--occur-buffer-remove-card))))
    (read-only-mode 1)))

(defun flashcard--filter-grep-buffer (tags any-or-all)
  "Remove results not matching TAGS.
ANY-OR-ALL determines whether cards must match any tag, or all TAGS."
  (read-only-mode -1)
  (unwind-protect
      (while (flashcard--grep-buffer-advance-card)
        (let ((card-id (match-string 1)))
          (unless (flashcard--matches-tag-p card-id tags any-or-all)
            (flashcard--grep-buffer-remove-card))))
    (read-only-mode 1)))

(defun flashcard--matches-tag-p (card-id tags any-or-all)
  "Return non-NIL if card with CARD-ID matches TAGS.
If ANY-OR-ALL is \"any\", card only needs to match at least one tag.
Otherwise it must match all tags."
  (let ((history-entry (org-id-find card-id)))
    (when history-entry
      (pcase-let ((`(,history-file . ,position) history-entry))
        (let ((file-was-open-p (get-file-buffer history-file))
              (buffer (find-file-noselect history-file)))
          (unwind-protect
              (with-current-buffer buffer
                (let* ((tags-str (org-entry-get position "flashcard-tags"))
                       (card-tags (when tags-str
                                    (mapcar #'string-trim
                                            (split-string tags-str "," t)))))
                  (pcase-exhaustive any-or-all
                    ("any" (seq-intersection tags card-tags #'string=))
                    ("all" (seq-every-p (lambda (tag) (member tag card-tags)) tags)))))
            (unless file-was-open-p
              (kill-buffer buffer))))))))

(defun flashcard--occur-buffer-remove-card ()
  "Delete card from grep buffer.
Called by `flashcard--filter-grep-buffer'."
  ;; TODO
  ;; (delete-region (line-beginning-position) (line-end-position))
  ;; (delete-char 1)
  )

(defun flashcard--grep-buffer-remove-card ()
  "Delete card from grep buffer.
Called by `flashcard--filter-grep-buffer'."
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(defun flashcard--occur-buffer-advance-card ()
  "Move point to line of next card.
Return card id."
  (re-search-forward (concat (regexp-quote flashcard-designator)
                             "[[:space:]]*\\("
                             flashcard--id-regexp
                             "\\)")
                     nil t))

(defun flashcard--grep-buffer-advance-card ()
  "Move point to line of next card.
Return card id."
  (re-search-forward (concat (regexp-quote flashcard-designator)
                             "[[:space:]]*\\("
                             flashcard--id-regexp
                             "\\)")
                     nil t))

(defun flashcard--review-next-card ()
  "Review the next card in the queue."
  (when flashcard--review-queue
    (let ((card (pop flashcard--review-queue)))
      (flashcard--review-card card))))

(defun flashcard--insert-cloze (cloze-str type)
  "Insert CLOZE-STR.

TYPE is either 'HIDE or 'REVEAL."
  (pcase-exhaustive type
    ('hide
     (insert (replace-regexp-in-string "{{[^}]*}}"
                                       "[...]"
                                       cloze-str)))
    ('reveal
     (insert (replace-regexp-in-string "{{\\([^}]*\\)}}"
                                       "\\1"
                                       cloze-str)))))

(defun flashcard--review-card (card)
  "Review CARD."
  (delete-other-windows)
  (let* ((buffer-name "*flashcard*")
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (pcase card
      (`(,id ,file ,line ,mode cloze ,cloze)
       (funcall mode)
       (flashcard--question-menu)
       (setq-local flashcard--current-id id)
       (setq flashcard--current-file file)
       (setq flashcard--current-line line)
       (setq-local flashcard--current-type 'cloze)
       (setq-local flashcard--current-cloze cloze)
       (flashcard--insert-cloze cloze 'hide))
      (`(,id ,file ,line ,mode question ,question ,answer)
       (funcall mode)
       (flashcard--question-menu)
       (setq-local flashcard--answer answer)
       (setq-local flashcard--current-id id)
       (setq flashcard--current-file file)
       (setq flashcard--current-line line)
       (setq-local flashcard--current-type 'question)
       (insert question))
      (_ (error "Unrecognized flashcard format: %s" card)))))

(transient-define-suffix flashcard-rate (&optional args)
  "Rate the just-revealed card from transient menu.
Then continue."
  (interactive (list (transient-args transient-current-command)))
  (unless flashcard--is-cramming
    (let ((grade (pcase-exhaustive (this-command-keys)
                   ("e" :easy)
                   ("g" :good)
                   ("h" :hard)
                   ("f" :forgot))))
      (flashcard--update-review-history flashcard--current-id grade)))
  (let ((visit-source-p (transient-arg-value "--visit-source" args)))
    (if (and flashcard--review-queue
             (not visit-source-p))
        (flashcard--review-next-card)
      (flashcard-quit-review)
      (when visit-source-p
        (flashcard--visit-source)))))

(defun flashcard--visit-source ()
  "Visit source file of current flashcard."
  (find-file flashcard--current-file)
  (goto-line flashcard--current-line))

(transient-define-suffix flashcard--quit-review-suffix (&optional args)
  "Quit flashcard review from transient menu."
  (interactive (list (transient-args transient-current-command)))
  (flashcard-quit-review)
  (when (transient-arg-value "--visit-source" args)
    (flashcard--visit-source)))

(transient-define-prefix flashcard--rate-menu ()
  "Menu for flashcards once revealed."
  :refresh-suffixes t
  [["Rating"
    ("e" "Easy" flashcard-rate)
    ("g" "Good" flashcard-rate)
    ("h" "Hard" flashcard-rate)
    ("f" "Forgot" flashcard-rate)]
   ["Abort"
    ("q" "Quit without rating card" flashcard--quit-review-suffix)]]
  ["After rating or abort"
   ("-s" "Visit source (quit reviewing)" "--visit-source")])

(transient-define-prefix flashcard--cram-reveal-menu ()
  "Menu for flashcards once revealed."
  :refresh-suffixes t
  [["Continue"
    ("n" "Next card" flashcard-rate)]
   ["Abort"
    ("q" "Quit" flashcard--quit-review-suffix)]]
  ["After abort"
   ("-s" "Visit source (quit reviewing)" "--visit-source")])

(defun flashcard-quit-review ()
  "Quit the current review session."
  (interactive)
  (setq flashcard--is-cramming nil)
  (kill-buffer "*flashcard*")
  (when (get-buffer-window "*flashcard*")
    (delete-window (get-buffer-window "*flashcard*")))
  (set-window-configuration flashcard--window-config-before-review))

(defun flashcard-show-answer ()
  "Reveal the answer to the current flashcard."
  (interactive)
  (cond
   ((eq flashcard--current-type 'cloze)
    (erase-buffer)
    (flashcard--insert-cloze flashcard--current-cloze 'reveal))
   ((eq flashcard--current-type 'question)
    (insert "\n\n---\n\n")
    (insert flashcard--answer)))
  (if flashcard--is-cramming
      (flashcard--cram-reveal-menu)
    (flashcard--rate-menu)))

(defun flashcard--update-review-history (id grade)
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
                     (days-since-last-review (flashcard--days-since-timestamp last-review-timestamp))
                     (retrievability (flashcard--retrievability days-since-last-review stability-num))
                     (stability (flashcard--stability difficulty-num stability-num retrievability grade))
                     (difficulty (flashcard--difficulty difficulty-num grade))
                     (days-til-due (flashcard--days-til-next-review 0.9 stability)))
                (goto-char position)
                (org-set-property "stability" (number-to-string stability))
                (org-set-property "difficulty" (number-to-string difficulty))
                (org-set-property "last-review-timestamp" current-timestamp)
                (org-set-property "next-review-deadline"
                                  (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                      (flashcard--time-add-days (current-time) days-til-due))))
            ;; else (initial review)
            (let ((initial-stability (flashcard--stability-initial grade))
                  (initial-difficulty (flashcard--difficulty-initial grade)))
              (goto-char position)
              (org-set-property "stability" (number-to-string initial-stability))
              (org-set-property "difficulty" (number-to-string initial-difficulty))
              (org-set-property "last-review-timestamp" current-timestamp)
              (org-set-property "next-review-deadline"
                                (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                    (flashcard--time-add-days (current-time)
                                                                              (flashcard--days-til-next-review 0.9
                                                                                                               initial-stability))))
              (list initial-stability initial-difficulty )))
          (save-buffer)))
      (kill-buffer buffer))))

(defun flashcard--days-since-timestamp (timestamp)
  "Days (float) since TIMESTAMP."
  (let ((time (encode-time (parse-time-string timestamp))))
    (/ (float-time (time-subtract (current-time) time))
       86400.0)))

(defun flashcard--time-add-days (time days)
  "Add DAYS (a decimal number) to TIME."
  (time-add time (seconds-to-time (* days 86400))))

(defun flashcard--due-for-review (tags any-or-all)
  "Return list of flashcard locations matching DESIGNATOR.
Filtered by those due for review.

Each location is a list of the form
`(,ID ,FILE ,LINE-NUMBER ,MAJOR-MODE ,@CONTENT), where CONTENT takes the
form `(question ,QUESTION ,ANSWER) or `(cloze ,FILL-IN-THE-BLANK)."
  (let ((cards (cond
                ((and (fboundp 'rg) (executable-find "rg"))
                 (flashcard--due-ripgrep))
                ((executable-find "grep")
                 (flashcard--due-grep))
                (t
                 (flashcard--due-native)))))
    (if tags
        (seq-filter (lambda (card)
                      (flashcard--matches-tag-p (car card) tags any-or-all))
                    cards)
      cards)))

(defun flashcard--due-ripgrep ()
  "Helper for `flashcard--due-for-review' using ripgrep."
  (save-excursion
    (let ((locations (flashcard--search-ripgrep))
          (results nil))
      (dolist (location locations results)
        (pcase-let ((`(,file ,line ,id) location))
          (pcase-let ((`(,history-file . ,position) (org-id-find id)))
            ;; Only collect cards due for review
            (when (or flashcard--is-cramming
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
                              (flashcard--parse-question-or-cloze-str-at-point))
                      results)))))))))

(defun flashcard--due-grep ()
  "Helper for `flashcard--due-for-review' using grep."
  (save-excursion
    (let ((locations (flashcard--search-grep))
          (results nil))
      (dolist (location locations results)
        (pcase-let ((`(,file ,line ,id) location))
          (pcase-let ((`(,history-file . ,position) (org-id-find id)))
            ;; Only collect cards due for review
            (when (or flashcard--is-cramming
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
                              (flashcard--parse-question-or-cloze-str-at-point))
                      results)))))))))

(defun flashcard--search-grep ()
  "Use grep to find flashcard locations."
  (let ((files (flashcard--get-all-flashcard-file-paths))
        (results))
    (with-temp-buffer
      (apply #'call-process "grep" nil t nil
             "--with-filename" "-n" "-e" flashcard-designator
             files)
      (goto-char (point-min))
      (thing-at-point 'number)

      (while (re-search-forward (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote flashcard-designator) "[[:space:]]*\\(" flashcard--id-regexp "\\)\\s-*") nil t)
        (let* ((file (match-string 1))
               (line (string-to-number (match-string 2)))
               (id (match-string 3)))
          (push (list file
                      line
                      id)
                results))))
    (nreverse results)))

(defun flashcard--search-ripgrep ()
  "Use ripgrep to find flashcard locations."
  (let ((files (flashcard--get-all-flashcard-file-paths))
        (results))
    (with-temp-buffer
      (apply #'call-process "rg" nil t nil
             "--with-filename" "-n" "-e" flashcard-designator
             files)
      (goto-char (point-min))
      (thing-at-point 'number)

      (while (re-search-forward (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote flashcard-designator) "[[:space:]]*\\(" flashcard--id-regexp "\\)\\s-*") nil t)
        (let* ((file (match-string 1))
               (line (string-to-number (match-string 2)))
               (id (match-string 3)))
          (push (list file
                      line
                      id)
                results))))
    (nreverse results)))

(defun flashcard--id-at-point ()
  "Grab id from buffer at point if it matches UUID format."
  (let ((id (buffer-substring-no-properties
             (point)
             (save-excursion
               (skip-chars-forward "^ \t\n")
               (point)))))
    (when (string-match-p flashcard--id-regexp id)
      id)))

(defun flashcard--due-native ()
  "Gather cards due for review.

Uses native Emacs search through files."
  (save-excursion
    (let ((files (flashcard--get-all-flashcard-file-paths))
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
            (while (re-search-forward (concat "^.*" (regexp-quote flashcard-designator)) nil t)
              (skip-chars-forward " \t\n\r\f")
              (let ((id (flashcard--id-at-point))
                    (line (line-number-at-pos)))
                (when id
                  (pcase-let ((`(,history-file . ,position) (org-id-find id)))
                    ;; Only collect cards due for review
                    (when (or flashcard--is-cramming
                              (with-current-buffer (find-file-noselect history-file)
                                (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                                  (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                               (current-time)))))
                      (move-end-of-line 1)
                      (push (append (list id file line saved-major-mode)
                                    (flashcard--parse-question-or-cloze-str-at-point))
                            results))))))))))))

(defun flashcard--store-new ()
  "Store new flashcard in persistent storage."
  (save-excursion
    (write-region "\n* Card" nil flashcard-history-file t)
    (let ((file-was-open-p (get-file-buffer flashcard-history-file))
          (buf (find-file-noselect flashcard-history-file)))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-max))
            (let ((flashcard-id (org-id-get-create))
                  (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
              (org-set-property "created" timestamp)
              (org-set-property "difficulty" "nil")
              (org-set-property "stability" "nil")
              (org-set-property "last-review-timestamp" "nil")
              (org-set-property "next-review-deadline" timestamp)
              (save-buffer)
              flashcard-id))
        (unless file-was-open-p
          (kill-buffer buf))))))

(defun flashcard--parse-question-or-cloze-str-at-point ()
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

(defun flashcard--mappend (fn the-list)
  "Apply FN to each element of THE-LIST and append the results."
  (apply #'append (mapcar fn the-list)))

(defun flashcard--get-all-flashcard-file-paths ()
  "Return list of file paths specified by `flashcard-path-list'."
  (seq-filter #'file-regular-p
              (flashcard--mappend #'file-expand-wildcards flashcard-path-list)))

(defun flashcard--comment-marker ()
  "Return comment marker for the current mode, or \"\"."
  (if comment-start
      (cond ((and (= (length (string-trim comment-start)) 1)
                  (not (string-match-p " $" comment-start)))
             (concat comment-start comment-start " "))
            (t (if (string-match-p " $" comment-start)
                   comment-start
                 (concat comment-start " "))))
    ""))

(transient-define-prefix flashcard--question-menu ()
  "Menu for displaying flashcards (before reveal)."
  :refresh-suffixes t
  [("r" "Reveal card" flashcard-show-answer)
   ("q" "Quit reviewing" flashcard-quit-review)])

;; ┌──────────────────────────────────────────────────────────────────────────────────┐
;; │ FSRS algorithm implementation for flashcard.el                                   │
;; │ ==============================================                                   │
;; │ retrievability R in [0,1] - probability of recall, computed dynamically          │
;; │ stability S in [0, inf] - time in days for R to go form 1 to 0.9, stored in card │
;; │ difficulty D in [1,10] - how hard it is to recall, stored in card                │
;; │                                                                                  │
;; │ User's self-rating is called the `grade':                                        │
;; │ 1 - "forgot"                                                                     │
;; │ 2 - "hard"                                                                       │
;; │ 3 - "good"                                                                       │
;; │ 4 - "easy"                                                                       │
;; │                                                                                  │
;; │ (2025-12-17) This implementation was based off                                   │
;; │ https://borretti.me/article/implementing-fsrs-in-100-lines                       │
;; └──────────────────────────────────────────────────────────────────────────────────┘
;; ┌───────────┐
;; │ Constants │
;; └───────────┘
(defconst flashcard-F (/ 19.0 81.0))
(defconst flashcard-C -0.5)
(defconst flashcard-W [0.40255 1.18385 3.173 15.69105 7.1949 0.5345 1.4604 0.0046 1.54575 0.1192
                          1.01925 1.9395 0.11 0.29605 2.2698 0.2315 2.9898 0.51655 0.6621])
(defmacro flashcard--w (i) "Convenience wrapper for accessing weights via I, the index into `flashcard-W' vector." `(aref ,flashcard-W ,i))

;; ┌──────────────────────┐
;; │ Days til next review │
;; └──────────────────────┘
(defun flashcard--days-til-next-review (desired-retention stability-of-card)
  "Days until next review as function of DESIRED-RETENTION and STABILITY-OF-CARD.
This function is based on the `flashcard--retrievability' function, but
manipulated algebraically."
  (* (/ stability-of-card
        flashcard-F)
     (- (expt desired-retention
              (/ 1.0 flashcard-C))
        1)))

;; ┌────────────────┐
;; │ Retrievability │
;; └────────────────┘
(defun flashcard--retrievability (days-since-last-review stability-of-card)
  "Retrievability R in [0,1] - probability of recall.

DAYS-SINCE-LAST-REVIEW and STABILITY-OF-CARD are floats."
  (expt (+ 1.0
           (* flashcard-F (/ days-since-last-review
                        stability-of-card)) )
        flashcard-C))

;; ┌───────────┐
;; │ Stability │
;; └───────────┘
(defun flashcard--stability-initial (grade)
  "Initial stability of flashcard given first GRADE."
  (flashcard--w (pcase grade
         (:forgot 0)
         (:hard 1)
         (:good 2)
         (:easy 3))))

(defun flashcard--stability-on-success (difficulty stability retrievability grade)
  "Return new STABILITY of flashcard upon success.
DIFFICULTY, STABILITY, and RETRIEVABILITY are floats.  GRADE is one of
{:forgot, :hard, :good, :easy}"
  (let ((t-d (- 11.0 difficulty))
        (t-s (expt stability (- (flashcard--w 9))))
        (t-r (- (exp (* (flashcard--w 10)
                        (- 1.0 retrievability)))
                1))
        (h (pcase grade
             (:hard (flashcard--w 15))
             (_ 1.0)))
        (b (pcase grade
             (:easy (::w 16))
             (_ 1.0)))
        (c (exp (flashcard--w 8))))
    (let ((α (+ 1.0 (* t-d t-s t-r h b c))))
      (* stability α))))

(defun flashcard--stability-on-failure (difficulty stability retrievability)
  "Return new stability of flashcard upon failure.
DIFFICULTY, STABILITY and RETRIEVABILITY are floats."
  (let ((d-f (expt difficulty
                   (- (flashcard--w 12))))
        (s-f (- (expt (+ stability 1.0)
                      (flashcard--w 13))
                1.0))
        (r-f (exp (* (flashcard--w 14)
                     (- 1.0 retrievability))))
        (c-f (flashcard--w 11)))
    (min stability
         (* d-f s-f r-f c-f))))

(defun flashcard--stability (difficulty stability retrievability grade)
  "Return updated stability of flashcard after review.
DIFFICULTY, STABILITY and RETRIEVABILITY are floats.  GRADE is one of
{:forgot, :hard, :good, :easy}"
  (pcase grade
    (:forgot (flashcard--stability-on-failure difficulty stability retrievability))
    (_ (flashcard--stability-on-success difficulty stability retrievability grade))))

;; ┌────────────┐
;; │ Difficulty │
;; └────────────┘
(defun flashcard--grade-num (grade)
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

(defun flashcard--difficulty-initial (grade)
  "Initial difficulty after first review.
GRADE is a keyword in {:forgot, :hard, :good, :easy}"
  (1+ (- (flashcard--w 4)
         (exp (* (flashcard--w 5)
                 (- (flashcard--grade-num grade)
                    1))))))

(defun flashcard--clamp-d (d)
  "Clamp D between 1.0 and 10.0."
  (max 1.0 (min d 10.0)))

(defun flashcard--difficulty (difficulty grade)
  "Return updated DIFFICULTY based on GRADE after review."
  (flashcard--clamp-d
   (+ (* (flashcard--w 7)
         (flashcard--difficulty-initial :easy))
      (* (- 1.0 (flashcard--w 7))
         (flashcard--dp difficulty grade)))))

(defun flashcard--dp (difficulty grade)
  "Helper for `flashcard--difficulty'.
DIFFICULTY is a float.  GRADE is one of {:forgot, :hard, :good :easy}."
  (+ difficulty (* (flashcard--delta-d grade)
                   (/ (- 10.0 difficulty)
                      9.0))))

(defun flashcard--delta-d (grade)
  "Helper for `flashcard--difficulty'.
GRADE is one of {:forgot, :hard, :good :easy}."
  (* (- (flashcard--w 6))
     (- (flashcard--grade-num grade)
        3.0)))

(provide 'flashcard)
;;; flashcard.el ends here
