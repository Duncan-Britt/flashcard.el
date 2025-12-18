;;; flashcard.el --- Spaced repetition in plain text -*- lexical-binding: t -*-

;; Author: Duncan Britt
;; Contact: https://github.com/Duncan-Britt/flashcard/issues
;; URL: https://github.com/Duncan-Britt/flashcard
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: flashcards, srs, memory
;; URL: https://github.com/Duncan-Britt/flashcard

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
;; creating and reviewing flashcards. Flashcards can be embedded among
;; your notes, or any text file, so long as you tell `flashcard.el'
;; where to look for them--by setting `flashcard-path-list'. The
;; intent is that creating flashcards should be as frictionless as
;; possible. You may be in the midst of writing down some notes, and
;; decide that a question and answer you have posed to yourself is
;; worth remembering. Put your cursor over the question, run
;;   M-x flashcard-make-at-point
;; and resume whatever you were doing.

;; ┌──────────┐
;; │ Features │
;; └──────────┘
;; - Flashcards can be embedded your notes.
;; - Minimal syntax compatible with most file types.
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
;;    (add-to-list 'flashcard-path-list (expand-file-name "~/Dropbox/notes/*.org")))

;; ┌────────────────────────────┐
;; │ Usage -- Making Flashcards │
;; └────────────────────────────┘
;; You can embed flashcards in any text file. A typical flashcard
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

;; The flash card is made. The metadata associated with the flashcard
;; is saved in `flashcard-history-file' (by default,
;; "<user-emacs-directory>/flashcard-history.org").

;; BUT--in order to drill your newly created flashcard, make sure the
;; file in which you wrote your flashcard is among those specified by
;; `flashcard-path-list'. This is how `flashcard.el' knows where to
;; look for flashcards.

;; Some examples:
;; (add-to-list 'flashcard-path-list (expand-file-name "~/path/to/your/notes/*"))
;; (add-to-list 'flashcard-path-list (expand-file-name "~/only/org/files/*.org"))
;; (add-to-list 'flashcard-path-list (expand-file-name "~/a/specific/file.txt"))
;; (add-to-list 'flashcard-path-list (expand-file-name "~/even/source/code.el"))

;; ┌───────────────────────────────┐
;; │ Usage -- Reviewing Flashcards │
;; └───────────────────────────────┘
;;  M-x flashcard-drill
;;
;; This is how you can review flashcards which are "due".

;; ┌─────────────────────────────┐
;; │ Usage -- Editing Flashcards │
;; └─────────────────────────────┘
;; Just revise the text of your card where you wrote it. You don't
;; need to run any additional commands. As long as the id is still
;; present above your card, all is well.

;;; Code:
(require 'transient)
(require 'flashcard-fsrs)
;; ┌────────┐
;; │ Custom │
;; └────────┘
(defgroup flashcard nil
  "Spaced repetition in plain text."
  :group 'applications)

(defcustom flashcard-path-list nil
  "A list of locations in which to search for flashcards.
    - file.txt       -- includes specific file
    - *.log          -- includes all =.log= files
    - notes/         -- includes entire directory
    - notes/**/*.org -- includes org files within notes or any subdirectories"
  :group 'flashcard)
;; In user config:
;; (add-to-list 'flashcard-path-list (expand-file-name "~/notes/*.org"))
;; (add-to-list 'flashcard-path-list (expand-file-name "./flashcard.el"))

(defcustom flashcard-designator "FC:"
  "Designator for flashcards.

For example, with the designator \"FC:\", a flashcard could be denoted
as \"FC: <flashcard-id>\n<Question>\n\n<Answer>\"."
  :group 'flashcard)

(defcustom flashcard-history-file (expand-file-name (concat user-emacs-directory "flashcard-history.org"))
  "A file in which to store users saved flashcard review history.

Essential for effective spaced repetition.")

;; ┌───────────────┐
;; │ Buffer Locals │
;; └───────────────┘
(defvar-local flashcard--current-id nil)
(defvar-local flashcard--current-type nil)
(defvar-local flashcard--answer nil)

;; ========

;; So, about to make a flashcard.

;; FC: B426A6EC-91E5-4EA6-865A-91F7973706B8
;; What's did I just do?

;; Warn if not in path list.

(defcustom flashcard-indicator-duration 1.0
  "Seconds to display success indicator.")

(defun flashcard--show-indicator (position)
  "Display success indicator overlay at POSITION.

Shows checkmark for `flashcard-indicator-duration' seconds.
Displays fetch timestamp in echo area if available.

Reuses existing indicator overlay if present, extending its timer.
This prevents overlay accumulation during rapid refreshes.

Called by `flashcard-make-at-point'."
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
fill-in-the-blank. Reformats text to include flashcard designator and id
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

(defvar flashcard--review-queue nil
  "Queue of flashcards to review.")

(defvar flashcard--window-config-before-drill)

(defun flashcard-drill ()
  "Drill flashcards."
  (interactive)
  (setq flashcard--window-config-before-drill (current-window-configuration))
  (setq flashcard--review-queue (flashcard--due-for-review))
  (if flashcard--review-queue
      (flashcard--drill-next-card)
    (message "No flashcards due today")))

(defun flashcard--drill-next-card ()
  "Drill the next card in the queue."
  (when flashcard--review-queue
    (let ((card (pop flashcard--review-queue)))
      (flashcard--drill-card card))))

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

;; FC: 5C032AFC-F05E-4775-9934-7D6D7A84F0A5
;; This is a {{flashcard}}.

;; FC: A683C1C9-15DD-40FD-971E-DBBC26189C0F
;; This is another one?

;; Yes it is

(defvar flashcard--current-file nil)
(defvar flashcard--current-line nil)

(defun flashcard--drill-card (card)
  "Drill card."
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

;; (defun flashcard-rate ()
;;   "Rate the current flashcard using keyword completion."
;;   (interactive)
;;   (let* ((choices '(("easy" . :easy) ("good" . :good) ("hard" . :hard) ("forgot" . :forgot)))
;;          (choice (completing-read "Rate this card (easy/good/hard/forgot): "
;;                                   (mapcar #'car choices) nil t))
;;          (rating (alist-get choice choices nil nil #'string=)))
;;     (when rating
;;       (flashcard--update-review-history flashcard--current-id rating)
;;       (if flashcard--review-queue
;;         (flashcard--drill-next-card)
;;         (flashcard-quit-review)))))

;; (defun flashcard--rate-easy ()
;;   "Rate the current flashcard easy."
;;   (interactive)
;;   (flashcard--update-review-history flashcard--current-id :easy)
;;   (if flashcard--review-queue
;;       (flashcard--drill-next-card)
;;     (flashcard-quit-review)))

(transient-define-suffix flashcard-rate (&optional args)
  "Rate the just-revealed card from transient menu.
Then continue."
  (interactive (list (transient-args transient-current-command)))
  (let ((grade (pcase (this-command-keys)
                 ("e" :easy)
                 ("g" :good)
                 ("h" :hard)
                 ("f" :forgot))))
    (flashcard--update-review-history flashcard--current-id grade)
    (if (and flashcard--review-queue
             (not (transient-arg-value "--visit-source" args)))
        (flashcard--drill-next-card)
      (flashcard-quit-review)
      (flashcard--visit-source))))

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
    ("-s" "Visit source (quit drilling)" "--visit-source")])

(defun flashcard-quit-review ()
  "Quit the current review session."
  (interactive)
  (kill-buffer "*flashcard*")
  (when (get-buffer-window "*flashcard*")
    (delete-window (get-buffer-window "*flashcard*")))
  (set-window-configuration flashcard--window-config-before-drill))

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
  (flashcard--rate-menu))

;; (pcase-let ((`(,id ,type . ,content) '("1F933760-D6B3-4A59-A9E2-09EC19A500CB" question
;;                                        ";; What is the powerhouse of the cell?\n" "\n;; Mitochondria.\n")))
;;   content)

;; (org-id-find "1F933760-D6B3-4A59-A9E2-09EC19A500CB")

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

;; (flashcard--update-review-history "1F933760-D6B3-4A59-A9E2-09EC19A500CB" :forgot)

(defun flashcard--days-since-timestamp (timestamp)
  "Days (float) since TIMESTAMP."
  (let ((time (encode-time (parse-time-string timestamp))))
    (/ (float-time (time-subtract (current-time) time))
       86400.0)))

;; (flashcard--days-since-timestamp "2025-12-10T20:19:18-0700")

(defun flashcard--time-add-days (time days)
  "Add DAYS (a decimal number) to TIME."
  (time-add time (seconds-to-time (* days 86400))))

;; (encode-time (parse-time-string "2025-12-16T20:19:18-0700"))


;; (format-time-string "%Y-%m-%dT%H:%M:%S%z")
;; "2025-12-16T20:19:18-0700"
;; (format-time-string "%Y-%m-%dT%H:%M:%S%z" (flashcard--time-add-days (current-time) 0.4))
;; "2025-12-17T05:55:28-0700"

;; (let ((initial-stability (flashcard--stability-initial :forgot))
;;       (initial-difficulty (flashcard--difficulty-initial :forgot)))
;;   (list initial-stability initial-difficulty (flashcard--days-til-next-review 0.9 initial-stability)))

;; (pcase-let ((`(1 ,x) '(2 2)))
;;   x)

;; (pcase '(2 2)
;;   (`(1 ,x) x))

(defun flashcard--due-for-review ()
  "Return list of flashcard locations matching DESIGNATOR.
Each location is a plist: (:file FILE :line LINE :text TEXT)."
  (cond
   ((and (fboundp 'rg) (executable-find "rg"))
    (flashcard--due-ripgrep))
   ((executable-find "grep")
    (flashcard--due-grep))
   (t
    (flashcard--due-native))))

;; (flashcard--due-grep)

(defun flashcard--due-ripgrep ()
  "Helper for `flashcard--due-for-review' using ripgrep."
  (save-excursion
    (let ((locations (flashcard--search-ripgrep))
          (results nil))
      (dolist (location locations results)
        (pcase-let ((`(,file ,line ,id) location))
          (pcase-let ((`(,history-file . ,position) (org-id-find id)))
            ;; Only collect cards due for review
            (when (with-temp-buffer
                    (org-mode)
                    (insert-file-contents history-file)
                    (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                      (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                   (current-time))))
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
            (when (with-temp-buffer
                    (org-mode)
                    (insert-file-contents history-file)
                    (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                      (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                   (current-time))))
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


;; (org-id-find "4D55B42A-1389-45CF-B242-E8EBFE7E0784")
;; (org-id-find "25F3A67D-142E-491A-B133-C753BA3D10F8")
;; ("~/.emacs.d/flashcard-history.org" . 243)

;; (org-entry-get (point) "created")


;; FC: 4984EF94-32FB-4DFA-903E-A09B05DBDE5C
;; Ok, new flashcard! What's going on?

;; Nothing much!

;; (flashcard--due-ripgrep)
;; (flashcard--search-ripgrep)

(defconst +flashcard--id-regexp+ "[0-9A-F]\\{8\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{12\\}")

;; (flashcard--get-all-flashcard-file-paths)

;; (flashcard--search-grep)

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

      (while (re-search-forward (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote flashcard-designator) "[[:space:]]*\\(" +flashcard--id-regexp+ "\\)\\s-*") nil t)
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

      (while (re-search-forward (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote flashcard-designator) "[[:space:]]*\\(" +flashcard--id-regexp+ "\\)\\s-*") nil t)
        (let* ((file (match-string 1))
               (line (string-to-number (match-string 2)))
               (id (match-string 3)))
          (push (list file
                      line
                      id)
                results))))
    (nreverse results)))

;; (apply #'call-process "rg" nil t nil
;;              "--with-filename" "-n" "-e" flashcard-designator
;;              (flashcard--get-all-flashcard-file-paths))

;; (apply #'call-process "grep" nil t nil
;;        "--with-filename" "-n" "-e" flashcard-designator
;;        '("/Users/duncan/code/my-emacs-packages/flashcard/flashcard.el"))


;; (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote flashcard-designator) "[[:space:]]*\\(" +flashcard--id-regexp+ "\\)$")



;; (re-search-forward "^\\([^:]+\\):\\([^:]+\\):.*FC:[[:space:]]*\\([0-9A-F]\\{8\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{12\\}\\)\\s-*" nil t)



(defun flashcard--id-at-point ()
  "Grab id from buffer at point if it matches UUID format."
  (let ((id (buffer-substring-no-properties
             (point)
             (save-excursion
               (skip-chars-forward "^ \t\n")
               (point)))))
    (when (string-match-p +flashcard--id-regexp+ id)
      id)))

;; (flashcard--due-native)
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
                    (when (with-current-buffer (find-file-noselect history-file)
                            (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                              (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                           (current-time))))
                      (move-end-of-line 1)
                      (push (append (list id file line saved-major-mode)
                                    (flashcard--parse-question-or-cloze-str-at-point))
                            results))))))))))))
;; (flashcard--due-native)

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

;; (flashcard--store-new)

;; (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
;; 2025-12-16T12:06:52-0700
;; [2025-12-16 Tue 11:58:57]
;; (parse-time-string "[2025-12-16 Tue 11:58:57]" "[%Y-%m-%d %a %T]")
;; (parse-time-string "2025-12-16T12:00:10-0700")
;; (pcase-let ((`(,sec ,min ,hour ,day ,mon ,year ,dow ,dst ,tz) '(57 58 11 16 12 2025 2 -1 nil)))
;;   year)

;; TODO replace pcase with pcase-exhaustive

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
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun flashcard--get-all-flashcard-file-paths ()
  "Return list of file paths specified by flashcard-path-list."
  (seq-filter #'file-regular-p
              (flashcard--mappend #'file-expand-wildcards flashcard-path-list)))

;; (flashcard--get-all-flashcard-file-paths)

;; (file-expand-wildcards "/Users/duncan/Dropbox/notes/*.org")

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
   ("q" "Quit drilling" flashcard-quit-review)])

(provide 'flashcard)
;;; flashcard.el ends here

;; Local Variables:
;; nameless-aliases: (("+fc" . "+flashcard"))
;; End:
