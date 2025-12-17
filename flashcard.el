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
;; (add-to-list 'flashcard-path-list (expand-file-name "~/Dropbox/notes/*.org"))
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
      (message "Created new flashcard: %s" flashcard-id))))

(defvar flashcard--review-queue nil
  "Queue of flashcards to review.")

(defun flashcard-drill ()
  "Drill flashcards."
  (interactive)
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

(defun flashcard--drill-card (card)
  "Drill card."
  (delete-other-windows)
  (let* ((buffer-name "*flashcard*")
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (pcase card
      (`(,id ,mode cloze ,cloze)
       (funcall mode)
       (flashcard-show-question-mode)
       (setq-local flashcard--current-id id)
       (setq-local flashcard--current-type 'cloze)
       (setq-local flashcard--current-cloze cloze)
       (insert (concat (substitute-command-keys
                        "\\[flashcard-quit-review]")
                       (propertize " ⇒ Quit  " 'face 'shadow)
                       (substitute-command-keys
                        "\\[flashcard-show-answer]")
                       (propertize " ⇒ Reveal answer\n\n" 'face 'shadow)))
       (flashcard--insert-cloze cloze 'hide))
      (`(,id ,mode question ,question ,answer)
       (funcall mode)
       (flashcard-show-question-mode)
       (setq-local flashcard--answer answer)
       (setq-local flashcard--current-id id)
       (setq-local flashcard--current-type 'question)
       (insert (concat (substitute-command-keys
                        "\\[flashcard-quit-review]")
                       (propertize " ⇒ Quit  " 'face 'shadow)
                       (substitute-command-keys
                        "\\[flashcard-show-answer]")
                       (propertize " ⇒ Reveal answer\n\n" 'face 'shadow)))
       (insert question))
      (_ (error "Unrecognized flashcard format: %s" card)))))

(define-minor-mode flashcard-show-question-mode
  "Mode for showing flashcard questions."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'flashcard-show-answer)
            (define-key map (kbd "q") 'flashcard-quit-review)
            map))

(defun flashcard-rate ()
  "Rate the current flashcard using keyword completion."
  (interactive)
  (let* ((choices '(("easy" . :easy) ("good" . :good) ("hard" . :hard) ("forgot" . :forgot)))
         (choice (completing-read "Rate this card (easy/good/hard/forgot): "
                                  (mapcar #'car choices) nil t))
         (rating (alist-get choice choices nil nil #'string=)))
    (when rating
      (flashcard--update-review-history flashcard--current-id rating)
      (if flashcard--review-queue
        (flashcard--drill-next-card)
        (flashcard-quit-review)))))

(defun flashcard-quit-review ()
  "Quit the current review session."
  (interactive)
  (kill-buffer "*flashcard*")
  (when (get-buffer-window "*flashcard*")
    (delete-window (get-buffer-window "*flashcard*"))))

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
  (flashcard-rate))

;; (pcase-let ((`(,id ,type . ,content) '("1F933760-D6B3-4A59-A9E2-09EC19A500CB" question
;;                                        ";; What is the powerhouse of the cell?\n" "\n;; Mitochondria.\n")))
;;   content)

;; (org-id-find "1F933760-D6B3-4A59-A9E2-09EC19A500CB")

(defun flashcard--update-review-history (id grade)
  "Update review history of card with ID.

GRADE is used to calculate the next review deadline according to the
FSRS algorithm."
  (save-excursion
    (pcase-let ((`(,history-file . ,position) (org-id-find id)))
      (with-current-buffer (find-file-noselect history-file)
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
          (save-buffer)
          (message "Updated flashcard: %s" id))))))

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
   ;; ((fboundp 'rgrep) TODO
   ;;  (flashcard--due-grep))
   (t
    (flashcard--due-native))))

(defun flashcard--due-ripgrep ()
  "Helper for `flashcard--due-for-review' using ripgrep."
  (save-excursion
    (let ((locations (flashcard--search-ripgrep))
          (results nil))
      (dolist (location locations results)
        (pcase-let ((`(,file ,line ,id) location))
          (pcase-let ((`(,history-file . ,position) (org-id-find id)))
            ;; Only collect cards due for review
            (when (with-current-buffer (find-file-noselect history-file)
                    (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                      (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                   (current-time))))
              (with-current-buffer (find-file-noselect file)
                (goto-line line)
                (move-end-of-line 1)
                (push (cons id (cons major-mode (flashcard--parse-question-or-cloze-str-at-point)))
                      results)))))))))


;; (org-id-find "4D55B42A-1389-45CF-B242-E8EBFE7E0784")
;; (org-id-find "25F3A67D-142E-491A-B133-C753BA3D10F8")
;; ("~/.emacs.d/flashcard-history.org" . 243)

;; (org-entry-get (point) "created")

;; (flashcard--due-ripgrep)
;; (flashcard--search-ripgrep)

(defconst +flashcard--id-regexp+ "[0-9A-F]\\{8\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{12\\}")

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

      (while (re-search-forward (concat "^\\([^:]+\\):\\([^:]+\\):.*" (regexp-quote flashcard-designator) "[[:space:]]*\\(" +flashcard--id-regexp+ "\\)$") nil t)
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


(defun flashcard--id-at-point ()
  "Grab id from buffer at point if it matches UUID format."
  (let ((id (buffer-substring-no-properties
             (point)
             (save-excursion
               (skip-chars-forward "^ \t\n")
               (point)))))
    (when (string-match-p +flashcard--id-regexp+ id)
      id)))

(defun flashcard--due-native ()
  "Gather cards due for review.

Uses native Emacs search through files."
  (save-excursion
    (let ((files (flashcard--get-all-flashcard-file-paths))
          results)
      (dolist (file files results)
        (let ((saved-major-mode (with-current-buffer (find-file-noselect file)
                                  major-mode)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward (concat "^.*" (regexp-quote flashcard-designator)) nil t)
              (skip-chars-forward " \t\n\r\f")
              (let ((id (flashcard--id-at-point)))
                (when id
                  (pcase-let ((`(,history-file . ,position) (org-id-find id)))
                    ;; Only collect cards due for review
                    (when (with-current-buffer (find-file-noselect history-file)
                            (let ((next-review-deadline-str (org-entry-get position "next-review-deadline")))
                              (time-less-p (encode-time (parse-time-string next-review-deadline-str))
                                           (current-time))))
                      (move-end-of-line 1)
                      (push (cons id (cons saved-major-mode (flashcard--parse-question-or-cloze-str-at-point)))
                            results))))))))))))
;; (flashcard--due-native)

(defun flashcard--store-new ()
  "Store new flashcard in persistent storage."
  (save-excursion
    (write-region "\n* Card" nil flashcard-history-file t)
    (with-current-buffer (find-file-noselect flashcard-history-file)
      (goto-char (point-max))
      (let ((flashcard-id (org-id-get-create))
            (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
        (org-set-property "created" timestamp)
        (org-set-property "difficulty" "nil")
        (org-set-property "stability" "nil")
        (org-set-property "last-review-timestamp" "nil")
        (org-set-property "next-review-deadline" timestamp)
        (save-buffer)
        flashcard-id))))

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
  (flashcard--mappend #'file-expand-wildcards flashcard-path-list))

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

(provide 'flashcard)
;;; flashcard.el ends here

;; Local Variables:
;; nameless-aliases: (("+fc" . "+flashcard"))
;; End:
