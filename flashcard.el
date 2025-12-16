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

(defcustom flashcard-designator "FC:"
  "Designator for flashcards.

For example, with the designator \"FC:\", a flashcard could be denoted
as \"FC: <flashcard-id> <Question> <Answer>\"."
  :group 'flashcard)

(defcustom flashcard-history-file (expand-file-name (concat user-emacs-directory "flashcard-history"))
  "A file in which to store users saved flashcard review history.

Essential for effective spaced repetition.")

;; ┌──────────┐
;; │ Commands │
;; └──────────┘
(defun flashcard-make-at-point ()
  "Make flashcard starting from `word-at-point'.

Treats `word-at-point' as the beginning of a flashcard question or
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
    (let ((flashcard-id (flashcard--store-new (flashcard--parse-question-or-cloze-str-at-point))))
      (insert flashcard-id)
      (message "Created new flashcard: %s" flashcard-id))))

(defun flashcard-update-at-point ()
  "Revise an existing flashcard whle preserving review history."
  (interactive)
  (pcase (flashcard--parse-flashcard-at-point)
    (`(,flashcard-id ,question-or-cloze)
     (flashcard--store-update flashcard-id question-or-cloze))
    ('nil (user-error "No flashcard at point"))))

;; ┌──────────┐
;; │ Internal │
;; └──────────┘
(defun flashcard--store-update (flashcard-id question-or-cloze)
  "TODO Update flashcard designated by FLASHCARD-ID.

Writes updated QUESTION-OR-CLOZE to persistent storage."
  (message "Updated flashcard %s successfully" flashcard-id))

(defun flashcard--id-at-point ()
  "Grab id from buffer at point."
  (buffer-substring-no-properties
   (point)
   (save-excursion
     (skip-chars-forward "^ \t\n")
     (point))))

(defun flashcard--parse-flashcard-this-line ()
  "Parse flashcard starting on this line."
  (save-excursion
    (beginning-of-line)
    (when (search-forward flashcard-designator (line-end-position) t)
      (skip-chars-forward " \t\n\r\f")
      (let ((flashcard-id (flashcard--id-at-point)))
        (move-end-of-line 1)
        (list flashcard-id (flashcard--parse-question-or-cloze-str-at-point))))))

(defun flashcard--parse-flashcard-at-point ()
  "Return (list FLASHCARD-ID QUESTION-OR-CLOZE) or nil.
Returns nil if no flashcard at point."
  (save-excursion
    (if-let (flashcard-data (flashcard--parse-flashcard-this-line))
        flashcard-data
      ;; else
      (move-beginning-of-line 1)
      (skip-chars-backward " \t\n\r\f")
      (if-let (flashcard-data (flashcard--parse-flashcard-this-line))
          flashcard-data
        ;; else
        (move-beginning-of-line 1)
        (skip-chars-backward " \t\n\r\f")
        (flashcard--parse-flashcard-this-line)))))

(defun flashcard--store-new (question-or-cloze-str)
  "TODO Store question or cloze in persistent storage."
  (pcase question-or-cloze-str
    (`(question ,question ,answer)
     "123456")
    (`(cloze ,cloze)
     "654321")))

(defun flashcard--parse-question-or-cloze-str-at-point ()
  "Return `(question ,question ,answer) or `(cloze ,cloze).
Look ahead to find question beginning at nearest nonwhitespace character."
  (save-excursion
    (skip-chars-forward " \t\n\r\f")
    (let ((question-or-cloze-str (buffer-substring-no-properties (point) (line-end-position))))
      (if (string-match-p "{{\\([^}]*\\|[^}]*}[^}]*\\)}}" question-or-cloze-str)
          (list 'cloze question-or-cloze-str)
        (move-end-of-line 1)
        (skip-chars-forward " \t\n\r\f")
        (let ((answer (buffer-substring-no-properties (point) (line-end-position))))
          (list 'question question-or-cloze-str answer))))))

(defun flashcard--mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun flashcard--get-all-flashcard-file-paths ()
  "Return list of file paths specified by flashcard-path-list."
  (flashcard--mappend #'file-expand-wildcards flashcard-path-list))

;; (flashcard--get-all-flashcard-file-paths)

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
