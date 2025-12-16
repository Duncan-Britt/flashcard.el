;;; flashcard-fsrs.el --- FSRS algorithm implementation for flashcard.el  -*- lexical-binding: t; -*-

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
;; retrievability R in [0,1] - probability of recall, computed dynamically
;; stability S in [0, inf] - time in days for R to go form 1 to 0.9, stored in card
;; difficulty D in [1,10] - how hard it is to recall, stored in card

;; For each card due today:
;;   1. Show the user the question
;;   2. User mentally recalls the answer, and flips the card
;;   3. User rates their recall performance on the card.
;;   4. the algorithm updates the card's stability and difficulty values,
;;      calculates the next review interval, and schedules the card for that day

;; Focus of this file: In response to user's performance, update the
;; state of the card and schedule it for the next review.

;; User's self-rating is called the `grade':
;; 1 - "forgot"
;; 2 - "hard"
;; 3 - "good"
;; 4 - "easy"

(defconst +flashcard--F+ (/ 19.0 81.0))
(defconst +flashcard--C+ -0.5)
(defconst +flashcard--W+ [0.40255 1.18385 3.173 15.69105 7.1949 0.5345 1.4604 0.0046 1.54575 0.1192
                                  1.01925 1.9395 0.11 0.29605 2.2698 0.2315 2.9898 0.51655 0.6621])
(defmacro flashcard--w (i) `(aref ,+flashcard--W+ ,i))

(defun flashcard--retrievability (days-since-last-review stability-of-card)
  "Retrievability R in [0,1] - probability of recall."
  (expt (+ 1.0
           (* +flashcard--F+ (/ days-since-last-review
                              stability-of-card)) )
        +flashcard--C+))

(defun flashcard--days-til-next-review (desired-retention stability-of-card)
  "Days until next review.
This function is based on the `flashcard--retrievability' function, but
manipulated algebraically. DESIRED-RETENTION is equivalent to
retrievability."
  (* (/ stability-of-card
        +flashcard--F+)
     (- (expt desired-retention
              (/ 1.0 +flashcard--C+))
        1)))


(defun flashcard--stability-initial (grade)
  "Initial stability of flashcard given first grade."
  (flashcard--w (pcase grade
                  (:forgot 0)
                  (:hard 1)
                  (:good 2)
                  (:easy 3))))

(defun flashcard--stability-on-success (difficulty stability retrievability grade)
  "Updated stability of flashcard upon success."
  )

(provide 'flashcard-fsrs)
;;; flashcard-fsrs.el ends here
