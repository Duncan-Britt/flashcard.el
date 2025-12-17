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

;; ┌───────────┐
;; │ Constants │
;; └───────────┘
(defconst +flashcard-F+ (/ 19.0 81.0))
(defconst +flashcard-C+ -0.5)
(defconst +flashcard-W+ [0.40255 1.18385 3.173 15.69105 7.1949 0.5345 1.4604 0.0046 1.54575 0.1192
                          1.01925 1.9395 0.11 0.29605 2.2698 0.2315 2.9898 0.51655 0.6621])
(defmacro flashcard--w (i) `(aref ,+flashcard-W+ ,i))

;; ┌──────────┐
;; │ "Export" │
;; └──────────┘
(defun flashcard--fsrs-update (difficulty stability retrievability grade)
  "Update card metadata relevant for FSRS after review based on grade.
Returns updated metadata as (DIFFICULTY STABILITY RETRIEVABILITY).
TODO"
  )

;; ┌──────────────────────┐
;; │ Days til next review │
;; └──────────────────────┘
(defun flashcard--days-til-next-review (desired-retention stability-of-card)
  "Days until next review.
This function is based on the `flashcard--retrievability' function, but
manipulated algebraically. DESIRED-RETENTION is equivalent to
retrievability."
  (* (/ stability-of-card
        +flashcard-F+)
     (- (expt desired-retention
              (/ 1.0 +flashcard-C+))
        1)))

;; ┌────────────────┐
;; │ Retrievability │
;; └────────────────┘
(defun flashcard--retrievability (days-since-last-review stability-of-card)
  "Retrievability R in [0,1] - probability of recall."
  (expt (+ 1.0
           (* +flashcard-F+ (/ days-since-last-review
                        stability-of-card)) )
        +flashcard-C+))


;; ┌───────────┐
;; │ Stability │
;; └───────────┘
(defun flashcard--stability-initial (grade)
  "Initial stability of flashcard given first grade."
  (flashcard--w (pcase grade
         (:forgot 0)
         (:hard 1)
         (:good 2)
         (:easy 3))))

(defun flashcard--stability-on-success (difficulty stability retrievability grade)
  "Return new stability of flashcard upon success."
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
  "Return new stability of flashcard upon failure."
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
  "Return updated stability of flashcard after review."
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
  "Initial difficulty after first review."
  (1+ (- (flashcard--w 4)
         (exp (* (flashcard--w 5)
                 (- (flashcard--grade-num grade)
                    1))))))

(defun flashcard--clamp-d (d)
  "Clamp D between 1.0 and 10.0"
  (max 1.0 (min d 10.0)))

(defun flashcard--difficulty (difficulty grade)
  "Return updated DIFFICULTY based on GRADE after review."
  (flashcard--clamp-d
   (+ (* (flashcard--w 7)
         (flashcard--difficulty-initial :easy))
      (* (- 1.0 (flashcard--w 7))
         (flashcard--dp difficulty grade)))))

(defun flashcard--dp (difficulty grade)
  "Helper for `flashcard--difficulty'."
  (+ difficulty (* (flashcard--delta-d grade)
                   (/ (- 10.0 difficulty)
                      9.0))))

(defun flashcard--delta-d (grade)
  "Helper for `flashcard--difficulty'."
  (* (- (flashcard--w 6))
     (- (flashcard--grade-num grade)
        3.0)))

(provide 'flashcard-fsrs)
;;; flashcard-fsrs.el ends here

;; Local Variables:
;; nameless-aliases: (("+fc" . "+flashcard"))
;; End:
