;;; mock.el --- Some mock data for testing

(require 'srs)
(setq srs-path-list (list (expand-file-name "./mock.el")))
(setq srs-history-file (expand-file-name "./hist"))
(srs-set-prefix-kbd "s-")

;; FC: 0BBE7BF9-5F8F-4655-AD6E-E0D74350CA48
;; This is a question card

;; And here is the answer

;; FC: 30EF300D-E8D4-41A6-8464-7BF7AF386E43
;; This is a {{cloze}}

;; Could this become a card?

;; yes

;; This line is {{commented}}.
