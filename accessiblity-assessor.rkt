#lang racket
(require csc151)
(require csc151/rex)
(require sxml)
(require html-parsing)
(require html-writing)
(require csc151www)
(require rackunit)

;;; (split-words str) -> list?
;;;  str : string?
;;; Returns the list of words in `str`
(define split-words
  (let ([splitter (rex-repeat (rex-any-of (rex-concat (rex-char-set ",;:.!?()@#$%^&~=_\"<>/[]{}|")
                                                      (rex-repeat-0 (rex-string " ")))
                                          (rex-string " ")
                                          (rex-string "\n")
                                          (rex-string "\r")
                                          (rex-string "\t")
                                          (rex-string "\r\n")))])
    (lambda (str)
      (rex-split-string splitter str))))

;;; (count-words str) -> integer?
;;;  str : string?
;;; Calculate the total number of words in `str`
(define count-words
  (lambda (str)
    (length (split-words (string-downcase str)))))


;;; (generate-alt-info sxml) -> list?
;;;  sxml : list?, an SXML structure
;;; Generate information about alt texts in `sxml`
(define generate-alt-info
  (lambda (sxml)
    (let* ([num-img (length (sxpath-match "//img" sxml))]
           [img-with-alt (sxpath-match "//img[@alt]" sxml)]
           [num-img-no-alt (- num-img (length img-with-alt))])
      (list 'p
            (string-append "The website has a total of "
                           (number->string num-img)
                           " images. "
                           "Of those, there are "
                           (number->string num-img-no-alt)
                           " images without alt text.")))))

;;; (generate-link-text-info sxml)
;;;  sxml : list?, an SXML structure
;;; Generate information about any text embeded with a link
(define generate-link-text-info
  (lambda (sxml)
    (let* ([links (sxpath-match "//a" sxml)]
           [list-text (reduce append (map (section sxpath-match
                                                   "//text()"
                                                   <>)
                                          links))]
           [bad-text? (lambda (str)
                        (or (equal? (string-downcase str) "click here")
                            (<= (count-words str) 1)))]
           [count-bad-text (length (filter bad-text? list-text))]
           [count-links (length links)]
           [is-or-are (if (> count-links 1)
                          "are"
                          "is")]
           [link-or-links (if (> count-links 1)
                              " links"
                              " link")]
           [link-to-be (if (> count-bad-text 1)
                           " links have"
                           " link has")])
      (list 'p
            (string-append "There "
                           is-or-are
                           " a total of "
                           (number->string (length links))
                           link-or-links
                           " on this website. "
                           "Of those, "
                           (number->string count-bad-text)
                           link-to-be
                           " insufficient descriptive text.")))))

;;; (tally-bad-link-pairs sxml) -> integer?
;;;  sxml : list?, an SXML structure
;;; Find two links next to one another that has no text in between
(define tally-bad-link-pairs
  (let ([poor-string? (lambda (str)
                        (and (string? str)
                             (rex-matches? (rex-repeat (rex-char-set " .!?"))
                                           str)))]
        [link? (lambda (sxml) (and (pair? sxml)
                                   (equal? 'a (car sxml))))])
    (lambda (sxml)
      (cond
        [(pair? sxml)
         (cond
           [(not (list? (car sxml)))
            (tally-bad-link-pairs (cdr sxml))]   
           [(and (link? (car sxml))
                 (not (null? (cdr sxml)))
                 (not (null? (cddr sxml))))
            (or (and (poor-string? (cadr sxml))
                     (link? (caddr sxml)))
                (link? (cadr sxml)))]
           [else
            (tally-value (map tally-bad-link-pairs sxml) #t)])]
        [else
         null]))))

;;; (generate-bad-link-pairs-info sxml) -> string?
;;;  sxml : list?, an SXML structure
;;; Generate information about any badly formated pairs of links
(define generate-bad-link-pairs-info
  (lambda (sxml)
    (let* ([tally (tally-bad-link-pairs sxml)]
           [is-or-are (if (<= tally 1)
                          "is "
                          "are ")])
      (list 'p
            (string-append "There "
                           is-or-are
                           (number->string tally)
                           " pairs of two links with no intervening text.")))))

;;; (assess-accessibility url) -> list?
;;;  url : string?, represents an URL
;;; Reads a page from the given URL and creates an SXML structure
;;; that reports on any accessibility flaws in the page.
(define assess-accessibility
  (lambda (url)
    (let ([fetched-sxml (fetch-page url)])
      (list 'div
            (generate-alt-info fetched-sxml)
            (generate-link-text-info fetched-sxml)
            (generate-bad-link-pairs-info fetched-sxml)))))