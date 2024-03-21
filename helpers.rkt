#lang racket
(require racket/vector
         racket/format
         gregor
         scribble/html
         scribble/html/html
         scribble/reader)

(define (rec-element els)
  (if (string? els) els
    (if (symbol? els)
      (make-element els null null)
      (call-with-values
        (λ () (attributes+body (drop els 1)))
        (λ (attrs body)
          (make-element (first els) attrs (map rec-element body)))))))

(define (rec-parse strn)
  (xml->string (map rec-element (read-inside (open-input-string strn)))))

(define (e->sl e)
  (list (literal (xml->string e))))

(define (divc clas cont)
  (e->sl (div 'class: clas cont)))

(define (rxtag s) (regexp (format "(?i:^~a: (.+)$)" s)))

(provide custom-parse)
(define (custom-parse file)
  (let ([ppl (make-immutable-hash
               (foldl (λ (line acc)
                         (if (empty? line)
                           acc
                           (let ([parts (string-split line)])
                             (append acc (list (list (first parts) (string-join (rest (drop-right parts 1))) (last parts)))))))
                      '()
                      (file->lines "people.txt")))])
    (for/fold ([body-lv null]
               [story-lv null]
               [lr-lv null]
               [block-class ""]
               #:result (add-newlines (append body-lv (divc "story" (append story-lv (divc "right" lr-lv))))))
      ([data (in-list (file->lines file))])
      (cond
        [(regexp-match? #rx"^--$" data)
         (if (string=? "" block-class)
           (values body-lv story-lv lr-lv "left")
           (if (string=? block-class "right")
             (values (append body-lv (divc "story" (append story-lv (divc "right" lr-lv)))) null null "left")
             (values body-lv (append story-lv (divc "left" lr-lv)) null "right")))]
        [(regexp-match? (rxtag "img") data)
         (let ([url (~a "https://efimero.github.io/xenia-images/" (second (regexp-match (rxtag "img") data)))])
           (values body-lv story-lv (append lr-lv (e->sl (a 'target: "_blank" 'href: url (img 'src: url)))) block-class))]
        [(regexp-match? (rxtag "sub") data)
         (values body-lv story-lv (append lr-lv (list (literal (~a "Submitted by: " (rec-parse (second (regexp-match (rxtag "sub") data))) "<br/>")))) block-class)]
        [(regexp-match? (rxtag "sref") data)
         (values body-lv story-lv (append lr-lv (e->sl (span (literal "Submitted by: ") (let ([subm (hash-ref ppl (second (regexp-match (rxtag "sref") data)))]) (a 'href: (second subm) (first subm))) (br)))) block-class)]
        [(regexp-match? (rxtag "aref") data)
         (values body-lv story-lv (append lr-lv (e->sl (div 'class: "author" (literal "Author: ") (let ([author (hash-ref ppl (second (regexp-match (rxtag "aref") data)))]) (a 'href: (second author) (first author))) (br)))) block-class)]
        [(regexp-match? (rxtag "author") data)
         (values body-lv story-lv (append lr-lv (e->sl (div 'class: "author" (literal (~a "Author: " (rec-parse (second (regexp-match (rxtag "author") data))) "<br/>"))))) block-class)]
        [(regexp-match? (rxtag "source") data)
         (values body-lv story-lv (append lr-lv (list (literal (~a "Source: " (rec-parse (second (regexp-match (rxtag "source") data))) "<br/>")))) block-class)]
        [(regexp-match? (rxtag "license") data)
         (values body-lv story-lv (append lr-lv (e->sl (div 'class: "license" (literal (~a "License: " (rec-parse (second (regexp-match (rxtag "license") data))) "<br/>"))))) block-class)]
        [(regexp-match? (rxtag "time") data)
         (let ([t (second (regexp-match (rxtag "time") data))])
           (values body-lv story-lv (append lr-lv (list (literal (~a (xml->string (element 'time 'datetime: t 'title: t (~t (iso8601->date t) "d MMMM y"))) "<br/>")))) block-class))]
        [else
          (values body-lv story-lv (append lr-lv (list (literal (rec-parse data)))) block-class)]))))
