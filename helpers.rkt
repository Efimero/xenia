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
  (for/fold ([body-lv null]
             [story-lv null]
             [lr-lv null]
             [block-class ""]
             #:result (add-newlines (append body-lv (divc "story" (append story-lv (divc "right" lr-lv))))))
            ([data (in-list (file->lines file))])
    (cond
      [(regexp-match? #rx"(?i:^left:\\s*$)" data)
       (if (string=? "" block-class)
         (values body-lv story-lv lr-lv "left")
         (values (append body-lv (divc "story" (append story-lv (divc "right" lr-lv)))) null null "left"))]
      [(regexp-match? #rx"(?i:^right:\\s*$)" data)
       (values body-lv (append story-lv (divc "left" lr-lv)) null "right")]
      [(regexp-match? (rxtag "img") data)
       (let ([url (~a "images/" (second (regexp-match (rxtag "img") data)))])
         (values body-lv story-lv (append lr-lv (e->sl (a 'target: "_blank" 'href: url (img 'src: url)))) block-class))]
      [(regexp-match? (rxtag "sub") data)
       (values body-lv story-lv (append lr-lv (list (literal (~a "Submitted by: " (rec-parse (second (regexp-match (rxtag "sub") data))) "<br/>")))) block-class)]
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
        (values body-lv story-lv (append lr-lv (list (literal (rec-parse data)))) block-class)])))
