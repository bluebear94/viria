#lang racket
(require "modules/common.rkt")
#|
An expression is:
X
X ++ Y
X + Y
X - Y
X * Y
X / Y
X ^ Y
and so on...
Znp [X,...]
Zp([X,...])
{X, ...}
and so on...
|#


(define (tok-len ln in-string?) ; determines the length of the first token in a line
  ; and returns a pair with the token length (-1 if EOL, 0 if the lexer itself should decide)
  ; and the new string mode
  (if (> (string-length ln) 0)
      (letrec ([fc (string-ref ln 0)]
               [nxd (next-delimiter ln)])
        (cond
          [(char=? fc #\space)
           (if in-string? (cons 1 true) (tok-len (substring ln 1) false))] ; i. e. initial spaces are discarded
          [(or (char=? fc #\") (char=? fc #\»)) (cons 1 (not in-string?))] ; Quotes
          [(char-numeric? fc) (cons 1 in-string?)] ; Numbers
          [(or (char=? fc #\#)
               (char=? fc #\@)
               (char=? fc #\|)) (cons 0 in-string?)] ; stuffs; this will be handled later
          [(char=? fc #\,) (cons 1 in-string?)] ; Commas!
          
          [(char=? fc #\+) (cons (if (and (> (string-length ln) 1) (char=? (string-ref ln 1) #\+))
                                     (if in-string? 1 2)
                                     1) in-string?)] ; either add or append
          [(or (char=? fc #\-)
               (char=? fc #\*)
               (char=? fc #\/)) (cons 1 in-string?)]
          [(char=? fc #\!) (cons (if (and (> (string-length ln) 1) (char=? (string-ref ln 1) #\=))
                                     3 1) in-string?)] ; either factorial or not equal
          [(char=? fc #\=) (cons (if (and (> (string-length ln) 1) (char=? (string-ref ln 1) #\=))
                                     2 1) in-string?)]
          [(char=? fc #\&) (cons (if (and (> (string-length ln) 1) (char=? (string-ref ln 1) #\&))
                                     2 1) in-string?)]
          [(char=? fc #\|) (cons (if (and (> (string-length ln) 1) (char=? (string-ref ln 1) #\|))
                                     2 1) in-string?)]
          [(char=? fc #\^) (cons (cond [(> (string-length ln) 1) (cond [(or (char=? (string-ref ln 1) #\^)
                                                                            (char=? (string-ref ln 1) #\2)
                                                                            (char=? (string-ref ln 1) #\3)
                                                                            ) 2]
                                                                       [(and (> (string-length ln) 2)
                                                                             (char=? (string-ref ln 2) #\1)
                                                                             (char=? (string-ref ln 1) #\-)
                                                                             ) 3]
                                                                       [else 1])]
                                       [else 1]
                                       ) in-string?)]
          [(and (>= (string-length ln) 5)
                (or (string=? (substring ln 0 5) "prgm ")
                    (string=? (substring ln 0 5) "exec "))) (cons 0 false)] ; We'll let the lexer decide this too.
          ))
      ; We've reach EOL.
      (cons -1 false)
      ))

(define (feed-line ln string-mode?)
  (if (string=? ln "") empty
      (letrec ([next-right-guillemet (ss2 ln #\»)]
               [nd (if (= next-right-guillemet -1) (string-length ln) next-right-guillemet)])
        (begin
          
          0
          ))))

(define (eos op)
  (letrec ([type (call-name op)])
    (cond [(or (string=? type "cmd")
               (string=? type "cmdp")) 12]
          [(string=? type "postfix")
           (if (string=? (substring (first (call-args op)) 0 2) "to") 2 11)]
          [(string=? type "op") (letrec ([lename (first (call-args op))])
                                  (cond [(or (string=? lename "^")
                                             (string=? lename "xrt")) 10]
                                        [(or (string=? lename "nPr")
                                             (string=? lename "nCr")) 8]
                                        [(or (string=? lename "*")
                                             (string=? lename "/")
                                             (string=? lename "")) 7]
                                        [(or (string=? lename "+")
                                             (string=? lename "-")
                                             (string=? lename "++")) 6]
                                        [(or (string=? lename "=")
                                             (string=? lename "!=")
                                             (string=? lename "<")
                                             (string=? lename ">")
                                             (string=? lename "<=")
                                             (string=? lename ">=")) 5]
                                        [(string=? lename "&&") 4]
                                        [(or (string=? lename "||")
                                             (string=? lename "^^")) 3]
                                        [else 1]
                                        ))]
          [(string=? type "negation") 9]
          )))

#|
(define (lex-viria-2 prog)
  (if (string=? prog "") empty
      (begin
        (letrec ([next-right-guillemet (ss2 prog #\»)]
                 [next-newline (ss2 prog #\newline)]
                 [next-colon (ss2 prog #\:)]
                 [nd2 (min next-right-guillemet next-newline)]
                 [next-delimiter (if in-string-mode? n2 (min nd2 next-colon))]
                 [the-token empty])
          ...))))

|#

