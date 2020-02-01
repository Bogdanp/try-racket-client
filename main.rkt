#lang racket/base

(require json
         net/base64
         net/http-client
         net/uri-codec
         racket/match
         racket/port)

(provide
 current-try-racket
 try-racket-host
 try-racket-port
 try-racket-ssl?

 exn:fail:try-racket?
 exn:fail:try-racket-status
 exn:fail:try-racket-response

 make-try-racket
 try-racket?
 try-racket-eval)

(struct exn:fail:try-racket exn:fail (status response))

(struct try-racket (sema conn [sid #:mutable]))

(define try-racket-host (make-parameter "try-racket.defn.io"))
(define try-racket-port (make-parameter 443))
(define try-racket-ssl? (make-parameter #t))

(define current-try-racket
  (make-parameter #f))

(define (make-try-racket)
  (try-racket (make-semaphore 1)
              (http-conn)

              #f))

(define COMMON-HEADERS
  (list #"Accept: application/json"
        #"Content-type: application/x-www-form-urlencoded"))

(define (try-racket-eval e [c 'not-provided])
  (match-define (and (try-racket sema conn sid) client)
    (resolve-client c))

  (define (connect!)
    (http-conn-open! conn
                     (try-racket-host)
                     #:ssl? (try-racket-ssl?)
                     #:port (try-racket-port)
                     #:auto-reconnect? #t))

  (unless (http-conn-live? conn)
    (connect!))

  (call-with-semaphore sema
    (lambda ()
      (define headers
        (if sid
            (cons (bytes-append #"Cookie: _sid=" sid) COMMON-HEADERS)
            COMMON-HEADERS))

      (define data
        (alist->form-urlencoded
         `((e . ,(call-with-output-string
                  (lambda (out)
                    (write e out)))))))

      (let loop ([failures 0])
        (with-handlers ([exn:fail?
                         (lambda (the-exn)
                           (cond
                             [(zero? failures)
                              (connect!)
                              (loop (add1 failures))]

                             [else
                              (raise the-exn)]))])
          (match/values (http-conn-sendrecv! conn
                                             #"/eval"
                                             #:method #"POST"
                                             #:headers headers
                                             #:data data)
            [((regexp #"^HTTP/... 200 ") headers in)
             (define new-sid
               (for*/first ([h (in-list headers)]
                            [m (in-value (regexp-match #rx#"(?i:set-cookie: _sid=([^;]+))" h))]
                            #:when m)
                 (cadr m)))

             (begin0 (hash-update (read-json in) 'output base64-decode/string)
               (set-try-racket-sid! client (or new-sid sid)))]

            [((regexp #"^HTTP/... ([^ ]+)" (list _ code)) headers in)
             (raise (exn:fail:try-racket "request failed"
                                         (current-continuation-marks)
                                         (bytes->number code)
                                         (read-json in)))]))))))

(define (resolve-client c)
  (match c
    ['not-provided
     #:when (current-try-racket)
     (current-try-racket)]

    ['not-provided
     (define client (make-try-racket))
     (begin0 client
       (current-try-racket client))]

    [_ c]))

(define bytes->number (compose1 string->number bytes->string/utf-8))
(define base64-decode/string (compose1 base64-decode string->bytes/utf-8))
