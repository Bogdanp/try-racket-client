# try-racket-client

A tiny Racket client library for [try-racket].

## Installation

    raco pkg install try-racket-client

## Usage

```racket
#lang racket/base

(require try-racket-client)

(try-racket-eval 1)
;; => '#hasheq((duration . 0.4970703125) (output . #"") (result . "1"))

(try-racket-eval '(begin
                    (define (fib n)
                      (if (< n 2)
                          n
                          (+ (fib (- n 2))
                             (fib (- n 1)))))

                    (fib 8)))
;; => '#hasheq((duration . 1.865966796875) (output . #"") (result . "21"))
```

## License

    try-racket-client is licensed under the 3-Clause BSD license.


[try-racket]: https://github.com/bogdanp/try-racket
