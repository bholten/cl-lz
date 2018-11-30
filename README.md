# cl-lz

This is a simple lazy evaluation library. It is mostly done for educational reasons, because I found that in learning Common Lisp re-implementing some of these functions and data structures is a good idea.

This library is not for production use. It does not implement any parallel or concurrent primitives (maybe someday). It is basically only for fun.

The main idea of this was lifted from the excellent book, "Land of Lisp", which I highly recommend.

### Example

```common-lisp
;; Contains a few generators for creating lazy lists, e.g.
(defparameter *integers* (from 1))
(defparameter *first10* (take 10 *integers*))
;; (1 2 3 4 5 6 7 8 9 10)

;; They can also be composed:
(defparameter *evens* (lz-filter #'evenp (from 2)))
(take 10 *events*)
;; (2 4 6 8 10 12 14 16 18 20)

(defun sieve (lz)
  (let ((head (lz-car lz))
        (tail (lz-cdr lz)))
    (lz-cons head (sieve (lz-filter (lambda (n)
                                      (not (eql 0 (mod n head))))
                                    tail)))))

(defparameter *prime-numbers* (sieve (from 2)))
(take 10 *prime-numbers*)
;; (2 3 5 7 11 13 17 23 29)

```

## License

MIT

