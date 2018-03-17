# cl-lz

This is a simple lazy evaluation library. It is mostly done for educational reasons, because as I am learning Common Lisp...

1. It is a good self-teaching exercise
2. Many of the primitives (map vs mapcar vs mapc vs mapcon, etc.) are confusing for beginners, and forcing myself to re-implement them is good
3. Easy to test against the eager counterparts
4. It is good practice with the standard Common Lisp packaging, workflow, testing, etc.

This library is not for production use. It does not implement any parallel or concurrent primitives (maybe someday). It is basically only for fun.

### Example

```common-lisp
(defparameter *integers* (from 1)) ; (1 2 3 4 5 6 ...) 
(defparameter *first10* (take 10 *integers*)) ; (1 2 3 4 5 6 7 8 9 10)
(defparameter *evens* (lz-filter #'evenp (from 2))) ; (2 4 6 8 ...)

(defun sieve (lz)
  (let ((head (lz-car lz))
        (tail (lz-cdr lz)))
    (lz-cons head (sieve (lz-filter (lambda (n)
                                      (not (eql 0 (mod n head))))
                                    tail)))))

(defparameter *prime-numbers* (sieve (from 2))) ; ((2 3 5 7 11 13 17 19 23 29 31 37 41 ...)

```

## License

MIT

