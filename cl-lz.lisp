;;;; cl-lz.lisp
;;;; Brennan Holten <bholten@protonmail.ch>

(in-package #:cl-lz)

(defmacro lazy (&body body)
  (let ((value (gensym))
        (force (gensym)))
    `(let ((,value nil)
           (,force nil))
       (lambda ()
         (unless ,force
           (setf ,value (progn ,@body))
           (setf ,force t))
         ,value))))

(defun force (lz-expr)
  (funcall lz-expr))

;; List Primitives
(defmacro lz-cons (a b)
  `(lazy (cons ,a ,b)))

(defun lz-nil ()
  (lazy nil))

(defun lz-null (x)
  (not (force x)))

(defun lz-car (lz-lst)
  (car (force lz-lst)))

(defun lz-cdr (lz-lst)
  (cdr (force lz-lst)))

;; Convienence Functions
(defun lz-caar (lz-list)
  (lz-car (lz-car lz-list)))

(defun lz-caaar (lz-list)
  (lz-car (lz-car (lz-car lz-list))))

(defun lz-caaaar (lz-list)
  (lz-car (lz-car (lz-car (lz-car lz-list)))))

(defun lz-cddr (lz-list)
  (lz-cdr (lz-cdr lz-list)))

(defun lz-cdddr (lz-list)
  (lz-cdr (lz-cdr (lz-cdr lz-list))))

(defun lz-cddddr (lz-list)
  (lz-cdr (lz-cdr (lz-cdr (lz-cdr lz-list)))))

(defun lz-cadr (lz-lst)
  (lz-car (lz-cdr lz-lst)))

(defun lz-caddr (lz-lst)
  (lz-car (lz-cdr (lz-cdr lz-lst))))

(defun lz-cadddr (lz-list)
  (lz-car (lz-cdr (lz-cdr (lz-cdr lz-list)))))

;; Constructors
(defun make-lz (lst)
  (lazy (when lst
          (cons (car lst) (make-lz (cdr lst))))))

(defun from (n &optional (step 1))
  (lz-cons n (from (+ step n))))

(defun repeat (n)
  (lz-cons n (repeat n)))

;; Destructors
(defun take (n lst)
  (unless (or (zerop n) (lz-null lst))
    (cons (lz-car lst) (take (1- n) (lz-cdr lst)))))

;; Higher order functions
(defun lz-mapcar (fn lst)
  (lazy (unless (lz-null lst)
          (cons (funcall fn (lz-car lst))
                (lz-mapcar fn (lz-cdr lst))))))

(defun lz-mapcan (fn lst)
  (labels ((f (lst-cur)
             (if (lz-null lst-cur)
                 (force (lz-mapcan fn (lz-cdr lst)))
                 (cons (lz-car lst-cur) (lazy (f (lz-cdr lst-cur)))))))
    (lazy (unless (lz-null lst)
            (f (funcall fn (lz-car lst)))))))

(defun lz-find-if (fn lst)
  (unless (lz-null lst)
    (let ((x (lz-car lst)))
      (if (funcall fn x)
          x
          (lz-find-if fn (lz-cdr lst))))))

(defun lz-nth (n lst)
  (if (zerop n)
      (lz-car lst)
      (lz-nth (1- n) (lz-cdr lst))))

(defun lz-filter (fn lst)
  (unless (lz-null lst)
    (let ((x (lz-car lst)))
      (if (funcall fn x)
          (lz-cons x (lz-filter fn (lz-cdr lst)))
          (lz-filter fn (lz-cdr lst))))))
