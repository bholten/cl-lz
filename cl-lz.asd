;;;; cl-lz.asd

(asdf:defsystem #:cl-lz
    :description "A simply lazy evaluation library"
    :author "Brennan Holten <bholten@protonmail.ch>"
    :license  "MIT"
    :version "0.0.1"
    :serial t
    :components ((:file "package")
                 (:file "cl-lz")))
