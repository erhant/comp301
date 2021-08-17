#lang racket
(require "top.scm")
(require "lang.scm")
(scan&parse "let x = proc (y) -(10,x) in (x (x 1))")