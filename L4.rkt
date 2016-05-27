#lang racket
(require racket/cmdline)
(require "L4toL3.rkt"
         "backend.rkt")

(command-line
 #:program "L4 compiler"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
          (lambda (in)
            (let*    ([l3-code (L3-node->quote (L4->L3-compile (read in)))])
              (write   l3-code))))]
       [else (error "Provide a filename")]))