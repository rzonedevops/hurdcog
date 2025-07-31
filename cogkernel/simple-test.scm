;;; Simple test to validate cognitive kernel components

;; Test AtomSpace
(format #t "=== Testing AtomSpace ===~%")
(load "atomspace/atomspace.scm")
(let ((atom (make-atom 'CONCEPT "test-concept")))
  (format #t "✓ Created atom: ~a (type: ~a)~%" (atom-name atom) (atom-type atom)))

;; Test Tensors
(format #t "=== Testing Tensors ===~%")
(load "tensors/tensors.scm")  
(let ((tensor (tensor-zeros '(2 2))))
  (format #t "✓ Created tensor with shape: ~a~%" (tensor-shape tensor)))

(format #t "=== Basic Cognitive Components Working! ===~%")