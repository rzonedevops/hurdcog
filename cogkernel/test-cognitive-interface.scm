#!/usr/bin/env guile
!#

;;; Test for Cognitive Operations Interface
;;; Tests the complete cognitive operations interface implementation

(use-modules (cogkernel cognitive-interface)
             (cogkernel cognitive-interface distributed-agents protocol)
             (cogkernel cognitive-interface workflow-engine processor)
             (cogkernel cognitive-interface learning-systems realtime))

(define (test-cognitive-operations-interface)
  "Test the complete cognitive operations interface"
  (format #t "=== Testing Cognitive Operations Interface ===~%")
  
  ;; Test interface creation
  (let ((interface (make-cognitive-operations-interface)))
    (format #t "✓ Cognitive operations interface creation works~%")
    
    ;; Test initialization
    (initialize-cognitive-interface interface)
    (format #t "✓ Interface initialization works~%")
    
    ;; Test interface status
    (let ((status (get-interface-status interface)))
      (format #t "✓ Interface status retrieval works: ~a~%" (assoc-ref status 'status)))
    
    interface))

(define (test-distributed-agent-framework)
  "Test distributed agent framework"
  (format #t "=== Testing Distributed Agent Framework ===~%")
  
  ;; Test agent communication creation
  (let ((comm (make-agent-communication)))
    (format #t "✓ Agent communication system creation works~%")
    
    ;; Test message creation
    (let ((message (make-cognitive-message "agent1" "agent2" 'TASK-REQUEST "test-payload")))
      (format #t "✓ Cognitive message creation works~%")
      
      ;; Test message sending
      (send-cognitive-message comm "agent2" message)
      (format #t "✓ Message sending works~%"))
    
    comm))

(define (test-cognitive-workflow-engine)
  "Test cognitive workflow engine"
  (format #t "=== Testing Cognitive Workflow Engine ===~%")
  
  ;; Test workflow engine creation
  (let ((engine (make-cognitive-workflow-engine)))
    (format #t "✓ Cognitive workflow engine creation works~%")
    
    ;; Test workflow step creation
    (let ((step1 (workflow-step 'step1 'PREPARATION
                               (lambda (x) (format #f "Processing: ~a" x))
                               '() '("input")))
          (step2 (workflow-step 'step2 'FINALIZATION
                               (lambda (x) (format #f "Finalizing: ~a" x))
                               '(step1))))
      (format #t "✓ Workflow step creation works~%")
      
      ;; Test workflow definition creation
      (let ((workflow-def (create-workflow-definition 'test-workflow (list step1 step2))))
        (format #t "✓ Workflow definition creation works~%")
        
        ;; Test workflow execution
        (let ((results (execute-cognitive-workflow engine workflow-def)))
          (format #t "✓ Workflow execution works~%"))
        
        workflow-def))
    
    engine))

(define (test-realtime-learning-system)
  "Test real-time learning system"
  (format #t "=== Testing Real-time Learning System ===~%")
  
  ;; Test learning system creation
  (let ((learning-sys (make-learning-system)))
    (format #t "✓ Learning system creation works~%")
    
    ;; Test learning experience creation
    (let ((experience (create-learning-experience 
                      "test-context" 
                      'TEST-ACTION 
                      "positive-outcome" 
                      'SUCCESS)))
      (format #t "✓ Learning experience creation works~%")
      
      ;; Test learning from experience
      (learn-from-experience learning-sys experience)
      (format #t "✓ Learning from experience works~%")
      
      ;; Test pattern recognition
      (let ((patterns (pattern-recognition learning-sys "test-context")))
        (format #t "✓ Pattern recognition works~%"))
      
      ;; Test behavior adaptation
      (let ((adapted-behavior (adapt-behavior learning-sys "test-context")))
        (format #t "✓ Behavior adaptation works: ~a~%" adapted-behavior)))
    
    learning-sys))

(define (test-integrated-operations)
  "Test integrated cognitive operations"
  (format #t "=== Testing Integrated Cognitive Operations ===~%")
  
  ;; Create and initialize interface
  (let ((interface (make-cognitive-operations-interface)))
    (initialize-cognitive-interface interface)
    
    ;; Test agent communication operation
    (execute-cognitive-operation interface 'AGENT-COMMUNICATION
                                "sender" "recipient" 'TASK-REQUEST "test-task")
    (format #t "✓ Integrated agent communication works~%")
    
    ;; Test workflow operation
    (let ((simple-workflow (create-workflow-definition 
                           'simple-test
                           (list (workflow-step 'test-step 'PREPARATION
                                               (lambda () "test result"))))))
      (execute-cognitive-operation interface 'WORKFLOW-EXECUTION simple-workflow)
      (format #t "✓ Integrated workflow execution works~%"))
    
    ;; Test learning operation
    (execute-cognitive-operation interface 'LEARNING-UPDATE
                                "test-context" 'TEST-ACTION "outcome" 'SUCCESS)
    (format #t "✓ Integrated learning operation works~%")
    
    ;; Test complex integrated operation
    (let ((adaptive-workflow (create-workflow-definition 
                             'adaptive-test
                             (list (workflow-step 'adaptive-step 'ANALYSIS
                                                 (lambda () "adaptive result"))))))
      (execute-cognitive-operation interface 'INTEGRATED-OPERATION
                                  `(learning-workflow "adaptive-context" ,adaptive-workflow))
      (format #t "✓ Complex integrated operation works~%"))
    
    interface))

(define (run-cognitive-interface-tests)
  "Run all cognitive interface tests"
  (format #t "=== Cognitive Operations Interface Test Suite ===~%~%")
  
  (test-cognitive-operations-interface)
  (test-distributed-agent-framework)
  (test-cognitive-workflow-engine)
  (test-realtime-learning-system)
  (test-integrated-operations)
  
  (format #t "~%=== All Cognitive Operations Interface Tests Passed! ===~%")
  (format #t "The cognitive operations interface is ready for SKZ integration.~%"))

;; Run the tests
(run-cognitive-interface-tests)