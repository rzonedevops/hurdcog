;;; Attention - ECAN Attention Allocation for Cognitive Kernel
;;; Implements Economic Attention Networks for priority management
;;; Dynamically allocates cognitive resources based on importance and urgency

(define-module (cogkernel attention)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (cogkernel atomspace)
  #:use-module (cogkernel agents)
  #:export (make-attention-value
            attention-value?
            attention-value-sti
            attention-value-lti
            attention-value-vlti
            make-attention-bank
            attention-bank-add!
            attention-bank-update!
            attention-bank-get-focus
            attention-bank-allocate!
            attention-bank-stimulate!
            *global-attention-bank*))

;;; AttentionValue record for Short/Long/Very-Long-Term Importance
(define-record-type <attention-value>
  (make-attention-value-record sti lti vlti)
  attention-value?
  (sti attention-value-sti set-attention-value-sti!)     ; Short-term importance
  (lti attention-value-lti set-attention-value-lti!)     ; Long-term importance  
  (vlti attention-value-vlti set-attention-value-vlti!)) ; Very-long-term importance

;;; AttentionBank for managing attention allocation across atoms and agents
(define-record-type <attention-bank>
  (make-attention-bank-record atom-av agent-av total-funds focus-threshold)
  attention-bank?
  (atom-av attention-bank-atom-av)      ; Hash table: atom -> attention-value
  (agent-av attention-bank-agent-av)    ; Hash table: agent -> attention-value
  (total-funds attention-bank-total-funds set-attention-bank-total-funds!)
  (focus-threshold attention-bank-focus-threshold set-attention-bank-focus-threshold!))

;;; Create new attention value
(define* (make-attention-value #:optional (sti 0) (lti 0) (vlti 0))
  "Create a new attention value with specified importance levels"
  (make-attention-value-record sti lti vlti))

;;; Create new attention bank
(define* (make-attention-bank #:optional (total-funds 1000) (focus-threshold 100))
  "Create a new attention bank with specified total funds and focus threshold"
  (make-attention-bank-record (make-hash-table) (make-hash-table) 
                              total-funds focus-threshold))

;;; Add atom or agent to attention bank
(define (attention-bank-add! bank object attention-value)
  "Add an object (atom or agent) to the attention bank with given attention value"
  (cond
    ((atom? object)
     (hash-set! (attention-bank-atom-av bank) object attention-value))
    ((agent? object)
     (hash-set! (attention-bank-agent-av bank) object attention-value))
    (else
     (error "Object must be atom or agent" object))))

;;; Update attention value for an object
(define (attention-bank-update! bank object delta-sti delta-lti delta-vlti)
  "Update attention values for an object by specified deltas"
  (let* ((current-av (cond
                       ((atom? object)
                        (hash-ref (attention-bank-atom-av bank) object))
                       ((agent? object)
                        (hash-ref (attention-bank-agent-av bank) object))
                       (else #f))))
    (when current-av
      (set-attention-value-sti! current-av 
                               (+ (attention-value-sti current-av) delta-sti))
      (set-attention-value-lti! current-av 
                               (+ (attention-value-lti current-av) delta-lti))
      (set-attention-value-vlti! current-av 
                                (+ (attention-value-vlti current-av) delta-vlti)))))

;;; Get objects in attentional focus
(define (attention-bank-get-focus bank)
  "Get all objects currently in attentional focus (above threshold)"
  (let ((focused-atoms '())
        (focused-agents '())
        (threshold (attention-bank-focus-threshold bank)))
    
    ; Check atoms
    (hash-for-each (lambda (atom av)
                    (when (>= (attention-value-sti av) threshold)
                      (set! focused-atoms (cons atom focused-atoms))))
                  (attention-bank-atom-av bank))
    
    ; Check agents
    (hash-for-each (lambda (agent av)
                    (when (>= (attention-value-sti av) threshold)
                      (set! focused-agents (cons agent focused-agents))))
                  (attention-bank-agent-av bank))
    
    (append focused-atoms focused-agents)))

;;; Allocate attention to an object
(define (attention-bank-allocate! bank object amount)
  "Allocate attention to an object, deducting from total funds"
  (when (>= (attention-bank-total-funds bank) amount)
    (set-attention-bank-total-funds! bank 
                                    (- (attention-bank-total-funds bank) amount))
    (let ((current-av (cond
                        ((atom? object)
                         (hash-ref (attention-bank-atom-av bank) object))
                        ((agent? object)
                         (hash-ref (attention-bank-agent-av bank) object))
                        (else #f))))
      (if current-av
          (set-attention-value-sti! current-av 
                                   (+ (attention-value-sti current-av) amount))
          (attention-bank-add! bank object (make-attention-value amount 0 0))))))

;;; Stimulate object with attention
(define (attention-bank-stimulate! bank object stimulus-type intensity)
  "Stimulate an object based on stimulus type and intensity"
  (let ((sti-delta (case stimulus-type
                     ((URGENT) (* intensity 10))
                     ((IMPORTANT) (* intensity 5))
                     ((ROUTINE) intensity)
                     (else intensity))))
    (attention-bank-update! bank object sti-delta 0 0)))

;;; Global attention bank instance
(define *global-attention-bank* (make-attention-bank))