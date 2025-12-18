
;; title: SignerGateway - Governance Contract
;; version: 1.0.0
;; description: Decentralized voting system for sBTC signer verification

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u501))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u502))
(define-constant ERR-PROPOSAL-EXPIRED (err u503))
(define-constant ERR-PROPOSAL-NOT-ACTIVE (err u504))
(define-constant ERR-ALREADY-VOTED (err u505))
(define-constant ERR-INSUFFICIENT-VOTES (err u506))
(define-constant ERR-ALREADY-EXECUTED (err u507))

;; Contract owner and constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant VOTING-PERIOD u1008) ;; ~1 week in blocks (assuming 10 min blocks)
(define-constant MIN-VOTES-REQUIRED u3) ;; Minimum votes needed to pass
(define-constant APPROVAL-THRESHOLD u60) ;; 60% approval needed

;; Data structures
(define-map proposals
  uint ;; proposal-id
  {
    proposer: principal,
    signer-address: principal,
    proposal-type: (string-ascii 20), ;; "verify", "reject", "remove"
    description: (string-ascii 200),
    start-block: uint,
    end-block: uint,
    yes-votes: uint,
    no-votes: uint,
    status: (string-ascii 10), ;; "active", "passed", "failed", "executed"
    created-at: uint
  }
)

;; Track who voted on which proposal
(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, block-height: uint }
)

;; Track eligible voters (for now, any registered signer can vote)
(define-map eligible-voters principal bool)

;; Proposal counter
(define-data-var proposal-counter uint u0)

;; List of all proposal IDs
(define-data-var all-proposals (list 1000 uint) (list))

;; Public function: Create a proposal for signer verification
(define-public (create-verification-proposal 
  (signer-address principal)
  (proposal-type (string-ascii 20))
  (description (string-ascii 200)))
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (start-block block-height)
    (end-block (+ block-height VOTING-PERIOD))
    (current-proposals (var-get all-proposals))
  )
    ;; Validate proposal type
    (asserts! (or (is-eq proposal-type "verify") 
                  (is-eq proposal-type "reject")
                  (is-eq proposal-type "remove")) ERR-NOT-AUTHORIZED)
    
    ;; Create the proposal
    (map-set proposals proposal-id {
      proposer: tx-sender,
      signer-address: signer-address,
      proposal-type: proposal-type,
      description: description,
      start-block: start-block,
      end-block: end-block,
      yes-votes: u0,
      no-votes: u0,
      status: "active",
      created-at: block-height
    })
    
    ;; Add to proposal list and increment counter
    (var-set all-proposals (unwrap! (as-max-len? (append current-proposals proposal-id) u1000) ERR-NOT-AUTHORIZED))
    (var-set proposal-counter proposal-id)
    
    ;; Make proposer eligible to vote (if not already)
    (map-set eligible-voters tx-sender true)
    
    (ok proposal-id)
  )
)

;; Public function: Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (voter tx-sender)
    (current-block block-height)
  )
    ;; Check if proposal is still active
    (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-NOT-ACTIVE)
    (asserts! (<= current-block (get end-block proposal)) ERR-PROPOSAL-EXPIRED)
    
    ;; Check if voter is eligible (for now, anyone can vote, but we track it)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: voter })) ERR-ALREADY-VOTED)
    
    ;; Record the vote
    (map-set votes { proposal-id: proposal-id, voter: voter } 
      { vote: vote-yes, block-height: current-block })
    
    ;; Update proposal vote counts
    (if vote-yes
      (map-set proposals proposal-id 
        (merge proposal { yes-votes: (+ (get yes-votes proposal) u1) }))
      (map-set proposals proposal-id 
        (merge proposal { no-votes: (+ (get no-votes proposal) u1) }))
    )
    
    ;; Make voter eligible for future votes
    (map-set eligible-voters voter true)
    
    (ok true)
  )
)

;; Public function: Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
    (approval-rate (if (> total-votes u0) 
                     (/ (* (get yes-votes proposal) u100) total-votes) 
                     u0))
  )
    ;; Check if proposal has expired
    (asserts! (> block-height (get end-block proposal)) ERR-PROPOSAL-NOT-ACTIVE)
    
    ;; Check if proposal is still in active state
    (asserts! (is-eq (get status proposal) "active") ERR-ALREADY-EXECUTED)
    
    ;; Check if minimum votes requirement is met
    (asserts! (>= total-votes MIN-VOTES-REQUIRED) ERR-INSUFFICIENT-VOTES)
    
    ;; Determine if proposal passed
    (if (>= approval-rate APPROVAL-THRESHOLD)
      (begin
        ;; Proposal passed
        (map-set proposals proposal-id (merge proposal { status: "passed" }))
        ;; TODO: Here we would call the signer-registry contract to update verification status
        ;; For MVP, we just mark as passed - frontend can handle the integration
        (ok true)
      )
      (begin
        ;; Proposal failed
        (map-set proposals proposal-id (merge proposal { status: "failed" }))
        (ok false)
      )
    )
  )
)

;; Admin function: Add eligible voter (for bootstrapping)
(define-public (add-eligible-voter (voter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set eligible-voters voter true)
    (ok true)
  )
)

;; Read-only function: Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

;; Read-only function: Get vote details
(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; Read-only function: Check if address can vote
(define-read-only (is-eligible-voter (voter principal))
  (default-to false (map-get? eligible-voters voter))
)

;; Read-only function: Get all proposal IDs
(define-read-only (get-all-proposals)
  (var-get all-proposals)
)

;; Read-only function: Get proposal count
(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

;; Read-only function: Get active proposals
(define-read-only (get-active-proposals)
  (filter is-proposal-active (var-get all-proposals))
)

;; Helper function to check if proposal is active
(define-private (is-proposal-active (proposal-id uint))
  (match (map-get? proposals proposal-id)
    some-proposal (and (is-eq (get status some-proposal) "active")
                      (<= block-height (get end-block some-proposal)))
    false
  )
)

;; Read-only function: Get proposal results
(define-read-only (get-proposal-results (proposal-id uint))
  (match (map-get? proposals proposal-id)
    some-proposal (let (
      (total-votes (+ (get yes-votes some-proposal) (get no-votes some-proposal)))
      (approval-rate (if (> total-votes u0) 
                       (/ (* (get yes-votes some-proposal) u100) total-votes) 
                       u0))
    )
      (ok {
        yes-votes: (get yes-votes some-proposal),
        no-votes: (get no-votes some-proposal),
        total-votes: total-votes,
        approval-rate: approval-rate,
        status: (get status some-proposal),
        meets-threshold: (and (>= total-votes MIN-VOTES-REQUIRED) 
                             (>= approval-rate APPROVAL-THRESHOLD))
      }))
    (err ERR-PROPOSAL-NOT-FOUND)
  )
)
