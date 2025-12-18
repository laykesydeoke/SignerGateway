
;; title: SignerGateway - sBTC Signer Registry Contract
;; version:
;; summary:
;; description: Registry for institutions to register as potential sBTC signers

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-REGISTERED (err u402))
(define-constant ERR-NOT-FOUND (err u403))
(define-constant ERR-INVALID-DATA (err u404))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map signers
  principal  ;; institution wallet address
  {
    name: (string-ascii 100),
    contact-email: (string-ascii 100),
    location: (string-ascii 50),
    website: (string-ascii 100),
    capabilities: (string-ascii 200),
    verification-status: (string-ascii 20),
    registration-block: uint,
    is-active: bool
  }
)

;; Track total number of registered signers
(define-data-var total-signers uint u0)

;; List of all registered signer addresses (for easy iteration)
(define-data-var signer-addresses (list 1000 principal) (list))

;; Public function: Register as a potential sBTC signer
(define-public (register-signer 
  (name (string-ascii 100))
  (contact-email (string-ascii 100))
  (location (string-ascii 50))
  (website (string-ascii 100))
  (capabilities (string-ascii 200)))
  (let (
    (signer-address tx-sender)
    (current-addresses (var-get signer-addresses))
  )
    ;; Check if already registered
    (asserts! (is-none (map-get? signers signer-address)) ERR-ALREADY-REGISTERED)
    
    ;; Validate required fields
    (asserts! (> (len name) u0) ERR-INVALID-DATA)
    (asserts! (> (len contact-email) u0) ERR-INVALID-DATA)
    
    ;; Register the signer
    (map-set signers signer-address {
      name: name,
      contact-email: contact-email,
      location: location,
      website: website,
      capabilities: capabilities,
      verification-status: "pending",
      registration-block: stacks-block-height,
      is-active: true
    })
    
    ;; Add to address list and increment counter
    (var-set signer-addresses (unwrap! (as-max-len? (append current-addresses signer-address) u1000) ERR-INVALID-DATA))
    (var-set total-signers (+ (var-get total-signers) u1))
    
    (ok true)
  )
)

;; Public function: Update signer information (only by signer themselves)
(define-public (update-signer-info
  (name (string-ascii 100))
  (contact-email (string-ascii 100))
  (location (string-ascii 50))
  (website (string-ascii 100))
  (capabilities (string-ascii 200)))
  (let (
    (signer-address tx-sender)
    (existing-signer (unwrap! (map-get? signers signer-address) ERR-NOT-FOUND))
  )
    ;; Validate required fields
    (asserts! (> (len name) u0) ERR-INVALID-DATA)
    (asserts! (> (len contact-email) u0) ERR-INVALID-DATA)
    
    ;; Update signer info (keep existing verification status and registration block)
    (map-set signers signer-address {
      name: name,
      contact-email: contact-email,
      location: location,
      website: website,
      capabilities: capabilities,
      verification-status: (get verification-status existing-signer),
      registration-block: (get registration-block existing-signer),
      is-active: (get is-active existing-signer)
    })
    
    (ok true)
  )
)

;; Public function: Deactivate signer (only by signer themselves)
(define-public (deactivate-signer)
  (let (
    (signer-address tx-sender)
    (existing-signer (unwrap! (map-get? signers signer-address) ERR-NOT-FOUND))
  )
    (map-set signers signer-address 
      (merge existing-signer { is-active: false }))
    (ok true)
  )
)

;; Admin function: Update verification status (only contract owner)
(define-public (update-verification-status 
  (signer-address principal)
  (new-status (string-ascii 20)))
  (let (
    (existing-signer (unwrap! (map-get? signers signer-address) ERR-NOT-FOUND))
  )
    ;; Only contract owner can update verification status
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set signers signer-address 
      (merge existing-signer { verification-status: new-status }))
    (ok true)
  )
)

;; Read-only function: Get signer information
(define-read-only (get-signer (signer-address principal))
  (map-get? signers signer-address)
)

;; Read-only function: Get total number of signers
(define-read-only (get-total-signers)
  (var-get total-signers)
)

;; Read-only function: Get all signer addresses
(define-read-only (get-all-signer-addresses)
  (var-get signer-addresses)
)

;; Read-only function: Check if address is registered signer
(define-read-only (is-registered-signer (signer-address principal))
  (is-some (map-get? signers signer-address))
)

;; Read-only function: Get signers by verification status
(define-read-only (get-signers-by-status (status (string-ascii 20)))
  (ok (fold check-status-fold (var-get signer-addresses) { status: status, result: (list) }))
)

;; Helper function for filtering by status using fold
(define-private (check-status-fold (signer-address principal) (acc { status: (string-ascii 20), result: (list 1000 principal) }))
  (let ((target-status (get status acc))
        (current-result (get result acc)))
    (match (map-get? signers signer-address)
      some-signer 
        (if (is-eq (get verification-status some-signer) target-status)
          { status: target-status, result: (unwrap-panic (as-max-len? (append current-result signer-address) u1000)) }
          acc)
      acc
    )
  )
)

;; Read-only function: Get active signers count
(define-read-only (get-active-signers-count)
  (len (filter is-active-signer (var-get signer-addresses)))
)

;; Helper function to check if signer is active
(define-private (is-active-signer (signer-address principal))
  (match (map-get? signers signer-address)
    some-signer (get is-active some-signer)
    false
  )
)

;; Read-only function: Get verified signers count
(define-read-only (get-verified-signers-count)
  (len (filter is-verified-signer (var-get signer-addresses)))
)

;; Helper function to check if signer is verified
(define-private (is-verified-signer (signer-address principal))
  (match (map-get? signers signer-address)
    some-signer (is-eq (get verification-status some-signer) "verified")
    false
  )
)
