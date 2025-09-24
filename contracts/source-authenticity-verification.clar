;; title: source-authenticity-verification
;; version: 1.0.0
;; summary: Cryptographic verification of news sources, documents, and multimedia content authenticity
;; description: A comprehensive smart contract for verifying the authenticity of news sources,
;;              documents, and multimedia content using cryptographic proofs and consensus mechanisms

;; traits
;;

;; token definitions
;;

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-hash (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-verification-type (err u105))
(define-constant err-insufficient-verifiers (err u106))
(define-constant err-already-verified (err u107))
(define-constant err-verification-expired (err u108))

;; Verification types
(define-constant verification-type-document u1)
(define-constant verification-type-multimedia u2)
(define-constant verification-type-source u3)

;; Verification status
(define-constant status-pending u0)
(define-constant status-verified u1)
(define-constant status-rejected u2)
(define-constant status-disputed u3)

;; data vars
;;
(define-data-var next-source-id uint u1)
(define-data-var next-document-id uint u1)
(define-data-var next-verification-id uint u1)
(define-data-var min-verifiers uint u3)
(define-data-var verification-threshold uint u66) ;; 66% consensus required
(define-data-var verification-expiry-blocks uint u144) ;; ~24 hours in blocks

;; data maps
;;
;; News source registration and metadata
(define-map sources
  { source-id: uint }
  {
    name: (string-ascii 256),
    url: (string-ascii 512),
    owner: principal,
    public-key: (buff 33),
    registration-height: uint,
    status: uint,
    reputation-score: uint,
    total-documents: uint
  }
)

;; Document hash registry
(define-map documents
  { document-id: uint }
  {
    content-hash: (buff 32),
    source-id: uint,
    document-type: uint,
    timestamp: uint,
    submitter: principal,
    metadata-uri: (string-ascii 512),
    verification-status: uint,
    verification-count: uint
  }
)

;; Verification records
(define-map verifications
  { verification-id: uint }
  {
    document-id: uint,
    verifier: principal,
    verification-type: uint,
    result: bool,
    proof-hash: (buff 32),
    timestamp: uint,
    stake-amount: uint
  }
)

;; Document verification aggregation
(define-map document-verification-summary
  { document-id: uint }
  {
    total-verifications: uint,
    positive-verifications: uint,
    negative-verifications: uint,
    final-status: uint,
    consensus-reached: bool,
    expiry-height: uint
  }
)

;; Source ownership mapping
(define-map source-owners { owner: principal } { source-ids: (list 100 uint) })

;; Verifier credentials
(define-map verified-verifiers
  { verifier: principal }
  {
    reputation: uint,
    total-verifications: uint,
    accuracy-rate: uint,
    stake-balance: uint,
    registration-height: uint
  }
)

;; public functions
;;

;; Register a new news source
(define-public (register-source (name (string-ascii 256)) (url (string-ascii 512)) (public-key (buff 33)))
  (let
    (
      (source-id (var-get next-source-id))
      (current-sources (default-to (list) (get source-ids (map-get? source-owners { owner: tx-sender }))))
    )
    (asserts! (< (len current-sources) u100) (err u109)) ;; Max 100 sources per owner
    (asserts! (> (len name) u0) (err u110)) ;; Name cannot be empty
    (asserts! (> (len url) u0) (err u111)) ;; URL cannot be empty
    (asserts! (is-eq (len public-key) u33) (err u112)) ;; Valid public key length
    
    (map-set sources
      { source-id: source-id }
      {
        name: name,
        url: url,
        owner: tx-sender,
        public-key: public-key,
        registration-height: stacks-block-height,
        status: status-verified,
        reputation-score: u100, ;; Start with base reputation
        total-documents: u0
      }
    )
    
    (map-set source-owners
      { owner: tx-sender }
      { source-ids: (unwrap-panic (as-max-len? (append current-sources source-id) u100)) }
    )
    
    (var-set next-source-id (+ source-id u1))
    (ok source-id)
  )
)

;; Submit a document for verification
(define-public (submit-document (content-hash (buff 32)) (source-id uint) (document-type uint) (metadata-uri (string-ascii 512)))
  (let
    (
      (document-id (var-get next-document-id))
      (source-info (unwrap! (map-get? sources { source-id: source-id }) err-not-found))
      (expiry-height (+ stacks-block-height (var-get verification-expiry-blocks)))
    )
    (asserts! (or (is-eq tx-sender (get owner source-info)) (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (or (is-eq document-type verification-type-document)
                  (is-eq document-type verification-type-multimedia)
                  (is-eq document-type verification-type-source)) err-invalid-verification-type)
    (asserts! (is-eq (len content-hash) u32) err-invalid-hash)
    
    (map-set documents
      { document-id: document-id }
      {
        content-hash: content-hash,
        source-id: source-id,
        document-type: document-type,
        timestamp: stacks-block-height,
        submitter: tx-sender,
        metadata-uri: metadata-uri,
        verification-status: status-pending,
        verification-count: u0
      }
    )
    
    (map-set document-verification-summary
      { document-id: document-id }
      {
        total-verifications: u0,
        positive-verifications: u0,
        negative-verifications: u0,
        final-status: status-pending,
        consensus-reached: false,
        expiry-height: expiry-height
      }
    )
    
    ;; Update source document count
    (map-set sources
      { source-id: source-id }
      (merge source-info { total-documents: (+ (get total-documents source-info) u1) })
    )
    
    (var-set next-document-id (+ document-id u1))
    (ok document-id)
  )
)

;; Register as a verifier
(define-public (register-verifier (stake-amount uint))
  (begin
    (asserts! (> stake-amount u0) (err u113))
    (asserts! (is-none (map-get? verified-verifiers { verifier: tx-sender })) err-already-exists)
    
    (map-set verified-verifiers
      { verifier: tx-sender }
      {
        reputation: u100, ;; Start with base reputation
        total-verifications: u0,
        accuracy-rate: u100,
        stake-balance: stake-amount,
        registration-height: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Submit verification for a document
(define-public (verify-document (document-id uint) (verification-type uint) (result bool) (proof-hash (buff 32)) (stake-amount uint))
  (let
    (
      (verification-id (var-get next-verification-id))
      (document-info (unwrap! (map-get? documents { document-id: document-id }) err-not-found))
      (verifier-info (unwrap! (map-get? verified-verifiers { verifier: tx-sender }) err-unauthorized))
      (summary (unwrap! (map-get? document-verification-summary { document-id: document-id }) err-not-found))
    )
    (asserts! (not (get consensus-reached summary)) err-already-verified)
    (asserts! (< stacks-block-height (get expiry-height summary)) err-verification-expired)
    (asserts! (>= (get stake-balance verifier-info) stake-amount) (err u114))
    (asserts! (is-eq (len proof-hash) u32) err-invalid-hash)
    
    ;; Record the verification
    (map-set verifications
      { verification-id: verification-id }
      {
        document-id: document-id,
        verifier: tx-sender,
        verification-type: verification-type,
        result: result,
        proof-hash: proof-hash,
        timestamp: stacks-block-height,
        stake-amount: stake-amount
      }
    )
    
    ;; Update verification summary
    (let
      (
        (new-total (+ (get total-verifications summary) u1))
        (new-positive (if result (+ (get positive-verifications summary) u1) (get positive-verifications summary)))
        (new-negative (if result (get negative-verifications summary) (+ (get negative-verifications summary) u1)))
        (positive-percentage (if (> new-total u0) (/ (* new-positive u100) new-total) u0))
        (consensus-reached (and (>= new-total (var-get min-verifiers))
                               (or (>= positive-percentage (var-get verification-threshold))
                                   (<= positive-percentage (- u100 (var-get verification-threshold))))))
        (final-status (if consensus-reached
                         (if (>= positive-percentage (var-get verification-threshold)) status-verified status-rejected)
                         status-pending))
      )
      
      (map-set document-verification-summary
        { document-id: document-id }
        {
          total-verifications: new-total,
          positive-verifications: new-positive,
          negative-verifications: new-negative,
          final-status: final-status,
          consensus-reached: consensus-reached,
          expiry-height: (get expiry-height summary)
        }
      )
      
      ;; Update document status if consensus reached
      (if consensus-reached
        (map-set documents
          { document-id: document-id }
          (merge document-info { verification-status: final-status })
        )
        true
      )
      
      ;; Update verifier stats
      (map-set verified-verifiers
        { verifier: tx-sender }
        (merge verifier-info {
          total-verifications: (+ (get total-verifications verifier-info) u1),
          stake-balance: (- (get stake-balance verifier-info) stake-amount)
        })
      )
    )
    
    (var-set next-verification-id (+ verification-id u1))
    (ok verification-id)
  )
)

;; Update source reputation based on document verification results
(define-public (update-source-reputation (source-id uint))
  (let
    (
      (source-info (unwrap! (map-get? sources { source-id: source-id }) err-not-found))
    )
    (asserts! (or (is-eq tx-sender (get owner source-info)) (is-eq tx-sender contract-owner)) err-unauthorized)
    ;; Implementation would calculate reputation based on verified documents
    ;; This is a simplified version
    (ok true)
  )
)

;; Admin function to set verification parameters
(define-public (set-verification-parameters (min-verifiers-new uint) (threshold-new uint) (expiry-blocks-new uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (> min-verifiers-new u0) (<= min-verifiers-new u100)) (err u115))
    (asserts! (and (> threshold-new u50) (< threshold-new u100)) (err u116))
    (asserts! (> expiry-blocks-new u0) (err u117))
    
    (var-set min-verifiers min-verifiers-new)
    (var-set verification-threshold threshold-new)
    (var-set verification-expiry-blocks expiry-blocks-new)
    (ok true)
  )
)

;; read only functions
;;

;; Get source information
(define-read-only (get-source (source-id uint))
  (map-get? sources { source-id: source-id })
)

;; Get document information
(define-read-only (get-document (document-id uint))
  (map-get? documents { document-id: document-id })
)

;; Get verification information
(define-read-only (get-verification (verification-id uint))
  (map-get? verifications { verification-id: verification-id })
)

;; Get document verification summary
(define-read-only (get-verification-summary (document-id uint))
  (map-get? document-verification-summary { document-id: document-id })
)

;; Get verifier information
(define-read-only (get-verifier-info (verifier principal))
  (map-get? verified-verifiers { verifier: verifier })
)

;; Get sources owned by a principal
(define-read-only (get-owned-sources (owner principal))
  (map-get? source-owners { owner: owner })
)

;; Check if document is verified
(define-read-only (is-document-verified (document-id uint))
  (match (map-get? document-verification-summary { document-id: document-id })
    summary (and (get consensus-reached summary) (is-eq (get final-status summary) status-verified))
    false
  )
)

;; Get verification parameters
(define-read-only (get-verification-parameters)
  {
    min-verifiers: (var-get min-verifiers),
    verification-threshold: (var-get verification-threshold),
    verification-expiry-blocks: (var-get verification-expiry-blocks)
  }
)

;; private functions
;;

;; Calculate reputation score based on verification history
(define-private (calculate-reputation-score (positive uint) (total uint))
  (if (is-eq total u0)
    u100
    (/ (* positive u200) total) ;; Scale to 0-200 range
  )
)

;; Validate content hash format
(define-private (is-valid-hash (hash (buff 32)))
  (is-eq (len hash) u32)
)
