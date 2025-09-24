;; title: journalist-credibility-system
;; version: 1.0.0
;; summary: Reputation scoring for journalists based on accuracy, bias detection, and peer review
;; description: A comprehensive smart contract for managing journalist credibility through
;;              transparent scoring, peer reviews, and accuracy tracking mechanisms

;; traits
;;

;; token definitions
;;

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-already-exists (err u202))
(define-constant err-unauthorized (err u203))
(define-constant err-invalid-score (err u204))
(define-constant err-invalid-period (err u205))
(define-constant err-insufficient-reviews (err u206))
(define-constant err-already-reviewed (err u207))
(define-constant err-self-review (err u208))
(define-constant err-invalid-article (err u209))
(define-constant err-expired-review (err u210))
(define-constant err-invalid-bias-score (err u211))

;; Journalist categories
(define-constant category-independent u1)
(define-constant category-mainstream u2)
(define-constant category-investigative u3)
(define-constant category-blogger u4)
(define-constant category-freelance u5)

;; Article categories
(define-constant article-breaking-news u1)
(define-constant article-investigation u2)
(define-constant article-opinion u3)
(define-constant article-analysis u4)
(define-constant article-feature u5)

;; Review statuses
(define-constant review-pending u0)
(define-constant review-completed u1)
(define-constant review-disputed u2)

;; Bias levels
(define-constant bias-neutral u0)
(define-constant bias-slight-left u1)
(define-constant bias-slight-right u2)
(define-constant bias-moderate-left u3)
(define-constant bias-moderate-right u4)
(define-constant bias-strong-left u5)
(define-constant bias-strong-right u6)

;; data vars
;;
(define-data-var next-journalist-id uint u1)
(define-data-var next-article-id uint u1)
(define-data-var next-review-id uint u1)
(define-data-var min-reviews-for-score uint u5)
(define-data-var review-expiry-blocks uint u1008) ;; ~7 days in blocks
(define-data-var credibility-decay-rate uint u2) ;; 2% decay per scoring period
(define-data-var min-reviewer-score uint u70) ;; Minimum score to be a reviewer

;; data maps
;;
;; Journalist profiles and credentials
(define-map journalists
  { journalist-id: uint }
  {
    name: (string-ascii 256),
    email: (string-ascii 256),
    bio: (string-ascii 1024),
    category: uint,
    owner: principal,
    registration-date: uint,
    credibility-score: uint,
    accuracy-score: uint,
    bias-score: uint,
    total-articles: uint,
    verified-articles: uint,
    peer-reviews-received: uint,
    peer-reviews-given: uint,
    last-activity: uint,
    is-active: bool
  }
)

;; Articles submitted by journalists
(define-map articles
  { article-id: uint }
  {
    title: (string-ascii 512),
    content-hash: (buff 32),
    journalist-id: uint,
    category: uint,
    publication-date: uint,
    source-urls: (list 10 (string-ascii 512)),
    accuracy-verified: bool,
    total-reviews: uint,
    average-accuracy: uint,
    average-bias: uint,
    verification-status: uint
  }
)

;; Peer review system
(define-map peer-reviews
  { review-id: uint }
  {
    article-id: uint,
    reviewer-id: uint,
    accuracy-score: uint, ;; 0-100
    bias-score: uint, ;; 0-6 (neutral to strong bias)
    fact-check-score: uint, ;; 0-100
    source-quality-score: uint, ;; 0-100
    overall-score: uint, ;; 0-100
    review-comments: (string-ascii 1024),
    review-date: uint,
    verification-proof: (buff 32),
    status: uint,
    stake-amount: uint
  }
)

;; Journalist ownership mapping
(define-map journalist-owners { owner: principal } { journalist-id: uint })

;; Article-journalist mapping
(define-map journalist-articles { journalist-id: uint } { article-ids: (list 1000 uint) })

;; Reviewer credentials and performance
(define-map reviewer-stats
  { reviewer-id: uint }
  {
    total-reviews: uint,
    accurate-reviews: uint,
    review-accuracy-rate: uint,
    specialization-areas: (list 5 uint),
    stake-balance: uint,
    reputation-score: uint,
    last-review-date: uint
  }
)

;; Accuracy tracking for articles
(define-map article-accuracy-tracking
  { article-id: uint }
  {
    fact-check-results: (list 20 bool),
    correction-count: uint,
    retraction-issued: bool,
    final-accuracy-score: uint,
    verification-date: uint
  }
)

;; Bias detection results
(define-map bias-analysis
  { article-id: uint }
  {
    political-bias: uint,
    source-diversity: uint,
    language-bias-score: uint,
    perspective-balance: uint,
    overall-bias-rating: uint
  }
)

;; public functions
;;

;; Register a new journalist profile
(define-public (register-journalist (name (string-ascii 256)) (email (string-ascii 256)) (bio (string-ascii 1024)) (category uint))
  (let
    (
      (journalist-id (var-get next-journalist-id))
    )
    (asserts! (is-none (map-get? journalist-owners { owner: tx-sender })) err-already-exists)
    (asserts! (> (len name) u0) (err u212)) ;; Name cannot be empty
    (asserts! (> (len email) u0) (err u213)) ;; Email cannot be empty
    (asserts! (and (>= category category-independent) (<= category category-freelance)) (err u214))
    
    (map-set journalists
      { journalist-id: journalist-id }
      {
        name: name,
        email: email,
        bio: bio,
        category: category,
        owner: tx-sender,
        registration-date: stacks-block-height,
        credibility-score: u100, ;; Start with base credibility
        accuracy-score: u100,
        bias-score: bias-neutral,
        total-articles: u0,
        verified-articles: u0,
        peer-reviews-received: u0,
        peer-reviews-given: u0,
        last-activity: stacks-block-height,
        is-active: true
      }
    )
    
    (map-set journalist-owners { owner: tx-sender } { journalist-id: journalist-id })
    (map-set journalist-articles { journalist-id: journalist-id } { article-ids: (list) })
    
    ;; Initialize reviewer stats
    (map-set reviewer-stats
      { reviewer-id: journalist-id }
      {
        total-reviews: u0,
        accurate-reviews: u0,
        review-accuracy-rate: u100,
        specialization-areas: (list),
        stake-balance: u0,
        reputation-score: u100,
        last-review-date: u0
      }
    )
    
    (var-set next-journalist-id (+ journalist-id u1))
    (ok journalist-id)
  )
)

;; Submit an article for review
(define-public (submit-article (title (string-ascii 512)) (content-hash (buff 32)) (category uint) (source-urls (list 10 (string-ascii 512))))
  (let
    (
      (article-id (var-get next-article-id))
      (journalist-info (unwrap! (map-get? journalist-owners { owner: tx-sender }) err-unauthorized))
      (journalist-id (get journalist-id journalist-info))
      (journalist-data (unwrap! (map-get? journalists { journalist-id: journalist-id }) err-not-found))
      (current-articles (default-to (list) (get article-ids (map-get? journalist-articles { journalist-id: journalist-id }))))
    )
    (asserts! (get is-active journalist-data) (err u215))
    (asserts! (> (len title) u0) (err u216))
    (asserts! (is-eq (len content-hash) u32) (err u217))
    (asserts! (and (>= category article-breaking-news) (<= category article-feature)) (err u218))
    (asserts! (< (len current-articles) u1000) (err u219)) ;; Max 1000 articles per journalist
    
    (map-set articles
      { article-id: article-id }
      {
        title: title,
        content-hash: content-hash,
        journalist-id: journalist-id,
        category: category,
        publication-date: stacks-block-height,
        source-urls: source-urls,
        accuracy-verified: false,
        total-reviews: u0,
        average-accuracy: u0,
        average-bias: u0,
        verification-status: review-pending
      }
    )
    
    ;; Update journalist article count
    (map-set journalists
      { journalist-id: journalist-id }
      (merge journalist-data {
        total-articles: (+ (get total-articles journalist-data) u1),
        last-activity: stacks-block-height
      })
    )
    
    ;; Add to journalist's article list
    (map-set journalist-articles
      { journalist-id: journalist-id }
      { article-ids: (unwrap-panic (as-max-len? (append current-articles article-id) u1000)) }
    )
    
    ;; Initialize accuracy tracking
    (map-set article-accuracy-tracking
      { article-id: article-id }
      {
        fact-check-results: (list),
        correction-count: u0,
        retraction-issued: false,
        final-accuracy-score: u0,
        verification-date: u0
      }
    )
    
    (var-set next-article-id (+ article-id u1))
    (ok article-id)
  )
)

;; Submit a peer review for an article
(define-public (submit-peer-review (article-id uint) (accuracy-score uint) (bias-score uint) (fact-check-score uint) (source-quality-score uint) (review-comments (string-ascii 1024)) (verification-proof (buff 32)) (stake-amount uint))
  (let
    (
      (review-id (var-get next-review-id))
      (reviewer-info (unwrap! (map-get? journalist-owners { owner: tx-sender }) err-unauthorized))
      (reviewer-id (get journalist-id reviewer-info))
      (reviewer-data (unwrap! (map-get? journalists { journalist-id: reviewer-id }) err-not-found))
      (article-data (unwrap! (map-get? articles { article-id: article-id }) err-invalid-article))
      (reviewer-stats-data (unwrap! (map-get? reviewer-stats { reviewer-id: reviewer-id }) err-not-found))
      (overall-score (calculate-overall-score accuracy-score bias-score fact-check-score source-quality-score))
    )
    (asserts! (not (is-eq reviewer-id (get journalist-id article-data))) err-self-review)
    (asserts! (>= (get credibility-score reviewer-data) (var-get min-reviewer-score)) err-unauthorized)
    (asserts! (and (<= accuracy-score u100) (<= fact-check-score u100) (<= source-quality-score u100)) err-invalid-score)
    (asserts! (<= bias-score bias-strong-right) err-invalid-bias-score)
    (asserts! (is-eq (len verification-proof) u32) (err u220))
    (asserts! (>= (get stake-balance reviewer-stats-data) stake-amount) (err u221))
    
    ;; Record the peer review
    (map-set peer-reviews
      { review-id: review-id }
      {
        article-id: article-id,
        reviewer-id: reviewer-id,
        accuracy-score: accuracy-score,
        bias-score: bias-score,
        fact-check-score: fact-check-score,
        source-quality-score: source-quality-score,
        overall-score: overall-score,
        review-comments: review-comments,
        review-date: stacks-block-height,
        verification-proof: verification-proof,
        status: review-completed,
        stake-amount: stake-amount
      }
    )
    
    ;; Update article review count and averages
    (let
      (
        (new-review-count (+ (get total-reviews article-data) u1))
        (current-accuracy-total (* (get average-accuracy article-data) (get total-reviews article-data)))
        (current-bias-total (* (get average-bias article-data) (get total-reviews article-data)))
        (new-accuracy-avg (/ (+ current-accuracy-total accuracy-score) new-review-count))
        (new-bias-avg (/ (+ current-bias-total bias-score) new-review-count))
      )
      
      (map-set articles
        { article-id: article-id }
        (merge article-data {
          total-reviews: new-review-count,
          average-accuracy: new-accuracy-avg,
          average-bias: new-bias-avg,
          verification-status: (if (>= new-review-count (var-get min-reviews-for-score)) review-completed review-pending),
          accuracy-verified: (if (and (>= new-review-count (var-get min-reviews-for-score))
                                    (>= new-accuracy-avg u70)) true false)
        })
      )
    )
    
    ;; Update reviewer statistics
    (map-set reviewer-stats
      { reviewer-id: reviewer-id }
      (merge reviewer-stats-data {
        total-reviews: (+ (get total-reviews reviewer-stats-data) u1),
        last-review-date: stacks-block-height,
        stake-balance: (- (get stake-balance reviewer-stats-data) stake-amount)
      })
    )
    
    ;; Update reviewer's given reviews count
    (map-set journalists
      { journalist-id: reviewer-id }
      (merge reviewer-data {
        peer-reviews-given: (+ (get peer-reviews-given reviewer-data) u1),
        last-activity: stacks-block-height
      })
    )
    
    (var-set next-review-id (+ review-id u1))
    (ok review-id)
  )
)

;; Update journalist credibility score
(define-public (update-credibility-score (journalist-id uint))
  (let
    (
      (journalist-data (unwrap! (map-get? journalists { journalist-id: journalist-id }) err-not-found))
      (new-score (calculate-credibility-score journalist-id))
    )
    (asserts! (or (is-eq tx-sender (get owner journalist-data)) (is-eq tx-sender contract-owner)) err-unauthorized)
    
    (map-set journalists
      { journalist-id: journalist-id }
      (merge journalist-data { credibility-score: new-score })
    )
    (ok new-score)
  )
)

;; Report article correction or retraction
(define-public (report-article-correction (article-id uint) (is-retraction bool))
  (let
    (
      (article-data (unwrap! (map-get? articles { article-id: article-id }) err-invalid-article))
      (journalist-data (unwrap! (map-get? journalists { journalist-id: (get journalist-id article-data) }) err-not-found))
      (accuracy-tracking (unwrap! (map-get? article-accuracy-tracking { article-id: article-id }) err-not-found))
    )
    (asserts! (or (is-eq tx-sender (get owner journalist-data)) (is-eq tx-sender contract-owner)) err-unauthorized)
    
    (map-set article-accuracy-tracking
      { article-id: article-id }
      (merge accuracy-tracking {
        correction-count: (+ (get correction-count accuracy-tracking) u1),
        retraction-issued: (or (get retraction-issued accuracy-tracking) is-retraction)
      })
    )
    (ok true)
  )
)

;; Add verifier stake
(define-public (add-reviewer-stake (amount uint))
  (let
    (
      (journalist-info (unwrap! (map-get? journalist-owners { owner: tx-sender }) err-unauthorized))
      (journalist-id (get journalist-id journalist-info))
      (reviewer-data (unwrap! (map-get? reviewer-stats { reviewer-id: journalist-id }) err-not-found))
    )
    (map-set reviewer-stats
      { reviewer-id: journalist-id }
      (merge reviewer-data { stake-balance: (+ (get stake-balance reviewer-data) amount) })
    )
    (ok true)
  )
)

;; Admin function to set review parameters
(define-public (set-review-parameters (min-reviews uint) (min-reviewer-score-new uint) (decay-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (> min-reviews u0) (<= min-reviews u50)) (err u222))
    (asserts! (and (>= min-reviewer-score-new u50) (<= min-reviewer-score-new u100)) (err u223))
    (asserts! (<= decay-rate u10) (err u224))
    
    (var-set min-reviews-for-score min-reviews)
    (var-set min-reviewer-score min-reviewer-score-new)
    (var-set credibility-decay-rate decay-rate)
    (ok true)
  )
)

;; read only functions
;;

;; Get journalist information
(define-read-only (get-journalist (journalist-id uint))
  (map-get? journalists { journalist-id: journalist-id })
)

;; Get article information
(define-read-only (get-article (article-id uint))
  (map-get? articles { article-id: article-id })
)

;; Get peer review information
(define-read-only (get-peer-review (review-id uint))
  (map-get? peer-reviews { review-id: review-id })
)

;; Get journalist by owner
(define-read-only (get-journalist-by-owner (owner principal))
  (map-get? journalist-owners { owner: owner })
)

;; Get journalist articles
(define-read-only (get-journalist-articles (journalist-id uint))
  (map-get? journalist-articles { journalist-id: journalist-id })
)

;; Get reviewer statistics
(define-read-only (get-reviewer-stats (reviewer-id uint))
  (map-get? reviewer-stats { reviewer-id: reviewer-id })
)

;; Get article accuracy tracking
(define-read-only (get-article-accuracy (article-id uint))
  (map-get? article-accuracy-tracking { article-id: article-id })
)

;; Get bias analysis
(define-read-only (get-bias-analysis (article-id uint))
  (map-get? bias-analysis { article-id: article-id })
)

;; Check if journalist can review
(define-read-only (can-review (journalist-id uint))
  (match (map-get? journalists { journalist-id: journalist-id })
    journalist (>= (get credibility-score journalist) (var-get min-reviewer-score))
    false
  )
)

;; Get review parameters
(define-read-only (get-review-parameters)
  {
    min-reviews-for-score: (var-get min-reviews-for-score),
    min-reviewer-score: (var-get min-reviewer-score),
    credibility-decay-rate: (var-get credibility-decay-rate),
    review-expiry-blocks: (var-get review-expiry-blocks)
  }
)

;; private functions
;;

;; Calculate overall review score
(define-private (calculate-overall-score (accuracy uint) (bias uint) (fact-check uint) (source-quality uint))
  (let
    (
      (bias-penalty (if (> bias bias-neutral) (* bias u5) u0))
      (weighted-score (+ (* accuracy u30) (* fact-check u35) (* source-quality u25) (* (- u100 bias-penalty) u10)))
    )
    (/ weighted-score u100)
  )
)

;; Calculate credibility score based on article performance
(define-private (calculate-credibility-score (journalist-id uint))
  (match (map-get? journalists { journalist-id: journalist-id })
    journalist
    (let
      (
        (base-score u100)
        (accuracy (get accuracy-score journalist))
        (article-count (get total-articles journalist))
        (verified-count (get verified-articles journalist))
        (accuracy-rate (if (> article-count u0) (/ (* verified-count u100) article-count) u100))
        (peer-review-count (get peer-reviews-received journalist))
        (review-bonus (if (>= peer-review-count u10) u10 (/ peer-review-count u1)))
      )
      (if (> (+ base-score (/ accuracy-rate u2) review-bonus) u200)
        u200
        (+ base-score (/ accuracy-rate u2) review-bonus))
    )
    u100
  )
)

;; Apply credibility decay over time
(define-private (apply-credibility-decay (current-score uint) (blocks-since-activity uint))
  (let
    (
      (decay-factor (/ blocks-since-activity u1008)) ;; Weekly decay periods
      (decay-amount (* decay-factor (var-get credibility-decay-rate)))
    )
    (if (> current-score decay-amount)
      (- current-score decay-amount)
      u50 ;; Minimum credibility floor
    )
  )
)
