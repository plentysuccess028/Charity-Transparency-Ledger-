(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-charity-not-verified (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-unauthorized (err u106))
(define-constant err-milestone-not-found (err u107))
(define-constant err-milestone-already-completed (err u108))
(define-constant err-invalid-milestone-target (err u109))

(define-data-var contract-balance uint u0)
(define-data-var next-charity-id uint u1)
(define-data-var next-donation-id uint u1)
(define-data-var next-milestone-id uint u1)

(define-map charities 
  { charity-id: uint }
  {
    name: (string-ascii 128),
    description: (string-ascii 256),
    wallet: principal,
    verified: bool,
    total-received: uint,
    registration-block: uint
  }
)

(define-map charity-by-wallet
  { wallet: principal }
  { charity-id: uint }
)

(define-map donations
  { donation-id: uint }
  {
    donor: principal,
    charity-id: uint,
    amount: uint,
    timestamp: uint,
    message: (optional (string-ascii 256)),
    withdrawn: bool
  }
)

(define-map donor-donations
  { donor: principal, index: uint }
  { donation-id: uint }
)

(define-map donor-donation-count
  { donor: principal }
  { count: uint }
)

(define-map charity-donations
  { charity-id: uint, index: uint }
  { donation-id: uint }
)

(define-map charity-donation-count
  { charity-id: uint }
  { count: uint }
)

(define-map milestones
  { milestone-id: uint }
  {
    charity-id: uint,
    title: (string-ascii 128),
    description: (string-ascii 256),
    target-amount: uint,
    current-amount: uint,
    completed: bool,
    completion-block: (optional uint),
    created-block: uint
  }
)

(define-map charity-milestones
  { charity-id: uint, index: uint }
  { milestone-id: uint }
)

(define-map charity-milestone-count
  { charity-id: uint }
  { count: uint }
)

(define-public (register-charity (name (string-ascii 128)) (description (string-ascii 256)) (wallet principal))
  (let
    (
      (charity-id (var-get next-charity-id))
      (existing-charity (map-get? charity-by-wallet { wallet: wallet }))
    )
    (asserts! (is-none existing-charity) err-already-exists)
    (map-set charities
      { charity-id: charity-id }
      {
        name: name,
        description: description,
        wallet: wallet,
        verified: false,
        total-received: u0,
        registration-block: stacks-block-height
      }
    )
    (map-set charity-by-wallet { wallet: wallet } { charity-id: charity-id })
    (var-set next-charity-id (+ charity-id u1))
    (ok charity-id)
  )
)

(define-public (verify-charity (charity-id uint))
  (let
    (
      (charity-data (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set charities
      { charity-id: charity-id }
      (merge charity-data { verified: true })
    )
    (ok true)
  )
)

(define-public (donate (charity-id uint) (message (optional (string-ascii 256))))
  (let
    (
      (donation-id (var-get next-donation-id))
      (charity-data (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (amount (stx-get-balance tx-sender))
      (donor-count (default-to u0 (get count (map-get? donor-donation-count { donor: tx-sender }))))
      (charity-count (default-to u0 (get count (map-get? charity-donation-count { charity-id: charity-id }))))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get verified charity-data) err-charity-not-verified)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set donations
      { donation-id: donation-id }
      {
        donor: tx-sender,
        charity-id: charity-id,
        amount: amount,
        timestamp: stacks-block-height,
        message: message,
        withdrawn: false
      }
    )
    
    (map-set donor-donations
      { donor: tx-sender, index: donor-count }
      { donation-id: donation-id }
    )
    
    (map-set donor-donation-count
      { donor: tx-sender }
      { count: (+ donor-count u1) }
    )
    
    (map-set charity-donations
      { charity-id: charity-id, index: charity-count }
      { donation-id: donation-id }
    )
    
    (map-set charity-donation-count
      { charity-id: charity-id }
      { count: (+ charity-count u1) }
    )
    
    (map-set charities
      { charity-id: charity-id }
      (merge charity-data { total-received: (+ (get total-received charity-data) amount) })
    )
    
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (var-set next-donation-id (+ donation-id u1))
    (ok donation-id)
  )
)

(define-public (withdraw-donation (donation-id uint))
  (let
    (
      (donation-data (unwrap! (map-get? donations { donation-id: donation-id }) err-not-found))
      (charity-data (unwrap! (map-get? charities { charity-id: (get charity-id donation-data) }) err-not-found))
      (amount (get amount donation-data))
    )
    (asserts! (is-eq tx-sender (get wallet charity-data)) err-unauthorized)
    (asserts! (not (get withdrawn donation-data)) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get wallet charity-data))))
    
    (map-set donations
      { donation-id: donation-id }
      (merge donation-data { withdrawn: true })
    )
    
    (var-set contract-balance (- (var-get contract-balance) amount))
    (ok amount)
  )
)

(define-public (create-milestone (charity-id uint) (title (string-ascii 128)) (description (string-ascii 256)) (target-amount uint))
  (let
    (
      (milestone-id (var-get next-milestone-id))
      (charity-data (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (milestone-count (default-to u0 (get count (map-get? charity-milestone-count { charity-id: charity-id }))))
    )
    (asserts! (is-eq tx-sender (get wallet charity-data)) err-unauthorized)
    (asserts! (> target-amount u0) err-invalid-milestone-target)
    
    (map-set milestones
      { milestone-id: milestone-id }
      {
        charity-id: charity-id,
        title: title,
        description: description,
        target-amount: target-amount,
        current-amount: u0,
        completed: false,
        completion-block: none,
        created-block: stacks-block-height
      }
    )
    
    (map-set charity-milestones
      { charity-id: charity-id, index: milestone-count }
      { milestone-id: milestone-id }
    )
    
    (map-set charity-milestone-count
      { charity-id: charity-id }
      { count: (+ milestone-count u1) }
    )
    
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (fund-milestone (milestone-id uint))
  (let
    (
      (milestone-data (unwrap! (map-get? milestones { milestone-id: milestone-id }) err-milestone-not-found))
      (charity-data (unwrap! (map-get? charities { charity-id: (get charity-id milestone-data) }) err-not-found))
      (amount (stx-get-balance tx-sender))
      (new-current-amount (+ (get current-amount milestone-data) amount))
      (target-reached (>= new-current-amount (get target-amount milestone-data)))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get verified charity-data) err-charity-not-verified)
    (asserts! (not (get completed milestone-data)) err-milestone-already-completed)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone-data {
        current-amount: new-current-amount,
        completed: target-reached,
        completion-block: (if target-reached (some stacks-block-height) none)
      })
    )
    
    (map-set charities
      { charity-id: (get charity-id milestone-data) }
      (merge charity-data { total-received: (+ (get total-received charity-data) amount) })
    )
    
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok target-reached)
  )
)

(define-public (withdraw-milestone-funds (milestone-id uint))
  (let
    (
      (milestone-data (unwrap! (map-get? milestones { milestone-id: milestone-id }) err-milestone-not-found))
      (charity-data (unwrap! (map-get? charities { charity-id: (get charity-id milestone-data) }) err-not-found))
      (amount (get current-amount milestone-data))
    )
    (asserts! (is-eq tx-sender (get wallet charity-data)) err-unauthorized)
    (asserts! (get completed milestone-data) err-milestone-already-completed)
    (asserts! (> amount u0) err-invalid-amount)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get wallet charity-data))))
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone-data { current-amount: u0 })
    )
    
    (var-set contract-balance (- (var-get contract-balance) amount))
    (ok amount)
  )
)

(define-read-only (get-charity (charity-id uint))
  (map-get? charities { charity-id: charity-id })
)

(define-read-only (get-charity-by-wallet (wallet principal))
  (match (map-get? charity-by-wallet { wallet: wallet })
    charity-ref (map-get? charities { charity-id: (get charity-id charity-ref) })
    none
  )
)

(define-read-only (get-donation (donation-id uint))
  (map-get? donations { donation-id: donation-id })
)

(define-read-only (get-donor-donations (donor principal) (limit uint))
  (let
    (
      (total-donations (default-to u0 (get count (map-get? donor-donation-count { donor: donor }))))
    )
    (if (> total-donations u0)
      (let ((last-index (- total-donations u1)))
        (list 
          (map-get? donations { donation-id: (default-to u0 (get donation-id (map-get? donor-donations { donor: donor, index: last-index }))) })
        )
      )
      (list)
    )
  )
)

(define-read-only (get-charity-donations (charity-id uint) (limit uint))
  (let
    (
      (total-donations (default-to u0 (get count (map-get? charity-donation-count { charity-id: charity-id }))))
    )
    (if (> total-donations u0)
      (let ((last-index (- total-donations u1)))
        (list 
          (map-get? donations { donation-id: (default-to u0 (get donation-id (map-get? charity-donations { charity-id: charity-id, index: last-index }))) })
        )
      )
      (list)
    )
  )
)

(define-read-only (trace-donation (donation-id uint))
  (let
    (
      (donation-data (unwrap! (map-get? donations { donation-id: donation-id }) (err "donation not found")))
      (charity-data (unwrap! (map-get? charities { charity-id: (get charity-id donation-data) }) (err "charity not found")))
    )
    (ok {
      donation: donation-data,
      charity: charity-data,
      status: (if (get withdrawn donation-data) "withdrawn" "pending")
    })
  )
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-charity-milestones (charity-id uint) (limit uint))
  (let
    (
      (total-milestones (default-to u0 (get count (map-get? charity-milestone-count { charity-id: charity-id }))))
    )
    (if (> total-milestones u0)
      (let ((last-index (- total-milestones u1)))
        (list 
          (map-get? milestones { milestone-id: (default-to u0 (get milestone-id (map-get? charity-milestones { charity-id: charity-id, index: last-index }))) })
        )
      )
      (list)
    )
  )
)

(define-read-only (get-milestone-progress (milestone-id uint))
  (let
    (
      (milestone-data (unwrap! (map-get? milestones { milestone-id: milestone-id }) (err "milestone not found")))
      (target (get target-amount milestone-data))
      (current (get current-amount milestone-data))
      (percentage (if (> target u0) (/ (* current u100) target) u0))
    )
    (ok {
      milestone-id: milestone-id,
      target-amount: target,
      current-amount: current,
      progress-percentage: percentage,
      completed: (get completed milestone-data),
      funds-available: (and (get completed milestone-data) (> current u0))
    })
  )
)

(define-read-only (get-contract-stats)
  {
    total-charities: (- (var-get next-charity-id) u1),
    total-donations: (- (var-get next-donation-id) u1),
    total-milestones: (- (var-get next-milestone-id) u1),
    contract-balance: (var-get contract-balance)
  }
)