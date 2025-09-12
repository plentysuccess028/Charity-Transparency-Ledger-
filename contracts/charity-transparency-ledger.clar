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
(define-constant err-matching-program-not-found (err u110))
(define-constant err-matching-funds-exhausted (err u111))
(define-constant err-invalid-match-ratio (err u112))

(define-data-var contract-balance uint u0)
(define-data-var next-charity-id uint u1)
(define-data-var next-donation-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var next-matching-program-id uint u1)

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

(define-map matching-programs
  { program-id: uint }
  {
    sponsor: principal,
    charity-id: (optional uint),
    match-ratio: uint,
    total-funds: uint,
    remaining-funds: uint,
    min-donation: uint,
    max-donation: uint,
    active: bool,
    created-block: uint,
    expiry-block: (optional uint)
  }
)

(define-map program-matches
  { program-id: uint, donation-id: uint }
  { matched-amount: uint, timestamp: uint }
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

(define-public (create-matching-program (charity-id (optional uint)) (match-ratio uint) (min-donation uint) (max-donation uint) (expiry-block (optional uint)))
  (let
    (
      (program-id (var-get next-matching-program-id))
      (amount (stx-get-balance tx-sender))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (and (> match-ratio u0) (<= match-ratio u500)) err-invalid-match-ratio)
    (asserts! (<= min-donation max-donation) err-invalid-amount)
    
    (match charity-id
      charity-id-val (asserts! (is-some (map-get? charities { charity-id: charity-id-val })) err-not-found)
      true
    )
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set matching-programs
      { program-id: program-id }
      {
        sponsor: tx-sender,
        charity-id: charity-id,
        match-ratio: match-ratio,
        total-funds: amount,
        remaining-funds: amount,
        min-donation: min-donation,
        max-donation: max-donation,
        active: true,
        created-block: stacks-block-height,
        expiry-block: expiry-block
      }
    )
    
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (var-set next-matching-program-id (+ program-id u1))
    (ok program-id)
  )
)

(define-public (donate-with-matching (charity-id uint) (message (optional (string-ascii 256))))
  (let
    (
      (donation-id (var-get next-donation-id))
      (charity-data (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (amount (stx-get-balance tx-sender))
      (donor-count (default-to u0 (get count (map-get? donor-donation-count { donor: tx-sender }))))
      (charity-count (default-to u0 (get count (map-get? charity-donation-count { charity-id: charity-id }))))
      (match-result (try-match-donation charity-id amount))
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
      (merge charity-data { total-received: (+ (get total-received charity-data) amount (get matched-amount match-result)) })
    )
    
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (var-set next-donation-id (+ donation-id u1))
    (ok { donation-id: donation-id, matched-amount: (get matched-amount match-result), program-id: (get program-id match-result) })
  )
)

(define-public (deactivate-matching-program (program-id uint))
  (let
    (
      (program-data (unwrap! (map-get? matching-programs { program-id: program-id }) err-matching-program-not-found))
      (remaining-funds (get remaining-funds program-data))
    )
    (asserts! (is-eq tx-sender (get sponsor program-data)) err-unauthorized)
    (asserts! (get active program-data) err-matching-program-not-found)
    
    (if (> remaining-funds u0)
      (try! (as-contract (stx-transfer? remaining-funds tx-sender (get sponsor program-data))))
      true
    )
    
    (map-set matching-programs
      { program-id: program-id }
      (merge program-data { active: false, remaining-funds: u0 })
    )
    
    (var-set contract-balance (- (var-get contract-balance) remaining-funds))
    (ok remaining-funds)
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

(define-read-only (get-matching-program (program-id uint))
  (map-get? matching-programs { program-id: program-id })
)

(define-read-only (get-available-matches-for-charity (charity-id uint) (donation-amount uint))
  (let
    (
      (matching-programs-list (list 
        { program-id: u1, match-amount: u0 }
        { program-id: u2, match-amount: u0 }
        { program-id: u3, match-amount: u0 }
      ))
    )
    (filter is-valid-match matching-programs-list)
  )
)

(define-read-only (get-program-match-history (program-id uint))
  (match (map-get? matching-programs { program-id: program-id })
    program (some { 
      program: program,
      total-matched: (- (get total-funds program) (get remaining-funds program)),
      match-count: u0
    })
    none
  )
)

(define-read-only (get-contract-stats)
  {
    total-charities: (- (var-get next-charity-id) u1),
    total-donations: (- (var-get next-donation-id) u1),
    total-milestones: (- (var-get next-milestone-id) u1),
    total-matching-programs: (- (var-get next-matching-program-id) u1),
    contract-balance: (var-get contract-balance)
  }
)

(define-private (try-match-donation (charity-id uint) (donation-amount uint))
  (let
    (
      (program-1 (map-get? matching-programs { program-id: u1 }))
      (program-2 (map-get? matching-programs { program-id: u2 }))
      (program-3 (map-get? matching-programs { program-id: u3 }))
    )
    (match program-1
      program-data
        (if (and 
              (get active program-data)
              (>= donation-amount (get min-donation program-data))
              (<= donation-amount (get max-donation program-data))
              (or (is-none (get charity-id program-data)) (is-eq (get charity-id program-data) (some charity-id)))
              (> (get remaining-funds program-data) u0))
          (apply-match u1 program-data donation-amount)
          { matched-amount: u0, program-id: u0 })
      { matched-amount: u0, program-id: u0 }
    )
  )
)

(define-private (apply-match (program-id uint) (program-data { sponsor: principal, charity-id: (optional uint), match-ratio: uint, total-funds: uint, remaining-funds: uint, min-donation: uint, max-donation: uint, active: bool, created-block: uint, expiry-block: (optional uint) }) (donation-amount uint))
  (let
    (
      (potential-match (/ (* donation-amount (get match-ratio program-data)) u100))
      (actual-match (if (<= potential-match (get remaining-funds program-data)) potential-match (get remaining-funds program-data)))
    )
    (if (> actual-match u0)
      (begin
        (map-set matching-programs
          { program-id: program-id }
          (merge program-data { remaining-funds: (- (get remaining-funds program-data) actual-match) })
        )
        { matched-amount: actual-match, program-id: program-id }
      )
      { matched-amount: u0, program-id: u0 }
    )
  )
)

(define-private (is-valid-match (match-data { program-id: uint, match-amount: uint }))
  (> (get program-id match-data) u0)
)