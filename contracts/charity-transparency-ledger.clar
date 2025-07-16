(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-charity-not-verified (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-unauthorized (err u106))

(define-data-var contract-balance uint u0)
(define-data-var next-charity-id uint u1)
(define-data-var next-donation-id uint u1)

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
        registration-block: block-height
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
        timestamp: block-height,
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
      (start-index (if (> total-donations limit) (- total-donations limit) u0))
    )
    (map get-donation-by-index (generate-sequence start-index total-donations donor))
  )
)

(define-read-only (get-charity-donations (charity-id uint) (limit uint))
  (let
    (
      (total-donations (default-to u0 (get count (map-get? charity-donation-count { charity-id: charity-id }))))
      (start-index (if (> total-donations limit) (- total-donations limit) u0))
    )
    (map get-charity-donation-by-index (generate-charity-sequence start-index total-donations charity-id))
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

(define-read-only (get-contract-stats)
  {
    total-charities: (- (var-get next-charity-id) u1),
    total-donations: (- (var-get next-donation-id) u1),
    contract-balance: (var-get contract-balance)
  }
)

(define-private (get-donation-by-index (data { index: uint, donor: principal }))
  (match (map-get? donor-donations { donor: (get donor data), index: (get index data) })
    donation-ref (map-get? donations { donation-id: (get donation-id donation-ref) })
    none
  )
)

(define-private (get-charity-donation-by-index (data { index: uint, charity-id: uint }))
  (match (map-get? charity-donations { charity-id: (get charity-id data), index: (get index data) })
    donation-ref (map-get? donations { donation-id: (get donation-id donation-ref) })
    none
  )
)

(define-private (generate-sequence (start uint) (end uint) (donor principal))
  (map create-donor-index-data (generate-range start end))
)

(define-private (generate-charity-sequence (start uint) (end uint) (charity-id uint))
  (map create-charity-index-data (generate-range start end))
)

(define-private (create-donor-index-data (index uint))
  { index: index, donor: tx-sender }
)

(define-private (create-charity-index-data (index uint))
  { index: index, charity-id: u0 }
)

(define-private (generate-range (start uint) (end uint))
  (if (>= start end)
    (list)
    (unwrap-panic (as-max-len? (append (generate-range start (- end u1)) end) u100))
  )
)
