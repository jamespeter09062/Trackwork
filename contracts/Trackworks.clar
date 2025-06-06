(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_PAYMENT_NOT_FOUND (err u104))
(define-constant ERR_ALREADY_PAID (err u105))
(define-constant ERR_PROJECT_EXISTS (err u106))

(define-data-var next-project-id uint u1)
(define-data-var next-payment-id uint u1)

(define-map projects
  { project-id: uint }
  {
    name: (string-ascii 100),
    contractor: principal,
    total-budget: uint,
    paid-amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    updated-at: uint
  }
)

(define-map payments
  { payment-id: uint }
  {
    project-id: uint,
    amount: uint,
    description: (string-ascii 200),
    status: (string-ascii 20),
    created-at: uint,
    paid-at: (optional uint)
  }
)

(define-map project-payments
  { project-id: uint, payment-id: uint }
  { active: bool }
)

(define-public (create-project (name (string-ascii 100)) (contractor principal) (total-budget uint))
  (let
    (
      (project-id (var-get next-project-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> total-budget u0) ERR_INVALID_STATUS)
    (map-set projects
      { project-id: project-id }
      {
        name: name,
        contractor: contractor,
        total-budget: total-budget,
        paid-amount: u0,
        status: "active",
        created-at: current-time,
        updated-at: current-time
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (update-project-status (project-id uint) (new-status (string-ascii 20)))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set projects
      { project-id: project-id }
      (merge project { status: new-status, updated-at: current-time })
    )
    (ok true)
  )
)

(define-public (create-payment (project-id uint) (amount uint) (description (string-ascii 200)))
  (let
    (
      (payment-id (var-get next-payment-id))
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_STATUS)
    (map-set payments
      { payment-id: payment-id }
      {
        project-id: project-id,
        amount: amount,
        description: description,
        status: "pending",
        created-at: current-time,
        paid-at: none
      }
    )
    (map-set project-payments
      { project-id: project-id, payment-id: payment-id }
      { active: true }
    )
    (var-set next-payment-id (+ payment-id u1))
    (ok payment-id)
  )
)

(define-public (approve-payment (payment-id uint))
  (let
    (
      (payment (unwrap! (map-get? payments { payment-id: payment-id }) ERR_PAYMENT_NOT_FOUND))
      (project-id (get project-id payment))
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (payment-amount (get amount payment))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (new-paid-amount (+ (get paid-amount project) payment-amount))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment) "pending") ERR_ALREADY_PAID)
    (asserts! (<= new-paid-amount (get total-budget project)) ERR_INSUFFICIENT_FUNDS)
    (map-set payments
      { payment-id: payment-id }
      (merge payment { 
        status: "approved", 
        paid-at: (some current-time) 
      })
    )
    (map-set projects
      { project-id: project-id }
      (merge project { 
        paid-amount: new-paid-amount,
        updated-at: current-time
      })
    )
    (ok true)
  )
)

(define-public (reject-payment (payment-id uint))
  (let
    (
      (payment (unwrap! (map-get? payments { payment-id: payment-id }) ERR_PAYMENT_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment) "pending") ERR_ALREADY_PAID)
    (map-set payments
      { payment-id: payment-id }
      (merge payment { 
        status: "rejected",
        paid-at: (some current-time)
      })
    )
    (ok true)
  )
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-payment (payment-id uint))
  (map-get? payments { payment-id: payment-id })
)

(define-read-only (get-project-budget-status (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (ok {
      total-budget: (get total-budget project),
      paid-amount: (get paid-amount project),
      remaining-budget: (- (get total-budget project) (get paid-amount project)),
      completion-percentage: (/ (* (get paid-amount project) u100) (get total-budget project))
    })
    ERR_PROJECT_NOT_FOUND
  )
)

(define-read-only (get-contractor-projects (contractor principal))
  (ok contractor)
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)

(define-read-only (get-next-payment-id)
  (var-get next-payment-id)
)

(define-read-only (is-project-payment (project-id uint) (payment-id uint))
  (default-to false (get active (map-get? project-payments { project-id: project-id, payment-id: payment-id })))
)

(define-read-only (get-contract-owner)
  CONTRACT_OWNER
)