(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_PAYMENT_NOT_FOUND (err u104))
(define-constant ERR_ALREADY_PAID (err u105))
(define-constant ERR_PROJECT_EXISTS (err u106))
(define-constant ERR_MILESTONE_NOT_FOUND (err u107))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u108))
(define-constant ERR_DEADLINE_PASSED (err u109))
(define-constant ERR_INVALID_MILESTONE_ORDER (err u110))
(define-constant ERR_MILESTONE_DEPENDENCIES_NOT_MET (err u111))
(define-constant ERR_INVALID_DEADLINE (err u112))

(define-data-var next-project-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var next-milestone-id uint u1)

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

(define-map milestones
  { milestone-id: uint }
  {
    project-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    deliverables: (list 5 (string-ascii 200)),
    deadline: uint,
    priority-level: uint,
    estimated-hours: uint,
    actual-hours: (optional uint),
    completion-percentage: uint,
    status: (string-ascii 20),
    assigned-to: principal,
    dependencies: (list 3 uint),
    created-at: uint,
    completed-at: (optional uint),
    approved-by: (optional principal),
    review-notes: (optional (string-ascii 500))
  }
)

(define-map project-milestones
  { project-id: uint, milestone-id: uint }
  { 
    active: bool,
    sequence-order: uint
  }
)

(define-map milestone-dependencies
  { milestone-id: uint, dependency-id: uint }
  { required: bool }
)

(define-map contractor-milestones
  { contractor: principal, milestone-id: uint }
  { assigned: bool }
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

(define-public (create-milestone 
  (project-id uint) 
  (title (string-ascii 100)) 
  (description (string-ascii 500))
  (deliverables (list 5 (string-ascii 200)))
  (deadline uint)
  (priority-level uint)
  (estimated-hours uint)
  (assigned-to principal)
  (dependencies (list 3 uint))
  (sequence-order uint)
)
  (let
    (
      (milestone-id (var-get next-milestone-id))
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get contractor project))) ERR_UNAUTHORIZED)
    (asserts! (> deadline current-time) ERR_INVALID_DEADLINE)
    (asserts! (> estimated-hours u0) ERR_INVALID_STATUS)
    (asserts! (<= priority-level u5) ERR_INVALID_STATUS)
    (map-set milestones
      { milestone-id: milestone-id }
      {
        project-id: project-id,
        title: title,
        description: description,
        deliverables: deliverables,
        deadline: deadline,
        priority-level: priority-level,
        estimated-hours: estimated-hours,
        actual-hours: none,
        completion-percentage: u0,
        status: "not-started",
        assigned-to: assigned-to,
        dependencies: dependencies,
        created-at: current-time,
        completed-at: none,
        approved-by: none,
        review-notes: none
      }
    )
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      { active: true, sequence-order: sequence-order }
    )
    (map-set contractor-milestones
      { contractor: assigned-to, milestone-id: milestone-id }
      { assigned: true }
    )
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (update-milestone-progress 
  (milestone-id uint) 
  (completion-percentage uint) 
  (actual-hours uint)
  (status (string-ascii 20))
)
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get assigned-to milestone))) ERR_UNAUTHORIZED)
    (asserts! (<= completion-percentage u100) ERR_INVALID_STATUS)
    (asserts! (not (is-eq (get status milestone) "completed")) ERR_MILESTONE_ALREADY_COMPLETED)
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone { 
        completion-percentage: completion-percentage,
        actual-hours: (some actual-hours),
        status: status
      })
    )
    (ok true)
  )
)

(define-public (complete-milestone 
  (milestone-id uint) 
  (review-notes (string-ascii 500))
)
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (dependencies (get dependencies milestone))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get assigned-to milestone))) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status milestone) "completed")) ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (unwrap! (check-milestone-dependencies milestone-id dependencies) ERR_MILESTONE_DEPENDENCIES_NOT_MET) ERR_MILESTONE_DEPENDENCIES_NOT_MET)
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone { 
        completion-percentage: u100,
        status: "completed",
        completed-at: (some current-time),
        review-notes: (some review-notes)
      })
    )
    (ok true)
  )
)

(define-public (approve-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status milestone) "completed") ERR_INVALID_STATUS)
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone { 
        status: "approved",
        approved-by: (some tx-sender)
      })
    )
    (ok true)
  )
)

(define-public (assign-milestone-dependency 
  (milestone-id uint) 
  (dependency-id uint)
)
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
      (dependency (unwrap! (map-get? milestones { milestone-id: dependency-id }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get project-id milestone) (get project-id dependency)) ERR_INVALID_STATUS)
    (map-set milestone-dependencies
      { milestone-id: milestone-id, dependency-id: dependency-id }
      { required: true }
    )
    (ok true)
  )
)

(define-private (check-milestone-dependencies 
  (milestone-id uint) 
  (dependencies (list 3 uint))
)
  (ok (fold check-single-dependency dependencies true))
)

(define-private (check-single-dependency (dependency-id uint) (acc bool))
  (if (is-eq dependency-id u0)
    acc
    (and acc 
      (match (map-get? milestones { milestone-id: dependency-id })
        dependency-milestone (is-eq (get status dependency-milestone) "completed")
        false
      )
    )
  )
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-project-milestones-summary (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (ok {
      project-id: project-id,
      total-milestones: (get-milestone-count-for-project project-id),
      completed-milestones: (get-completed-milestone-count project-id),
      in-progress-milestones: (get-in-progress-milestone-count project-id),
      overdue-milestones: (get-overdue-milestone-count project-id)
    })
    ERR_PROJECT_NOT_FOUND
  )
)

(define-read-only (get-milestone-performance-metrics (milestone-id uint))
  (match (map-get? milestones { milestone-id: milestone-id })
    milestone (ok {
      milestone-id: milestone-id,
      estimated-hours: (get estimated-hours milestone),
      actual-hours: (get actual-hours milestone),
      efficiency-ratio: (calculate-efficiency-ratio 
        (get estimated-hours milestone) 
        (default-to u0 (get actual-hours milestone))
      ),
      completion-percentage: (get completion-percentage milestone),
      days-to-deadline: (calculate-days-to-deadline 
        (get deadline milestone)
      ),
      status: (get status milestone)
    })
    ERR_MILESTONE_NOT_FOUND
  )
)

(define-read-only (get-contractor-milestone-workload (contractor principal))
  (ok {
    contractor: contractor,
    active-milestones: (get-contractor-active-milestone-count contractor),
    completed-milestones: (get-contractor-completed-milestone-count contractor),
    total-estimated-hours: (get-contractor-total-hours contractor),
    average-completion-time: (get-contractor-average-completion-time contractor)
  })
)

(define-read-only (is-milestone-overdue (milestone-id uint))
  (match (map-get? milestones { milestone-id: milestone-id })
    milestone (let
      (
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (deadline (get deadline milestone))
        (status (get status milestone))
      )
      (and 
        (> current-time deadline)
        (not (is-eq status "completed"))
        (not (is-eq status "approved"))
      )
    )
    false
  )
)

(define-private (get-milestone-count-for-project (project-id uint))
  u0
)

(define-private (get-completed-milestone-count (project-id uint))
  u0
)

(define-private (get-in-progress-milestone-count (project-id uint))
  u0
)

(define-private (get-overdue-milestone-count (project-id uint))
  u0
)

(define-private (calculate-efficiency-ratio (estimated uint) (actual uint))
  (if (is-eq actual u0)
    u100
    (/ (* estimated u100) actual)
  )
)

(define-private (calculate-days-to-deadline (deadline uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (if (> deadline current-time)
      (/ (- deadline current-time) u86400)
      u0
    )
  )
)

(define-private (get-contractor-active-milestone-count (contractor principal))
  u0
)

(define-private (get-contractor-completed-milestone-count (contractor principal))
  u0
)

(define-private (get-contractor-total-hours (contractor principal))
  u0
)

(define-private (get-contractor-average-completion-time (contractor principal))
  u0
)

(define-read-only (get-next-milestone-id)
  (var-get next-milestone-id)
)