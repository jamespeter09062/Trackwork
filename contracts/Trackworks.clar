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
(define-constant ERR_WORK_SESSION_NOT_FOUND (err u113))
(define-constant ERR_SESSION_ALREADY_ENDED (err u114))
(define-constant ERR_INVALID_HOURLY_RATE (err u115))
(define-constant ERR_SESSION_NOT_STARTED (err u116))
(define-constant ERR_INVOICE_NOT_FOUND (err u117))
(define-constant ERR_INVALID_TIME_ENTRY (err u118))

(define-data-var next-project-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var next-work-session-id uint u1)
(define-data-var next-invoice-id uint u1)

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

;; Time tracking and invoicing data structures
(define-map work-sessions
  { session-id: uint }
  {
    project-id: uint,
    contractor: principal,
    task-description: (string-ascii 300),
    hourly-rate: uint,
    start-time: uint,
    end-time: (optional uint),
    total-hours: (optional uint),
    billable-amount: (optional uint),
    status: (string-ascii 20),
    approved-by: (optional principal),
    created-at: uint
  }
)

(define-map project-hourly-rates
  { project-id: uint, contractor: principal }
  { 
    rate-per-hour: uint,
    currency: (string-ascii 10),
    effective-from: uint
  }
)

(define-map time-invoices
  { invoice-id: uint }
  {
    project-id: uint,
    contractor: principal,
    billing-period-start: uint,
    billing-period-end: uint,
    total-hours: uint,
    total-amount: uint,
    session-ids: (list 10 uint),
    status: (string-ascii 20),
    generated-at: uint,
    approved-at: (optional uint),
    paid-at: (optional uint)
  }
)

(define-map contractor-sessions
  { contractor: principal, session-id: uint }
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

;; Time tracking and invoicing functions
(define-public (set-hourly-rate 
  (project-id uint) 
  (contractor principal) 
  (rate-per-hour uint)
  (currency (string-ascii 10))
)
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> rate-per-hour u0) ERR_INVALID_HOURLY_RATE)
    (map-set project-hourly-rates
      { project-id: project-id, contractor: contractor }
      {
        rate-per-hour: rate-per-hour,
        currency: currency,
        effective-from: current-time
      }
    )
    (ok true)
  )
)

(define-public (start-work-session 
  (project-id uint) 
  (task-description (string-ascii 300))
)
  (let
    (
      (session-id (var-get next-work-session-id))
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
      (rate-info (unwrap! (map-get? project-hourly-rates { project-id: project-id, contractor: tx-sender }) ERR_INVALID_HOURLY_RATE))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Verify contractor is authorized for this project
    (asserts! (is-eq tx-sender (get contractor project)) ERR_UNAUTHORIZED)
    (map-set work-sessions
      { session-id: session-id }
      {
        project-id: project-id,
        contractor: tx-sender,
        task-description: task-description,
        hourly-rate: (get rate-per-hour rate-info),
        start-time: current-time,
        end-time: none,
        total-hours: none,
        billable-amount: none,
        status: "active",
        approved-by: none,
        created-at: current-time
      }
    )
    (map-set contractor-sessions
      { contractor: tx-sender, session-id: session-id }
      { active: true }
    )
    (var-set next-work-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (end-work-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? work-sessions { session-id: session-id }) ERR_WORK_SESSION_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (start-time (get start-time session))
      (hourly-rate (get hourly-rate session))
      (duration-seconds (- current-time start-time))
      (duration-hours (/ duration-seconds u3600)) ;; Convert seconds to hours
      (billable-amount (* duration-hours hourly-rate))
    )
    (asserts! (is-eq tx-sender (get contractor session)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status session) "active") ERR_SESSION_ALREADY_ENDED)
    (asserts! (is-none (get end-time session)) ERR_SESSION_ALREADY_ENDED)
    (map-set work-sessions
      { session-id: session-id }
      (merge session {
        end-time: (some current-time),
        total-hours: (some duration-hours),
        billable-amount: (some billable-amount),
        status: "completed"
      })
    )
    (ok {
      duration-hours: duration-hours,
      billable-amount: billable-amount
    })
  )
)

(define-public (approve-work-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? work-sessions { session-id: session-id }) ERR_WORK_SESSION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status session) "completed") ERR_INVALID_STATUS)
    (map-set work-sessions
      { session-id: session-id }
      (merge session {
        status: "approved",
        approved-by: (some tx-sender)
      })
    )
    (ok true)
  )
)

(define-public (generate-time-invoice 
  (project-id uint) 
  (contractor principal)
  (billing-period-start uint)
  (billing-period-end uint)
  (session-ids (list 10 uint))
)
  (let
    (
      (invoice-id (var-get next-invoice-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (total-calculations (calculate-invoice-totals session-ids))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (< billing-period-start billing-period-end) ERR_INVALID_TIME_ENTRY)
    (map-set time-invoices
      { invoice-id: invoice-id }
      {
        project-id: project-id,
        contractor: contractor,
        billing-period-start: billing-period-start,
        billing-period-end: billing-period-end,
        total-hours: (get total-hours total-calculations),
        total-amount: (get total-amount total-calculations),
        session-ids: session-ids,
        status: "generated",
        generated-at: current-time,
        approved-at: none,
        paid-at: none
      }
    )
    (var-set next-invoice-id (+ invoice-id u1))
    (ok invoice-id)
  )
)

(define-public (approve-invoice (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? time-invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status invoice) "generated") ERR_INVALID_STATUS)
    (map-set time-invoices
      { invoice-id: invoice-id }
      (merge invoice {
        status: "approved",
        approved-at: (some current-time)
      })
    )
    (ok true)
  )
)

(define-public (mark-invoice-paid (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? time-invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status invoice) "approved") ERR_INVALID_STATUS)
    (map-set time-invoices
      { invoice-id: invoice-id }
      (merge invoice {
        status: "paid",
        paid-at: (some current-time)
      })
    )
    (ok true)
  )
)

;; Helper function to calculate invoice totals from session IDs
(define-private (calculate-invoice-totals (session-ids (list 10 uint)))
  (fold accumulate-session-totals session-ids { total-hours: u0, total-amount: u0 })
)

(define-private (accumulate-session-totals 
  (session-id uint) 
  (acc { total-hours: uint, total-amount: uint })
)
  (match (map-get? work-sessions { session-id: session-id })
    session (if (is-eq (get status session) "approved")
      {
        total-hours: (+ (get total-hours acc) (default-to u0 (get total-hours session))),
        total-amount: (+ (get total-amount acc) (default-to u0 (get billable-amount session)))
      }
      acc
    )
    acc
  )
)

;; Read-only functions for time tracking data
(define-read-only (get-work-session (session-id uint))
  (map-get? work-sessions { session-id: session-id })
)

(define-read-only (get-project-hourly-rate (project-id uint) (contractor principal))
  (map-get? project-hourly-rates { project-id: project-id, contractor: contractor })
)

(define-read-only (get-time-invoice (invoice-id uint))
  (map-get? time-invoices { invoice-id: invoice-id })
)

(define-read-only (get-contractor-time-summary (contractor principal) (project-id uint))
  (ok {
    contractor: contractor,
    project-id: project-id,
    total-sessions: (get-contractor-session-count contractor project-id),
    total-billable-hours: (get-contractor-billable-hours contractor project-id),
    total-earnings: (get-contractor-earnings contractor project-id),
    active-sessions: (get-contractor-active-sessions contractor)
  })
)

(define-read-only (get-project-time-analytics (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (ok {
      project-id: project-id,
      total-logged-hours: (get-project-total-hours project-id),
      total-labor-cost: (get-project-labor-cost project-id),
      average-hourly-rate: (get-project-average-rate project-id),
      active-sessions-count: (get-project-active-sessions project-id)
    })
    ERR_PROJECT_NOT_FOUND
  )
)

;; Helper functions for analytics (simplified implementations)
(define-private (get-contractor-session-count (contractor principal) (project-id uint))
  u0 ;; Would implement session counting logic
)

(define-private (get-contractor-billable-hours (contractor principal) (project-id uint))
  u0 ;; Would implement hours calculation logic
)

(define-private (get-contractor-earnings (contractor principal) (project-id uint))
  u0 ;; Would implement earnings calculation logic
)

(define-private (get-contractor-active-sessions (contractor principal))
  u0 ;; Would implement active session counting
)

(define-private (get-project-total-hours (project-id uint))
  u0 ;; Would implement total hours calculation
)

(define-private (get-project-labor-cost (project-id uint))
  u0 ;; Would implement cost calculation
)

(define-private (get-project-average-rate (project-id uint))
  u0 ;; Would implement average rate calculation
)

(define-private (get-project-active-sessions (project-id uint))
  u0 ;; Would implement active session counting
)

(define-read-only (get-next-work-session-id)
  (var-get next-work-session-id)
)

(define-read-only (get-next-invoice-id)
  (var-get next-invoice-id)
)


