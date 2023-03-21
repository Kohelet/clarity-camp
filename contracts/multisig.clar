;; title: Multi Sig
;; version: 0.5
;; summary: A basic implementation of a multi sig contract.
;; description: Implementation of a multi-sig contract as a learning exercise to pickup Clarity.

;; Owner
(define-constant contract-owner tx-sender)

;;Error Definitions
(define-constant err-not-contract-owner (err u100))
(define-constant err-proposer-threshold-not-met (err u101))
(define-constant err-voter-thresdhold-not-met (err u102))
(define-constant err-above-contributor-proportion (err u103))
(define-constant err-withdraw-failed (err u104))
(define-constant err-contribution-failed (err u105))
(define-constant err-contract-balance-too-low (err u106))
(define-constant err-no-matching-proposal (err u107))
(define-constant err-proposal-executed (err u108))
(define-constant err-vote-failed (err u109))

;;MAX Connstants
;; These are need because of the multiplication use for threshold calculations.
(define-constant MAX_CONTRIBUTORS u113427455640312821154458202477256070485)
(define-constant MAX_VOTES u85070591730234615865843651857942052863)

;; Data Structures
(define-data-var proposer-threshold uint u250)
(define-data-var voter-threshold uint u500)
(define-map proposals {id: uint} {proposer: principal, amount: uint, destination: principal, votes: uint, executed: bool})
(define-data-var proposal-count uint u0)
(define-map contributors principal uint)
(define-data-var contributors-count uint u0)
(define-data-var total-contributed uint u0)

;; Participant Functions
(define-private (meets-proposer-threshold)
    (>= (default-to u0 (map-get? contributors tx-sender)) (var-get proposer-threshold))
)

(define-private (meets-voter-threshold)
    (>= (default-to u0 (map-get? contributors tx-sender)) (var-get voter-threshold))
)

(define-private (check-contract-balance (amount uint))
        (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) false)
)
(define-private (check-amount-below-contributor-proportion (amount uint))
    (begin
        (asserts! (>= (default-to u0 (map-get? contributors tx-sender)) amount) false)
        (asserts! (<= (* amount (var-get total-contributed)) (* (stx-get-balance (as-contract tx-sender)) (var-get contributors-count))) false)
    )
)

(define-private (increment-vote-count (proposal_id uint))
    (let ((proposal (map-get? proposals {id: proposal_id})))
        (asserts! (is-some proposal) false)
        (map-set proposals {id: proposal_id} 
        { 
            proposer: (unwrap-panic (get proposer proposal)), 
            amount: (unwrap-panic (get amount proposal)), 
            destination: (unwrap-panic (get destination proposal)), 
            votes: (+ (unwrap-panic (get votes proposal)) u1), 
            executed: (unwrap-panic (get executed proposal))
        })
        true
    )
)

(define-private (execute (proposal_id uint))
    (let ((proposal (map-get? proposals {id: proposal_id})))
        (asserts! (is-some proposal) false)
        (asserts! (not (unwrap-panic (get executed proposal))) false)
        (asserts! (check-contract-balance (unwrap-panic (get amount proposal))) false)
        (asserts! (is-ok (stx-transfer? (unwrap-panic (get amount proposal)) (as-contract tx-sender) (unwrap-panic (get destination proposal)))) false)
        (map-set proposals {id: proposal_id} 
        { 
            proposer: (unwrap-panic (get proposer proposal)), 
            amount: (unwrap-panic (get amount proposal)), 
            destination: (unwrap-panic (get destination proposal)), 
            votes: (unwrap-panic (get votes proposal)), 
            executed: true
        })
       true
    )
)

(define-private (votes-met (proposal_id uint))
    (let ((proposal (map-get? proposals {id: proposal_id})))
        (asserts! (is-some proposal) false)
        (asserts! (>= (* (unwrap-panic (get votes proposal)) u4) (* (var-get contributors-count) u3)) false)
    )
)

 ;; Functions
(define-public (contribute (amount uint))
    (begin 
        (if (is-eq (map-get? contributors tx-sender) none)
            (var-set contributors-count (+ (var-get contributors-count) u1))
            false
        )
        (asserts! (is-ok (stx-transfer? amount tx-sender (as-contract tx-sender))) err-contribution-failed)
        (map-set contributors tx-sender (+ (default-to u0 (map-get? contributors tx-sender)) amount))
        (ok (var-set total-contributed (+ (var-get total-contributed) amount)))
    )
)

;; Withdraw is a work in progress. Eventually it will be updated to only withdraw a proportion of the contract's balance that matches the proportion contributed by the contributor.
(define-public (withdraw (amount uint))
    (begin
        (asserts! (check-contract-balance amount) err-contract-balance-too-low)
        (asserts! (check-amount-below-contributor-proportion amount) err-above-contributor-proportion)
        (propose amount tx-sender)
    )
)

(define-public (propose (amount uint) (destination principal))
    (begin
        (asserts! (meets-proposer-threshold) err-proposer-threshold-not-met)
        (map-set proposals {id: (var-get proposal-count)} {proposer: tx-sender, amount: amount, destination: destination, votes: u0, executed: false})
        (var-set proposal-count (+ (var-get proposal-count) u1))
        (ok true)
    )
)

(define-public (vote (proposal_id uint))
    (begin 
        (asserts! (meets-voter-threshold) err-voter-thresdhold-not-met)
        (let ((proposal (map-get? proposals {id: proposal_id})))
            (asserts! (is-some proposal) err-no-matching-proposal)
            (asserts! (not (unwrap-panic (get executed proposal))) err-proposal-executed)
            (asserts! (increment-vote-count proposal_id) err-vote-failed)
            (if (votes-met proposal_id)
                (ok (execute proposal_id))
                (ok false)
            )
        )
    )
)
