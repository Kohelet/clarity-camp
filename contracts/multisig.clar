;; Owner
(define-constant contract-owner tx-sender)

;;Error Definitions
(define-constant err-not-contract-owner (err u100))
(define-constant err-proposer-threshold-not-met (err u101))
(define-constant err-voter-thresdhold-not-met (err u102))
(define-constant err-contributor-balance-too-low (err u103))
(define-constant err-withdraw-failed (err u104))
(define-constant err-contribution-failed (err u105))

;; Data Structure
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

(define-public (withdraw (amount uint))
    (begin
        (asserts! (>= (default-to u0 (map-get? contributors tx-sender)) amount) err-contributor-balance-too-low)
        (asserts! (is-ok (stx-transfer? amount (as-contract tx-sender) tx-sender)) err-withdraw-failed)
        (map-set contributors tx-sender (- (default-to u0 (map-get? contributors tx-sender)) amount))
        (if (is-eq (default-to  u0 (map-get? contributors tx-sender)) u0) 
            (var-set contributors-count (- (var-get contributors-count) u1))
            false
        )
        (ok (var-set total-contributed (- (var-get total-contributed) amount)))
    )
)
