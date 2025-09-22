;; amm-pool.clar
;; Constant-product AMM for token-token swap
;; - LP shares are an on-contract fungible token lp-token
;; - Protocol fee (bps) configurable by admin
;; - No TWAP/oracles included (simple AMM)

;; ------------------------
;; Traits
;; ------------------------
(use-trait ft-trait .sip-010-trait-ft-standard.sip-010-trait)

;; ------------------------
;; Constants / Errors
;; ------------------------
(define-constant BPS u10000)
(define-constant TWO u2)

(define-constant ERR_NOT_OWNER u100)
(define-constant ERR_BAD_AMOUNT u101)
(define-constant ERR_INSUFFICIENT_LIQ u102)
(define-constant ERR_TRANSFER u103)
(define-constant ERR_POOL_NOT_INIT u104)
(define-constant ERR_SAME_TOKEN u105)
(define-constant ERR_SLIPPAGE u106)

;; ------------------------
;; Tokens & config
;; ------------------------
(define-data-var token-a (optional principal) none)
(define-data-var token-b (optional principal) none)
(define-data-var fee-bps uint u30) ;; default 0.30%

;; ------------------------
;; Reserves & LP accounting
;; ------------------------
(define-data-var reserve-a uint u0) ;; token A units held by pool
(define-data-var reserve-b uint u0) ;; token B units held by pool

;; LP shares are represented by lp-token fungible token
(define-fungible-token lp-token)

(define-data-var total-shares uint u0) ;; total LP shares minted
(define-map shares { lp: principal } { amount: uint })

;; ------------------------
;; Events (print for indexers)
;; ------------------------
(define-private (emit (m (string-ascii 64)) (data (optional (tuple (k (string-ascii 32)) (v uint)))))
  (match data d
    (print {event: m, key: (get k d), value: (get v d)})
    (print {event: m, key: "", value: u0})))

;; ------------------------
;; Internal helpers
;; ------------------------
(define-private (assert-owner)
  (begin
    (asserts! (is-eq tx-sender contract-caller) (err ERR_NOT_OWNER))
    (ok true)))

(define-private (ft-transfer-from (token <ft-trait>) (amount uint) (from principal) (to principal))
  (as-contract
    (contract-call? token transfer? amount from to none)))

(define-private (ft-transfer-contract-to (token <ft-trait>) (amount uint) (to principal))
  (as-contract
    (contract-call? token transfer? amount (as-contract tx-sender) to none)))

;; ------------------------
;; Utility: sqrt (integer)
;; ------------------------
(define-read-only (sqrt (x uint))
  (let ((z (+ (/ x TWO) u1)))
    (let ((y (/ (+ z (/ x z)) TWO)))
      (if (< y z)
          (let ((ny (/ (+ y (/ x y)) TWO)))
            (if (< ny y) ny y))
          z))))

;; update reserves to actual contract-owned balances
(define-public (sync-reserves (token-x <ft-trait>) (token-y <ft-trait>))
  (begin
    ;; validate tokens
    (asserts! (is-eq (contract-of token-x) (unwrap! (var-get token-a) (err ERR_POOL_NOT_INIT))) (err ERR_POOL_NOT_INIT))
    (asserts! (is-eq (contract-of token-y) (unwrap! (var-get token-b) (err ERR_POOL_NOT_INIT))) (err ERR_POOL_NOT_INIT))
    ;; get actual balances
    (let ((balance-a (unwrap! (contract-call? token-x get-balance (as-contract tx-sender)) (err ERR_TRANSFER)))
          (balance-b (unwrap! (contract-call? token-y get-balance (as-contract tx-sender)) (err ERR_TRANSFER))))
      ;; update reserves
      (var-set reserve-a balance-a)
      (var-set reserve-b balance-b)
      (print { event: "reserves-synced", shares: u0 })
      (ok { a: balance-a, b: balance-b }))))

;; compute min of two uints
(define-read-only (min (x uint) (y uint))
  (if (< x y) x y))

;; ------------------------
;; Admin: initialize pool tokens (only once)
;; ------------------------
(define-public (initialize (tok-a principal) (tok-b principal) (protocol-fee-bps uint))
  (begin
    (try! (assert-owner))
    (asserts! (not (is-eq tok-a tok-b)) (err ERR_SAME_TOKEN))
    ;; validate fee is within bounds (0-100%)
    (asserts! (<= protocol-fee-bps BPS) (err ERR_BAD_AMOUNT))
    (var-set token-a (some tok-a))
    (var-set token-b (some tok-b))
    (var-set fee-bps protocol-fee-bps)
    ;; reserves are 0 until liquidity added
    (var-set reserve-a u0)
    (var-set reserve-b u0)
    (var-set total-shares u0)
    (ok true)))

;; ------------------------
;; View: get reserves
;; ------------------------
(define-read-only (get-reserves)
  { a: (var-get reserve-a), b: (var-get reserve-b) })

(define-read-only (get-fee)
  (var-get fee-bps))

(define-read-only (get-lp-balance (who principal))
  (default-to u0 (get amount (map-get? shares { lp: who }))))

;; ------------------------
;; Add liquidity
;; ------------------------
(define-public (add-liquidity (token-x <ft-trait>) (token-y <ft-trait>) (amount-a uint) (amount-b uint))
    ;; validate amounts and tokens
    (begin 
      (asserts! (> amount-a u0) (err ERR_BAD_AMOUNT))
      (asserts! (> amount-b u0) (err ERR_BAD_AMOUNT))
      (asserts! (is-eq (contract-of token-x) (unwrap! (var-get token-a) (err ERR_POOL_NOT_INIT))) (err ERR_POOL_NOT_INIT))
      (asserts! (is-eq (contract-of token-y) (unwrap! (var-get token-b) (err ERR_POOL_NOT_INIT))) (err ERR_POOL_NOT_INIT))
      
      (let ((ra (var-get reserve-a))
            (rb (var-get reserve-b))
            (ts (var-get total-shares)))
        (let ((mint-amount (if (or (is-eq ra u0) (is-eq rb u0) (is-eq ts u0))
                              (sqrt (* amount-a amount-b))
                              (min (/ (* amount-a ts) ra)
                                   (/ (* amount-b ts) rb)))))
          ;; validate mint amount
          (asserts! (> mint-amount u0) (err ERR_BAD_AMOUNT))
          ;; transfer tokens to pool
          (try! (contract-call? token-x transfer? amount-a tx-sender (as-contract tx-sender) none))
          (try! (contract-call? token-y transfer? amount-b tx-sender (as-contract tx-sender) none))
          ;; update state
          (var-set total-shares (+ ts mint-amount))
          (var-set reserve-a (+ ra amount-a))
          (var-set reserve-b (+ rb amount-b))
          (map-set shares 
                  { lp: tx-sender }
                  { amount: (+ (default-to u0 
                              (get amount (map-get? shares { lp: tx-sender })))
                            mint-amount) })
          ;; emit and return
          (print { event: "liquidity-added", shares: mint-amount })
          (ok mint-amount)))))

;; ------------------------
;; Remove liquidity
;; ------------------------
(define-public (remove-liquidity (token-x <ft-trait>) (token-y <ft-trait>) (share-amount uint))
  (begin
    (asserts! (> share-amount u0) (err ERR_BAD_AMOUNT))
    ;; validate tokens
    (asserts! (is-eq (contract-of token-x) (unwrap! (var-get token-a) (err ERR_POOL_NOT_INIT))) (err ERR_POOL_NOT_INIT))
    (asserts! (is-eq (contract-of token-y) (unwrap! (var-get token-b) (err ERR_POOL_NOT_INIT))) (err ERR_POOL_NOT_INIT))
    (let ((ts (var-get total-shares)))
      (asserts! (>= ts share-amount) (err ERR_INSUFFICIENT_LIQ))
      (let ((user-sh (default-to u0 (get amount (map-get? shares { lp: tx-sender })))))
        (asserts! (>= user-sh share-amount) (err ERR_INSUFFICIENT_LIQ))
        (let ((ra (var-get reserve-a))
              (rb (var-get reserve-b)))
          (let ((out-a (/ (* ra share-amount) ts))
                (out-b (/ (* rb share-amount) ts)))
            ;; update accounting
            (var-set total-shares (- ts share-amount))
            (map-set shares { lp: tx-sender } { amount: (- user-sh share-amount) })
            (var-set reserve-a (- ra out-a))
            (var-set reserve-b (- rb out-b))
            ;; transfer tokens to user
            (try! (contract-call? token-x transfer? out-a (as-contract tx-sender) tx-sender none))
            (try! (contract-call? token-y transfer? out-b (as-contract tx-sender) tx-sender none))
            (print { event: "liquidity-removed", shares: share-amount })
            (ok { out-a: out-a, out-b: out-b })))))))

;; ------------------------
;; Swap
;; ------------------------
(define-public (swap (token-x <ft-trait>) (token-y <ft-trait>) (amount-in uint) (min-out uint))
  (let ((token-a-val (unwrap! (var-get token-a) (err ERR_POOL_NOT_INIT)))
        (token-b-val (unwrap! (var-get token-b) (err ERR_POOL_NOT_INIT))))
    (asserts! (> amount-in u0) (err ERR_BAD_AMOUNT))
    ;; validate tokens
    (asserts! (or (is-eq (contract-of token-x) token-a-val)
                  (is-eq (contract-of token-x) token-b-val)) (err ERR_POOL_NOT_INIT))
    (asserts! (is-eq (contract-of token-y)
                     (if (is-eq (contract-of token-x) token-a-val)
                         token-b-val
                         token-a-val)) (err ERR_POOL_NOT_INIT))
    (let ((is-a (is-eq (contract-of token-x) token-a-val))
          (ra (var-get reserve-a))
          (rb (var-get reserve-b))
          (reserve-in (if is-a ra rb))
          (reserve-out (if is-a rb ra)))
      (begin
        (asserts! (> reserve-in u0) (err ERR_INSUFFICIENT_LIQ))
        (asserts! (> reserve-out u0) (err ERR_INSUFFICIENT_LIQ))
        ;; pull token-in from taker to contract
        (try! (contract-call? token-x transfer? amount-in tx-sender (as-contract tx-sender) none))
        ;; amount-in-with-fee
        (let ((amount-in-after (/ (* amount-in (- BPS (var-get fee-bps))) BPS)))
          (let ((amount-out (/ (* amount-in-after reserve-out)
                             (+ reserve-in amount-in-after))))
            (begin
              (asserts! (>= amount-out min-out) (err ERR_SLIPPAGE))
              ;; update reserves
              (if is-a
                  (begin
                    (var-set reserve-a (+ ra amount-in))
                    (var-set reserve-b (- rb amount-out)))
                  (begin
                    (var-set reserve-b (+ rb amount-in))
                    (var-set reserve-a (- ra amount-out))))
              ;; transfer out to taker
              (try! (contract-call? token-y transfer? amount-out (as-contract tx-sender) tx-sender none))
              (print { event: "swap", shares: amount-out })
              (ok amount-out))))))))

;; ------------------------
;; Read-only helpers exposed
;; ------------------------
(define-read-only (get-token-a)
  (var-get token-a))

(define-read-only (get-token-b)
  (var-get token-b))

(define-read-only (get-total-shares)
  (var-get total-shares))