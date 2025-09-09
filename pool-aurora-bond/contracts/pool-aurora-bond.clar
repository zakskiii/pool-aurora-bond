;; Aurora Liquid Staking Protocol - Trimmed Version

;; Error Constants
(define-constant ERR-UNAUTHORIZED (err u1000))
(define-constant ERR-CONTRACT-PAUSED (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1003))
(define-constant ERR-POOL-NOT-FOUND (err u1004))
(define-constant ERR-VALIDATOR-NOT-FOUND (err u1005))
(define-constant ERR-OPERATION-FAILED (err u1006))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-STAKE u100)
(define-constant BASE-YIELD-RATE u500) ;; 5%

;; State Variables
(define-data-var contract-paused bool false)
(define-data-var total-staked uint u0)
(define-data-var validator-counter uint u0)
(define-data-var pool-counter uint u0)

;; Data Maps
(define-map pools
    uint
    {
        name: (string-ascii 32),
        tvl: uint,
        apy: uint,
        active: bool,
        min-stake: uint
    }
)

(define-map validators
    principal
    {
        id: uint,
        stake: uint,
        active: bool,
        commission: uint
    }
)

(define-map staking-positions
    {user: principal, pool: uint}
    {
        amount: uint,
        shares: uint,
        entry-block: uint,
        last-claim: uint
    }
)

;; Utility Functions
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (contract-call-check)
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (ok true)
    )
)

(define-private (calculate-shares (amount uint) (pool-id uint))
    (match (map-get? pools pool-id)
        pool-data
        (let ((current-tvl (get tvl pool-data)))
            (if (is-eq current-tvl u0)
                amount
                (/ (* amount u1000000) current-tvl)))
        amount
    )
)

(define-private (calculate-yield (shares uint) (pool-id uint) (blocks-staked uint))
    (match (map-get? pools pool-id)
        pool-data
        (let ((base-apy (get apy pool-data)))
            (/ (* shares base-apy blocks-staked) u100000))
        u0
    )
)

;; Pool Management
(define-public (create-pool (name (string-ascii 32)) (apy uint) (min-stake uint))
    (let ((pool-id (+ (var-get pool-counter) u1)))
        (begin
            (try! (contract-call-check))
            (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
            (asserts! (>= min-stake MIN-STAKE) ERR-INVALID-AMOUNT)
            
            (map-set pools pool-id {
                name: name,
                tvl: u0,
                apy: apy,
                active: true,
                min-stake: min-stake
            })
            (var-set pool-counter pool-id)
            (ok pool-id)
        )
    )
)

;; Validator Management
(define-public (register-validator (initial-stake uint) (commission uint))
    (let ((validator-id (+ (var-get validator-counter) u1)))
        (begin
            (try! (contract-call-check))
            (asserts! (>= initial-stake MIN-STAKE) ERR-INVALID-AMOUNT)
            (asserts! (is-none (map-get? validators tx-sender)) ERR-OPERATION-FAILED)
            
            (map-set validators tx-sender {
                id: validator-id,
                stake: initial-stake,
                active: true,
                commission: commission
            })
            
            (var-set validator-counter validator-id)
            (ok validator-id)
        )
    )
)

;; Staking Functions
(define-public (stake-to-pool (pool-id uint) (amount uint))
    (let ((shares (calculate-shares amount pool-id)))
        (begin
            (try! (contract-call-check))
            (asserts! (> amount u0) ERR-INVALID-AMOUNT)
            
            (match (map-get? pools pool-id)
                pool-data
                (begin
                    (asserts! (get active pool-data) ERR-POOL-NOT-FOUND)
                    (asserts! (>= amount (get min-stake pool-data)) ERR-INVALID-AMOUNT)
                    
                    ;; Update or create position
                    (match (map-get? staking-positions {user: tx-sender, pool: pool-id})
                        existing-pos
                        (map-set staking-positions {user: tx-sender, pool: pool-id}
                            (merge existing-pos {
                                amount: (+ (get amount existing-pos) amount),
                                shares: (+ (get shares existing-pos) shares)
                            })
                        )
                        (map-set staking-positions {user: tx-sender, pool: pool-id} {
                            amount: amount,
                            shares: shares,
                            entry-block: block-height,
                            last-claim: block-height
                        })
                    )
                    
                    ;; Update pool TVL
                    (map-set pools pool-id 
                        (merge pool-data {tvl: (+ (get tvl pool-data) amount)}))
                    (ok shares)
                )
                ERR-POOL-NOT-FOUND
            )
        )
    )
)

(define-public (unstake-from-pool (pool-id uint) (shares uint))
    (let (
        (position (unwrap! (map-get? staking-positions {user: tx-sender, pool: pool-id}) ERR-INSUFFICIENT-BALANCE))
        (withdrawal-amount (/ (* shares (get amount position)) (get shares position)))
    )
        (begin
            (try! (contract-call-check))
            (asserts! (>= (get shares position) shares) ERR-INSUFFICIENT-BALANCE)
            
            (if (is-eq shares (get shares position))
                (map-delete staking-positions {user: tx-sender, pool: pool-id})
                (map-set staking-positions {user: tx-sender, pool: pool-id}
                    (merge position {
                        amount: (- (get amount position) withdrawal-amount),
                        shares: (- (get shares position) shares)
                    })
                )
            )
            
            ;; Update pool TVL
            (match (map-get? pools pool-id)
                pool-data
                (map-set pools pool-id 
                    (merge pool-data {tvl: (- (get tvl pool-data) withdrawal-amount)}))
                false
            )
            (ok withdrawal-amount)
        )
    )
)

;; Yield Functions
(define-public (claim-yield (pool-id uint))
    (let (
        (position (unwrap! (map-get? staking-positions {user: tx-sender, pool: pool-id}) ERR-INSUFFICIENT-BALANCE))
        (blocks-staked (- block-height (get last-claim position)))
        (yield-amount (calculate-yield (get shares position) pool-id blocks-staked))
    )
        (begin
            (try! (contract-call-check))
            (asserts! (> yield-amount u0) ERR-INSUFFICIENT-BALANCE)
            
            (map-set staking-positions {user: tx-sender, pool: pool-id}
                (merge position {last-claim: block-height}))
            
            (ok yield-amount)
        )
    )
)

;; Admin Functions
(define-public (pause-contract)
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (resume-contract)
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (var-set contract-paused false)
        (ok true)
    )
)

;; Read-Only Functions
(define-read-only (get-pool-info (pool-id uint))
    (map-get? pools pool-id)
)

(define-read-only (get-validator-info (validator principal))
    (map-get? validators validator)
)

(define-read-only (get-staking-position (user principal) (pool-id uint))
    (map-get? staking-positions {user: user, pool: pool-id})
)

(define-read-only (get-protocol-stats)
    {
        total-staked: (var-get total-staked),
        total-validators: (var-get validator-counter),
        total-pools: (var-get pool-counter),
        contract-paused: (var-get contract-paused)
    }
)