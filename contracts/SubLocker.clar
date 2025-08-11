
;; title: SubLocker
;; version: 1.0.0
;; summary: Recurring payments in BTC terms with STX conversion
;; description: A subscription management system that allows users to create and manage 
;;              recurring payments denominated in satoshis but paid in STX at current market rates.
;;              Solves volatility issues in crypto subscriptions by fixing amounts in BTC terms.

;; traits
;;

;; token definitions
;;

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-PAYMENT-NOT-DUE (err u105))
(define-constant ERR-SUBSCRIPTION-INACTIVE (err u106))
(define-constant ERR-INVALID-INTERVAL (err u107))
(define-constant ERR-SELF-SUBSCRIPTION (err u108))
(define-constant ERR-CONTRACT-PAUSED (err u109))

;; Subscription limits
(define-constant MIN-SUBSCRIPTION-AMOUNT u1000)      ;; 1000 sats minimum
(define-constant MAX-SUBSCRIPTION-AMOUNT u100000000) ;; 1 BTC maximum
(define-constant MIN-BILLING-INTERVAL u4320)         ;; ~30 days in blocks (144 blocks/day * 30)
(define-constant MAX-SUBSCRIPTIONS-PER-USER u50)

;; Platform settings
(define-constant PLATFORM-FEE-BPS u250)              ;; 2.5% platform fee
(define-constant BASIS-POINTS u10000)
(define-constant SATOSHIS-PER-BTC u100000000)

;; data vars
(define-data-var subscription-counter uint u0)
(define-data-var btc-to-stx-rate uint u30000000000)  ;; Default: 30,000 STX per BTC (multiplied by 1M for precision)
(define-data-var platform-fee-recipient principal CONTRACT-OWNER)
(define-data-var contract-paused bool false)
(define-data-var last-rate-update uint u0)

;; data maps
;; Core subscription data
(define-map subscriptions
    uint ;; subscription-id
    {
        subscriber: principal,
        merchant: principal,
        amount-sats: uint,
        billing-interval: uint,
        is-active: bool,
        created-at: uint,
        last-payment: uint,
        next-payment-due: uint,
        total-payments: uint
    }
)

;; Payment records for history tracking
(define-map payments
    {subscription-id: uint, payment-id: uint}
    {
        amount-sats: uint,
        amount-stx: uint,
        btc-stx-rate: uint,
        platform-fee: uint,
        block-height: uint,
        timestamp: uint
    }
)

;; Track user's active subscriptions count
(define-map user-subscription-count
    principal
    uint
)

;; Track subscription IDs by user (for enumeration)
(define-map user-subscriptions
    {user: principal, index: uint}
    uint ;; subscription-id
)

;; Track merchant earnings
(define-map merchant-earnings
    principal
    uint ;; total STX earned
)


;; public functions

;; Create a new subscription
(define-public (create-subscription (merchant principal) (amount-sats uint) (billing-interval uint))
    (let (
        (subscription-id (+ (var-get subscription-counter) u1))
        (current-block stacks-block-height)
        (next-payment (+ current-block billing-interval))
        (user-sub-count (default-to u0 (map-get? user-subscription-count tx-sender)))
    )
        ;; Validation checks
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (not (is-eq tx-sender merchant)) ERR-SELF-SUBSCRIPTION)
        (asserts! (and (>= amount-sats MIN-SUBSCRIPTION-AMOUNT) 
                      (<= amount-sats MAX-SUBSCRIPTION-AMOUNT)) ERR-INVALID-AMOUNT)
        (asserts! (>= billing-interval MIN-BILLING-INTERVAL) ERR-INVALID-INTERVAL)
        (asserts! (< user-sub-count MAX-SUBSCRIPTIONS-PER-USER) ERR-UNAUTHORIZED)
        
        ;; Create the subscription
        (map-set subscriptions subscription-id
            {
                subscriber: tx-sender,
                merchant: merchant,
                amount-sats: amount-sats,
                billing-interval: billing-interval,
                is-active: true,
                created-at: current-block,
                last-payment: u0,
                next-payment-due: next-payment,
                total-payments: u0
            }
        )
        
        ;; Update user subscription tracking
        (map-set user-subscriptions 
            {user: tx-sender, index: user-sub-count} 
            subscription-id)
        (map-set user-subscription-count tx-sender (+ user-sub-count u1))
        
        ;; Update global counter
        (var-set subscription-counter subscription-id)
        
        (ok subscription-id)
    )
)

;; Process payment for a subscription
(define-public (process-subscription-payment (subscription-id uint))
    (let (
        (subscription (unwrap! (map-get? subscriptions subscription-id) ERR-NOT-FOUND))
        (current-block stacks-block-height)
        (btc-rate (var-get btc-to-stx-rate))
        (amount-sats (get amount-sats subscription))
        (amount-stx (calculate-stx-from-sats amount-sats btc-rate))
        (platform-fee (/ (* amount-stx PLATFORM-FEE-BPS) BASIS-POINTS))
        (merchant-amount (- amount-stx platform-fee))
        (payment-id (+ (get total-payments subscription) u1))
        (subscriber (get subscriber subscription))
        (merchant (get merchant subscription))
    )
        ;; Validation checks
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (get is-active subscription) ERR-SUBSCRIPTION-INACTIVE)
        (asserts! (>= current-block (get next-payment-due subscription)) ERR-PAYMENT-NOT-DUE)
        (asserts! (>= (stx-get-balance subscriber) amount-stx) ERR-INSUFFICIENT-BALANCE)
        
        ;; Process payments
        (try! (stx-transfer? merchant-amount subscriber merchant))
        (try! (stx-transfer? platform-fee subscriber (var-get platform-fee-recipient)))
        
        ;; Record the payment
        (map-set payments
            {subscription-id: subscription-id, payment-id: payment-id}
            {
                amount-sats: amount-sats,
                amount-stx: amount-stx,
                btc-stx-rate: btc-rate,
                platform-fee: platform-fee,
                block-height: current-block,
                timestamp: current-block
            }
        )
        
        ;; Update subscription
        (map-set subscriptions subscription-id
            (merge subscription 
                {
                    last-payment: current-block,
                    next-payment-due: (+ current-block (get billing-interval subscription)),
                    total-payments: payment-id
                }
            )
        )
        
        ;; Update merchant earnings
        (map-set merchant-earnings merchant
            (+ (default-to u0 (map-get? merchant-earnings merchant)) merchant-amount))
        
        (ok {
            payment-id: payment-id,
            amount-stx: amount-stx,
            platform-fee: platform-fee,
            next-payment-due: (+ current-block (get billing-interval subscription))
        })
    )
)

;; Cancel a subscription
(define-public (cancel-subscription (subscription-id uint))
    (let (
        (subscription (unwrap! (map-get? subscriptions subscription-id) ERR-NOT-FOUND))
    )
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-eq tx-sender (get subscriber subscription)) ERR-UNAUTHORIZED)
        (asserts! (get is-active subscription) ERR-SUBSCRIPTION-INACTIVE)
        
        ;; Deactivate subscription
        (map-set subscriptions subscription-id
            (merge subscription {is-active: false})
        )
        
        (ok true)
    )
)

;; Update BTC to STX exchange rate (owner only)
(define-public (update-btc-stx-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (asserts! (> new-rate u0) ERR-INVALID-AMOUNT)
        
        (var-set btc-to-stx-rate new-rate)
        (var-set last-rate-update stacks-block-height)
        
        (ok true)
    )
)

;; Update platform fee recipient (owner only)
(define-public (set-platform-fee-recipient (new-recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (var-set platform-fee-recipient new-recipient)
        (ok true)
    )
)

;; Emergency pause/unpause contract (owner only)
(define-public (toggle-contract-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (var-set contract-paused (not (var-get contract-paused)))
        (ok (var-get contract-paused))
    )
)

;; read only functions

;; Get subscription details
(define-read-only (get-subscription (subscription-id uint))
    (map-get? subscriptions subscription-id)
)

;; Get payment details
(define-read-only (get-payment (subscription-id uint) (payment-id uint))
    (map-get? payments {subscription-id: subscription-id, payment-id: payment-id})
)

;; Get user's subscription count
(define-read-only (get-user-subscription-count (user principal))
    (default-to u0 (map-get? user-subscription-count user))
)

;; Get user's subscription by index
(define-read-only (get-user-subscription-by-index (user principal) (index uint))
    (map-get? user-subscriptions {user: user, index: index})
)

;; Get merchant total earnings
(define-read-only (get-merchant-earnings (merchant principal))
    (default-to u0 (map-get? merchant-earnings merchant))
)

;; Get current BTC to STX rate
(define-read-only (get-current-btc-rate)
    (var-get btc-to-stx-rate)
)

;; Calculate STX amount from satoshis
(define-read-only (calculate-stx-amount (amount-sats uint))
    (calculate-stx-from-sats amount-sats (var-get btc-to-stx-rate))
)

;; Calculate total payment amount including platform fee
(define-read-only (calculate-payment-breakdown (amount-sats uint))
    (let (
        (amount-stx (calculate-stx-from-sats amount-sats (var-get btc-to-stx-rate)))
        (platform-fee (/ (* amount-stx PLATFORM-FEE-BPS) BASIS-POINTS))
        (merchant-amount (- amount-stx platform-fee))
    )
        {
            total-stx: amount-stx,
            merchant-amount: merchant-amount,
            platform-fee: platform-fee
        }
    )
)

;; Check if subscription payment is due
(define-read-only (is-payment-due (subscription-id uint))
    (match (map-get? subscriptions subscription-id)
        subscription
        (and 
            (get is-active subscription)
            (>= stacks-block-height (get next-payment-due subscription))
        )
        false
    )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
    {
        total-subscriptions: (var-get subscription-counter),
        btc-stx-rate: (var-get btc-to-stx-rate),
        platform-fee-bps: PLATFORM-FEE-BPS,
        contract-paused: (var-get contract-paused),
        last-rate-update: (var-get last-rate-update),
        platform-fee-recipient: (var-get platform-fee-recipient)
    }
)

;; Get subscription limits and constraints
(define-read-only (get-subscription-limits)
    {
        min-amount-sats: MIN-SUBSCRIPTION-AMOUNT,
        max-amount-sats: MAX-SUBSCRIPTION-AMOUNT,
        min-billing-interval: MIN-BILLING-INTERVAL,
        max-subscriptions-per-user: MAX-SUBSCRIPTIONS-PER-USER,
        platform-fee-bps: PLATFORM-FEE-BPS
    }
)


;; private functions

;; Calculate STX amount from satoshis using given rate
(define-private (calculate-stx-from-sats (sats uint) (rate uint))
    (/ (* sats rate) (* SATOSHIS-PER-BTC u1000000))
)
