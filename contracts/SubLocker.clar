
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