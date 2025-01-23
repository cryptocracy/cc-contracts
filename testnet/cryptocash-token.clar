;; title: cryptocash-token
;; version: 1.3.1
;; summary: CryptoCash Token Contract
;; description: used to handle token CryptoCash logic - sip-010

;; CONTRACT OWNER
(define-constant CONTRACT_OWNER tx-sender)

;; TRAIT DEFINITIONS
(impl-trait .cryptocash-token-trait.cryptocash-token)
(use-trait coreTrait .cryptocash-core-trait.cryptocash-core)

;; ERROR CODES
(define-constant ERR_UNAUTHORIZED u2000)
(define-constant ERR_TOKEN_NOT_ACTIVATED u2001)
(define-constant ERR_TOKEN_ALREADY_ACTIVATED u2002)
(define-constant ERR_INVALID_AMOUNT u2003)
(define-constant ERR_SELF_TRANSFER u2004)
(define-constant ERR_INVALID_URI u2005)
(define-constant ERR_INVALID_RECIPIENT u2006)

(define-constant cryptocash-core .cryptocash-core)

;; SIP-010 DEFINITION
(impl-trait 'ST1ZK4MRVTQQJMVAAJQWBV2WPQ87QV2851YCTHD7X.sip-010-trait-ft-standard.sip-010-trait)

(define-fungible-token CC)

;; SIP-010 FUNCTIONS
(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq from tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (not (is-eq from to)) (err ERR_SELF_TRANSFER))
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    (if (is-some memo)
      (print memo)
      none
    )
    (ft-transfer? CC amount from to)
  )
)


(define-read-only (get-name)
  (ok "CryptoCash")
)

(define-read-only (get-symbol)
  (ok "CC")
)

(define-read-only (get-decimals)
  (ok u0)
)

(define-read-only (get-balance (user principal))
  (ok (ft-get-balance CC user))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply CC))
)

(define-read-only (get-token-uri)
  (ok (var-get tokenUri))
)

;; TOKEN CONFIGURATION

;; how many blocks until the next halving occurs
(define-constant TOKEN_HALVING_BLOCKS u210000)

;; store block height at each halving, set by register-user in core contract 
(define-data-var coinbaseThreshold1 uint u0)
(define-data-var coinbaseThreshold2 uint u0)
(define-data-var coinbaseThreshold3 uint u0)
(define-data-var coinbaseThreshold4 uint u0)
(define-data-var coinbaseThreshold5 uint u0)

;; once activated, thresholds cannot be updated again
(define-data-var tokenActivated bool false)

;; core contract states
(define-constant STATE_DEPLOYED u0)
(define-constant STATE_ACTIVE u1)
(define-constant STATE_INACTIVE u2)

;; one-time function to activate the token
(define-public (activate-token (blockHeight uint))
  (begin
    (asserts! (> blockHeight u0) (err ERR_UNAUTHORIZED))
    (asserts! (is-eq CONTRACT_OWNER tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (not (var-get tokenActivated)) (err ERR_TOKEN_ALREADY_ACTIVATED))
    (var-set tokenActivated true)
    (var-set coinbaseThreshold1 (+ blockHeight TOKEN_HALVING_BLOCKS))
    (var-set coinbaseThreshold2 (+ blockHeight (* u2 TOKEN_HALVING_BLOCKS)))
    (var-set coinbaseThreshold3 (+ blockHeight (* u3 TOKEN_HALVING_BLOCKS)))
    (var-set coinbaseThreshold4 (+ blockHeight (* u4 TOKEN_HALVING_BLOCKS)))
    (var-set coinbaseThreshold5 (+ blockHeight (* u5 TOKEN_HALVING_BLOCKS)))
    (ok true)
 )
)

;; return coinbase thresholds if token activated
(define-read-only (get-coinbase-thresholds)
  (let
    (
      (activated (var-get tokenActivated))
    )
    (asserts! activated (err ERR_TOKEN_NOT_ACTIVATED))
    (ok {
      coinbaseThreshold1: (var-get coinbaseThreshold1),
      coinbaseThreshold2: (var-get coinbaseThreshold2),
      coinbaseThreshold3: (var-get coinbaseThreshold3),
      coinbaseThreshold4: (var-get coinbaseThreshold4),
      coinbaseThreshold5: (var-get coinbaseThreshold5)
    })
  )
)

;; UTILITIES

(define-data-var tokenUri (optional (string-utf8 256)) (some u"https://stackers.cc/cryptocash.json"))

;; set token URI to new value, only accessible by Auth
(define-public (set-token-uri (newUri (optional (string-utf8 256))))
    (begin
        (try! (is-authorized-auth)) ;; Check authorization, returns error if unauthorized
        (asserts! (not (is-none newUri)) (err ERR_INVALID_URI)) ;; Ensure `newUri` is present

        (ok (var-set tokenUri newUri))
    )
)

;; mint new tokens, only accessible by a Core contract
(define-public (mint (amount uint) (recipient principal))
    (let
        (
            (check (is-eq contract-caller cryptocash-core))
        )
        (begin
            (asserts! check (err ERR_UNAUTHORIZED))
            (asserts! (not (is-eq recipient cryptocash-core)) (err ERR_INVALID_RECIPIENT)) ;; Ensure recipient is not cryptocash-core
            (asserts! (> amount u0) (err ERR_INVALID_AMOUNT)) ;; Ensure amount is greater than zero

            ;; Additional checks for recipient can go here if necessary

            (if check
                (ft-mint? CC amount recipient)
                (ok false)
            )
        )
    )
)


(define-public (burn (amount uint) (owner principal))
  (begin
    (asserts! (is-eq tx-sender owner) (err ERR_UNAUTHORIZED))
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    (ft-burn? CC amount owner)
  )
)

;; checks if caller is Auth contract
(define-private (is-authorized-auth)
    (if (is-eq tx-sender CONTRACT_OWNER)
        (ok true)
        (err ERR_UNAUTHORIZED)
    )
) ;; Return an error if unauthorized


;; SEND-MANY

(define-public (send-many (recipients (list 200 { to: principal, amount: uint, memo: (optional (buff 34)) })))
  (fold check-err
    (map send-token recipients)
    (ok true)
  )
)

(define-private (check-err (result (response bool uint)) (prior (response bool uint)))
  (match prior ok-value result
               err-value (err err-value)
  )
)

(define-private (send-token (recipient { to: principal, amount: uint, memo: (optional (buff 34)) }))
  (send-token-with-memo (get amount recipient) (get to recipient) (get memo recipient))
)

(define-private (send-token-with-memo (amount uint) (to principal) (memo (optional (buff 34))))
  (let
    (
      (transferOk (try! (transfer amount tx-sender to memo)))
    )
    (ok transferOk)
  )
)