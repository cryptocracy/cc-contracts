;; title: cryptocash-token-trait
;; version: 1.3.1
;; summary: CryptoCash Token Trait Contract

(define-trait cryptocash-token
  (
    (activate-token (uint)
      (response bool uint)
    )

    (set-token-uri ((optional (string-utf8 256)))
      (response bool uint)
    )

    (mint (uint principal)
      (response bool uint)
    )

    (burn (uint principal)
      (response bool uint)
    )

    (transfer (uint principal principal (optional (buff 34))) (response bool uint))

    (get-name () (response (string-ascii 32) uint))

    (get-symbol () (response (string-ascii 32) uint))

    (get-decimals () (response uint uint))

    (get-balance (principal) (response uint uint))

    (get-total-supply () (response uint uint))

    (get-token-uri () (response (optional (string-utf8 256)) uint))

    (send-many ((list 200 { to: principal, amount: uint, memo: (optional (buff 34)) }))
      (response bool uint)
    )
  )
)