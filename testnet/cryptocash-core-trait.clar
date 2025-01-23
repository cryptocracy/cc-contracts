;; title: cryptocash-core-trait
;; version: 1.3.1
;; summary: CryptoCash Core Trait Contract

(define-trait cryptocash-core
  (
    (mine ((list 200 uint))
      (response bool uint)
    )

    (claim-mining-reward (uint)
      (response bool uint)
    )

    (stack (uint uint)
      (response bool uint)
    )

    (claim-stacking-reward (uint)
      (response bool uint)
    )

    (set-foundation-wallet (principal)
      (response bool uint)
    )   

  )
)