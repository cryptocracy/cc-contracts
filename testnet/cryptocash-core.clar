;; title: cryptocash-core
;; version: 1.3.1
;; summary: CryptoCash Core Contract
;; description: used to handle core CryptoCash logic

;; GENERAL CONFIGURATION
 
(impl-trait 'ST1ZK4MRVTQQJMVAAJQWBV2WPQ87QV2851YCTHD7X.cryptocash-core-trait.cryptocash-core)
(define-constant CONTRACT_OWNER tx-sender)

;; ERROR CODES

(define-constant ERR_UNAUTHORIZED u1000)
(define-constant ERR_USER_ALREADY_REGISTERED u1001)
(define-constant ERR_USER_NOT_FOUND u1002)
(define-constant ERR_USER_ID_NOT_FOUND u1003)
(define-constant ERR_ACTIVATION_THRESHOLD_REACHED u1004)
(define-constant ERR_CONTRACT_NOT_ACTIVATED u1005)
(define-constant ERR_USER_ALREADY_MINED u1006)
(define-constant ERR_INSUFFICIENT_COMMITMENT u1007)
(define-constant ERR_INSUFFICIENT_BALANCE u1008)
(define-constant ERR_USER_DID_NOT_MINE_IN_BLOCK u1009)
(define-constant ERR_CLAIMED_BEFORE_MATURITY u1010)
(define-constant ERR_NO_MINERS_AT_BLOCK u1011)
(define-constant ERR_DUPLICATE_CLAIM u1012)
(define-constant ERR_MINER_DID_NOT_WIN u1013)
(define-constant ERR_NO_VRF_SEED_FOUND u1014)
(define-constant ERR_STACKING_NOT_AVAILABLE u1015)
(define-constant ERR_CANNOT_STACK u1016)
(define-constant ERR_REWARD_CYCLE_NOT_COMPLETED u1017)
(define-constant ERR_NOTHING_TO_REDEEM u1018)
(define-constant ERR_UNABLE_TO_FIND_FOUNDATION_WALLET u1019)
(define-constant ERR_CLAIM_IN_WRONG_CONTRACT u1020)
(define-constant ERR_INVALID_WALLET u1021)
(define-constant ERR_INVALID_MEMO u1022)
(define-constant ERR_FUTURE_BLOCK u1023)
(define-constant ERR_INVALID_LOCK_PERIOD u1024)
(define-constant ERR_INVALID_AMOUNT u1025)
(define-constant ERR_INVALID_CYCLE u1026)
(define-constant ERR_VALIDATING_CLAIM_MINING_LIST u1027)
(define-constant ERR_VALIDATING_CLAIM_STACKING_LIST u1028)

;; FOUNDATION WALLET MANAGEMENT

;; initial value for foundation wallet, set to this
(define-data-var foundationWallet principal 'ST337NP61BD34ES77QK4XZP6R9AXV235GV52G1BDF)

;; returns set foundation wallet principal
(define-read-only (get-foundation-wallet)
  (var-get foundationWallet)
)

;; protected function to update foundation wallet variable
(define-public (set-foundation-wallet (newFoundationWallet principal))
    (if (is-eq tx-sender (get-foundation-wallet))
        (begin
            (asserts! (not (is-eq newFoundationWallet (get-foundation-wallet))) (err ERR_INVALID_WALLET)) ;; Ensure it's not the same as the current wallet
            (ok (var-set foundationWallet newFoundationWallet))
        )
        (ok false)
    )
)


;; REGISTRATION

(define-data-var activationBlock uint u340282366920938463463374607431768211455)
;; (define-data-var activationDelay uint u120)
(define-data-var activationReached bool false)

;; (define-data-var activationThreshold uint u20)
(define-data-var usersNonce uint u0)

;; returns block activation block height if activated
(define-read-only (get-activation-block)
  (let
    (
      (activated (var-get activationReached))
    )
    (asserts! activated (err ERR_CONTRACT_NOT_ACTIVATED))
    (ok (var-get activationBlock))
  )
)

;; returns activation status as boolean
(define-read-only (get-activation-status)
  (var-get activationReached)
)

;; returns number of registered users, used for activation and tracking user IDs
(define-read-only (get-registered-users-nonce)
  (var-get usersNonce)
)

;; store user principal by user id
(define-map Users
  uint
  principal
)

;; store user id by user principal
(define-map UserIds
  principal
  uint
)

;; returns (some userId) or none
(define-read-only (get-user-id (user principal))
  (map-get? UserIds user)
)

;; returns (some userPrincipal) or none
(define-read-only (get-user (userId uint))
  (map-get? Users userId)
)

;; returns user ID if it has been created, or creates and returns new ID
(define-private (get-or-create-user-id (user principal))
  (match
    (map-get? UserIds user)
    value value
    (let
      (
        (newId (+ u1 (var-get usersNonce)))
      )
      (map-set Users newId user)
      (map-set UserIds user newId)
      (var-set usersNonce newId)
      newId
    )
  )
)

;; MINING CONFIGURATION

;; define split to custodied wallet address for the foundation
(define-constant FOUNDATION_PCT u5)

;; how long a miner must wait before block winner can claim their minted tokens
(define-data-var tokenRewardMaturity uint u100)

;; At a given block height:
;; - how many miners were there
;; - what was the total amount submitted
;; - what was the total amount submitted to the Foundation
;; - what was the total amount submitted to Stackers
;; - was the block reward claimed
(define-map MiningStatsAtBlock
  uint
  {
    minersCount: uint,
    amount: uint,
    amountToFoundation: uint,
    amountToStackers: uint,
    rewardClaimed: bool
  }
)

;; returns map MiningStatsAtBlock at a given block height if it exists
(define-read-only (get-mining-stats-at-block (blockHeight uint))
  (map-get? MiningStatsAtBlock blockHeight)
)

;; returns map MiningStatsAtBlock at a given block height
;; or, an empty structure
(define-read-only (get-mining-stats-at-block-or-default (blockHeight uint))
  (default-to {
      minersCount: u0,
      amount: u0,
      amountToFoundation: u0,
      amountToStackers: u0,
      rewardClaimed: false
    }
    (map-get? MiningStatsAtBlock blockHeight)
  )
)

;; At a given block height and user ID:
;; - what is their ustx commitment
;; - what are the low/high values (used for VRF)
(define-map MinersAtBlock
  {
    blockHeight: uint,
    userId: uint
  }
  {
    ustx: uint,
    lowValue: uint,
    highValue: uint,
    winner: bool
  }
)

;; returns true if a given miner has already mined at a given block height
(define-read-only (has-mined-at-block (blockHeight uint) (userId uint))
  (is-some 
    (map-get? MinersAtBlock { blockHeight: blockHeight, userId: userId })
  )
)

;; returns map MinersAtBlock at a given block height for a user ID
(define-read-only (get-miner-at-block (blockHeight uint) (userId uint))
  (map-get? MinersAtBlock { blockHeight: blockHeight, userId: userId })
)

;; returns map MinersAtBlock at a given block height for a user ID
;; or, an empty structure
(define-read-only (get-miner-at-block-or-default (blockHeight uint) (userId uint))
  (default-to {
    highValue: u0,
    lowValue: u0,
    ustx: u0,
    winner: false
  }
    (map-get? MinersAtBlock { blockHeight: blockHeight, userId: userId }))
)

;; At a given block height:
;; - what is the max highValue from MinersAtBlock (used for VRF)
(define-map MinersAtBlockHighValue
  uint
  uint
)

;; returns last high value from map MinersAtBlockHighValue
(define-read-only (get-last-high-value-at-block (blockHeight uint))
  (default-to u0
    (map-get? MinersAtBlockHighValue blockHeight))
)

;; At a given block height:
;; - what is the userId of miner who won this block
(define-map BlockWinnerIds
  uint
  uint
)

(define-read-only (get-block-winner-id (blockHeight uint))
  (map-get? BlockWinnerIds blockHeight)
)

;; Return the current burn-block-height
(define-read-only (get-current-burn-block-height)
  (ok burn-block-height)
)

;; MINING ACTIONS
(define-public (mine (amounts (list 200 uint)))
  (begin
    (asserts! (get-activation-status) (err ERR_CONTRACT_NOT_ACTIVATED))
    (asserts! (> (fold + amounts u0) u0) (err ERR_INSUFFICIENT_COMMITMENT))
    (match (fold mine-block amounts (ok { userId: (get-or-create-user-id tx-sender), toStackers: u0, toFoundation: u0, blockHeight: burn-block-height }))
      okReturn 
      (begin
        (asserts! (>= (stx-get-balance tx-sender) (+ (get toStackers okReturn) (get toFoundation okReturn))) (err ERR_INSUFFICIENT_BALANCE))
        (if (> (get toStackers okReturn ) u0)
          (try! (stx-transfer? (get toStackers okReturn ) tx-sender (as-contract tx-sender)))
          false
        )
        (try! (stx-transfer? (get toFoundation okReturn) tx-sender (var-get foundationWallet)))

        ;; Event log
        (print 
          { 
            event: "mine", 
            who: tx-sender,
            userId: (get-or-create-user-id tx-sender),
            total: (fold + amounts u0),
            firstBlock: burn-block-height, 
            lastBlock: (- (+ burn-block-height (len amounts)) u1),
            totalBlocks: (len amounts)
          }
        )

        (ok true)
      )
      errReturn (err errReturn)
    )
  )
)

(define-private (mine-block 
  (amountUstx uint) 
  (return (response 
    { 
      userId: uint,
      toStackers: uint,
      toFoundation: uint,
      blockHeight: uint
    }
    uint
  )))
  
  (match return okReturn
    (let
      (
        (blockHeight (get blockHeight okReturn))
        (rewardCycle (default-to u0 (get-reward-cycle blockHeight)))
        (stackingActive (stacking-active-at-cycle rewardCycle))
        (toFoundation
          (if stackingActive
            (/ (* FOUNDATION_PCT amountUstx) u100)
            amountUstx
          )
        )
        (toStackers (- amountUstx toFoundation))
      )
      (asserts! (not (has-mined-at-block blockHeight (get userId okReturn))) (err ERR_USER_ALREADY_MINED))
      (asserts! (> amountUstx u0) (err ERR_INSUFFICIENT_COMMITMENT))
      (try! (set-tokens-mined (get userId okReturn) blockHeight amountUstx toStackers toFoundation))
      (ok (merge okReturn 
        {
          toStackers: (+ (get toStackers okReturn) toStackers),
          toFoundation: (+ (get toFoundation okReturn) toFoundation),
          blockHeight: (+ blockHeight u1)
        }
      ))
    )
    errReturn (err errReturn)
  ) 
)

(define-private (set-tokens-mined (userId uint) (blockHeight uint) (amountUstx uint) (toStackers uint) (toFoundation uint))
  (let
    (
      (blockStats (get-mining-stats-at-block-or-default blockHeight))
      (newMinersCount (+ (get minersCount blockStats) u1))
      (minerLowVal (get-last-high-value-at-block blockHeight))
      (rewardCycle (unwrap! (get-reward-cycle blockHeight)
        (err ERR_STACKING_NOT_AVAILABLE)))
      (rewardCycleStats (get-stacking-stats-at-cycle-or-default rewardCycle))
    )
    (map-set MiningStatsAtBlock
      blockHeight
      {
        minersCount: newMinersCount,
        amount: (+ (get amount blockStats) amountUstx),
        amountToFoundation: (+ (get amountToFoundation blockStats) toFoundation),
        amountToStackers: (+ (get amountToStackers blockStats) toStackers),
        rewardClaimed: false
      }
    )
    (map-set MinersAtBlock
      {
        blockHeight: blockHeight,
        userId: userId
      }
      {
        ustx: amountUstx,
        lowValue: (if (> minerLowVal u0) (+ minerLowVal u1) u0),
        highValue: (+ minerLowVal amountUstx),
        winner: false
      }
    )
    (map-set MinersAtBlockHighValue
      blockHeight
      (+ minerLowVal amountUstx)
    )
    (if (> toStackers u0)
      (map-set StackingStatsAtCycle
        rewardCycle
        {
          amountUstx: (+ (get amountUstx rewardCycleStats) toStackers),
          amountToken: (get amountToken rewardCycleStats)
        }
      )
      false
    )
    (ok true)
  )
)

;; MINING REWARD CLAIM ACTIONS

;; calls function to claim mining reward in active logic contract
(define-public (claim-mining-reward (minerBlockHeight uint))
    (begin
        (asserts! (<= minerBlockHeight burn-block-height) (err ERR_FUTURE_BLOCK)) ;; Ensure it's not in the future
        ;; Call claim-mining-reward-at-block after validation
        (try! (claim-mining-reward-at-block tx-sender burn-block-height minerBlockHeight))
        (ok true)
    )
)

;; Helper function for validation phase of claim-mining-reward-list
(define-private (validate-mining-claim-list (blockHeight uint) (acc (response bool uint)))
  (if (is-ok acc)
    ;; If accumulator is ok, proceed to validate the current block
    (if (can-claim-mining-reward tx-sender blockHeight)
      (ok true)
      (err ERR_VALIDATING_CLAIM_MINING_LIST)
    )
    ;; If accumulator is an error, propagate it
    acc
  )
)


;; Helper function for execution phase of claim-mining-reward-list
(define-private (execute-mining-claim-list (blockHeight uint) (acc (response bool uint)))
  (if (is-ok acc)
    ;; If accumulator is ok, proceed to process the current claim
    (let ((claim-result (claim-mining-reward blockHeight)))
      (if (is-ok claim-result)
        (ok true)
        claim-result  ;; Propagate the error from claim-mining-reward
      )
    )
    ;; If accumulator is an error, propagate it
    acc
  )
)

(define-public (claim-mining-reward-list (minerBlockHeights (list 200 uint)))
  (let
    (
      ;; First, validate all mining claims
      (validation (fold validate-mining-claim-list minerBlockHeights (ok true)))
    )
    (if (is-ok validation)
      ;; If all validations pass, execute all mining claims
      (let
        (
          ;; Process all mining claims, ensuring any error halts execution
          (results (fold execute-mining-claim-list minerBlockHeights (ok true)))
        )
        results
      )
      ;; If validation fails, return the error
      validation
    )
  )
)

;; Determine whether or not the given principal can claim the mined tokens at a particular block height,
;; given the miners record for that block height, a random sample, and the current block height.
(define-private (claim-mining-reward-at-block (user principal) (blockHeight uint) (minerBlockHeight uint))
  (let
    (
      (maturityHeight (+ (var-get tokenRewardMaturity) minerBlockHeight))
      (userId (unwrap! (get-user-id user) (err ERR_USER_ID_NOT_FOUND)))
      (blockStats (unwrap! (get-mining-stats-at-block minerBlockHeight) (err ERR_NO_MINERS_AT_BLOCK)))
      (minerStats (unwrap! (get-miner-at-block minerBlockHeight userId) (err ERR_USER_DID_NOT_MINE_IN_BLOCK)))
      (isMature (asserts! (> blockHeight maturityHeight) (err ERR_CLAIMED_BEFORE_MATURITY)))
      (vrfSample (unwrap! (contract-call? .cryptocash-vrf get-random-uint-at-block maturityHeight) (err ERR_NO_VRF_SEED_FOUND)))
      (commitTotal (get-last-high-value-at-block minerBlockHeight))
      (winningValue (mod vrfSample commitTotal))
    )
    (asserts! (not (get rewardClaimed blockStats)) (err ERR_DUPLICATE_CLAIM))
    (asserts! (and (>= winningValue (get lowValue minerStats)) (<= winningValue (get highValue minerStats)))
      (err ERR_MINER_DID_NOT_WIN))
    (try! (set-mining-reward-claimed userId minerBlockHeight))

    ;; Event log
    (print {
      event: "claim-mining-reward",
      who: tx-sender,
      userId: userId,
      claimHeight: minerBlockHeight
    })

    (ok true)
  )
)

(define-private (set-mining-reward-claimed (userId uint) (minerBlockHeight uint))
  (let
    (
      (blockStats (get-mining-stats-at-block-or-default minerBlockHeight))
      (minerStats (get-miner-at-block-or-default minerBlockHeight userId))
      (user (unwrap! (get-user userId) (err ERR_USER_NOT_FOUND)))
    )
    (map-set MiningStatsAtBlock
      minerBlockHeight
      {
        minersCount: (get minersCount blockStats),
        amount: (get amount blockStats),
        amountToFoundation: (get amountToFoundation blockStats),
        amountToStackers: (get amountToStackers blockStats),
        rewardClaimed: true
      }
    )
    (map-set MinersAtBlock
      {
        blockHeight: minerBlockHeight,
        userId: userId
      }
      {
        ustx: (get ustx minerStats),
        lowValue: (get lowValue minerStats),
        highValue: (get highValue minerStats),
        winner: true
      }
    )
    (map-set BlockWinnerIds
      minerBlockHeight
      userId
    )
    (try! (mint-coinbase user minerBlockHeight))
    (ok true)
  )
)

(define-read-only (is-block-winner (user principal) (minerBlockHeight uint))
  (is-block-winner-and-can-claim user minerBlockHeight false)
)

(define-read-only (can-claim-mining-reward (user principal) (minerBlockHeight uint))
  (is-block-winner-and-can-claim user minerBlockHeight true)
)

(define-private (is-block-winner-and-can-claim (user principal) (minerBlockHeight uint) (testCanClaim bool))
  (let
    (
      (userId (unwrap! (get-user-id user) false))
      (blockStats (unwrap! (get-mining-stats-at-block minerBlockHeight) false))
      (minerStats (unwrap! (get-miner-at-block minerBlockHeight userId) false))
      (maturityHeight (+ (var-get tokenRewardMaturity) minerBlockHeight))
      (vrfSample (unwrap! (contract-call? .cryptocash-vrf get-random-uint-at-block maturityHeight) false))
      (commitTotal (get-last-high-value-at-block minerBlockHeight))
      (winningValue (mod vrfSample commitTotal))
    )
    (if (and (>= winningValue (get lowValue minerStats)) (<= winningValue (get highValue minerStats)))
      (if testCanClaim (not (get rewardClaimed blockStats)) true)
      false
    )
  )
)

;; STACKING CONFIGURATION

(define-constant MAX_REWARD_CYCLES u32)
(define-constant REWARD_CYCLE_INDEXES (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31))

;; how long a reward cycle is
(define-data-var rewardCycleLength uint u2100)

;; At a given reward cycle:
;; - how many Stackers were there
;; - what is the total uSTX submitted by miners
;; - what is the total amount of tokens stacked
(define-map StackingStatsAtCycle
  uint
  {
    amountUstx: uint,
    amountToken: uint
  }
)

;; returns the total stacked tokens and committed uSTX for a given reward cycle
(define-read-only (get-stacking-stats-at-cycle (rewardCycle uint))
  (map-get? StackingStatsAtCycle rewardCycle)
)

;; returns the total stacked tokens and committed uSTX for a given reward cycle
;; or, an empty structure
(define-read-only (get-stacking-stats-at-cycle-or-default (rewardCycle uint))
  (default-to { amountUstx: u0, amountToken: u0 }
    (map-get? StackingStatsAtCycle rewardCycle))
)

;; At a given reward cycle and user ID:
;; - what is the total tokens Stacked?
;; - how many tokens should be returned? (based on Stacking period)
(define-map StackerAtCycle
  {
    rewardCycle: uint,
    userId: uint
  }
  {
    amountStacked: uint,
    toReturn: uint
  }
)

(define-read-only (get-stacker-at-cycle (rewardCycle uint) (userId uint))
  (map-get? StackerAtCycle { rewardCycle: rewardCycle, userId: userId })
)

(define-read-only (get-stacker-at-cycle-or-default (rewardCycle uint) (userId uint))
  (default-to { amountStacked: u0, toReturn: u0 }
    (map-get? StackerAtCycle { rewardCycle: rewardCycle, userId: userId }))
)

;; get the reward cycle for a given block height
(define-read-only (get-reward-cycle (blockHeight uint))
  (let
    (
      (firstStackingBlock (var-get activationBlock))
      (rcLen (var-get rewardCycleLength))
    )
    (if (>= blockHeight firstStackingBlock)
      (some (/ (- blockHeight firstStackingBlock) rcLen))
      none)
  )
)

;; determine if stacking is active in a given cycle
(define-read-only (stacking-active-at-cycle (rewardCycle uint))
  (is-some
    (get amountToken (map-get? StackingStatsAtCycle rewardCycle))
  )
)

;; get the first block height for a given reward cycle.
(define-read-only (get-first-block-in-reward-cycle (rewardCycle uint))
  (+ (var-get activationBlock) (* (var-get rewardCycleLength) rewardCycle))
)

;; getter for get-entitled-stacking-reward that specifies block height
(define-read-only (get-stacking-reward (userId uint) (targetCycle uint))
  (get-entitled-stacking-reward userId targetCycle burn-block-height)
)

;; get uSTX a Stacker can claim, given reward cycle they stacked in and current block height
;; this method only returns a positive value if:
;; - the current block height is in a subsequent reward cycle
;; - the stacker actually locked up tokens in the target reward cycle
;; - the stacker locked up _enough_ tokens to get at least one uSTX
;; it is possible to Stack tokens and not receive uSTX:
;; - if no miners commit during this reward cycle
;; - the amount stacked by user is too few that you'd be entitled to less than 1 uSTX
(define-private (get-entitled-stacking-reward (userId uint) (targetCycle uint) (blockHeight uint))
  (let
    (
      (rewardCycleStats (get-stacking-stats-at-cycle-or-default targetCycle))
      (stackerAtCycle (get-stacker-at-cycle-or-default targetCycle userId))
      (totalUstxThisCycle (get amountUstx rewardCycleStats))
      (totalStackedThisCycle (get amountToken rewardCycleStats))
      (userStackedThisCycle (get amountStacked stackerAtCycle))
    )
    (match (get-reward-cycle blockHeight)
      currentCycle
      (if (or (<= currentCycle targetCycle) (is-eq u0 userStackedThisCycle))
        ;; this cycle hasn't finished, or Stacker contributed nothing
        u0
        ;; (totalUstxThisCycle * userStackedThisCycle) / totalStackedThisCycle
        (/ (* totalUstxThisCycle userStackedThisCycle) totalStackedThisCycle)
      )
      ;; before first reward cycle
      u0
    )
  )
)

;; STACKING ACTIONS

(define-public (stack (amountTokens uint) (lockPeriod uint))
    (let
        (
            (userId (get-or-create-user-id tx-sender))
        )
        (begin
            ;; Ensure amountTokens is greater than zero
            (asserts! (> amountTokens u0) (err ERR_INVALID_AMOUNT))
            
            ;; Validate lockPeriod to ensure it's between 1 and 32 cycles
            (asserts! (>= lockPeriod u1) (err ERR_INVALID_LOCK_PERIOD))
            (asserts! (<= lockPeriod u32) (err ERR_INVALID_LOCK_PERIOD))
            
            ;; Call stack-tokens-at-cycle after validations
            (try! (stack-tokens-at-cycle tx-sender userId amountTokens burn-block-height lockPeriod))
            (ok true)
        )
    )
)

(define-private (stack-tokens-at-cycle (user principal) (userId uint) (amountTokens uint) (startHeight uint) (lockPeriod uint))
  (let
    (
      (currentCycle (unwrap! (get-reward-cycle startHeight) (err ERR_STACKING_NOT_AVAILABLE)))
      (targetCycle (+ u1 currentCycle))
      (commitment {
        stackerId: userId,
        amount: amountTokens,
        first: targetCycle,
        last: (+ targetCycle lockPeriod)
      })
    )
    (asserts! (get-activation-status) (err ERR_CONTRACT_NOT_ACTIVATED))
    (asserts! (and (> lockPeriod u0) (<= lockPeriod MAX_REWARD_CYCLES))
      (err ERR_CANNOT_STACK))
    (asserts! (> amountTokens u0) (err ERR_CANNOT_STACK))
    (try! (contract-call? .cryptocash-token transfer amountTokens tx-sender (as-contract tx-sender) none))

    ;; Event log
    (print {
      event:"stack",
      who: tx-sender,
      userId: userId,
      amountStacked: amountTokens,
      firstCycle: targetCycle, 
      lastCycle: (- (+ targetCycle lockPeriod) u1),
      lockPeriod: lockPeriod
    })

    (match (fold stack-tokens-closure REWARD_CYCLE_INDEXES (ok commitment))
      okValue (ok true)
      errValue (err errValue)
    )
  )
)

(define-private (stack-tokens-closure (rewardCycleIdx uint)
  (commitmentResponse (response 
    {
      stackerId: uint,
      amount: uint,
      first: uint,
      last: uint
    }
    uint
  )))

  (match commitmentResponse
    commitment 
    (let
      (
        (stackerId (get stackerId commitment))
        (amountToken (get amount commitment))
        (firstCycle (get first commitment))
        (lastCycle (get last commitment))
        (targetCycle (+ firstCycle rewardCycleIdx))
        (stackerAtCycle (get-stacker-at-cycle-or-default targetCycle stackerId))
        (amountStacked (get amountStacked stackerAtCycle))
        (toReturn (get toReturn stackerAtCycle))
      )
      (begin
        (if (and (>= targetCycle firstCycle) (< targetCycle lastCycle))
          (begin
            (if (is-eq targetCycle (- lastCycle u1))
              (set-tokens-stacked stackerId targetCycle amountToken amountToken)
              (set-tokens-stacked stackerId targetCycle amountToken u0)
            )
            true
          )
          false
        )
        commitmentResponse
      )
    )
    errValue commitmentResponse
  )
)

(define-private (set-tokens-stacked (userId uint) (targetCycle uint) (amountStacked uint) (toReturn uint))
  (let
    (
      (rewardCycleStats (get-stacking-stats-at-cycle-or-default targetCycle))
      (stackerAtCycle (get-stacker-at-cycle-or-default targetCycle userId))
    )
    (map-set StackingStatsAtCycle
      targetCycle
      {
        amountUstx: (get amountUstx rewardCycleStats),
        amountToken: (+ amountStacked (get amountToken rewardCycleStats))
      }
    )
    (map-set StackerAtCycle
      {
        rewardCycle: targetCycle,
        userId: userId
      }
      {
        amountStacked: (+ amountStacked (get amountStacked stackerAtCycle)),
        toReturn: (+ toReturn (get toReturn stackerAtCycle))
      }
    )
  )
)

;; STACKING REWARD CLAIMS

;; calls function to claim stacking reward in active logic contract
(define-public (claim-stacking-reward (targetCycle uint))
    (begin
        ;; Get the current reward cycle based on the current block height
        (let
            (
                (currentCycle (unwrap! (get-reward-cycle burn-block-height) (err ERR_INVALID_CYCLE)))
            )
            ;; Ensure that the target cycle is not in the future
            (asserts! (<= targetCycle currentCycle) (err ERR_INVALID_CYCLE))
            
            ;; Call claim-stacking-reward-at-cycle after validation
            (try! (claim-stacking-reward-at-cycle tx-sender burn-block-height targetCycle))
            (ok true)
        )
    )
)


;; Helper function for execution phase of claim-stacking-reward-list
(define-private (execute-stacking-claim-list (targetCycle uint) (acc (response bool uint)))
  (let 
    ((res  (claim-stacking-reward targetCycle)))
    (if (is-ok acc) 
      (asserts! (is-ok res) (err ERR_VALIDATING_CLAIM_STACKING_LIST))
      false
    )
    res
  )
)

(define-public (claim-stacking-reward-list (targetCycles (list 32 uint)))
  (begin 
      (asserts!          
          (is-ok 
            (fold execute-stacking-claim-list targetCycles (ok true))
          )       
      (err ERR_VALIDATING_CLAIM_STACKING_LIST))
    (ok true)
  )
)


(define-private (claim-stacking-reward-at-cycle (user principal) (blockHeight uint) (targetCycle uint))
  (let
    (
      (currentCycle (unwrap! (get-reward-cycle blockHeight) (err ERR_STACKING_NOT_AVAILABLE)))
      (userId (unwrap! (get-user-id user) (err ERR_USER_ID_NOT_FOUND)))
      (entitledUstx (get-entitled-stacking-reward userId targetCycle blockHeight))
      (stackerAtCycle (get-stacker-at-cycle-or-default targetCycle userId))
      (toReturn (get toReturn stackerAtCycle))
    )

    ;; Event log
    (print {
      event: "claim-stacking-reward", 
      who: user, 
      userId: userId,
      reward: entitledUstx, 
      amountStacked: toReturn,
      targetCycle: targetCycle
    })

    (asserts! (or
      ;; Compare current cycle and target cycle
      (> currentCycle targetCycle))
      (err ERR_REWARD_CYCLE_NOT_COMPLETED))
    (asserts! (or (> toReturn u0) (> entitledUstx u0)) (err ERR_NOTHING_TO_REDEEM))
    ;; disable ability to claim again
    (map-set StackerAtCycle
      {
        rewardCycle: targetCycle,
        userId: userId
      }
      {
        amountStacked: u0,
        toReturn: u0
      }
    )
    ;; send back tokens if user was eligible
    (if (> toReturn u0)
      (try! (as-contract (contract-call? .cryptocash-token transfer toReturn tx-sender user none)))
      true
    )
    ;; send back rewards if user was eligible
    (if (> entitledUstx u0)
      (try! (as-contract (stx-transfer? entitledUstx tx-sender user)))
      true
    )
    (ok true)
  )
)

;; Check to see if the given user principal can claim a stacking reward for the given targetCycle
(define-read-only (can-claim-stacking-reward (user principal) (targetCycle uint))
  (let
    (
      (currentCycleOpt (get-reward-cycle burn-block-height))
      (userIdOpt (get-user-id user))
    )
    (if (and (is-some currentCycleOpt) (is-some userIdOpt))
      (let
        (
          (currentCycle (unwrap! currentCycleOpt false))
          (userId (unwrap! userIdOpt false))
        )
        (if (> currentCycle targetCycle)
          (let
            (
              (entitledUstx (get-entitled-stacking-reward userId targetCycle burn-block-height))
              (stackerAtCycle (get-stacker-at-cycle-or-default targetCycle userId))
              (toReturn (get toReturn stackerAtCycle))
            )
            (or (> toReturn u0) (> entitledUstx u0))
          )
          false
        )
        false
      )
      false
    )
  )
)

;; TOKEN CONFIGURATION

;; store block height at each halving, set during activate-cryptocash
(define-data-var coinbaseThreshold1 uint u0)
(define-data-var coinbaseThreshold2 uint u0)
(define-data-var coinbaseThreshold3 uint u0)
(define-data-var coinbaseThreshold4 uint u0)
(define-data-var coinbaseThreshold5 uint u0)

(define-private (set-coinbase-thresholds)
  (let
    (
      (coinbaseAmounts (try! (contract-call? .cryptocash-token get-coinbase-thresholds)))
    )
      (var-set coinbaseThreshold1 (get coinbaseThreshold1 coinbaseAmounts))
      (var-set coinbaseThreshold2 (get coinbaseThreshold2 coinbaseAmounts))
      (var-set coinbaseThreshold3 (get coinbaseThreshold3 coinbaseAmounts))
      (var-set coinbaseThreshold4 (get coinbaseThreshold4 coinbaseAmounts))
      (var-set coinbaseThreshold5 (get coinbaseThreshold5 coinbaseAmounts))
      (ok true)
  )
)

;; return coinbase thresholds if contract activated
(define-read-only (get-coinbase-thresholds)
  (let
    (
      (activated (var-get activationReached))
    )
    (asserts! activated (err ERR_CONTRACT_NOT_ACTIVATED))
    (ok {
      coinbaseThreshold1: (var-get coinbaseThreshold1),
      coinbaseThreshold2: (var-get coinbaseThreshold2),
      coinbaseThreshold3: (var-get coinbaseThreshold3),
      coinbaseThreshold4: (var-get coinbaseThreshold4),
      coinbaseThreshold5: (var-get coinbaseThreshold5)
    })
  )
)

;; function for deciding how many tokens to mint, depending on when they were mined
(define-read-only (get-coinbase-amount (minerBlockHeight uint))
  (begin
    ;; if contract is not active, return 0
    (asserts! (>= minerBlockHeight (var-get activationBlock)) u0)
    ;; if contract is active, return based on issuance schedule
    ;; halvings occur every 210,000 blocks for 1,050,000 blocks
    ;; then mining continues indefinitely with 100 tokens as the reward
    (asserts! (> minerBlockHeight (var-get coinbaseThreshold1)) u3200)
    (asserts! (> minerBlockHeight (var-get coinbaseThreshold2)) u1600)
    (asserts! (> minerBlockHeight (var-get coinbaseThreshold3)) u800)
    (asserts! (> minerBlockHeight (var-get coinbaseThreshold4)) u400)
    (asserts! (> minerBlockHeight (var-get coinbaseThreshold5)) u200)
    ;; default value after 5th halving
    u100
  )
)

;; mint new tokens for claimant who won at given block height
(define-private (mint-coinbase (recipient principal) (blockHeight uint))
  (begin 
    (print {recipient:recipient, blockHeight:blockHeight, coinbaseAmount:(get-coinbase-amount blockHeight)})
    (as-contract (contract-call? .cryptocash-token mint (get-coinbase-amount blockHeight) recipient))
  )
)

;; check if contract caller is foundation wallet
(define-private (is-authorized-founder)
  (is-eq contract-caller (var-get foundationWallet))
)

;; check if contract caller is contract owner
(define-private (is-authorized-owner)
  (is-eq contract-caller CONTRACT_OWNER)
)

(define-public (activate-cryptocash)
  (let
    (
      (activationBlockVal (+ burn-block-height u100))
    )
      (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_UNAUTHORIZED))
      (try! (contract-call? .cryptocash-token activate-token activationBlockVal))
      (try! (set-coinbase-thresholds))
      (var-set activationReached true)
      (var-set activationBlock activationBlockVal)
      (ok true)
  )
)