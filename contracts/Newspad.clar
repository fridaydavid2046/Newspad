(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_STORY_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_STORY_NOT_FUNDED (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_STORY_ALREADY_PUBLISHED (err u107))

(define-data-var story-counter uint u0)
(define-data-var min-funding-amount uint u1000000)
(define-data-var voting-period uint u144)

(define-map stories
  { story-id: uint }
  {
    journalist: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    current-funding: uint,
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    voting-ends-at: uint,
    is-funded: bool,
    is-published: bool,
    content-hash: (optional (string-ascii 64))
  }
)

(define-map story-funders
  { story-id: uint, funder: principal }
  { amount: uint }
)

(define-map story-voters
  { story-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

(define-map journalist-reputation
  { journalist: principal }
  { stories-published: uint, total-funding-received: uint }
)

(define-public (propose-story (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint))
  (let
    (
      (story-id (+ (var-get story-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> funding-goal (var-get min-funding-amount)) ERR_INVALID_AMOUNT)
    (map-set stories
      { story-id: story-id }
      {
        journalist: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        current-funding: u0,
        votes-for: u0,
        votes-against: u0,
        created-at: current-block,
        voting-ends-at: (+ current-block (var-get voting-period)),
        is-funded: false,
        is-published: false,
        content-hash: none
      }
    )
    (var-set story-counter story-id)
    (ok story-id)
  )
)

(define-public (fund-story (story-id uint) (amount uint))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (current-funding (get current-funding story))
      (funding-goal (get funding-goal story))
      (existing-contribution (default-to u0 (get amount (map-get? story-funders { story-id: story-id, funder: tx-sender }))))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-funded story)) ERR_STORY_ALREADY_PUBLISHED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let
      (
        (new-funding (+ current-funding amount))
        (new-contribution (+ existing-contribution amount))
        (is-now-funded (>= new-funding funding-goal))
      )
      (map-set stories
        { story-id: story-id }
        (merge story { 
          current-funding: new-funding,
          is-funded: is-now-funded
        })
      )
      (map-set story-funders
        { story-id: story-id, funder: tx-sender }
        { amount: new-contribution }
      )
      (ok new-funding)
    )
  )
)

(define-public (vote-on-story (story-id uint) (vote-for bool))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (get is-funded story) ERR_STORY_NOT_FUNDED)
    (asserts! (<= current-block (get voting-ends-at story)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? story-voters { story-id: story-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    (let
      (
        (new-votes-for (if vote-for (+ (get votes-for story) u1) (get votes-for story)))
        (new-votes-against (if vote-for (get votes-against story) (+ (get votes-against story) u1)))
      )
      (map-set stories
        { story-id: story-id }
        (merge story {
          votes-for: new-votes-for,
          votes-against: new-votes-against
        })
      )
      (map-set story-voters
        { story-id: story-id, voter: tx-sender }
        { vote: vote-for, voted-at: current-block }
      )
      (ok true)
    )
  )
)

(define-public (publish-story (story-id uint) (content-hash (string-ascii 64)))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get journalist story)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-funded story) ERR_STORY_NOT_FUNDED)
    (asserts! (> current-block (get voting-ends-at story)) ERR_VOTING_ENDED)
    (asserts! (> (get votes-for story) (get votes-against story)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-published story)) ERR_STORY_ALREADY_PUBLISHED)
    (map-set stories
      { story-id: story-id }
      (merge story {
        is-published: true,
        content-hash: (some content-hash)
      })
    )
    (let
      (
        (journalist (get journalist story))
        (current-rep (default-to { stories-published: u0, total-funding-received: u0 } 
                      (map-get? journalist-reputation { journalist: journalist })))
      )
      (map-set journalist-reputation
        { journalist: journalist }
        {
          stories-published: (+ (get stories-published current-rep) u1),
          total-funding-received: (+ (get total-funding-received current-rep) (get current-funding story))
        }
      )
    )
    (try! (as-contract (stx-transfer? (get current-funding story) tx-sender (get journalist story))))
    (ok true)
  )
)

(define-public (refund-story (story-id uint))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (current-block stacks-block-height)
      (contribution (unwrap! (map-get? story-funders { story-id: story-id, funder: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (> current-block (get voting-ends-at story)) ERR_VOTING_ENDED)
    (asserts! (or 
      (not (get is-funded story))
      (and (get is-funded story) (<= (get votes-for story) (get votes-against story)))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-published story)) ERR_STORY_ALREADY_PUBLISHED)
    (let
      (
        (refund-amount (get amount contribution))
      )
      (map-delete story-funders { story-id: story-id, funder: tx-sender })
      (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
      (ok refund-amount)
    )
  )
)

(define-public (set-min-funding-amount (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set min-funding-amount amount)
    (ok true)
  )
)

(define-public (set-voting-period (blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set voting-period blocks)
    (ok true)
  )
)

(define-read-only (get-story (story-id uint))
  (map-get? stories { story-id: story-id })
)

(define-read-only (get-story-funding (story-id uint) (funder principal))
  (map-get? story-funders { story-id: story-id, funder: funder })
)

(define-read-only (get-story-vote (story-id uint) (voter principal))
  (map-get? story-voters { story-id: story-id, voter: voter })
)

(define-read-only (get-journalist-reputation (journalist principal))
  (map-get? journalist-reputation { journalist: journalist })
)

(define-read-only (get-story-counter)
  (var-get story-counter)
)

(define-read-only (get-min-funding-amount)
  (var-get min-funding-amount)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-story-status (story-id uint))
  (match (map-get? stories { story-id: story-id })
    story
    (let
      (
        (current-block stacks-block-height)
        (voting-ended (> current-block (get voting-ends-at story)))
        (funding-complete (get is-funded story))
        (vote-passed (> (get votes-for story) (get votes-against story)))
      )
      (ok {
        funding-complete: funding-complete,
        voting-ended: voting-ended,
        vote-passed: vote-passed,
        can-publish: (and funding-complete voting-ended vote-passed),
        can-refund: (and voting-ended (or (not funding-complete) (not vote-passed)))
      })
    )
    ERR_STORY_NOT_FOUND
  )
)