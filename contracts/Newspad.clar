(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_STORY_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_STORY_NOT_FUNDED (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_STORY_ALREADY_PUBLISHED (err u107))
(define-constant ERR_MILESTONE_NOT_FOUND (err u108))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u109))
(define-constant ERR_MILESTONE_NOT_READY (err u110))
(define-constant ERR_INSUFFICIENT_MILESTONE_VOTES (err u111))
(define-constant ERR_MILESTONE_DEADLINE_PASSED (err u112))
(define-constant ERR_INVALID_MILESTONE_INDEX (err u113))
(define-constant ERR_MILESTONE_ALREADY_APPROVED (err u114))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u115))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u116))
(define-constant ERR_INSUFFICIENT_ACCESS_LEVEL (err u117))
(define-constant ERR_CONTENT_NOT_FOUND (err u118))
(define-constant ERR_INVALID_SUBSCRIPTION_TIER (err u119))
(define-constant ERR_SUBSCRIPTION_ALREADY_ACTIVE (err u120))

(define-data-var story-counter uint u0)
(define-data-var min-funding-amount uint u1000000)
(define-data-var voting-period uint u144)
(define-data-var milestone-voting-period uint u72)
(define-data-var min-milestone-approval-percentage uint u60)
(define-data-var subscription-tier-counter uint u0)
(define-data-var premium-content-counter uint u0)

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
    content-hash: (optional (string-ascii 64)),
    milestone-count: uint,
    milestone-funds-released: uint
  }
)

(define-map story-milestones
  { story-id: uint, milestone-index: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    funding-percentage: uint,
    deadline: uint,
    evidence-hash: (optional (string-ascii 64)),
    is-completed: bool,
    is-approved: bool,
    approval-votes: uint,
    rejection-votes: uint,
    voting-deadline: uint,
    funds-released: uint
  }
)

(define-map milestone-voters
  { story-id: uint, milestone-index: uint, voter: principal }
  { vote: bool, voted-at: uint }
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

(define-map subscription-tiers
  { tier-id: uint }
  {
    journalist: principal,
    name: (string-ascii 50),
    description: (string-ascii 200),
    monthly-price: uint,
    access-level: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map subscriber-memberships
  { subscriber: principal, tier-id: uint }
  {
    subscribed-at: uint,
    expires-at: uint,
    is-active: bool,
    total-paid: uint
  }
)

(define-map premium-content
  { content-id: uint }
  {
    journalist: principal,
    title: (string-ascii 100),
    description: (string-ascii 300),
    content-hash: (string-ascii 64),
    required-access-level: uint,
    created-at: uint,
    is-published: bool
  }
)

(define-map content-access-log
  { content-id: uint, subscriber: principal }
  { accessed-at: uint, tier-used: uint }
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
        content-hash: none,
        milestone-count: u0,
        milestone-funds-released: u0
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

(define-public (create-milestone (story-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (funding-percentage uint) (deadline-blocks uint))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (current-block stacks-block-height)
      (milestone-index (get milestone-count story))
    )
    (asserts! (is-eq tx-sender (get journalist story)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-published story)) ERR_STORY_ALREADY_PUBLISHED)
    (asserts! (and (> funding-percentage u0) (<= funding-percentage u100)) ERR_INVALID_AMOUNT)
    (asserts! (> deadline-blocks u0) ERR_INVALID_AMOUNT)
    (map-set story-milestones
      { story-id: story-id, milestone-index: milestone-index }
      {
        title: title,
        description: description,
        funding-percentage: funding-percentage,
        deadline: (+ current-block deadline-blocks),
        evidence-hash: none,
        is-completed: false,
        is-approved: false,
        approval-votes: u0,
        rejection-votes: u0,
        voting-deadline: u0,
        funds-released: u0
      }
    )
    (map-set stories
      { story-id: story-id }
      (merge story { milestone-count: (+ milestone-index u1) })
    )
    (ok milestone-index)
  )
)

(define-public (submit-milestone-evidence (story-id uint) (milestone-index uint) (evidence-hash (string-ascii 64)))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (milestone (unwrap! (map-get? story-milestones { story-id: story-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get journalist story)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-funded story) ERR_STORY_NOT_FUNDED)
    (asserts! (not (get is-completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (<= current-block (get deadline milestone)) ERR_MILESTONE_DEADLINE_PASSED)
    (map-set story-milestones
      { story-id: story-id, milestone-index: milestone-index }
      (merge milestone {
        evidence-hash: (some evidence-hash),
        is-completed: true,
        voting-deadline: (+ current-block (var-get milestone-voting-period))
      })
    )
    (ok true)
  )
)

(define-public (vote-on-milestone (story-id uint) (milestone-index uint) (approve bool))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (milestone (unwrap! (map-get? story-milestones { story-id: story-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
      (current-block stacks-block-height)
      (funder-contribution (unwrap! (map-get? story-funders { story-id: story-id, funder: tx-sender }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (get is-completed milestone) ERR_MILESTONE_NOT_READY)
    (asserts! (<= current-block (get voting-deadline milestone)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? milestone-voters { story-id: story-id, milestone-index: milestone-index, voter: tx-sender })) ERR_ALREADY_VOTED)
    (let
      (
        (voter-weight (get amount funder-contribution))
        (new-approval-votes (if approve (+ (get approval-votes milestone) voter-weight) (get approval-votes milestone)))
        (new-rejection-votes (if approve (get rejection-votes milestone) (+ (get rejection-votes milestone) voter-weight)))
      )
      (map-set story-milestones
        { story-id: story-id, milestone-index: milestone-index }
        (merge milestone {
          approval-votes: new-approval-votes,
          rejection-votes: new-rejection-votes
        })
      )
      (map-set milestone-voters
        { story-id: story-id, milestone-index: milestone-index, voter: tx-sender }
        { vote: approve, voted-at: current-block }
      )
      (ok true)
    )
  )
)

(define-public (release-milestone-funds (story-id uint) (milestone-index uint))
  (let
    (
      (story (unwrap! (map-get? stories { story-id: story-id }) ERR_STORY_NOT_FOUND))
      (milestone (unwrap! (map-get? story-milestones { story-id: story-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (get is-completed milestone) ERR_MILESTONE_NOT_READY)
    (asserts! (> current-block (get voting-deadline milestone)) ERR_VOTING_ENDED)
    (asserts! (not (get is-approved milestone)) ERR_MILESTONE_ALREADY_APPROVED)
    (let
      (
        (total-votes (+ (get approval-votes milestone) (get rejection-votes milestone)))
        (approval-percentage (if (> total-votes u0) (/ (* (get approval-votes milestone) u100) total-votes) u0))
        (milestone-passed (>= approval-percentage (var-get min-milestone-approval-percentage)))
      )
      (asserts! milestone-passed ERR_INSUFFICIENT_MILESTONE_VOTES)
      (let
        (
          (milestone-amount (/ (* (get current-funding story) (get funding-percentage milestone)) u100))
          (journalist (get journalist story))
        )
        (map-set story-milestones
          { story-id: story-id, milestone-index: milestone-index }
          (merge milestone {
            is-approved: true,
            funds-released: milestone-amount
          })
        )
        (map-set stories
          { story-id: story-id }
          (merge story { milestone-funds-released: (+ (get milestone-funds-released story) milestone-amount) })
        )
        (try! (as-contract (stx-transfer? milestone-amount tx-sender journalist)))
        (ok milestone-amount)
      )
    )
  )
)

(define-public (set-milestone-voting-period (blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set milestone-voting-period blocks)
    (ok true)
  )
)

(define-public (set-min-milestone-approval-percentage (percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= percentage u0) (<= percentage u100)) ERR_INVALID_AMOUNT)
    (var-set min-milestone-approval-percentage percentage)
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

(define-read-only (get-milestone-voting-period)
  (var-get milestone-voting-period)
)

(define-read-only (get-min-milestone-approval-percentage)
  (var-get min-milestone-approval-percentage)
)

(define-read-only (get-story-milestone (story-id uint) (milestone-index uint))
  (map-get? story-milestones { story-id: story-id, milestone-index: milestone-index })
)

(define-read-only (get-milestone-vote (story-id uint) (milestone-index uint) (voter principal))
  (map-get? milestone-voters { story-id: story-id, milestone-index: milestone-index, voter: voter })
)

(define-read-only (get-milestone-status (story-id uint) (milestone-index uint))
  (match (map-get? story-milestones { story-id: story-id, milestone-index: milestone-index })
    milestone
    (let
      (
        (current-block stacks-block-height)
        (deadline-passed (> current-block (get deadline milestone)))
        (voting-ended (> current-block (get voting-deadline milestone)))
        (total-votes (+ (get approval-votes milestone) (get rejection-votes milestone)))
        (approval-percentage (if (> total-votes u0) (/ (* (get approval-votes milestone) u100) total-votes) u0))
        (vote-passed (>= approval-percentage (var-get min-milestone-approval-percentage)))
      )
      (ok {
        is-completed: (get is-completed milestone),
        is-approved: (get is-approved milestone),
        deadline-passed: deadline-passed,
        voting-ended: voting-ended,
        vote-passed: vote-passed,
        approval-percentage: approval-percentage,
        can-release-funds: (and (get is-completed milestone) voting-ended vote-passed (not (get is-approved milestone)))
      })
    )
    ERR_MILESTONE_NOT_FOUND
  )
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

(define-public (create-subscription-tier (name (string-ascii 50)) (description (string-ascii 200)) (monthly-price uint) (access-level uint))
  (let
    (
      (tier-id (+ (var-get subscription-tier-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> monthly-price u0) ERR_INVALID_AMOUNT)
    (asserts! (and (> access-level u0) (<= access-level u5)) ERR_INVALID_SUBSCRIPTION_TIER)
    (map-set subscription-tiers
      { tier-id: tier-id }
      {
        journalist: tx-sender,
        name: name,
        description: description,
        monthly-price: monthly-price,
        access-level: access-level,
        is-active: true,
        created-at: current-block
      }
    )
    (var-set subscription-tier-counter tier-id)
    (ok tier-id)
  )
)

(define-public (subscribe-to-tier (tier-id uint) (months uint))
  (let
    (
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (current-block stacks-block-height)
      (total-cost (* (get monthly-price tier) months))
      (blocks-per-month u4320)
      (subscription-duration (* months blocks-per-month))
      (existing-membership (map-get? subscriber-memberships { subscriber: tx-sender, tier-id: tier-id }))
    )
    (asserts! (get is-active tier) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (> months u0) ERR_INVALID_AMOUNT)
    (asserts! (> total-cost u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? total-cost tx-sender (get journalist tier)))
    (let
      (
        (new-expires-at (+ current-block subscription-duration))
        (existing-total (default-to u0 (get total-paid existing-membership)))
      )
      (map-set subscriber-memberships
        { subscriber: tx-sender, tier-id: tier-id }
        {
          subscribed-at: current-block,
          expires-at: new-expires-at,
          is-active: true,
          total-paid: (+ existing-total total-cost)
        }
      )
      (ok new-expires-at)
    )
  )
)

(define-public (create-premium-content (title (string-ascii 100)) (description (string-ascii 300)) (content-hash (string-ascii 64)) (required-access-level uint))
  (let
    (
      (content-id (+ (var-get premium-content-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (and (> required-access-level u0) (<= required-access-level u5)) ERR_INVALID_SUBSCRIPTION_TIER)
    (map-set premium-content
      { content-id: content-id }
      {
        journalist: tx-sender,
        title: title,
        description: description,
        content-hash: content-hash,
        required-access-level: required-access-level,
        created-at: current-block,
        is-published: true
      }
    )
    (var-set premium-content-counter content-id)
    (ok content-id)
  )
)

(define-public (access-premium-content (content-id uint) (tier-id uint))
  (let
    (
      (content (unwrap! (map-get? premium-content { content-id: content-id }) ERR_CONTENT_NOT_FOUND))
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (membership (unwrap! (map-get? subscriber-memberships { subscriber: tx-sender, tier-id: tier-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (get is-published content) ERR_CONTENT_NOT_FOUND)
    (asserts! (get is-active membership) ERR_SUBSCRIPTION_EXPIRED)
    (asserts! (> (get expires-at membership) current-block) ERR_SUBSCRIPTION_EXPIRED)
    (asserts! (>= (get access-level tier) (get required-access-level content)) ERR_INSUFFICIENT_ACCESS_LEVEL)
    (map-set content-access-log
      { content-id: content-id, subscriber: tx-sender }
      { accessed-at: current-block, tier-used: tier-id }
    )
    (ok (get content-hash content))
  )
)

(define-public (cancel-subscription (tier-id uint))
  (let
    (
      (membership (unwrap! (map-get? subscriber-memberships { subscriber: tx-sender, tier-id: tier-id }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (get is-active membership) ERR_SUBSCRIPTION_NOT_FOUND)
    (map-set subscriber-memberships
      { subscriber: tx-sender, tier-id: tier-id }
      (merge membership { is-active: false })
    )
    (ok true)
  )
)

(define-public (deactivate-subscription-tier (tier-id uint))
  (let
    (
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get journalist tier)) ERR_NOT_AUTHORIZED)
    (map-set subscription-tiers
      { tier-id: tier-id }
      (merge tier { is-active: false })
    )
    (ok true)
  )
)

(define-public (update-tier-pricing (tier-id uint) (new-monthly-price uint))
  (let
    (
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get journalist tier)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-monthly-price u0) ERR_INVALID_AMOUNT)
    (map-set subscription-tiers
      { tier-id: tier-id }
      (merge tier { monthly-price: new-monthly-price })
    )
    (ok true)
  )
)

(define-read-only (get-subscription-tier (tier-id uint))
  (map-get? subscription-tiers { tier-id: tier-id })
)

(define-read-only (get-subscriber-membership (subscriber principal) (tier-id uint))
  (map-get? subscriber-memberships { subscriber: subscriber, tier-id: tier-id })
)

(define-read-only (get-premium-content (content-id uint))
  (map-get? premium-content { content-id: content-id })
)

(define-read-only (get-content-access-log (content-id uint) (subscriber principal))
  (map-get? content-access-log { content-id: content-id, subscriber: subscriber })
)

(define-read-only (get-subscription-tier-counter)
  (var-get subscription-tier-counter)
)

(define-read-only (get-premium-content-counter)
  (var-get premium-content-counter)
)

(define-read-only (is-subscription-active (subscriber principal) (tier-id uint))
  (match (map-get? subscriber-memberships { subscriber: subscriber, tier-id: tier-id })
    membership
    (let
      (
        (current-block stacks-block-height)
      )
      (and 
        (get is-active membership)
        (> (get expires-at membership) current-block)
      )
    )
    false
  )
)

(define-read-only (can-access-content (subscriber principal) (content-id uint) (tier-id uint))
  (match (map-get? premium-content { content-id: content-id })
    content
    (match (map-get? subscription-tiers { tier-id: tier-id })
      tier
      (let
        (
          (has-active-subscription (is-subscription-active subscriber tier-id))
          (sufficient-access-level (>= (get access-level tier) (get required-access-level content)))
        )
        (and has-active-subscription sufficient-access-level)
      )
      false
    )
    false
  )
)



