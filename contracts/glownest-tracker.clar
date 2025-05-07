;; glownest-tracker
;; 
;; This contract manages user wellness data, goals, and achievements for the GlowNest wellness application.
;; It allows users to securely track their sleep, hydration, and mindfulness metrics, set personal goals,
;; and earn achievements based on their consistency and progress. The contract implements a gamified approach
;; through achievement badges and wellness scores to motivate users toward healthier habits while ensuring
;; complete data sovereignty and privacy.

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-INVALID-METRIC (err u1002))
(define-constant ERR-INVALID-VALUE (err u1003))
(define-constant ERR-GOAL-NOT-FOUND (err u1004))
(define-constant ERR-METRIC-VALUE-TOO-HIGH (err u1005))
(define-constant ERR-USER-NOT-FOUND (err u1006))
(define-constant ERR-ACHIEVEMENT-ALREADY-EARNED (err u1007))
(define-constant ERR-CANNOT-OVERWRITE-PREVIOUS-ENTRY (err u1008))

;; Data Maps and Variables

;; Store basic user profiles
(define-map users
  { user: principal }
  {
    joined-at: uint,
    wellness-score: uint,
    streak-days: uint
  }
)

;; Store daily wellness metrics (sleep, hydration, mindfulness)
(define-map daily-metrics
  { user: principal, date: uint }
  {
    sleep-hours: uint,
    water-ml: uint,
    meditation-minutes: uint,
    recorded-at: uint
  }
)

;; Store user's personal wellness goals
(define-map user-goals
  { user: principal }
  {
    sleep-hours-goal: uint,
    water-ml-goal: uint,
    meditation-minutes-goal: uint,
    last-updated: uint
  }
)

;; Track user achievements (badges)
(define-map user-achievements
  { user: principal, achievement-id: uint }
  {
    earned-at: uint,
    achievement-name: (string-utf8 50)
  }
)

;; Define achievement types
(define-map achievement-definitions
  { achievement-id: uint }
  {
    name: (string-utf8 50),
    description: (string-utf8 255),
    category: (string-utf8 20),
    threshold: uint
  }
)

;; Private Functions

;; Check if user exists and initialize if not
(define-private (ensure-user-exists (user principal))
  (if (is-some (map-get? users { user: user }))
    true  ;; User exists
    (begin
      (map-set users
        { user: user }
        {
          joined-at: (unwrap-panic (get-block-info? time u0)),
          wellness-score: u0,
          streak-days: u0
        }
      )
      true  ;; User initialized
    )
  )
)

;; Validate metric values based on reasonable ranges
(define-private (validate-metric-value (metric-type (string-utf8 20)) (value uint))
  (if (is-eq metric-type u"sleep-hours")
    (if (and (>= value u0) (<= value u24))
      true
      false
    )
  (if (is-eq metric-type u"water-ml")
      (if (and (>= value u0) (<= value u10000))
        true
        false
      )
      (if (is-eq metric-type u"meditation-minutes")
        (if (and (>= value u0) (<= value u1440))
          true
          false
        )
        false
      )
    )
  )
)

;; Calculate wellness score based on consistency and goal achievement
(define-private (calculate-wellness-score (user principal))
  (let (
    (user-data (unwrap! (map-get? users { user: user }) u0))
    (goals (unwrap! (map-get? user-goals { user: user }) u0))
    (current-time (unwrap-panic (get-block-info? time u0)))
    (yesterday (- current-time (* u60 u60 u24)))
    (metrics (map-get? daily-metrics { user: user, date: yesterday }))
  )
    (if (is-some metrics)
      (let (
        (current-metrics (unwrap-panic metrics))
        (sleep-percent (if (> (get sleep-hours-goal goals) u0)
          (if (<= (* u100 (/ (get sleep-hours current-metrics) (get sleep-hours-goal goals))) u100)
            (* u100 (/ (get sleep-hours current-metrics) (get sleep-hours-goal goals)))
            u100)
          u0))
        (water-percent (if (> (get water-ml-goal goals) u0)
          (if (<= (* u100 (/ (get water-ml current-metrics) (get water-ml-goal goals))) u100)
            (* u100 (/ (get water-ml current-metrics) (get water-ml-goal goals)))
            u100)
          u0))
        (meditation-percent (if (> (get meditation-minutes-goal goals) u0)
          (if (<= (* u100 (/ (get meditation-minutes current-metrics) (get meditation-minutes-goal goals))) u100)
            (* u100 (/ (get meditation-minutes current-metrics) (get meditation-minutes-goal goals)))
            u100)
          u0))
        (average-percent (/ (+ sleep-percent water-percent meditation-percent) u3))
        (current-score (get wellness-score user-data))
        (new-score (+ (/ current-score u10) (* (/ average-percent u100) u90)))
      )
        (map-set users 
          { user: user }
          (merge user-data { wellness-score: new-score })
        )
        new-score
      )
      ;; If no metrics for yesterday, slightly decrease the score
      (let (
        (current-score (get wellness-score user-data))
        (new-score (if (> current-score u5) (- current-score u5) u0))
      )
        (map-set users 
          { user: user }
          (merge user-data { wellness-score: new-score })
        )
        new-score
      )
    )
  )
)

;; Check if user has met goal for specific metric
(define-private (check-goal-achievement (user principal) (metric-type (string-utf8 20)) (value uint))
  (let (
    (goals (map-get? user-goals { user: user }))
  )
    (if (is-some goals)
      (let (
        (user-goal-data (unwrap-panic goals))
      )
        (if (is-eq metric-type u"sleep-hours")
          (>= value (get sleep-hours-goal user-goal-data))
          (if (is-eq metric-type u"water-ml")
            (>= value (get water-ml-goal user-goal-data))
            (if (is-eq metric-type u"meditation-minutes")
              (>= value (get meditation-minutes-goal user-goal-data))
              false
            )
          )
        )
      )
      false
    )
  )
)

;; Helper function to get default user data
(define-private (default-user)
  {
    joined-at: u0,
    wellness-score: u0,
    streak-days: u0
  }
)

;; Issue an achievement to a user if they don't already have it
(define-private (issue-achievement (user principal) (achievement-id uint) (achievement-name (string-utf8 50)) (timestamp uint))
  (let (
    (existing-achievement (map-get? user-achievements { user: user, achievement-id: achievement-id }))
  )
    (if (is-none existing-achievement)
      (begin
        (map-set user-achievements
          { user: user, achievement-id: achievement-id }
          { 
            earned-at: timestamp,
            achievement-name: achievement-name
          }
        )
        (ok true)
      )
      (ok true) ;; User already has this achievement, just return OK
    )
  )
)

;; Format date to YYYYMMDD from timestamp
(define-private (format-date-from-timestamp (timestamp uint))
  (let (
    (seconds-per-day (* u60 u60 u24))
    (days-since-epoch (/ timestamp seconds-per-day))
  )
    (* days-since-epoch seconds-per-day)
  )
)