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
    achievement-name: (string-ascii 50)
  }
)

;; Define achievement types
(define-map achievement-definitions
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 255),
    category: (string-ascii 20),
    threshold: uint
  }
)

;; Private Functions

;; Check if user exists and initialize if not
(define-private (ensure-user-exists (user principal))
  (match (map-get? users { user: user })
    existing-user true
    (begin
      (map-set users
        { user: user }
        {
          joined-at: (unwrap-panic (get-block-info? time u0)),
          wellness-score: u0,
          streak-days: u0
        }
      )
      true
    )
  )
)

;; Validate metric values based on reasonable ranges
(define-private (validate-metric-value (metric-type (string-ascii 20)) (value uint))
  (if (string-eq? metric-type "sleep-hours")
    (if (and (>= value u0) (<= value u24))
      true
      false
    )
    (if (string-eq? metric-type "water-ml")
      (if (and (>= value u0) (<= value u10000))
        true
        false
      )
      (if (string-eq? metric-type "meditation-minutes")
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
          (min u100 (* u100 (/ (get sleep-hours current-metrics) (get sleep-hours-goal goals))))
          u0))
        (water-percent (if (> (get water-ml-goal goals) u0)
          (min u100 (* u100 (/ (get water-ml current-metrics) (get water-ml-goal goals))))
          u0))
        (meditation-percent (if (> (get meditation-minutes-goal goals) u0)
          (min u100 (* u100 (/ (get meditation-minutes current-metrics) (get meditation-minutes-goal goals))))
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
(define-private (check-goal-achievement (user principal) (metric-type (string-ascii 20)) (value uint))
  (let (
    (goals (map-get? user-goals { user: user }))
  )
    (if (is-some goals)
      (let (
        (user-goals (unwrap-panic goals))
      )
        (if (string-eq? metric-type "sleep-hours")
          (>= value (get sleep-hours-goal user-goals))
          (if (string-eq? metric-type "water-ml")
            (>= value (get water-ml-goal user-goals))
            (if (string-eq? metric-type "meditation-minutes")
              (>= value (get meditation-minutes-goal user-goals))
              false
            )
          )
        )
      )
      false
    )
  )
)

;; Update user streak based on daily entries
(define-private (update-streak (user principal))
  (let (
    (user-data (unwrap! (map-get? users { user: user }) (default-user)))
    (current-time (unwrap-panic (get-block-info? time u0)))
    (yesterday-timestamp (- current-time (* u60 u60 u24)))
    (yesterday-metrics (map-get? daily-metrics { user: user, date: yesterday-timestamp }))
  )
    (if (is-some yesterday-metrics)
      ;; User logged metrics yesterday, increase streak
      (map-set users 
        { user: user }
        (merge user-data { streak-days: (+ (get streak-days user-data) u1) })
      )
      ;; User didn't log metrics yesterday, reset streak
      (map-set users 
        { user: user }
        (merge user-data { streak-days: u1 })
      )
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

;; Check and issue achievements if applicable
(define-private (check-and-issue-achievements (user principal))
  (let (
    (user-data (unwrap! (map-get? users { user: user }) (default-user)))
    (streak (get streak-days user-data))
    (current-time (unwrap-panic (get-block-info? time u0)))
  )
    ;; Check streak-based achievements
    (if (>= streak u7)
      (try! (issue-achievement user u1 "7-Day Streak" current-time))
      true
    )
    (if (>= streak u30)
      (try! (issue-achievement user u2 "30-Day Streak" current-time))
      true
    )
    (if (>= streak u100)
      (try! (issue-achievement user u3 "100-Day Streak" current-time))
      true
    )
    
    ;; Check wellness score achievements
    (let ((score (get wellness-score user-data)))
      (if (>= score u50)
        (try! (issue-achievement user u4 "Wellness Beginner" current-time))
        true
      )
      (if (>= score u75)
        (try! (issue-achievement user u5 "Wellness Enthusiast" current-time))
        true
      )
      (if (>= score u90)
        (try! (issue-achievement user u6 "Wellness Master" current-time))
        true
      )
    )
    (ok true)
  )
)

;; Issue an achievement to a user if they don't already have it
(define-private (issue-achievement (user principal) (achievement-id uint) (achievement-name (string-ascii 50)) (timestamp uint))
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

;; Read-Only Functions

;; Get user profile information
(define-read-only (get-user-profile (user principal))
  (map-get? users { user: user })
)

;; Get user's daily metrics for a specific date
(define-read-only (get-daily-metrics (user principal) (date uint))
  (map-get? daily-metrics { user: user, date: date })
)

;; Get user's current wellness goals
(define-read-only (get-user-goals (user principal))
  (map-get? user-goals { user: user })
)

;; Get all achievements earned by a user
(define-read-only (get-user-achievements (user principal))
  (ok (map-get? user-achievements { user: user, achievement-id: u0 }))
)

;; Check if user has earned a specific achievement
(define-read-only (has-achievement (user principal) (achievement-id uint))
  (is-some (map-get? user-achievements { user: user, achievement-id: achievement-id }))
)

;; Get achievement definition
(define-read-only (get-achievement-definition (achievement-id uint))
  (map-get? achievement-definitions { achievement-id: achievement-id })
)

;; Public Functions

;; Register or update user goals
(define-public (set-wellness-goals 
    (sleep-hours-goal uint) 
    (water-ml-goal uint) 
    (meditation-minutes-goal uint))
  (let (
    (user tx-sender)
    (current-time (unwrap-panic (get-block-info? time u0)))
  )
    ;; Validate goal values
    (asserts! (validate-metric-value "sleep-hours" sleep-hours-goal) ERR-INVALID-VALUE)
    (asserts! (validate-metric-value "water-ml" water-ml-goal) ERR-INVALID-VALUE)
    (asserts! (validate-metric-value "meditation-minutes" meditation-minutes-goal) ERR-INVALID-VALUE)
    
    ;; Ensure user exists
    (ensure-user-exists user)
    
    ;; Set or update goals
    (map-set user-goals
      { user: user }
      {
        sleep-hours-goal: sleep-hours-goal,
        water-ml-goal: water-ml-goal,
        meditation-minutes-goal: meditation-minutes-goal,
        last-updated: current-time
      }
    )
    (ok true)
  )
)

;; Record daily wellness metrics
(define-public (record-daily-metrics (sleep-hours uint) (water-ml uint) (meditation-minutes uint))
  (let (
    (user tx-sender)
    (current-time (unwrap-panic (get-block-info? time u0)))
    (today-date (format-date-from-timestamp current-time))
    (existing-entry (map-get? daily-metrics { user: user, date: today-date }))
  )
    ;; Ensure user exists
    (ensure-user-exists user)
    
    ;; Validate metric values
    (asserts! (validate-metric-value "sleep-hours" sleep-hours) ERR-INVALID-VALUE)
    (asserts! (validate-metric-value "water-ml" water-ml) ERR-INVALID-VALUE)
    (asserts! (validate-metric-value "meditation-minutes" meditation-minutes) ERR-INVALID-VALUE)
    
    ;; Don't allow overwriting previous entries from the same day (optional)
    (asserts! (is-none existing-entry) ERR-CANNOT-OVERWRITE-PREVIOUS-ENTRY)
    
    ;; Record the metrics
    (map-set daily-metrics
      { user: user, date: today-date }
      {
        sleep-hours: sleep-hours,
        water-ml: water-ml,
        meditation-minutes: meditation-minutes,
        recorded-at: current-time
      }
    )
    
    ;; Update user streak
    (update-streak user)
    
    ;; Recalculate wellness score
    (calculate-wellness-score user)
    
    ;; Check and issue achievements
    (try! (check-and-issue-achievements user))
    
    (ok true)
  )
)

;; Update or add a single wellness metric
(define-public (update-single-metric (metric-type (string-ascii 20)) (value uint))
  (let (
    (user tx-sender)
    (current-time (unwrap-panic (get-block-info? time u0)))
    (today-date (format-date-from-timestamp current-time))
    (existing-entry (map-get? daily-metrics { user: user, date: today-date }))
  )
    ;; Ensure user exists
    (ensure-user-exists user)
    
    ;; Validate metric type
    (asserts! (or (string-eq? metric-type "sleep-hours") 
                (string-eq? metric-type "water-ml") 
                (string-eq? metric-type "meditation-minutes")) 
            ERR-INVALID-METRIC)
    
    ;; Validate metric value
    (asserts! (validate-metric-value metric-type value) ERR-INVALID-VALUE)
    
    ;; Update or create the entry
    (if (is-some existing-entry)
      (let (
        (current-metrics (unwrap-panic existing-entry))
      )
        (map-set daily-metrics
          { user: user, date: today-date }
          (merge current-metrics 
            (if (string-eq? metric-type "sleep-hours")
              { sleep-hours: value, recorded-at: current-time }
              (if (string-eq? metric-type "water-ml")
                { water-ml: value, recorded-at: current-time }
                { meditation-minutes: value, recorded-at: current-time }
              )
            )
          )
        )
      )
      ;; No existing entry, create with defaults
      (map-set daily-metrics
        { user: user, date: today-date }
        (merge {
            sleep-hours: u0,
            water-ml: u0,
            meditation-minutes: u0,
            recorded-at: current-time
          }
          (if (string-eq? metric-type "sleep-hours")
            { sleep-hours: value }
            (if (string-eq? metric-type "water-ml")
              { water-ml: value }
              { meditation-minutes: value }
            )
          )
        )
      )
    )
    
    ;; Update user streak
    (update-streak user)
    
    ;; Recalculate wellness score
    (calculate-wellness-score user)
    
    ;; Check for achievements
    (check-and-issue-achievements user)
    
    (ok true)
  )
)

;; Initialize achievement definitions - should be called by contract deployer
(define-public (initialize-achievements)
  (begin
    ;; Streak-based achievements
    (map-set achievement-definitions
      { achievement-id: u1 }
      { 
        name: "7-Day Streak",
        description: "Logged wellness metrics for 7 consecutive days",
        category: "streak",
        threshold: u7
      }
    )
    
    (map-set achievement-definitions
      { achievement-id: u2 }
      { 
        name: "30-Day Streak",
        description: "Logged wellness metrics for 30 consecutive days",
        category: "streak",
        threshold: u30
      }
    )
    
    (map-set achievement-definitions
      { achievement-id: u3 }
      { 
        name: "100-Day Streak",
        description: "Logged wellness metrics for 100 consecutive days",
        category: "streak",
        threshold: u100
      }
    )
    
    ;; Wellness score achievements
    (map-set achievement-definitions
      { achievement-id: u4 }
      { 
        name: "Wellness Beginner",
        description: "Reached a wellness score of 50",
        category: "score",
        threshold: u50
      }
    )
    
    (map-set achievement-definitions
      { achievement-id: u5 }
      { 
        name: "Wellness Enthusiast",
        description: "Reached a wellness score of 75",
        category: "score",
        threshold: u75
      }
    )
    
    (map-set achievement-definitions
      { achievement-id: u6 }
      { 
        name: "Wellness Master",
        description: "Reached a wellness score of 90",
        category: "score",
        threshold: u90
      }
    )
    
    (ok true)
  )
)