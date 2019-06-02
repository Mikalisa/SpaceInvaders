(require 2htdp/universe)
(require 2htdp/image)



;; Space Invaders


;; Constants:

(define WIDTH  400)
(define HEIGHT 600)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 4)
(define MISSILE-SPEED 12)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (overlay (rectangle WIDTH HEIGHT "solid" "black")
                            (empty-scene WIDTH HEIGHT)))



(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))


(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "green")       ;gun
                     (rectangle 20 10 "solid" "green"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define TANK-HEAD (-  HEIGHT (image-height TANK)))

(define MISSILE (ellipse 2 15 "solid" "red"))





;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; - (cons invader ListOfInvader)
;; interp. a list of invaders.

(define loi empty)
(define loi1 (cons (make-invader 150 100 12) empty))
(define loi2 (cons (make-invader 100 200 -20 ) (cons (make-invader 150 HEIGHT -10) empty)))

#;
(define (fn-for-los loi)
  (cond [(empty? loi) (...)]                              ;BASE CASE
        [else (fn-for-invader (first loi))                 ;invader
              (fn-for-loi (rest loi))]))                 ;ListOfInvader

;; Template rules used:
;; - one of : 2 cases.
;; - atomic distinct: empty.
;; - compound: (cons invader ListOfInvader).
;; - self-reference : ListOfInvader is (rest loi).
;; - reference: invader is (first loi)



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define x (game-invaders G3))

;; ListOfMissles is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of missiles.

(define lom empty)
(define lom1 (cons (make-missile 150 300) empty))
(define lom2 (cons (make-missile (invader-x I1) (+ (invader-y I1) 10 )) (cons (make-missile (invader-x I1 ) (+ (invader-y I1) 5)) empty)))


#;
(define (fn-for-los lom)
  (cond [(empty? lom) (...)]                              ;BASE CASE
        [else (fn-for-missile (first lom))                ;Missile
              (fn-for-lom (rest lom))]))                  ;ListOfMissile


;; Template rules used:
;; - one of : 2 cases.
;; - atomic distinct: empty.
;; - compound: (cons Missile ListOfMissile).
;; - self-reference : ListOfMissile is (rest lom).
;; - reference: Missile is (first loi)


;; =================
;; Functions:

;; game -> game
;; start the world with (main (make-game (cons (make-invader 1 1 2) empty) empty T0)).
;; 
(define (main g)
  (big-bang g                           ; game
            (on-tick   start-game)      ; game -> game
            (to-draw   render-game)     ; game -> Image
            (stop-when game-over)       ; game -> Boolean
            (on-key    handle-key)))    ; game KeyEvent -> game

;; game -> game
;; moving List of invaders, list of missile, and the tank on BACKGROUND
;; !!!

;(define I1 (make-invader 150 100 12))           ;not landed, moving right
;(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
;(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


(check-expect (start-game G0) (make-game empty empty T0))

(check-expect (start-game G2) (make-game (list (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12)) (list (make-missile 150 (- 300 MISSILE-SPEED))) (make-tank 50 1 )))

(check-expect (start-game (make-game (list (make-invader WIDTH 100 12)) (list (make-missile 200 10)) (make-tank WIDTH 1)))
                          (make-game (list (make-invader (- WIDTH INVADER-X-SPEED)  (+ 100 INVADER-Y-SPEED) -12)) (list (make-missile  200 (- 10 MISSILE-SPEED))) (make-tank WIDTH 1)))

(check-expect (start-game (make-game (list (make-invader 0 100 -12) (make-invader 0 100 -12)) (list (make-missile 200 10) (make-missile 200 10)) (make-tank (+ 10 WIDTH) 1)))
                          (make-game (list (make-invader (+ 0 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12) (make-invader (+ 0 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12))
                                     (list (make-missile 200 (- 10 MISSILE-SPEED)) (make-missile 200 (- 10 MISSILE-SPEED))) (make-tank WIDTH -1)))

(check-expect (start-game (make-game (list (make-invader 100 90 12) (make-invader 100 0 12)) (list (make-missile 200 10) (make-missile 200 10)) (make-tank (- 0 10) 1)))
                          (make-game (list (make-invader (+ 100 12) (+ 90 INVADER-Y-SPEED) 12) (make-invader (+ 100 12) (+ 0 INVADER-Y-SPEED) 12))
                                     (list (make-missile 200  (- 10 MISSILE-SPEED)) (make-missile 200 (- 10 MISSILE-SPEED))) (make-tank  0 1)))

(define T (random INVADE-RATE))


(define (start-game g)
  (make-game (distroy-inv
              (game-missiles g) (add-invaders
                                 (random INVADE-RATE)
                                 (move-invaders (game-invaders g))))
             
              (move-missile (game-missiles g))
              (move-tank (game-tank g))))

;(define (start-game g) g) ;the stub


;; INVADER -> INVADER.
;; move the invaders at a 45 degree angle and along the top of the screen.
;; !!!

(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (cons (make-invader 100 WIDTH 12) empty)) (cons (make-invader (+ 100 12) (+ WIDTH INVADER-Y-SPEED) 12) empty))
(check-expect (move-invaders (cons  (make-invader 100 0 12) (cons (make-invader 100 0 12) empty)))
              (cons (make-invader (+ 100 12) (+ INVADER-Y-SPEED 0) 12) (cons (make-invader (+ 100 12) (+ INVADER-Y-SPEED 0) 12) empty)))

;(define (move-invaders n) n) ;the stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]                              ;BASE CASE
        [else
         (cons (invader-start (first loi))                 ;invader
               (move-invaders (rest loi)))])) 

;; invader -> invader
;; move the invaders at a 45 degree angle and along the top of the screen.
;; !!!

(check-expect (invader-start (make-invader (- WIDTH 12) 100 12))
              (make-invader WIDTH 101.5 12))

(check-expect (invader-start (make-invader 12 100 -12))
              (make-invader 0 (+ 100 INVADER-Y-SPEED) -12))

(check-expect (invader-start (make-invader 290 100 -12)) 
              (make-invader (+ 290 -12)  101.5 -12))

(check-expect (invader-start (make-invader 11 100 -12))
              (make-invader 12.5  (+ 100 INVADER-Y-SPEED) 12))



(check-expect (invader-start (make-invader 100 100 -12))
              (make-invader (+ 100 -12) (+ 100 INVADER-Y-SPEED) -12))

;(define (invader-start v ) v) ;the stub

#;;<template took from invader>
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define (invader-start invader)
  (cond [(> (+ (invader-x invader) (invader-dx invader)) WIDTH) (make-invader  (- (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader)))]
        [(< (+ (invader-x invader) (invader-dx invader)) 0) (make-invader (+ (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED)  (- (invader-dx invader)))]
        
        [else
         (make-invader (+ (invader-x invader) (invader-dx invader))
                       (+ (invader-y invader) INVADER-Y-SPEED)
                       (invader-dx invader))]))
 

;; Natural[1, 100] ListOfInvader -> ListOfInvader
;; produce a random number of INVADERS on BACKGROUND.
;; !!!

(check-random (add-invaders (random INVADE-RATE) (list (make-invader 100 90 12) (make-invader 100 0 12)))
              (list (make-invader 100 90 12) (make-invader 100 0 12)))

(define (add-invaders r lon)
  (cond [(< r 3)
         (cons (make-invader (random WIDTH) 0 3) lon)]
        [else
         lon]))
      

;; MISSILE -> MISSILE.
;; fire the missiles from the tank at a constant speed.
;; !!!

(check-expect (move-missile empty) empty)
(check-expect (move-missile (cons (make-missile 200 10) empty)) (cons (make-missile 200 (- 10 MISSILE-SPEED)) empty))
(check-expect (move-missile (cons (make-missile 200 10) (cons (make-missile 200 10) empty))) (cons (make-missile 200 (- 10 MISSILE-SPEED)) (cons (make-missile 200 (- 10 MISSILE-SPEED)) empty)))

;(define (move-missile m) m) ;the stub

#;
(define (fn-for-los lom)
  (cond [(empty? lom) (...)]                              ;BASE CASE
        [else (fn-for-missile (first lom))                ;Missile
              (fn-for-lom (rest lom))]))                  ;ListOfMissile


(define (move-missile lom)
  (cond [(empty? lom) empty]                              ;BASE CASE
        [else
         (cons (fire-missile (first lom))                 ;Missile
               (move-missile (rest lom)))]))              ;ListOfMissile


;; MISSILE -> MISSILE.
;; fire the missiles from the tank.


(check-expect (fire-missile (make-missile 200 10)) (make-missile 200  (- 10 MISSILE-SPEED)))

;(define (fire-missile m) m)

#;;<template>
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define (fire-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))





;; TANK -> TANK.
;; making the tank move left and right at a constant speed.
;; !!!
(check-expect (move-tank (make-tank  200 1)) (make-tank  200 1))

(define (move-tank t)
  (cond [(< (tank-x t) 0) (make-tank 0 1)]
        [(> (tank-x t) WIDTH) (make-tank WIDTH -1)]
        [else
         t]
        )
  )


;; ListOfMissile ListOfInvader -> ListOfInvader.
;; remove the invader if the missile hit it.
;; !!!

(check-expect (distroy-inv empty empty) empty)
(check-expect (distroy-inv empty (cons (make-invader 200 20 12) empty)) (cons (make-invader 200 20 12) empty))
(check-expect (distroy-inv (cons (make-missile 200 30) empty) empty) empty)
(check-expect (distroy-inv (cons (make-missile 200 30) empty) (cons (make-invader 200 20 12) empty)) (cons (make-invader 200 20 12) empty))

;(define (distroy-inv lom lonv) lonv)

(define (distroy-inv lom loi)
  (cond[(empty? loi) empty]
       [(empty? lom) loi]
       [else
        (if (invader-find (first loi) lom)
            (rest loi)
            (cons (first loi) (distroy-inv lom (rest loi))))]))

;; Invader ListOfMissile -> Boolean
;; if the invaders x and y position is found within the hitbox of the missile, return true

(check-expect (invader-find (make-invader 200 20 12) empty) false)
(check-expect (invader-find (make-invader 200 20 12) (cons (make-missile 210 30) empty)) false)


;(define (invader-find i lom) false)

(define (invader-find i lom)
  (cond[(empty? lom) false]
       [else
        (if (and (< (- (invader-x i) HIT-RANGE) (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                 (< (- (invader-y i) HIT-RANGE) (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)))
            true
            (invader-find i (rest lom)))]))



;; game -> Image
;; render the images of invaders, missiles, tank on the apropriate place on BACKGROUND


(check-expect (render-game (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 (- HEIGHT TANK-HEIGHT/2))))
              (place-image INVADER 100 WIDTH (place-image MISSILE 200 10 (place-image TANK 200 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
              
;(define (render-game g) BACKGROUND)

;(define K (rest(rest (cons (make-invader 100 WIDTH 12) (cons (make-missile 200 10) (cons (make-tank 200 (- HEIGHT TANK-HEIGHT/2)) empty))))))

(define (render-game g)
  (render-invader (game-invaders g)
                  (render-missile (game-missiles g)
                                  (render-tank (game-tank g)))))


;; ListOfInvader ListOfMissile -> Image
;; render list of invaders.


(check-expect (render-invader empty (render-missile empty (render-tank T0))) (render-missile empty (render-tank T0)))

(check-expect (render-invader (cons (make-invader 100 WIDTH 12) empty) (render-missile empty (render-tank T0)))
              (place-image INVADER 100 WIDTH (render-missile empty (render-tank T0))))

(check-expect (render-invader (cons (make-invader 100 WIDTH 12) (cons (make-invader 200 100 12) empty)) (render-missile empty (render-tank T0)))
              (place-image INVADER 100 WIDTH (place-image INVADER 200 100 (render-missile empty (render-tank T0)))))

;(define (render-invader nv ) nv)



(define (render-invader ls img)
  (cond[(empty? ls) img]
       [else
       (place-image
                INVADER
                (invader-x (first ls))
                (invader-y (first ls))
                (render-invader (rest ls) img))]))


;; ListOfMissile TANK -> Image.
;; render list of missiles.

(check-expect (render-missile empty (render-tank T0)) (render-tank T0))
(check-expect (render-missile (cons (make-missile 200 10) empty) (render-tank T0)) (place-image MISSILE 200 10 (render-tank T0)))

;(define (render-missile m) m)

(define (render-missile lm img)
  (cond[(empty? lm) img]
       [else
        (place-image MISSILE
              (missile-x (first lm))
              (missile-y (first lm))
             (render-missile (rest lm) img))]))


;; TANK -> Image.
;; render tank at the bottom of the BACKGROUND.



(check-expect (render-tank (make-tank 200 (- HEIGHT TANK-HEIGHT/2)))
              (place-image TANK 200 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;(define (render-tank t) t)

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))




;; game -> Boolean
;; stop the game when the invaders reaches the bottom of the screen.
;; !!!

(check-expect (game-over (make-game (cons (make-invader 100 50 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 (- HEIGHT TANK-HEIGHT/2))))
                         false)
(check-expect (game-over (make-game (cons (make-invader 100 HEIGHT 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 (- HEIGHT TANK-HEIGHT/2))))
                         true)
(check-expect (game-over (make-game (cons (make-invader 100 50 12) (cons (make-invader 100 HEIGHT 12) empty)) (cons (make-missile 200 10) empty) (make-tank 200 (- HEIGHT TANK-HEIGHT/2))))
                         true)


(define (game-over g)
  (lst-invaders (game-invaders g)))

;; ListOfInvader -> Boolean
;; produce true if invaders reache the bottom, otherwise false.
;; !!!

(check-expect (lst-invaders empty) false)
(check-expect (lst-invaders (cons (make-invader 100 50 12) empty)) false)
(check-expect (lst-invaders (cons (make-invader 100 50 12) (cons (make-invader 100 HEIGHT 12) empty))) true)
(check-expect (lst-invaders (cons (make-invader 100 50 12) (cons (make-invader 100 20 12) empty))) false)

;(define (lst-invaders loi) true) ;the stub

#;;<template took from ListOfInvader>
(define (fn-for-los loi)
  (cond [(empty? loi) (...)]                              ;BASE CASE
        [else
         (... (fn-for-invader (first loi))                 ;invader
              (fn-for-loi (rest loi)))]))                 ;ListOfInvader


(define (lst-invaders loi)
  (cond [(empty? loi) false]                              ;BASE CASE
        [else
         (if (check-invader (first loi))
             true
             (lst-invaders (rest loi)))]))                 ;ListOfInvader


;; Invader -> Boolean.
;; produce true if invader reaches the bottom, otherwise false.
;; !!!
(check-expect (check-invader (make-invader 100 50 12)) false)
(check-expect (check-invader (make-invader 100 HEIGHT 12)) true)

;(define (check-invader i) true) ;the stub

(define (check-invader i)
  (if (= (invader-y i) HEIGHT)
      true
      false))



;; game KeyEvent -> game
;; move the TANK left and RIGHT at a constant speed , and fire when click space buttom.


(check-expect (handle-key (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 1)) "left")
                          (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank (- 200 TANK-SPEED) 1)))

(check-expect (handle-key (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 1)) "right")
                          (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank (+ 200 TANK-SPEED) 1)))

(check-expect (handle-key (make-game (cons (make-invader 100 WIDTH 12) empty) empty (make-tank 200 1)) " ")
                          (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 TANK-HEAD) empty) (make-tank 200 1)))

(check-expect (handle-key (make-game (cons (make-invader 100 WIDTH 12) empty) empty (make-tank 200 1)) "up")
                          (make-game (cons (make-invader 100 WIDTH 12) empty) empty (make-tank 200 1)))

(check-expect (handle-key (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 1)) "down")
                          (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 1)))


(define (handle-key g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g) (game-missiles g)  (make-tank (- (tank-x (game-tank g)) TANK-SPEED) (tank-dir (game-tank g))))]
        [(key=? ke "right")
         (make-game (game-invaders g) (game-missiles g) (make-tank (+ (tank-x (game-tank g)) TANK-SPEED) (tank-dir (game-tank g))))]
        [(key=? ke " ")
         (make-game (game-invaders g) (fire g (game-missiles g))  (game-tank g))]
        [else
         g]
       ))



;; game ListOfMissile -> ListOfMissile.
;; fire the missile from the tank on y-coordinates, on BACKGROUND.


(check-expect (fire (make-game (cons (make-invader 100 WIDTH 12) empty) (cons (make-missile 200 10) empty) (make-tank 200 1)) empty)
              (cons (make-missile 200 TANK-HEAD) empty))


(define (fire g lsm)
  (cons (make-missile (tank-x (game-tank g)) TANK-HEAD) lsm))

