;Creative Common lisansı cc2 
;Canay Akın
;04.08.2018
;canayakin2004@gmail.com
;Canay Akın productions gururla sunar... 
#lang racket
; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)
(require (only-in racket/gui/base play-sound))
(require test-engine/racket-tests)


; ***** vector/cisim image fonksiyonları
; ***** fonksiyonları buradan kopyalanabilir
(define GAME-OVER (overlay (text "GAME OVER!" 100 "white") (rectangle 800 600 "solid" "black")))
(define EKRAN-GENİŞLİĞİ 800)
(define EKRAN-YÜKSEKLİĞİ 600)
(define BACKGROUND  (bitmap "imaj/orman.jpg"))
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))
(define-struct vc (x y))
(define-struct cisim (imaj yer hız ivme))
(define (place-image/vc im v sahne)
  (place-image/align im (vc-x v) (vc-y v) "left" "top"  sahne))
(define HİGH-SCORE 0)
(define TEHLİKE-RESİM (scale 0.18 (bitmap "imaj/meowth.png")))
(define HEDEF-RESİM (scale 0.18 (bitmap "imaj/pikachu.png")))
(define KARAKTER-RESİM (scale 0.25 (bitmap "imaj/ash.png")))
(define TEHLİKE (make-cisim TEHLİKE-RESİM (make-vc (random EKRAN-GENİŞLİĞİ) 0) (make-vc 0 10) (make-vc 0 0)))
(define HEDEF (make-cisim HEDEF-RESİM (make-vc (random EKRAN-GENİŞLİĞİ) 0) (make-vc 0 8) (make-vc 0 0)))
(define KARAKTER (make-cisim KARAKTER-RESİM (make-vc 400 500) (make-vc 0 0) (make-vc 0 0)))
(define SKOR 0)
; place-line/vc vc vc color scene -> scene
; add line from v1 to v2 to scene
(check-expect
 (place-line/vc (make-vc 20 30) (make-vc 30 40) "black" (rectangle 200 100 "solid" "white"))
 (add-line (rectangle 200 100 "solid" "white") 20 30 30 40 "black"))

(define (place-line/vc v1 v2 renk sahne)
  (add-line sahne (vc-x v1) (vc-y v1) (vc-x v2) (vc-y v2) renk ))

; place-cisim cisim scene -> scene
; bir cisim kendi yer kordinatlarına göre sahneye yerleştir
(check-expect
 (place-cisim (make-cisim (circle 10 "solid" "yellow") (make-vc 10 10) (make-vc 99 99) (make-vc 99 99)) (square 100 "solid" "black"))
 (place-image/vc (circle 10 "solid" "yellow") (make-vc 10 10)  (square 100 "solid" "black")))

(define (place-cisim c sahne)
  (place-image/vc (cisim-imaj c) (cisim-yer c) sahne))

; vektör testleri
(define v1 (make-vc 150 150))
(define v2 (make-vc 120 30))
(define origin (make-vc 0 0))
;(tvc v1 v2)->vektör
;verilen vektörlerin toplamını çizer
(check-expect
 (+vc (make-vc 1 2) (make-vc 2 5) )
 (make-vc (+ 1 2) (+ 2 5)) )
(check-expect
 (+vc (make-vc 2 3) (make-vc 5 6) )
 (make-vc (+ 2 5) (+ 3 6))) 
(define (+vc vec1 vec2 )
  (make-vc (+ (vc-x vec1) (vc-x vec2)) (+ (vc-y vec1) (vc-y vec2)))
  )
;(evc v1 v2)->vektör
;verilen vektörlerin toplamını çizer
(check-expect
 (evc (make-vc 1 2) (make-vc 2 5) )
 (make-vc (- 1 2) (- 2 5)) )
(check-expect
 (evc (make-vc 2 3) (make-vc 5 6) )
 (make-vc (- 2 5) (- 3 6))) 
(define (evc vec1 vec2 )
  (make-vc (- (vc-x vec1) (vc-x vec2)) (- (vc-y vec1) (vc-y vec2)))
  )
;(lvc v1 )->sayı2
;verilen vektörün usunluğunu verir
(check-expect
 (lvc (make-vc 6 8)) (sqrt (+ (sqr 6) (sqr 8))))
(check-expect
 (lvc (make-vc 3 4)) (sqrt (+ (sqr 3) (sqr 4))))
(define
  (lvc vec) (sqrt (+ (sqr (vc-x vec)) (sqr (vc-y vec)))))

;(mvc vec sayı)->vec
;verilen vektörü verilen sayıyla çarpar
(check-expect (mvc (make-vc 12 3) 2)
              (make-vc (* 12 2) (* 3 2)))
(check-expect (mvc (make-vc 1 2) 2)
              (make-vc (* 1 2) (* 2 2)))
(define (mvc vec x)
  (make-vc (* x (vc-x vec)) (* x (vc-y vec))))

; place-image/vc resim vc scene -> scene
; bir sahneye vectora göre bir imaj yerleştir 
(check-expect
 (place-image/vc (circle 10 "solid" "yellow") (make-vc 5 10) (square 30 "solid" "red"))
 (place-image/align (circle 10 "solid" "yellow") 5 10 "left" "top" (square 30 "solid" "red")))

; vektör toplam gösterisi
(place-line/vc origin (+vc v1 v2) "green"
               (place-line/vc origin v1 "pink"
                              (place-line/vc v1 (+vc v1 v2)  "yellow" (rectangle 300 225 "solid" "black"))))
;Cisim
;imaj:resim
;yer:vc
;hız:vc
;ivme:vc

(define obje (make-cisim (circle 50 "solid" "lightgreen") (make-vc 0 0) (make-vc 10 10) (make-vc 0 1)))
;(cisim-ilerle cisim)->cisim
;cisimin 1 saniyedeki ilerlemesi
(define (cisim-ilerle
         cismim)
  (make-cisim (cisim-imaj cismim)
              (+vc (cisim-yer cismim) (cisim-hız cismim))
              (+vc (cisim-hız cismim) (cisim-ivme cismim))
              (cisim-ivme cismim)))
;hedef- cisim
;tehlike- cisim
;karakter- cisim
;;skor- sayı
(define-struct dünya ( hedef tehlike karakter skor high-score))
;(skor-kontrol s)->sayı
(define (skor-kontrol s t h k ) (cond
                           ((çarpıştılar-mı t k) -99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)
                           ((çarpıştılar-mı h k) 25)
                           (else 0)
                           ))
;(skor-güncelle sayı)->sayı
(define (skor-güncelle s t h k) (+ (skor-kontrol s t h k) s))

;(dünya-çiz dünya)->sahne
;cisimleri fona koyar ve sahneyi oluşturur
(define (dünya-çiz w) (cond
                        ((< (dünya-skor w) 0) (place-image (text (string-append "High Score: " (number->string (dünya-high-score w))) 30 "navy") 400 400 GAME-OVER))
                        (else (place-cisim (dünya-karakter w) (place-cisim (dünya-tehlike w) (place-cisim (dünya-hedef w)
                                                                                               (place-image 
                                                                                                (text (string-append "CANAY AKIN                 İNDİM DEREYE, ASH BULAMADIM.                        skor:" (number->string (dünya-skor w))) 20 "white") 380 20
                                                                                                            BACKGROUND-SCENE)) )))
                        ))

(define yaradılış (make-dünya HEDEF TEHLİKE KARAKTER SKOR HİGH-SCORE))

;(tuş-hareket vc t)->vc
;basılan tuşa göre vektörünü verir
(define (tuş-hareket v t) (cond
                            ((string=? t "right") (make-vc 10 0))
                            ((string=? t "left") (make-vc -10 0))
                            (else v)))

(define (dünya-tuş w t) (make-dünya
                         (dünya-hedef w)
                         (dünya-tehlike w)
                         (make-cisim
                          (cisim-imaj (dünya-karakter w))
                          (cisim-yer (dünya-karakter w))
                          (tuş-hareket (cisim-hız (dünya-karakter w)) t)
                          (cisim-ivme (dünya-karakter w))
                          )
                         (cond
                           ((and (string=? t " ") (< (dünya-skor w) 0)) 0)
                           (else (dünya-skor w))
                           ) (dünya-high-score w)) )
(define (dünya-fare w x y m) w)
;(düştü-mü c)->mantıksal
(define (düştü-mü c) (>= (vc-y (cisim-yer c)) EKRAN-YÜKSEKLİĞİ))
;(th-kontrol c)->c
(define (h-kontrol c1 c2 w)
  (make-cisim
   (cisim-imaj c2)
   (cond
     ((düştü-mü c2) (make-vc (random 750) (- (vc-y (cisim-yer c2)) 801)) )
     ((çarpıştılar-mı c1 c2) (begin (play-sound "abc.wav" true) true) (make-vc (random 750) (- (vc-y (cisim-yer c2)) 800)) )
     (else (+vc (cisim-hız c2) (cisim-yer c2)))
     )
   (cond
     ((and (<= 100 (dünya-skor w)) (> 200 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 7 (sin (/ (vc-y (cisim-yer c2)) 7))) 14)))
     ((and (<= 200 (dünya-skor w)) (> 300 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 14 (sin (/ (vc-y (cisim-yer c2)) 14))) 18)))
     ((and (<= 300 (dünya-skor w)) (> 400 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 21 (sin (/ (vc-y (cisim-yer c2)) 21))) 22)))
     ((and (<= 400 (dünya-skor w)) (> 500 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 28 (sin (/ (vc-y (cisim-yer c2)) 28))) 26)))
     ((<= 500 (dünya-skor w)) (make-vc (* 35 (sin (/ (vc-y (cisim-yer c2)) 35))) 30))
     (else (make-vc 0 10))  
     )
   (cisim-ivme c2)
   ))
(define (t-kontrol c1 c2 w)
  (make-cisim
   (scale (cond
            ((and (<= 100 (dünya-skor w)) (> 300 (dünya-skor w))) 1.2)
            ((and (<= 300 (dünya-skor w)) (> 500 (dünya-skor w))) 1.4)
            ((<= 500 (dünya-skor w)) 1.6)
            (else 1)
            ) TEHLİKE-RESİM)
   (cond
     ((or (çarpıştılar-mı c1 c2) (düştü-mü c2)) (make-vc (random 770) (- (vc-y (cisim-yer c2)) 800)) )
     (else (+vc (cisim-hız c2) (cisim-yer c2)))
     )
   (cond
     ((and (<= 100 (dünya-skor w)) (> 200 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 10 (sin (/ (vc-y (cisim-yer c2)) 10))) 16)))
     ((and (<= 200 (dünya-skor w)) (> 300 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 20 (sin (/ (vc-y (cisim-yer c2)) 20))) 20)))
     ((and (<= 300 (dünya-skor w)) (> 400 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 30 (sin (/ (vc-y (cisim-yer c2)) 30))) 24)))
     ((and (<= 400 (dünya-skor w)) (> 500 (dünya-skor w))) (+vc (cisim-ivme c2) (make-vc (* 40 (sin (/ (vc-y (cisim-yer c2)) 40))) 28)))
     ((<= 500 (dünya-skor w)) (make-vc (* 50 (sin (/ (vc-y (cisim-yer c2)) 50))) 32))
     (else (make-vc 0 12)) 
     )
   (cisim-ivme c2)
   ))
;(mesafe c1 c2)-> iki cisim arasındaki mesafeyi verir
(define (mesafe c1 c2)
  (sqrt (+ (sqr (- (vc-x (cisim-yer c1)) (vc-x (cisim-yer c2)))) (sqr (- (vc-y (cisim-yer c1)) (vc-y (cisim-yer c2)))))))
;(çarpıştılar-mı c1 c2)->mantıksal
(define (çarpıştılar-mı c1 c2) (<= (mesafe c1 c2) 60))
;(sağa-çarptı-mı cisim)->mantıksal
(define (sağa-çarptı-mı c) (>= (vc-x (cisim-yer c)) (- EKRAN-GENİŞLİĞİ (image-width (cisim-imaj c))) ))
;(sola-çarptı-mı cisim)->mantıksal
(define (sola-çarptı-mı c) (<= (vc-x (cisim-yer c)) 0))


;(cisim-hareket-kontrol c)->c
(define (cisim-hareket-kontrol c) (cond
                                    ((sağa-çarptı-mı c) (make-cisim (cisim-imaj c) (make-vc (- (vc-x (cisim-yer c)) 5 ) (vc-y (cisim-yer c)))
                                                                    (make-vc 0 0)
                                                                    (cisim-ivme c)))
                                    ((sola-çarptı-mı c) (make-cisim (cisim-imaj c) (make-vc (+ (vc-x (cisim-yer c)) 5 ) (vc-y (cisim-yer c)))
                                                                    (make-vc 0 0)
                                                                    (cisim-ivme c)) )
                                    (else (cisim-ilerle c))))
;high-score güncelle hs -> hs
(define (high-score-güncelle hs s) (cond
                                     ((< hs s) s)
                                     (else hs)
                                     ))

;(dünya-ilerle dünya)-> dünya
;dünyanın ilerlemiş halini verir
(define (dünya-ilerle w)
  (make-dünya
   (h-kontrol (dünya-karakter w) (dünya-hedef w) w)
   (t-kontrol (dünya-karakter w) (dünya-tehlike w) w)
   (cisim-hareket-kontrol (dünya-karakter w))
   (skor-güncelle (dünya-skor w) (dünya-tehlike w) (dünya-hedef w) (dünya-karakter w))
   (high-score-güncelle (dünya-high-score w) (dünya-skor w))))



(big-bang yaradılış
  (on-tick dünya-ilerle (/ 1 30))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare))
