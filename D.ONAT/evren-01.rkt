;HI FOR FOREGIN DEVELOPERS! PLEASE CONTACT WITH ME...
;3 AUGUST 2K18
;DUHAN ONAT KARADAYI
;SATELITTE DUMP !
;U'RE RUNING AWAY FROM THE SATELLITE DUMP IN THE ATMOSPHERE.
;CREATIVE COMMONS LICANCE CC2
#lang racket
; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)
(require (only-in racket/gui/base play-sound))
(require test-engine/racket-tests)
(define go (place-image (text "D.ONAT KARADAYI" 20 "blue") 200 570 (place-image (rotate 270 (text "GAME OVER" 50 "blue")) 20 280 (rectangle 405 591 "solid" "black"))))
(define roketx 170)
(define rokety 400)
;======================================================================
;cisim
; imaj:resim
; yer:vc 
; hız:vc 
; ivme:vc
(define-struct cisim (imaj yer hiz ivme))
;vektör==================================================
;x-sayı
;y-sayı
(define-struct vc (x y))
;mag-vc vc-sayi
;vc vektorunun uzunlugunu hesaplamak
(check-expect (mag-vc (make-vc 0 1)) 1)
(define (mag-vc v) 
        (sqrt(+ (sqr(vc-x v)) (sqr(vc-y v)))))
;+vc vc vc -> vc
;iki vektörün toplamını hesaplar
(check-expect (+vc (make-vc 3 4) (make-vc 6 7))
              (make-vc 9 11))
(define (+vc m n)
  (make-vc
   (+ (vc-x m) (vc-x n))
   (+ (vc-y m) (vc-y n))))
;fark-vc vc vc -> vc
;iki vektörün farkını hesaplar
(check-expect (fvc (make-vc 3 4) (make-vc 6 7))
              (make-vc -3 -3))
(define (fvc m n)
  (make-vc
   (- (vc-x m) (vc-x n)) 
   (- (vc-y m) (vc-y n)))) 
;xdeis cisim--cisim
;cisim alan x ini random degistiren kod
(define (xdeis c) (make-cisim (cisim-imaj c) (make-vc (random 3 355) 0) (cisim-hiz c) (cisim-ivme c)))
;=================================================================================================================================
(define uzayliy 10)
(define benziny 10)
(define randombenzin (random 3 355))
(define randomuzayli (random 5 300))
;degdi-mi  cisim sayi--cisim 
;eski cismi alir kontrol eder ve y kooortinadini limiti aştıysa cismi dondurur
;===================================================================================================================
;(check-expect (degdi-mi (make-cisim (circle 50 "solid" "red") (make-vc 1 481) (make-vc 2 4) (make-vc 3 6)) 480)
;              (make-cisim (circle 50 "solid" "red") (make-vc 1 10) (make-vc 2 4) (make-vc 3 6)))=
;================================================================================================================
(define (degdi-mi c limit) 
  (cond 
    ((> (vc-y(cisim-yer c)) limit) (make-cisim (cisim-imaj c) (make-vc (random 3 355) 0) (cisim-hiz c) (cisim-ivme c)))
    (else c))) 
;=================================================================================================================================
(define (oyuncu-x-kontrol x t)(cond
                          [(or(string=? t "a") (string=? t "left")) ( - x 30)]
                          [(or(string=? t "d") (string=? t "right")) (+ x 30)]
                          [else x]))
(define (oyuncu-y-kontrol y t)(cond
                          [(or(string=? t "w") (string=? t "up")) ( - y 30)]
                          [(or(string=? t "s") (string=? t "down")) (+ y 30)]
                          [else y])) 
;oyuncu-yer-kontrol vc t---vc  
(define (oyuncu-yer-kontrol v t) (make-vc(oyuncu-x-kontrol (vc-x v) t)(oyuncu-y-kontrol (vc-y v) t)))
 

 
;keypress cisim tus---cisim
(define (keypress c t) (make-cisim (cisim-imaj c)(oyuncu-yer-kontrol (cisim-yer c)t)(cisim-hiz c) (cisim-ivme c)))    
 
;================================================================================================================
(define BACKGROUND (rotate 90 (scale 0.40 (bitmap "imaj/bagro.jpeg"))))  
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))
(define SKOR 0 )
(define skorx 200)  
(define skory 20)
(define ROKET (make-cisim (scale 0.18(bitmap "imaj/rocket.png")) (make-vc roketx rokety) (make-vc 0 0) (make-vc 0 0)))
(define BENZIN (make-cisim (scale 0.07(bitmap "imaj/jery.png")) (make-vc randombenzin 10) (make-vc 0 6) (make-vc 0 0)))
(define UZAYLI (make-cisim (scale 0.22(bitmap "imaj/s.png")) (make-vc randomuzayli 10) (make-vc 0 6) (make-vc 0 0)))
(define FRAME-RATE 40)
;=====================================================================================================
;dunya yapisi
(define-struct dunya(ROKET BENZIN UZAYLI SKOR))

  

(define obje1 (make-cisim (circle 50 "solid" "red") (make-vc 1 2) (make-vc 2 4) (make-vc 3 6)))
(define obje2 (make-cisim (star 50 "solid" "purple") (make-vc 4 8) (make-vc 5 10) (make-vc 6 12)))

;cisim-ilerleme cisim-> cisim
;fizik kurallarına göre cismin ilerlemesini sağlar
(check-expect
 (cisim-ilerleme obje2)              
 (make-cisim
  (star 50 "solid" "purple")
  (make-vc 9 18)
  (make-vc 11 22)
  (make-vc 6 12)))

(define
  (cisim-ilerleme b)
  (make-cisim
   (cisim-imaj b)
   (+vc (cisim-yer b) (cisim-hiz b))
   (+vc (cisim-hiz b) (cisim-ivme b))
   (cisim-ivme b)))
(define myworld
  (make-dunya
   ROKET
   BENZIN
   UZAYLI
   SKOR))
                 
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;===============================================================================================================================================
; ***** vector/cisim image fonksiyonları
; ***** fonksiyonları buradan kopyalanabilir

; place-image/vc resim vc scene -> scene
; bir sahneye vectora göre bir imaj yerleştir
(check-expect
 (place-image/vc (circle 10 "solid" "yellow") (make-vc 5 10) (square 30 "solid" "red"))
 (place-image/align (circle 10 "solid" "yellow") 5 10 "left" "top" (square 30 "solid" "red")))

(define (place-image/vc im v sahne)
  (place-image/align im (vc-x v) (vc-y v) "left" "top"  sahne))

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

 
; vektör toplam gösterisi
(place-line/vc origin (+vc v1 v2) "green"
               (place-line/vc origin v1 "pink"
                              (place-line/vc v1 (+vc v1 v2)  "yellow" (rectangle 300 225 "solid" "black"))))
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;===============================================================================================================================================

;dunya-ciz ---dunya----resim
;dunya alir cisimleri yapistirir
(check-expect (dunya-ciz myworld)(place-cisim (dunya-ROKET myworld)
                                              (place-cisim (dunya-BENZIN myworld)
                                                           (place-cisim (dunya-UZAYLI myworld)
                                                                        (place-image (text (string-append "SATELLITE DUMP by D.ONAT KARADAYI=" (number->string (dunya-SKOR myworld))) 15 "blue") skorx skory BACKGROUND )))))
(define (dunya-ciz w)(cond
                       [(< (dunya-SKOR w) 0) go ]
                       [else (place-cisim (dunya-ROKET w)
                                   (place-cisim (dunya-BENZIN w)
                                                (place-cisim (dunya-UZAYLI w)
                                                             (place-image  (text (string-append "SATELLITE DUMP by D.ONAT KARADAYI=" (number->string (dunya-SKOR w))) 15 "blue") skorx skory BACKGROUND ))))]))
;dunya-ilerleme---dunya alir dunya verir 
;cisimleri ilerletip yeni bir dunya olusturur.
(check-expect (dunya-ilerleme myworld)(make-dunya (cisim-ilerleme (dunya-ROKET myworld)) (cisim-ilerleme (dunya-BENZIN myworld)) (cisim-ilerleme (dunya-UZAYLI myworld)) (dunya-SKOR myworld)))
(define (dunya-ilerleme w)
  (make-dunya (degdi-mi (cisim-ilerleme (dunya-ROKET w)) (image-height BACKGROUND))
              (degdi-mi (cisim-ilerleme (dunya-BENZIN w)) (image-height BACKGROUND))
              (degdi-mi (cisim-ilerleme (dunya-UZAYLI w)) (image-height BACKGROUND))
              (dunya-SKOR w)))

;=================================================================================================================================
;cisim-mesafe cisim-cisim--sayi
(define (cisim-mesafe c1 c2) (mag-vc (fvc (cisim-yer c1) (cisim-yer c2))))
  
;(define moad
;cisim-carpistimi cisim-cisim--mantiksal
;iki cisim arasindaki mesafe yaricaplarinin toplamindan az mi?
(define (cisim-carpistimi a b) 
    (< (cisim-mesafe a b) ( / (image-height (cisim-imaj ROKET)) 3)))


;dunya-carpisma-1 dunya verir. 
;roket ile uzaylinin carpisip carpismadigina karar verir.
(define (dunya-carpisma-1 w) (cond
                             [(cisim-carpistimi (dunya-ROKET w)  (dunya-UZAYLI w)) (begin (play-sound "ses/car.wav" true))(make-dunya
                                                                                    (dunya-ROKET w)
                                                                                    (dunya-BENZIN w)
                                                                                    (xdeis (dunya-UZAYLI w))
                                                                                    (- (dunya-SKOR w) 35))]
                             [else w]))

(define (dunya-carpisma-2 w) (cond
                             [(cisim-carpistimi (dunya-ROKET w)  (dunya-BENZIN w)) (begin (play-sound "ses/drip.wav" true)) (make-dunya
                                                                                    (dunya-ROKET w)
                                                                                    (xdeis (dunya-BENZIN w))
                                                                                    (dunya-UZAYLI w)
                                                                                    (+ (dunya-SKOR w) 20 ))]
                             [else w]))
      
;104 roket yaricap
;54 UZAYLI yaricap
;dunya-yeni butun nesnelerin carpismalarini kontrol eder. yeni dunya verir.
(define (dunya-yeni w)(cond
               [(<(dunya-SKOR w) 0) w]
               [else (dunya-carpisma-2 (dunya-carpisma-1 (dunya-ilerleme w)))]))

;dunya-tus dunya alir tus alir dunya dondururs 
(define (dunya-tus w t) (make-dunya
                        (keypress (dunya-ROKET w) t) 
                        (dunya-BENZIN w)
                        (dunya-UZAYLI w)
                        (dunya-SKOR w)))

(define (dunya-fare w x y m) w)

(define yaradilis (make-dunya ROKET BENZIN UZAYLI SKOR))

(big-bang yaradilis
  (on-tick dunya-yeni (/ 1.0 FRAME-RATE))
  (on-draw dunya-ciz)
  (on-key dunya-tus)
  (on-mouse dunya-fare))

