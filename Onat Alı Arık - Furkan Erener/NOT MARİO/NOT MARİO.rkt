;;;;ONAT ALİ ARIK TARAFINDAN YAPILDI;;;;
;;;;FURKAN ERENER'E ÖZEL TEŞEKKÜRLER;;;;
;;;;04.08.2018;;;;
;;;; ÇokOnat Studios Gururla Sunar ;;;;
;;; Telif Hakları Onat Ali Arık Tarafından Alınmıştır. ;;;



#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require (only-in racket/gui/base play-sound))
(require test-engine/racket-tests)
(begin (play-sound "./theme.wav" true) true)      


;hedef , tehlike , oyuncu boyutunu tanımlar.
(define TEHLİKE-GENİŞLİK 50)
(define TEHLİKE-YÜKSEKLİK 50)

(define HEDEF-GENİŞLİK 20)
(define HEDEF-YÜKSEKLİK 20)

(define OYUNCU-GENİŞLİK 100)
(define OYUNCU-YÜKSEKLİK 100)

(define EKRAN-GENİŞLİK 400)
(define EKRAN-YÜKSEKLİK 300)

(define OYUNCU-HIZ 70)
;vc
;x sayı x komponenti
;y sayı y kompenenti

(define-struct vc (x y))


;+vc: vc vc --> vc
;iki vektörün toplamını hesaplar.
(check-expect
 (+vc (make-vc 7 5) (make-vc 3 4))
 (make-vc 10 9))

(define
  (+vc v1 v2)
       (make-vc
       (+ (vc-x v1) (vc-x v2))
       (+ (vc-y v1) (vc-y v2))))

;-vc : vc vc ---> vc
;iki vektörün farkını hesaplar.
(check-expect
 (-vc (make-vc 3 5)  (make-vc 3 9))
      (make-vc 0 -4))

 (define
   (-vc -v1 -v2)
            (make-vc
             (- (vc-x -v1) (vc-x -v2))
             (- (vc-y -v1) (vc-y -v2))))


;cisim :obje
;imaj :resim
;yer :vc 
;hız :vc 
;ivme :vc 
(define-struct cisim
  (resim yer hız ivme))

(define obje (make-cisim
              (circle 40 "solid" "gold") (make-vc 1 1) (make-vc 2 3) (make-vc -1 -10)))

(define obje2 (make-cisim
              (circle 40 "solid" "gold") (make-vc 2 2) (make-vc 15 5) (make-vc -1 -10)))
;cisim-çarptı?
;cisim --> mantıksal
(define (cisim-çarptı-mı? c) (vc-x (cisim-yer c)))
(define (yön-değiştir c) (make-cisim (cisim-resim c) (cisim-yer c) (make-vc (vc-x (cisim-hız c)) (* -1 (vc-y cisim-hız c))) (cisim-ivme c)))



;(define (cisim-çarptı? c)
;(and (< 0 (vc-y (cisim-ivme c)))
;  (<= (image-height BACKGROUND)
 ;                                (vc-y (cisim-yer c)))))


;yön-değiştir
;cismin yönünü değiştirir.
;(define (yön-değiştir c) ((make-cisim (cisim-resim c)
                                    ; (cisim-yer c) (make-vc (vc-x (cisim-hız c)) (* (vc-y (cisim-hız c)) -1))) (cisim-ivme c)))

;cisim-hareket
;cisim --> cisim
;cisim çarptıysa yönünü değiştirir.
;çarpmadıysa cisim-ilerleme fonksiyonunu çağırır.

;(define (cisim-hareket c) (cond
                           ; [(cisim-çarptı? c) (yön-değiştir c)]
                            ;[else (cisim-ilerleme c)]))

;cisim-ilerleme cisim ---> cisim
;fizik kurallarına göre cisim 1 saniye ilerliyor.




(check-expect
 (cisim-ilerleme (make-cisim (circle 40 "solid" "gold")
                          (make-vc 3 4) (make-vc 1 0) (make-vc -1 -3)))
              (make-cisim (circle 40 "solid" "gold")
                          (make-vc 4 4) (make-vc 0 -3) (make-vc -1 -3)))

(check-expect
 (cisim-ilerleme (make-cisim  (circle 40 "solid" "gold")
                          (make-vc 3 5) (make-vc 2 4) (make-vc -1 -3)))
 (make-cisim (circle 40 "solid" "gold")
                          (make-vc 5 9) (make-vc 1 1) (make-vc -1 -3)))

(define
 (cisim-ilerleme c) (make-cisim (cisim-resim c)
                            (+vc (cisim-yer c) (cisim-hız c)) (+vc (cisim-hız c) (cisim-ivme c)) (cisim-ivme c)))
                            
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
;(check-expect
 ;(place-cisim (make-cisim (circle 10 "solid" "yellow") (make-vc 10 10) (make-vc 99 99) (make-vc 99 99)) (square 100 "solid" "black"))
 ;(place-image/vc (circle 10 "solid" "yellow") (make-vc 10 10)  (square 100 "solid" "black")))

(define (place-cisim c sahne)
  (place-image/vc (cisim-resim c) (cisim-yer c) sahne))

;oyuncu , tehlike , hedef tanımlar.
(define HEDEF (make-cisim (scale 0.7 (bitmap "mushroom.png")) (make-vc 600 150) (make-vc -15 0) (make-vc 0 0)))
(define TEHLİKE (make-cisim (scale 0.7 (bitmap "goomba.png")) (make-vc 500 215) (make-vc -10 0) (make-vc 0 0)))
(define OYUNCU (make-cisim (bitmap "mario2.png") (make-vc 0 200) (make-vc 0 10) (make-vc 0 0)))
(define SKOR 100)


; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)

(define FRAME-RATE 12)
(define BACKGROUND (bitmap "./notmariobg.jpg"))
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))


;güvenli-aralık : cisim --> Mantıksal
(define (güvenli-aralık c)
(and (<= (vc-y (cisim-yer c)) 200) (>= (vc-y (cisim-yer c)) 100)))

;oyuncu-kontrol : cisim --> cisim
(define (oyuncu-kontrol c)
  (cond
    [(güvenli-aralık c) c]
    [else (make-cisim
           (cisim-resim c)
           (make-vc 0 200)
           (cisim-hız c)
           (cisim-ivme c))]))

;güvenli-sol : cisim ---> mantıksal
(define (güvenli-sol c)
  (< (vc-x (cisim-yer c)) -50))

;sol-kontrol : cisim ---> cisim
(define (sol-kontrol c)
  (cond
    [(güvenli-sol c) (make-cisim
                      (cisim-resim c)
                      (make-vc 405 (vc-y (cisim-yer c)))
                      (cisim-hız c)
                      (cisim-ivme c))]
    [else c]))
  
;cisim-çarptı-mı? : cisim --> mantıksal

     
;tehlike -> cisim
;hedef -> cisim
;oyuncu -> cisim
;skor -> sayı

(define-struct dünya
  (tehlike hedef oyuncu skor))


;c -->> c
;içeriye aldığı cismin yer vektörün x eksenini sabit bırakır, y ekseninden 50 çıkarır, diğer değerleri (imaj/hız/ivme) sabit kalır.

(define (dünya-çiz w) (cond
                        [(<= (dünya-skor w) 0) (overlay (bitmap "mario2.png") (place-image (text "GAME OVER !!!!!!" 40 "olive")
                                                                                            200 200
                                                                                           (rectangle 400 300 "solid" "black")))]
                        [else                     
  (place-cisim (dünya-tehlike w)
                                   (place-cisim (dünya-hedef w)
                                                (place-cisim (dünya-oyuncu w)
                                                             (place-image (text (string-append "Score: " (number->string (dünya-skor w))) 20 "black")
                                                                        200 15 
                                                             BACKGROUND-SCENE))))]))

;oyuncu-konum-güncelle : vc t ---> vc


(define (oyuncu-y-kontrol y t ) (begin (play-sound "./jump.mp3" true) true)
  (cond
    [(and (string=? t " ")(= y 200)) (- y 100)]
    (else y)))


(define (oyuncu-konum-güncelle c t )(make-vc (vc-x
                                              (cisim-yer c)) (oyuncu-y-kontrol (vc-y (cisim-yer c)) t)))

(define (oyuncu-hareket c t) (make-cisim
    (cisim-resim c) (oyuncu-konum-güncelle c t) (cisim-hız c) (cisim-ivme c)))
;vc-uzunluk
; vector ---> sayı
(define (vc-uzunluk v) (sqrt (+ (sqr (vc-x v)) (sqr (vc-y v)))))

;cisim-mesafe
;cisim cisim --> sayı

(define (cisim-mesafe c1 c2) (vc-uzunluk (-vc (cisim-yer c1) (cisim-yer c2))))

;çarptı-mı
;cisim cisim --> mantıksal
(define (çarptı-mı c1 c2) (<= (cisim-mesafe c1 c2) (/ (+ (image-height (cisim-resim c1)) (image-height (cisim-resim c2))) 3)))

;tehlike-konum-güncelle : dünya --> cisim
(define (tehlike-konum-güncelle w) (make-cisim
                                    (cisim-resim (dünya-tehlike w))
                                    (make-vc  405 (vc-y (cisim-yer (dünya-tehlike w))))
                                    (cisim-hız (dünya-tehlike w))
                                    (cisim-ivme (dünya-tehlike w))))
;hedef-konum-güncelle : dünya --> cisim
(define (hedef-konum-güncelle w) (make-cisim
                                    (cisim-resim (dünya-hedef w))
                                    (make-vc 405 (vc-y (cisim-yer (dünya-hedef w))))
                                    (cisim-hız (dünya-hedef w))
                                    (cisim-ivme (dünya-hedef w))))

;tehlike-çarpışma hedef-çarpışma
;dünya ---> dünya

(define (tehlike-çarpışma w) (cond
                               [(çarptı-mı (dünya-oyuncu w) (dünya-tehlike w))
                                (make-dünya
                                 (tehlike-konum-güncelle w)
                                 (dünya-hedef w)
                                 (dünya-oyuncu w)
                                 (- (dünya-skor w) 50))]
                               [else w]))
             
(define (hedef-çarpışma w) (cond
                               [(çarptı-mı (dünya-oyuncu w) (dünya-hedef w))
                                (make-dünya
                                 (dünya-tehlike w)
                                 (hedef-konum-güncelle w)
                                 (dünya-oyuncu w)
                                 (+ (dünya-skor w) 25))]
                               [else w]))
                                           

 ;(cisim-hız (dünya-oyuncu w))
  ;  (cisim-ivme (dünya-oyuncu w))))
  
(define (dünya-tuş w t)
  (make-dünya (dünya-tehlike w)
              (dünya-hedef w)
              (oyuncu-hareket (dünya-oyuncu w) t)
              (dünya-skor w)))
                     

(define (dünya-fare w x y m) w)

;dünya-ilerleme : dünya ---> dünya

(define (dünya-ilerleme w) (make-dünya  (sol-kontrol (cisim-ilerleme (dünya-tehlike w)))
                                       (sol-kontrol (cisim-ilerleme (dünya-hedef w)))
                                       (oyuncu-kontrol (cisim-ilerleme (dünya-oyuncu w)))
                                       (dünya-skor w)))
                                                                 

(define (dünya-yeni w) (tehlike-çarpışma (hedef-çarpışma (dünya-ilerleme w))))

;dünya=>cisim
(define yaradılış (make-dünya TEHLİKE HEDEF OYUNCU SKOR))



(big-bang yaradılış
  (on-tick dünya-yeni (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş))