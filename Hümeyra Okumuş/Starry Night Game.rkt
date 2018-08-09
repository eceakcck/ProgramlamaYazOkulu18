;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Starry Night Game|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;Starry Night
;Hümeyra Okumuş 2018
;xhumeyraokumus@gmail.com
;Creative Commons lisansı (CC2)
;-----------------------------------------------
; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)


(define FRAME-RATE 12)
(define BACKGROUND
(place-image (text/font "Hümeyra Okumuş 2018" 15 "gold" "Arial" 'default 'normal 'bold #f ) 610 480
  (place-image (text/font "Starry Night" 30 "gold" "Forte" 'default 'normal 'bold #f ) 330 30 (bitmap "imaj/starrynighteni.jpg"))))
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))

(define BACKGROUND-2
(place-image (text/font "Hümeyra Okumuş 2018" 15 "DarkBlue" "Arial" 'default 'normal 'bold #f ) 610 480
  (place-image (text/font "Starry Night" 30 "DarkBlue" "Forte" 'default 'normal 'bold #f ) 380 30 (bitmap "imaj/thebedroom.jpg" ))))
(define BACKGROUND-SCENE-2
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))

(define BACKGROUND-3
(place-image (text/font "Hümeyra Okumuş 2018" 15 "DarkBlue" "Arial" 'default 'normal 'bold #f ) 610 480
  (place-image (text/font "Starry Night" 30 "gold" "Forte" 'default 'normal 'bold #f ) 335 30 (bitmap "imaj/kafe.png" ))))
(define BACKGROUND-SCENE-3
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;vc
;x sayı x kompenantı
;y sayı y kompenantı
(define-struct vc (x y))
(define myvector
  (make-vc 3 4))

(define myvector2
  (make-vc 7 5))

; +vc vc vc -> vc
; verilen iki vektörü toplar
(check-expect
 (+vc (make-vc 7 5) (make-vc 3 4))
 (make-vc 10 9))

(define (+vc myvector myvector2)
  (make-vc (+ (vc-x myvector)(vc-x myvector2) )    ( + (vc-y myvector) (vc-y myvector2)  )     ))

; -vc vc vc -> vc
;Verilen iki vektörü birbirinden çıkarır.
(check-expect
 (-vc (make-vc 3 4) (make-vc 5 8))
 (make-vc -2 -4))

(define (-vc myvector myvector2)
  (make-vc ( - (vc-x myvector) (vc-x myvector2)) ( - (vc-y myvector) (vc-y myvector2))))
;;;;;;;;;


(define-struct cisim
  (imaj yer hız ivme))

(define obje (make-cisim (star 10 "solid" "black") (make-vc 1 1) (make-vc 2 3 ) (make-vc -1 -3)))
(define obje2 (make-cisim (square 10 "solid" "blue") (make-vc 2 2) (make-vc 4 5) (make-vc -3 -5)))
 
;cisim-ilerleme : cisim -> cisim
;fizik kurallarına göre cisim 1 saniye ilerliyor.
(check-expect
 (cisim-ilerleme obje) (make-cisim (star 10 "solid" "black") ( make-vc 3 4) ( make-vc 1 0) (make-vc -1 -3)))
(check-expect
 (cisim-ilerleme obje2) (make-cisim (square 10 "solid" "blue") (make-vc 6 7 ) (make-vc 1 0) (make-vc -3 -5)))
(define
  (cisim-ilerleme c)
  (make-cisim (cisim-imaj c) (+vc (cisim-yer c) (cisim-hız c)) (+vc (cisim-hız c) (cisim-ivme c)) (cisim-ivme c)))


;;;;;

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
                                                                            
;;;;;;;;;;;;;;;;;;;;
(define OYUNCU (make-cisim (scale 0.2(bitmap "imaj/bulanıkkafa.png")) (make-vc 200 400) (make-vc 0 0) (make-vc 0 0)))
(define TEHLİKE (make-cisim (scale 0.4( bitmap "imaj/lettera.png")) (make-vc 100 0) (make-vc 0 6) (make-vc 0 0)))
;(define TEHLİKE-2 (make-cisim (triangle 30 "solid" "red") (make-vc 200 50) (make-vc 4 2) (make-vc 0 0)))
(define HEDEF (make-cisim (scale 0.3( bitmap "imaj/hedeff.png")) (make-vc 300 0) (make-vc 0 5) (make-vc 0 0)))
(define SKOR 0)





;dünya -> Veri yapısı
;oyuncu -> cisim
;tehlike -> cisim
;tehlike-2 -> cisim -> şu an yok
;hedef -> cisim
;score -> sayı
;arkaplan -> resim
;
(define-struct dünya (oyuncu tehlike hedef skor seviye))
;(define dünya1 (make-dünya OYUNCU TEHLİKE HEDEF SKOR (dünya-seviye w )))
;*********************************
;background-seç sayı-> görsel
;sayıya göre arkaplanı belirler.
;(check-expect (background-seç 1) BACKGROUND-SCENE)
;(check-expect (background-seç 2) BACKGROUND-SCENE-2)
;(check-expect (background-seç 3) BACKGROUND-SCENE-3)
(define (background-seç seviye)
  (cond
    [(<= seviye 1) BACKGROUND]
    [(<= seviye 2) BACKGROUND-2]
    [(<= seviye 3) BACKGROUND-3]))
;dünya-bölüm: dünya -> dünya
(define (dünya-bölüm w)
  (cond
    [(>= (dünya-skor w) 200 ) (make-dünya (dünya-oyuncu w) (dünya-tehlike w) (dünya-hedef w) (dünya-skor w) 3)]
    [(>= (dünya-skor w) 100 ) (make-dünya (dünya-oyuncu w) (dünya-tehlike w) (dünya-hedef w) (dünya-skor w) 2)]
    [(< (dünya-skor w) 100 ) (make-dünya (dünya-oyuncu w) (dünya-tehlike w) (dünya-hedef w) (dünya-skor w) 1)]
    (else w)))

;(define YDÜNYA (make-dünya obje))
;(define YYDÜNYA (make-dünya obje2))
; (define TOP (circle 40 "solid" "yellow")(make-vc 0 0) (make-vc 6 7) (make-vc 0 0))

;seviye-güncelle : sayı(skor) cisim
;Skor 100'e gelince cisimin hızını artırır.
(define (seviye-güncelle skor cisim) (cond
                                       [   (= skor 20) (make-cisim (cisim-imaj cisim)
                                                                    (cisim-yer cisim)
                                                                    (make-vc (vc-x (cisim-hız cisim)) (+ (vc-y (cisim-hız cisim)) 1))
                                                                    (cisim-ivme cisim))]
                                       [   (= skor 50) (make-cisim (cisim-imaj cisim)
                                                                    (cisim-yer cisim)
                                                                    (make-vc (vc-x (cisim-hız cisim)) (+ (vc-y (cisim-hız cisim)) 1))
                                                                    (cisim-ivme cisim))]
                                       [   (= skor 80) (make-cisim (cisim-imaj cisim)
                                                                    (cisim-yer cisim)
                                                                    (make-vc (vc-x (cisim-hız cisim)) (+ (vc-y (cisim-hız cisim)) 1))
                                                                    (cisim-ivme cisim))]
                                        [   (= skor 120) (make-cisim (cisim-imaj cisim)
                                                                    (cisim-yer cisim)
                                                                    (make-vc (vc-x (cisim-hız cisim)) (+ (vc-y (cisim-hız cisim)) 1))
                                                                    (cisim-ivme cisim))]
                                         [   (= skor 160) (make-cisim (cisim-imaj cisim)
                                                                    (cisim-yer cisim)
                                                                    (make-vc (vc-x (cisim-hız cisim)) (+ (vc-y (cisim-hız cisim)) 2))
                                                                    (cisim-ivme cisim))]
                                         [   (= skor 200) (make-cisim (cisim-imaj cisim)
                                                                    (cisim-yer cisim)
                                                                    (make-vc (vc-x (cisim-hız cisim)) (+ (vc-y (cisim-hız cisim)) 2))
                                                                    (cisim-ivme cisim))]
                                       [ else (make-cisim (cisim-imaj cisim)
                                                          (cisim-yer cisim)
                                                          (cisim-hız cisim)
                                                          (cisim-ivme cisim))]))

;dünya-ilerleme dünya -> dünya
;dünyanın içine cisim çağırıp yeni dünya oluşturur.Cisimler zamana göre hareket eder.
;(check-expect (dünya-ilerleme dünya1)
   ;         (make-dünya
   ;          (cisim-ilerleme (dünya-oyuncu dünya1))
   ;         (cisim-random (cisim-ilerleme  (dünya-tehlike dünya1)))
   ;         (cisim-random (cisim-ilerleme (dünya-hedef dünya1)))
   ;           SKOR
   ;           (dünya-seviye w )))

(define (dünya-ilerleme w)
 (make-dünya (cisim-ilerleme (dünya-oyuncu w))
             (cisim-random (cisim-ilerleme  (dünya-tehlike w)))            
             (cisim-random (cisim-ilerleme (dünya-hedef w)))
             (dünya-skor w)
             (dünya-seviye w )))


;dünya-çiz -> dünya : dünya
;dünyanın içindeki verileri kullanarak bir dünya çizer
;(check-expect (dünya-çiz dünya1)
 ;            (place-cisim (dünya-oyuncu dünya1) (place-cisim (dünya-tehlike dünya1)
  ;                                              (place-cisim (dünya-hedef dünya1)
   ;          (place-image (text (number->string (dünya-skor dünya1)) 20 "white" )
    ;                                                          50 30
     ;                                           (background-seç (dünya-seviye w)) )))))
(define (dünya-çiz w) 
   (place-cisim (dünya-oyuncu w) (place-cisim  (dünya-tehlike w)                                 
                                 (place-cisim (dünya-hedef w)
                                 (place-image (text (number->string (dünya-skor w)) 20 "white")
                                              50 30
                                           (background-seç (dünya-seviye w)))))))


;________________________________________
(define (oyuncu-x-kontrol x t )
  (cond
    [(string=? t "left") (- x 10 )]
    [(string=? t "right") ( + x 10)]
    (else x )))

;oyuncu-yer-kontrol vc t -> vc
(define (oyuncu-yer-kontrol v t)
  (make-vc (oyuncu-x-kontrol (vc-x v) t) (vc-y v)))

;oyuncu-hareket: cisim(c) tuş (t) -> cisim
(define (oyuncu-hareket cisim t)
  (make-cisim (cisim-imaj cisim)
              (oyuncu-yer-kontrol (cisim-yer cisim) t)
              (cisim-hız cisim) (cisim-ivme cisim)))


;dünya-tuş: dünya(w) tuş(t) -> dünya
(define (dünya-tuş w t)
  (make-dünya (oyuncu-hareket (dünya-oyuncu w) t)
              (dünya-tehlike w)
              (dünya-hedef w)
              (dünya-skor w)
              (dünya-seviye w )))
;_______________________________
;vc-uzunluk : vektör -> sayı
;Vektörün uzunluğunu hesaplar.
;(check-expect (vc-uzunluk vektör) (sqrt (+ (sqr (vc-x (make-vc 2 5)(sqr (vc-y (make-vc 3 6))))))))
(define (vc-uzunluk v)
  (sqrt (+ (sqr (vc-x v)) (sqr (vc-y v)))))

;cisim-mesafe : cisim cisim -> sayı
(define (cisim-mesafe cisim1 cisim2)
  (vc-uzunluk (-vc (cisim-yer cisim1) (cisim-yer cisim2))))

;çarptı-mı? : cisim cisim -> mantıksal
;İki cismin y koordinatlarını alıp üst taraflarından çarpışıp çarpışmadıklarını kontrol eder.
;(İki cismin arasındaki mesafe yarıçapından az mı?)
;(check-expect
;(define (çarptı-mı? cisim1 cisim2)
 ; ( <= ( / (image-height cisim1) 2) ( / (image-height cisim2) 2)))

;cisim-çarpıştı-mı? :cisim cisim -> mantıksal
;(İki cismin arasındaki mesafe yarıçapından az mı?)
(define (cisim-çarpıştı-mı? cisim1 cisim2)
  (<= (cisim-mesafe cisim1 cisim2) (/ (+ (image-height( cisim-imaj cisim1)) (image-height (cisim-imaj cisim2))) 2)))

;tehlike-çarpışma
;Tehlike ve oyuncunun çarpışınca olacakları yapar.
(define (tehlike-çarpışma  w ) (cond
                                           [(cisim-çarpıştı-mı? (dünya-tehlike w) (dünya-oyuncu w))
                                            (make-dünya
                                                                        (dünya-oyuncu w)
                                                                       (seviye-güncelle (dünya-skor w) (cisim-random-ç (dünya-tehlike w)))
                                                                        (dünya-hedef w)
                                                                        ( - (dünya-skor w) 10)
                                                                        (dünya-seviye w ))]
                                           (else w)))
;hedef-çarpışma
;Hedef ve oyuncu çarpışınca olacakalrı yapar.
(define (hedef-çarpışma w) (cond
                             [(cisim-çarpıştı-mı? (dünya-hedef w) (dünya-oyuncu w))
                              (make-dünya
                               (dünya-oyuncu w)
                               (dünya-tehlike w)
                               (seviye-güncelle (dünya-skor w) (cisim-random-ç (dünya-hedef w)))
                               ( + (dünya-skor w) 10)
                               (dünya-seviye w ))]
                             (else w)))

;dünya-yeni dünya-> dünya
(define (dünya-yeni w)
(dünya-bölüm (hedef-çarpışma (tehlike-çarpışma (dünya-ilerleme w)))))
                                                                                   
                                                                                   

;________________________

(define (dünya-fare w x y m) w)

(define TOP  (make-cisim (circle 40 "solid" "yellow")(make-vc 0 0) (make-vc 6 7) (make-vc 0 0)))
(define yaradılış (make-dünya OYUNCU TEHLİKE HEDEF SKOR 1))
;-----------------------------------------
;cisim-çarptı-mı?
;mantıksal-> mantıksal
;Cismin çarpıp çarpmadığını kontrol eder.
;(check-expect (cisim-çarptı-mı? TOP) #true )
(define (cisim-çarptı-mı? cisim )
   (> (vc-y (cisim-yer cisim))
       (image-height BACKGROUND)))

;cisim-random
;
;Cisim çarptıysa rastgele bir konumdan cisim tekrar gelir.
(define (cisim-random cisim) (cond
                              [ (cisim-çarptı-mı? cisim)(make-cisim (cisim-imaj cisim)
                                 (make-vc (random 50 ( - (image-width BACKGROUND) 50)) 0) (cisim-hız cisim) (cisim-ivme cisim))]
                              [ else (cisim-ilerleme cisim) ]))

;cisim-random-ç
;cisimler çarpıştıktan sonra tekrar rastgele gelmelerini sağlar.
(define (cisim-random-ç cisim) (make-cisim (cisim-imaj cisim)
                                 (make-vc (random 50 ( - (image-width BACKGROUND) 50)) 0) (cisim-hız cisim) (cisim-ivme cisim))) 
;-------------------------------------------
;yön-değiştir
;cisim-> cisim
;Cismin yönünü değiştirir.
;(define (yön-değiştir cisim) (make-cisim (cisim-imaj cisim) (cisim-yer cisim)
                                        ; (make-vc (vc-x (cisim-hız cisim)) ( * -1 (vc-y (cisim-hız cisim))))
                                         ;(cisim-ivme cisim))) 
;cisim-hareket
;cisim -> cisim
;Cisim çarptıysa cismin yönünü değiştirir.Cisim çarpmadıysa cisim-ilerleme fonksiyonunu çağırır.
;( define (cisim-hareket cisim) (cond
                                ; [ (cisim-çarptı-mı? cisim) (yön-değiştir cisim) ] 
                                ; [ else (cisim-ilerleme cisim)]))

(big-bang yaradılış
 (on-tick dünya-yeni (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare))




