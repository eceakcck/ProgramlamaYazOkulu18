;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ultimate-nights) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; S. Ekin Yağmur
;sekinya1@gmail.com
;creative commons license
;CC2
; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)

(define FRAME-RATE 12)

(define BACKGROUND (bitmap "arkaplan/bg1.jpg"))
(define BACKGROUND-SCENE
(place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))

(define BACKGROUND2 (bitmap "arkaplan/bg2.jpg"))
(define BACKGROUND2-SCENE
(place-image/align BACKGROUND2  0 0 "left" "top" (empty-scene (image-width BACKGROUND2) (image-height BACKGROUND2))))

(define BACKGROUND3 (bitmap "arkaplan/bg3.gif"))
(define BACKGROUND3-SCENE
(place-image/align BACKGROUND3  0 0 "left" "top" (empty-scene (image-width BACKGROUND3) (image-height BACKGROUND3))))

(define BACKGROUND4 (bitmap "arkaplan/bg5.gif"))
(define BACKGROUND4-SCENE
(place-image/align BACKGROUND4  0 0 "left" "top" (empty-scene (image-width BACKGROUND3) (image-height BACKGROUND3))))

;(define WELCOME (bitmap "arkaplan/bg1.jpg"))
;(define BACKGROUND-SCENE
;(place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))



;vektör
  ;x-sayı
  ;y-sayı
(define-struct vc (x y))

;cisim->
 ; imaj:resim
 ; yer:vc
 ; hız:vc
 ; ivme:vc
(define-struct cisim (imaj yer hız ivme))

;dünya->
  ;oyuncu-cisim
  ;tehlike-cisim
  ;hedef-cisim
  ;skor-sayı
(define-struct dünya (oyuncu tehlike hedef seviye skor ))



(define OYUNCU (make-cisim  (bitmap "tavşan.png") (make-vc  270 590) (make-vc 0 0) (make-vc 0 0)))
(define TEHLİKE (make-cisim (bitmap "tilki.png") (make-vc 180 0 ) (make-vc 0 5) (make-vc 0 0)))
(define HEDEF (make-cisim (scale 0.9 (bitmap "bento.png")) (make-vc 180 0) (make-vc 0 8) (make-vc 0 0)))
(define SKOR 0)
(define GAME-OVER (place-image (text "Ultımte Nights Ekin Yağmur 2018 " 15 "lightblue") 400 700 (bitmap "arkaplan/gover.jpg")))
(define DÜNYAM (make-dünya OYUNCU TEHLİKE HEDEF 0 1 ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vektörcük (make-vc 3 4))
(define vektöör (make-vc 7 8))

;toplam-vc vc vc -> vc
;iki vektörün toplamını hesaplar.
(check-expect (toplam-vc (make-vc 3 4) (make-vc 6 7))
              (make-vc 9 11))
(define (toplam-vc vektörcük vektöör)
        (make-vc
          (+ (vc-x vektörcük) (vc-x vektöör))
          (+ (vc-y vektörcük) (vc-y vektöör))))

;fark-vc vc vc -> vc
;iki vektörün farkını hesaplar
(check-expect (fark-vc (make-vc 3 5) (make-vc 6 7))
              (make-vc -3 -2))
(define (fark-vc vektörcük vektöör)
              (make-vc
                 (- (vc-x vektörcük) (vc-x vektöör))
                 (- (vc-y vektörcük) (vc-y vektöör))))                                           


(define OBJE1 (make-cisim (circle 50 "solid" "red") (make-vc 1 2) (make-vc 2 4) (make-vc 1 5)))
(define OBJE2 (make-cisim (star 50 "solid" "purple") (make-vc 1 4) (make-vc 3 0) (make-vc 3 2)))

;cisim-ilerleme cisim-> cisim
;fizik kurallarına göre cismin ilerlemesini sağlar
(check-expect
  (cisim-ilerleme  OBJE1)              
            (make-cisim
                   (circle 50 "solid" "red")
                   (make-vc 3 6)
                   (make-vc 3 9)
                   (make-vc 1 5)))


(define
  (cisim-ilerleme b)
            (make-cisim
                   (cisim-imaj b)
                   (toplam-vc (cisim-yer b) (cisim-hız b))
                   (toplam-vc (cisim-hız b) (cisim-ivme b))
                   (cisim-ivme b)))



;vc-uzunluk vc-> sayı
;vc vektörünün uzunluğunu hesaplar.
(check-expect (vc-uzunluk (make-vc 3 4)) 5)
(define (vc-uzunluk v) (sqrt (+ (sqr (vc-x v)) (sqr (vc-y v)))))

;cisim-mesafe cisim cisim-> sayı
;cisimlerin ararsındaki uzaklığı hesaplar.
(check-expect (cisim-mesafe OBJE1 OBJE2) (vc-uzunluk (fark-vc (cisim-yer OBJE1) (cisim-yer OBJE2))))
(define (cisim-mesafe c1 c2) (vc-uzunluk (fark-vc (cisim-yer c1) (cisim-yer c2))))

;çaprıştılar-mı? cisim cisim -> mantıksal
;cisim ile cismin çarpışıp çarpışmadığını kontrol eder.
(define (çarpıştılar-mı? c1 c2) (<= (cisim-mesafe c1 c2) (/ (+ (image-height (cisim-imaj c1)) (image-height (cisim-imaj c2))) 3)))
        
;tehlike-çarpışma dünya -> dünya
;tehlike ve oyuncu çarpıştığında ne olacağına karar verir.
(define (tehlike-çarpışma w) (cond
                                   [(çarpıştılar-mı? (dünya-oyuncu w) (dünya-tehlike w))
                                     (make-dünya
                                           (dünya-oyuncu w)
                                           (randoom (dünya-tehlike w))
                                           (dünya-hedef w)
                                           (dünya-seviye w)
                                           (- (dünya-skor w) 10))]
                                   [else w]))

;hedef-çarpışma dünya -> dünya
;hedef ve oyuncu çarpıştığında ne olacağına karar verir.
(define (hedef-çarpışma w) (cond
                                   [(çarpıştılar-mı? (dünya-oyuncu w) (dünya-hedef w))
                                     (make-dünya
                                           (dünya-oyuncu w)
                                           (dünya-tehlike w)
                                           (randoom (dünya-hedef w))
                                           (dünya-seviye w)
                                           (+ (dünya-skor w) 20))]
                                   [else w]))




;(define LEDA (make-dünya OBJE2))
;(define CASTOR (make-dünya (make-cisim (circle 50 "solid" "red") (make-vc 1 2) (make-vc 2 4) (make-vc 3 6)))) 

;cisim-çarptı-mı? cisim -> mantıksal
;cismi yerden çaprtırır.
;(check-expect (cisim-çarptı-mı? LEDA) #true)
(define (cisim-çarptı-mı? cisim) (> (vc-y (cisim-yer cisim)) (image-height BACKGROUND)))

;dünya-kontrol dünya ->dünya
(define (dünya-kontrol w) (dünya-bölüm (hedef-çarpışma (tehlike-çarpışma (dünya-ilerleme w)))))

;randoom cisim -> cisim
;cisimlerin random gelmesini sağlar
(define (randoom c) (make-cisim (cisim-imaj c) (make-vc (random 25 (- (image-width BACKGROUND) 10)) 0) (cisim-hız c) (cisim-ivme c)))

;random-cisim cisim -> cisim
;cisim uzunluğu arkaplanı geçerse rastgele bir konumdaan tekrar gelmesini sağlar.
(define (random-cisim c)
  (cond
    [(cisim-çarptı-mı? c) (randoom c)]
    [else (cisim-ilerleme c)]))




;yön-değiştir cisim -> cisim
;verilen cismin yönünü değiştir.
(define (yön-değiştir c)
  ((make-cisim (cisim-imaj c) (cisim-yer c) (make-vc (vc-x (cisim-hız c)) (* -1 (vc-y (cisim-hız c)))) (cisim-ivme c))))

;cisim-hareket cisim ->cisim
;cisim çarptıysa yönünü değiştirir çarpmadıysa ilerleme fonksiyonunu çagırır.
;(define (cisim-hareket cisim)(cond
 ;                             [(cisim-çarptı-mı? cisim)(yön-değiştir cisim)]
  ;                             [else (cisim-ilerleme cisim)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;background-seç sayı-> görsel
;levele göre arkaplan seçer
(define (background-seç s) (cond
                             [(<= s 1) BACKGROUND-SCENE]
                             [(<= s 2) BACKGROUND2-SCENE]
                             [(<= s 3) BACKGROUND3-SCENE] 
                             [else BACKGROUND4-SCENE]))

;dünya-bölüm dünya -> dünya
;
(define (dünya-bölüm w) (cond
                          [(>= (dünya-skor w) 300) (make-dünya (dünya-oyuncu w) (dünya-tehlike w) (dünya-hedef w) 4 (dünya-skor w))]
                          [(>= (dünya-skor w) 200) (make-dünya (dünya-oyuncu w) (dünya-tehlike w) (dünya-hedef w) 3 (dünya-skor w))]
                          [(>= (dünya-skor w) 100) (make-dünya (dünya-oyuncu w) (dünya-tehlike w) (dünya-hedef w) 2 (dünya-skor w))]
                          [else w]))

;dünya-ilerleme dünya-> dünya
;dünyanın içindeki cisimi hareket ettirek yeni dünya oluşturur.

;(check-expect (dünya-ilerleme  DÜNYAM)
 ;             (make-dünya
  ;             (cisim-ilerleme  (dünya-oyuncu DÜNYAM))
   ;            (cisim-ilerleme  (dünya-tehlike DÜNYAM)) 
    ;           (cisim-ilerleme  (dünya-hedef DÜNYAM))
     ;          (dünya-skor DÜNYAM)))
                                   
(define (dünya-ilerleme w)
        (make-dünya
            (cisim-ilerleme (dünya-oyuncu w))
            (random-cisim (cisim-ilerleme (dünya-tehlike w)))
            (random-cisim (cisim-ilerleme (dünya-hedef w)))
            (dünya-seviye w)
            (dünya-skor w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(place-line/vc origin (toplam-vc v1 v2) "green"
                                     (place-line/vc origin v1 "pink"
                                                    (place-line/vc v1 (toplam-vc v1 v2)  "yellow" (rectangle 300 225 "solid" "black"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;dünya-çiz dünya-> dünya
;verilen dünyaya sahneyi vb ekler ve yeni dünya yaratır.
;(check-expect (dünya-çiz DÜNYAM) (place-cisim (dünya-oyuncu DÜNYAM) (place-cisim (dünya-tehlike DÜNYAM) (place-cisim (dünya-hedef DÜNYAM)
 ;                     (place-image (text (string-append "SKOR:" (number->string (dünya-skor DÜNYAM))) 20 "white") 50 50 BACKGROUND-SCENE)))))
  
(define (dünya-çiz w) (cond
                        [(< (dünya-skor w) 0) GAME-OVER]
                      [else (place-cisim (dünya-tehlike w)
                      (place-cisim (dünya-hedef w)
                      (place-cisim (dünya-oyuncu w)
   (place-image (text (string-append "SKOR " (number->string (dünya-skor w))) 20 "white") 50 50
   (place-image (text (string-append "SEVİYE " (number->string (dünya-seviye w))) 15 "lightblue") 50 20  (background-seç (dünya-seviye w)))))))] ))

  ;oyuncu-x-kontrol sayı tuş -> sayı
(define (oyuncu-x-kontrol x t)
  (cond
    [(string=? t "left") (- x 10)]
    [(string=? t "right") (+ x 10)]
    [else x]))

;oyuncu-yer-kontrol vektör tuş -> vektör
(define (oyuncu-yer-kontrol v t) (make-vc (oyuncu-x-kontrol (vc-x v) t)(vc-y v)))
  
;oyncu-hareket cisim tuş ->cisim
(define (oyuncu-hareket c t) (make-cisim (cisim-imaj c) (oyuncu-yer-kontrol (cisim-yer c) t) (cisim-hız c) (cisim-ivme c)))

;oyun-başlat skor tuş -> skor
;space tuşuna basıldığıında skoru 0 olarak günceller.
(define (oyun-başlat w t) (cond
                            [(and (< (dünya-skor w) 0 ) (string=? t " ")) 0]
                            [else (dünya-skor w)]))
;dünya-tuş tuş dünya-> dünya-
;dünyanın tamamı ile tuşuları bağlar.
(define (dünya-tuş w t)
         (make-dünya (oyuncu-hareket (dünya-oyuncu w) t) (dünya-tehlike w) (dünya-hedef w) (dünya-seviye w) (oyun-başlat w t)))
  

    
(define (dünya-fare w x y m) w)

(define yaradılış (make-dünya OYUNCU TEHLİKE HEDEF 1 SKOR ))




(big-bang yaradılış
  (on-tick dünya-kontrol (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare))



            
                   
           
  
            
             
