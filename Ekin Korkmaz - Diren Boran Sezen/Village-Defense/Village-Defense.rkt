;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Village-Defense) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;04.08.2018
;Ekin Korkmaz
;Diren Boran Sezen

;Creative Commons License
;CC2

(require 2htdp/universe)
(require 2htdp/image)

;SÖZLEŞMELER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TASARIM

;BAŞLIK metin
;oyunun adını içerir
(define BAŞLIK "Village-Defense")
(define İSİM "   Diren Boran Sezen
          Ekin Korkmaz")

;level metin 
;level adını içerir
(define LEVEL "level")
;skor sayı
;elde edilen skoru yazar
(define SKOR 0)
;vc x y
;vektör tanımlar
(define-struct vc ( x y))

;+vc vc vc -> vc
;alınan iki vektörün toplamını hesaplar
(check-expect (+vc (make-vc 5 10) (make-vc 2 7))
  (make-vc (+ (vc-x (make-vc 5 10)) (vc-x (make-vc 2 7))) (+ (vc-y (make-vc 5 10)) (vc-y (make-vc 2 7)))))

(check-expect (+vc (make-vc 7 3) (make-vc 5 6))
  (make-vc (+ (vc-x (make-vc 7 3)) (vc-x (make-vc 5 6))) (+ (vc-y (make-vc 7 3)) (vc-y (make-vc 5 6)))))

(define (+vc vc1 vc2)
  (make-vc (+ (vc-x vc1) (vc-x vc2)) (+ (vc-y vc1) (vc-y vc2))))

;-vc vc vc -> vc
;alınan iki vektörün farkını hesaplar
(check-expect (-vc (make-vc 5 10) (make-vc 2 7))
  (make-vc (- (vc-x (make-vc 5 10)) (vc-x (make-vc 2 7))) (- (vc-y (make-vc 5 10)) (vc-y (make-vc 2 7)))))

(check-expect (-vc (make-vc 7 3) (make-vc 5 6))
  (make-vc (- (vc-x (make-vc 7 3)) (vc-x (make-vc 5 6))) (- (vc-y (make-vc 7 3)) (vc-y (make-vc 5 6)))))

(define (-vc vc1 vc2)
  (make-vc (- (vc-x vc1) (vc-x vc2)) (- (vc-y vc1) (vc-y vc2))))


;ekranla ilgili tasarım bilgilerini içerir
(define FRAME-RATE 30)
(define BACKGROUND (overlay/align "right" "top"
                           (text İSİM 20 "gold")
                                  (overlay/align "left" "top"
                                          (text BAŞLIK 30 "gold")
                                                     (scale 0.8 (bitmap "kule.jpg")))))
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND 0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;l;;;;;;;;;;;;;;;;;;
;GÖRÜNTÜLER


(define-struct cisim (imaj yer hız ivme))

;oyuncu cisim
;oyuncunun özelliklerini içerir
(define oyuncu (make-cisim (scale 0.35 (bitmap "alih.png")) (make-vc (/ (image-width BACKGROUND) 2) (/ (image-height BACKGROUND) 2))
                           (make-vc 0 0) (make-vc 0 0)))
;tehlike cisim
;tehlikenin özelliklerini içerir
(define tehlike (make-cisim (scale 0.29 (bitmap "çöp.png")) (make-vc 1280 (random 50 910))
                            (make-vc -7 0) (make-vc 0 0)))
;hedef cisim
;hedefin özelliklerini içerir
(define hedef (make-cisim (scale 0.2 (bitmap "coin.png")) (make-vc 0 (random 50 910))
                          (make-vc 7 0) (make-vc 0 0)))
;silah cisim
;silahın özelliklerini içerir
(define silah (make-cisim (scale 0.05 (bitmap "teb.png")) (make-vc -500 -500)
                          (make-vc 0 0) (make-vc 0 0)))

;killscreen imaj
;oyun sonu görüntüsünü içerir
(define KILLSCREEN (scale 0.8 (bitmap "nisanyan3.jpg")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;cisimleri ekranın içinde tutmaya yarar
(define ekran-genişliği (image-width BACKGROUND))
(define ekran-yüksekliği (image-height BACKGROUND))

;cisim-genişliği cisim -> sayı
(check-expect  (cisim-genişliği hedef) (image-width (cisim-imaj hedef)))
(check-expect  (cisim-genişliği tehlike) (image-width (cisim-imaj tehlike)))

(define (cisim-genişliği cisim) (image-width (cisim-imaj cisim)))

;cisim-yüksekliği cisim ->sayı
(check-expect (cisim-yüksekliği tehlike) (image-height (cisim-imaj tehlike)))
(check-expect (cisim-yüksekliği hedef) (image-height (cisim-imaj hedef)))

(define (cisim-yüksekliği cisim) (image-height (cisim-imaj cisim)))

;sağdan-sola-dönüş cisim sayı yön yön -> cisim
;sağdan çıkan cismi soldan rastlantısal bir yükseklikte geri getirir
(define (sağdan-sola-dönüş cisim ekran-genişliği)
  (cond
      ((> (vc-x (cisim-yer cisim)) ekran-genişliği)
           (make-cisim 
           (cisim-imaj cisim)
           (make-vc 0 (random 0 ekran-yüksekliği))
           (cisim-hız cisim)
           (cisim-ivme cisim)))
      (else cisim)))

;soldan-sağa-dönüş cisim sayı yön yön -> cisim
;soldan çıkan cismi sağdan rastlantısal bir yükseklikte geri getirir
(define (soldan-sağa-dönüş cisim ekran-genişliği)
  (cond
      ((< (vc-x (cisim-yer cisim)) 0)
           (make-cisim
           (cisim-imaj cisim)
           (make-vc ekran-genişliği (random 0 ekran-yüksekliği))
           (cisim-hız cisim)
           (cisim-ivme cisim)))
      (else cisim)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;HAREKETLER


;cisim-ilerleme cisim -> cisim
;cismi verilen vektör değerlerine göre hareket ettiririsim
(check-expect (cisim-ilerleme hedef) (make-cisim (cisim-imaj hedef) (+vc (cisim-yer hedef) (cisim-hız hedef))
                                                 (+vc (cisim-hız hedef) (cisim-ivme hedef)) (cisim-ivme hedef)))
(check-expect (cisim-ilerleme tehlike) (make-cisim (cisim-imaj tehlike) (+vc (cisim-yer tehlike) (cisim-hız tehlike))
                                                  (+vc (cisim-hız tehlike) (cisim-ivme tehlike)) (cisim-ivme tehlike)))


                                              
(define (cisim-ilerleme cisim) (make-cisim (cisim-imaj cisim) (+vc (cisim-yer cisim) (cisim-hız cisim))
                               (+vc (cisim-hız cisim)(cisim-ivme cisim))
                               (cisim-ivme cisim))) 
;silah-ilerleme


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ÇARPIŞMALAR

;mag-vc vc-> sayı
;mag-vc vektörün uzunluğunu hesaplamalı
(define (mag-vc v)
  (sqrt (+ (sqr (vc-x v)) (sqr (vc-y v)))))

;cisim-mesafe cisim cisim-> sayı

(define (cisim-mesafe c1 c2)
  (mag-vc (-vc (cisim-yer c1) (cisim-yer c2)))) 

;cisim-çarpıştı-mı? cisim cisim -> mantıksal
;iki cisim arasındaki mesafe yarıçaplarının toplamından az mı?

(define (cisim-çarpıştı-mı? oyuncu tehlike) (< (cisim-mesafe oyuncu tehlike) 90))
                                               
 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;KÖY


(define-struct köy (oyuncu hedef tehlike silah SKOR ))


;place-image  imaj vc sahne yön yön -> sahne
;bir sahneye verilen konum vektörü ve yönlere göre bir imaj yerleştirir
(check-expect (place-image/vc (cisim-imaj oyuncu) (make-vc 1 5) BACKGROUND "left" "top")
  (place-image/align (cisim-imaj oyuncu)
                     (vc-x  (make-vc 1 5))(vc-y  (make-vc 1 5))
                     "left" "top"
                     BACKGROUND))
(check-expect (place-image/vc (cisim-imaj oyuncu) (make-vc 1 5) BACKGROUND "left" "top")
  (place-image/align (cisim-imaj oyuncu)
                     (vc-x  (make-vc 1 5))(vc-y  (make-vc 1 5))
                     "left" "top"
                     BACKGROUND))
(define (place-image/vc imaj vc sahne yön1 yön2)
  (place-image/align imaj
                     (vc-x vc)(vc-y vc)
                     yön1 yön2
                     sahne))

;place-cisim cisim sahne -> sahne
;bir cismi verilen yer koordinatlararına göre sahneye yerleştirir
(check-expect (place-cisim oyuncu BACKGROUND "left" "middle")
  (place-image/vc (cisim-imaj oyuncu) (cisim-yer oyuncu) BACKGROUND "left" "middle"))

(define (place-cisim cisim sahne yön1 yön2)
 (place-image/vc (cisim-imaj cisim) (cisim-yer cisim) sahne yön1 yön2))


;köyü-yarat cisim cisim cisim cisim -> sahne
;verilen cisimleri köyde yaratır
(define köyü-yarat (make-köy oyuncu hedef tehlike silah SKOR))

;köy-çiz köy -> sahne
;bir köy alır ve sahne yaratır


(define (köy-çiz k)
    (cond
      ((> (köy-SKOR k) 1500) KILLSCREEN )
        (else 
       (place-image (text (number->string (köy-SKOR k)) 50 "silver") 550 30
                     (place-cisim (köy-oyuncu k )
                       (place-cisim (köy-hedef k )
                          (place-cisim (köy-tehlike k )
                             (place-cisim (köy-silah k )
                                     BACKGROUND-SCENE "left" "middle" )
                                                          "left" "middle" )
                                                              "left" "middle" )
                                                                  "left" "middle")))))


;köy-ilerleme köy -> köy
;bir köy alarak ilerlemiş yeni bir köy verir

(define (köy-ilerleme k)
  (make-köy
   (cisim-ilerleme (köy-oyuncu k))
   (sağdan-sola-dönüş (cisim-ilerleme (köy-hedef k)) (image-width BACKGROUND))
   (soldan-sağa-dönüş (cisim-ilerleme (köy-tehlike k)) (image-width BACKGROUND))
   (cisim-ilerleme(köy-silah k))
   (köy-SKOR k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
(define (oyuncu-yön-kontrol-x x tuş)
  (cond
    ((string=? tuş "left") (- x 15))
    ((string=? tuş "right") (+ x 15)) 
    (else x)))

(define (oyuncu-yön-kontrol-y y tuş)
  (cond
    ((string=? tuş "down") (+ y 15))
    ((string=? tuş "up") (- y 15))
    (else y)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;oyuncu-yer-kontrol vektör tuş -> vektör
(define (oyuncu-yer-kontrol vc tuş)
  (make-vc (oyuncu-yön-kontrol-x (vc-x vc) tuş) (oyuncu-yön-kontrol-y (vc-y vc)tuş)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;oyuncu-hareket cisim tuş -> cisim
(define (oyuncu-hareket cisim tuş)
  (make-cisim (cisim-imaj cisim)
              (oyuncu-yer-kontrol (cisim-yer cisim) tuş)
              (cisim-hız cisim)
              (cisim-ivme cisim)))

(define (silah-hareket k tuş)
 (cond
   ((string=? tuş " ") (make-cisim
                        (cisim-imaj (köy-silah k))
                        (cisim-yer (köy-oyuncu k))
                        (make-vc 10 0)
                        (cisim-ivme (köy-silah k))))
   (else (köy-silah k))))


;köy-tuş köy tuş-> köy
;bir köy alarak tuşlarla kontrol edilen bir köy verir
(define (köy-tuş k tuş )
  (make-köy (oyuncu-hareket (köy-oyuncu k) tuş)
                            (köy-hedef k)
                            (köy-tehlike k)
                            (silah-hareket k tuş)
                            (köy-SKOR k)))

; köy-çarpışma dünya -> dünya
(define (köy-çarpışma1 k)
(cond
  ((cisim-çarpıştı-mı? (köy-oyuncu k) (köy-tehlike k))    (make-köy
                                                           (köy-oyuncu k)
                                                           (köy-hedef k)
                                                          (make-cisim
                                                            (cisim-imaj tehlike)
                                                            (make-vc 1280 (random 150 810))
                                                            (cisim-hız tehlike)
                                                            (cisim-ivme tehlike))
                                                            (köy-silah k)
                                                            (-  (köy-SKOR k) 100)))
  (else k)))

(define (köy-çarpışma2 k)
(cond
  ((cisim-çarpıştı-mı? (köy-oyuncu k) (köy-hedef k))    (make-köy
                                                           (köy-oyuncu k)
                                                          (make-cisim
                                                            (cisim-imaj hedef)
                                                            (make-vc 0 (random 150 810))
                                                            (cisim-hız hedef)
                                                            (cisim-ivme hedef))
                                                          (köy-tehlike k)
                                                            (köy-silah k)
                                                            (+ 100 (köy-SKOR k))))
  (else k)))


(define (köy-çarpışma3 k)
  (cond
    ((cisim-çarpıştı-mı? (köy-silah k) (köy-tehlike k)) (make-köy 
                                                         (köy-oyuncu k)
                                                         (köy-hedef k)
                                                         (make-cisim
                                                          (cisim-imaj tehlike)
                                                          (make-vc 1280 (random 150 810))
                                                          (cisim-hız tehlike)
                                                          (cisim-ivme tehlike))
                                                         (köy-silah k)
                                                         (+ 70 (köy-SKOR k))))
    (else k)))
                                                         
 

(define (köy-yeni k)
  (köy-çarpışma3 (köy-çarpışma2 (köy-çarpışma1 (köy-ilerleme k)))))
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(big-bang köyü-yarat
  (on-tick köy-yeni (/ 1.0 FRAME-RATE))
  (on-draw köy-çiz)
  (on-key köy-tuş))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;