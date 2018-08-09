;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname WELCOMETOTHEVOİİDD) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


;Karin Saka'nın eseridir. ->> WELCOME TO THE VOID

; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)



(define FRAME-RATE 12)
(define BACKGROUND (place-image (text/font "Karin Saka 2018" 20 "lightblue" "Century Gothic" 'swiss 'italic 'bold #f) 420 600
                    (place-image  (text/font "W E L C O M E T O T H E V O I D" 20 "pink" "Century Gothic" 'swiss 'italic 'bold #f) 230 20
                                 (bitmap "welcome-img/açılış-arkaplan.gif"))))
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))

(define-struct cisim
(imaj yer hız ivme))

(define-struct gizem (cisim aktif-mi))

(define-struct dünya (oyuncu tehlike tehlikem ödül skor gizem ))



;;;;VEKTÖRLER
;vc
(define-struct vc ( x y ) )
(define benim-vektörüm
  (make-vc 3 4) )
;vc2
(define vektör-karin
  (make-vc 7 9 ) )

; toplam-vektör vc vc -> vc
;vektörleri toplar
(check-expect ( toplam-vektör (make-vc 7 5) (make-vc 3 4))  ( make-vc 10 9) ) 
(define (toplam-vektör benim-vektörüm vektör-karin)
               (make-vc 
               (+ (vc-x benim-vektörüm) (vc-x vektör-karin))
               ( + (vc-y benim-vektörüm) (vc-y vektör-karin))))


;fark-vektör
;vektörleri çıkarır
(check-expect ( fark-vektör (make-vc 7 5) (make-vc 3 4))  ( make-vc 4 1) ) 
(define (fark-vektör benim-vektörüm vektör-karin)
               (make-vc 
               (- (vc-x benim-vektörüm) (vc-x vektör-karin))
               ( - (vc-y benim-vektörüm) (vc-y vektör-karin))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;DÜNYANIN İÇİNDEKİLERR
;oyuncu-> cisim
;tehlike-> cisim
;ödül-> cisim
;skor-> sayı
;arkaplan-> imaj 

(define OYUNCU (make-cisim (scale 0.4 (bitmap "welcome-img/ayıcıkaptal.png")) (make-vc 230 540)
                                                     (make-vc  0 0 ) (make-vc 0 0 )))
(define TEHLİKE (make-cisim (scale 0.8 (bitmap "welcome-img/düşmanmı.png")) (make-vc 100 0) (make-vc 0 10 ) (make-vc 0 0)))
(define TEHLİKEM (make-cisim (scale 0.4 (bitmap "welcome-img/hançer.png")) (make-vc 200 0 ) (make-vc 0 4) (make-vc 0 0 )))
(define ÖDÜL (make-cisim (scale 0.5 (bitmap "welcome-img/yumurtam.png")) (make-vc 250 0 ) (make-vc 0 8) (make-vc 0 0)))
(define SKOR 50 )
(define GİZEM (make-gizem (make-cisim
                           (scale 0.2 (bitmap "welcome-img/çilekim.png")) (make-vc -500 -500)
                         (make-vc 0 0) (make-vc 0 0))
                          false))
(define OYUN-BİTTİ (place-image  (text/font "E N D OF YOUR STORY" 40 "pink" "Bauhaus 93" 'swiss 'italic 'bold #f) 255 300
                                              (place-image (scale 0.5 (bitmap "welcome-img/açılışayıcığım.png")) 230 400 
                                 (bitmap "welcome-img/yeter.jpg"))))

(define DÜNYAM (make-dünya  OYUNCU TEHLİKE  TEHLİKEM  ÖDÜL SKOR GİZEM))

;(define ARKAPLAN imaj)



;;;;;;;;;;;;;;;;;;;;;;;;CİSİM İLERLETME / ÇARPTI MI? / CİSİM-HAREKET

(define obje1
  (make-cisim (triangle 60 "solid" "darkred") (make-vc 2 2) (make-vc 4 4) (make-vc 3 4 )))
(define obje2
  (make-cisim (square 30 "solid" "pink") (make-vc 1 1) (make-vc 2 2) (make-vc 5 5)))

;cisim-ilerleme cisim -> cisim
;fizik kurallarına göre cisim 1 saniye ilerliyor


(check-expect (cisim-ilerleme obje1) (make-cisim  (triangle 60 "solid" "darkred") (make-vc 6 6 ) (make-vc 7 8 ) (make-vc 3 4) ) )
(check-expect (cisim-ilerleme obje2) (make-cisim (square 30 "solid" "pink") (make-vc 3 3 ) (make-vc 7 7 ) (make-vc 5 5 ) ) )

(define (cisim-ilerleme c)
  (make-cisim (cisim-imaj c )  (toplam-vektör ( cisim-yer c) (cisim-hız c ) )
              (toplam-vektör (cisim-hız c) (cisim-ivme c ) ) (cisim-ivme c ) ) )

;;;;;;;;;CİSİMLERİN GERİ DÖNMESİNİ SAĞLIYORUM
;cisim-çarptı-mı? cisim ->mantıksal
;cismin y kordinatını alarak ekrandan çıkıp çıkmadığnı kontrol eder.
(check-expect (cisim-çarptı-mı? OYUNCU) #false)
(define (cisim-çarptı-mı? cisim) (> (vc-y (cisim-yer cisim)) (image-height BACKGROUND)))

;gizem ekranda mı diye kontrol eder
;gizem -->> mantıksal
(define (gizem-kontrol gizem)(cond
                               [(> (vc-y (cisim-yer (gizem-cisim gizem))) -10)
                                (make-gizem
                                 (cisim-ilerleme (gizem-cisim gizem))
                                 true)]
                               [else (make-gizem
                                 (gizem-cisim gizem)
                                 false)]))


;cisim-random
;cisim çarptıysa rastgele bir konumdan cisim tekrar gelir.
(define (cisim-random cisim) (cond
                               [(cisim-çarptı-mı? cisim ) (make-cisim (cisim-imaj cisim)
                                 (make-vc (random 25 ( - (image-width BACKGROUND) 25 )) 0) (cisim-hız cisim) (cisim-ivme cisim))]
                               [else (cisim-ilerleme cisim) ]))
;rastgele-cisim cisim-> cisim
;oyundaki tehlike ve ödüllerin rastgele gelmesi için yardımcı fonk.
(define (rastgele-cisim c ) (make-cisim (cisim-imaj c)
                                 (make-vc (random 25 ( - (image-width BACKGROUND) 25 )) 0) (cisim-hız c) (cisim-ivme c)))

;;;;;;DÜNYA-İLERLEME

;(define VENÜS (make-dünya obje2) )

;dünya-ilerleme dünya-> dünya
;dünyadaki cismi çağırarak hareket etmesini sağlar
(define (dünya-ilerleme w )
         (make-dünya
          (cisim-ilerleme (dünya-oyuncu w ))
          (cisim-random (cisim-ilerleme (dünya-tehlike w)))
          (cisim-random(cisim-ilerleme (dünya-tehlikem w)))
          (cisim-random (cisim-ilerleme (dünya-ödül w)))
          (dünya-skor w)
          (gizem-kontrol (dünya-gizem w))))

;;;;;;;;;;;;;;;;;;
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
(place-line/vc origin (toplam-vektör v1 v2) "green"
                                     (place-line/vc origin v1 "pink"
                                                    (place-line/vc v1 (toplam-vektör v1 v2)  "yellow" (rectangle 300 225 "solid" "black"))))
;;;;;
;dünya-çiz dünya-> sahne
;dünyanın içindeki verileri kullanarak yeni dünyanın sahnesini verir.
(define (dünya-çiz w) (cond
                       [(<= (dünya-skor w) 0 ) OYUN-BİTTİ ]
                       [else 
                         (place-cisim (dünya-oyuncu w ) (place-cisim (dünya-tehlike w)
                                                                  (place-cisim (dünya-tehlikem w)
                                                                  (place-cisim (dünya-ödül w)
                                                                               (place-image (text/font(number->string (dünya-skor w ))
                                                                                                      20 "pink" "Cooper Black" 'swiss 'normal 'bold #f) 230 50
                                                                                                           (place-cisim (gizem-cisim (dünya-gizem w))
                                                                                                            BACKGROUND-SCENE))))))])) 
;;;;;TUŞLAR KULLANILIYOR
;oyuncu-x-kontrol sayı tuş-> sayı
;oyuncunun x kordinatına tuşlara göre ekleme ve çıkarma yapar.
(define (oyuncu-x-kontrol x t ) (cond
                                 [(string=? t "left") ( - x 30 ) ]
                                 [(string=? t "right") (+ x 30)]
                                 [ else x ]))
;oyuncu-yer-kontrol vektör tuş -> vektör
;oyuncunun x ve y kordinatlarını kontrol eder
(define (oyuncu-yer-kontrol v t ) (make-vc (oyuncu-x-kontrol (vc-x v)t ) (vc-y v)))

;oyuncu-hareket cisim tuş -> cisim
(define (oyuncu-hareket c t ) (make-cisim
                               (cisim-imaj c)
                               (oyuncu-yer-kontrol (cisim-yer c) t )
                               (cisim-hız c) (cisim-ivme c)))


;gizem-güncelle:oyuncu-yer gizem tuş -> gizem
;gizemin
(define (gizem-güncelle w t) (cond
                                              [(and (string=? t " ") (< (vc-y (cisim-yer (gizem-cisim (dünya-gizem w)))) 0))
                                               (make-gizem
                                                (make-cisim (cisim-imaj (gizem-cisim (dünya-gizem w)))
                                                            (cisim-yer (dünya-oyuncu w))
                                                            (make-vc 0 -10)
                                                            (cisim-ivme (gizem-cisim (dünya-gizem w) )))
                                                true)]
                                              [else (dünya-gizem w)]))
                                                                    

;;;;;;OYUN İÇİ ETKİLEŞİMLER
;vc-uzunluk vc-> sayı
;vc vektörünün uzunluğunu hesaplar
;(check-expect (vc-uzunluk (make-vc 0 1) (make-vc 3 5 )) 5)
 (define (vc-uzunluk v)
   (sqrt ( + (sqr (vc-x v)) (sqr (vc-y v))))) 

;;cisim-mesafe: cisim cisim ->sayı
;iki cisim arasındaki mesafeyi ölçer
;(check-expect (cisim-mesafe
(define (cisim-mesafe c1 c2) (vc-uzunluk (fark-vektör (cisim-yer c1) (cisim-yer c2))))

;;çarptı-mı?; cisim cisim2  -> mantıksal 
;iki cisim arasındaki mesafe, yarıçaplarının toplamından az mı? 
(define (çarptı-mı? c1 c2)
  (<= (cisim-mesafe c1 c2) (/( + (image-height (cisim-imaj c1)) (image-height (cisim-imaj c2))) 2)))


;tehlike-çarpışma cisim cisim-> mantıksal 
;tehlike ve oyuncu çarpışırsa ne olacağına karar verir.
(define (tehlike-çarpışma w) (cond
                                   [(çarptı-mı? (dünya-oyuncu w) (dünya-tehlike w)) (make-dünya
                                                        (dünya-oyuncu w)
                                                        (rastgele-cisim (dünya-tehlike w))
                                                        (dünya-tehlikem w)
                                                        (dünya-ödül w)
                                                        (- (dünya-skor w) 30)
                                                        (dünya-gizem w))]
                                   [else w ]))
                                       

;tehlikem-çarpışmam cisim cisim ->mantıksal
;2.tehlike ve oyuncu çarpışırsa ne olacağına karar verir.
(define (tehlikem-çarpışma w) (cond
                                    [(çarptı-mı? (dünya-oyuncu w) (dünya-tehlikem w)) (make-dünya
                                                         (dünya-oyuncu w)
                                                         (dünya-tehlike w)
                                                         (rastgele-cisim (dünya-tehlikem w))
                                                         (dünya-ödül w)
                                                         (- (dünya-skor w) 10)
                                                         (dünya-gizem w))]
                                    [ else w]))
;ödül-çarpışma cisim cisim -> cisim
;ödül ve oyuncu çarpışırsa ne olacağına karar verir.
(define (ödül-çarpışma w) (cond
                                [(çarptı-mı? (dünya-oyuncu w) (dünya-ödül w)) (make-dünya
                                                     (dünya-oyuncu w)
                                                     (dünya-tehlike w )
                                                     (dünya-tehlikem w)
                                                     (rastgele-cisim (dünya-ödül w))
                                                     (+ (dünya-skor w) 50)
                                                     (dünya-gizem w))]
                                [else w ]))
;gizem-çarpışma
;gizem tehlikeye çarptığında ne olacağına karar verir
(define (gizem-çarpışma w)(cond
                            [(çarptı-mı? (gizem-cisim (dünya-gizem w)) (dünya-tehlike w))
                             (make-dünya
                              (dünya-oyuncu w)
                              (rastgele-cisim (dünya-tehlike w))
                              (dünya-tehlikem w)
                              (rastgele-cisim (dünya-ödül w))
                              (+ (dünya-skor w) 20)
                              GİZEM)]
                            [else w]))
                             

;;;dünya-yeni dünya -> dünya
;
(define (dünya-yeni w) (gizem-çarpışma (tehlike-çarpışma (tehlikem-çarpışma (ödül-çarpışma (dünya-ilerleme w))))))
                                                          





;dünya-tuş dünya tuş -> dünya
(define (dünya-tuş w t ) (make-dünya (oyuncu-hareket (dünya-oyuncu w) t)
                                     (dünya-tehlike w)
                                     (dünya-tehlikem w)
                                     (dünya-ödül w)
                                     (cond
                                       [ (and (<= (dünya-skor w) 0 ) (string=? t "a" )) 50 ]
                                       [else (dünya-skor w)])
                                     (gizem-güncelle w t)))


(define (dünya-fare w x y m) w)


;;;;
(define yaradılış (make-dünya OYUNCU TEHLİKE TEHLİKEM ÖDÜL SKOR GİZEM))

             
(big-bang yaradılış
  (on-tick dünya-yeni (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare)) 












               
