;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dragonrun) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;ZEYNEP BATTAL TARAFINDAN YAZILDI;;;;;
;;;;;04 AĞUSTOS 2018;;;;;
;;;;;DRAGON RUN;;;;;
;;;;;CREATIVE COMMONS LICENCE;;;;;
;;CC2;;




; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)

;vc
;x sayı x component
;y sayı component
(define-struct vc(x y))

;(+vc vc1 vc2)-> vc
;iki vectorun toplamını hesaplar
(check-expect (+vc (make-vc  3 5) (make-vc 4 5))
              (make-vc 7 10))


(define (+vc vector1 vector2) (make-vc (+ (vc-x vector1) (vc-x vector2)) (+ (vc-y vector1) (vc-y vector2))))


;(-vc vc1 vc2) --> vc
;iki vectorun farkını hesaplar
(check-expect (-vc (make-vc 5 4) (make-vc 3 2))
              (make-vc 2 2))

(define (-vc vector1 vector2) (make-vc (- (vc-x vector1) (vc-x vector2)) (- (vc-y vector1) (vc-y vector2))))
                       

;cisim
;imaj:resim
;yer:vc
;hız:vc
;ivme:vc


(define-struct cisim
  (imaj yer hız ivme))

;(define obje
 ;  (make-cisim (circle 20 "solid" "yellow")))

;cisim-yenileme cisim-->cisim
;fizik koşullarına göre cisimi ilerletir
(check-expect (cisim-ilerleme (make-cisim (bitmap "thanos.png")
                          (make-vc 5 10)
                           (make-vc 20 30)
                          (make-vc 40 50)))
              (make-cisim (bitmap "thanos.png")
                          (make-vc 25 40)
                          (make-vc 60 80)
                          (make-vc 40 50)))

(define (cisim-ilerleme c)
  (make-cisim (cisim-imaj c)
              (+vc (cisim-yer c) (cisim-hız c))
              (+vc (cisim-hız c) (cisim-ivme c))
              (cisim-ivme c)))


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


       


(define FRAME-RATE 36)
(define BACKGROUND-01 (bitmap "background01.jpg"))
(define BACKGROUND-SCENE-01
  (place-image/align (text "DRAGON RUN-ZEYNEP BATTAL 2018" 20 "white") 0 20 "left" "top"
                     (place-image/align BACKGROUND-01  0 0 "left" "top" (empty-scene (image-width BACKGROUND-01) (image-height BACKGROUND-01)))))

(define BACKGROUND-02 (bitmap "background02.jpg"))
(define BACKGROUND-SCENE-02
   (place-image/align (text "DRAGON RUN-ZEYNEP BATTAL 2018" 20 "white") 0 20 "left" "top"
                      (place-image/align BACKGROUND-02  0 0 "left" "top" (empty-scene (image-width BACKGROUND-02) (image-height BACKGROUND-02)))))

(define BACKGROUND-03 (bitmap "background03.jpg"))
(define BACKGROUND-SCENE-03
   (place-image/align (text "DRAGON RUN-ZEYNEP BATTAL 2018" 20 "white") 0 20 "left" "top"
                      (place-image/align BACKGROUND-03  0 0 "left" "top" (empty-scene (image-width BACKGROUND-03) (image-height BACKGROUND-03)))))

;mermi
;görünüyor--> mantıksal
;c--> cisim
(define-struct mermi (görünüyor cisim))

;(define örnek-dünya-1

;karakter: ejderha (cisim)
;düşman: insan (cisim)
;ödül: yumurta (cisim)
;seviye: 1000 skorda yeni arkaplan ve farklı hız (sayı)
;skor: sayı
;mermi: mermi 
600
(define-struct dünya (karakter düşman ödül seviye skor mermi))

(define KARAKTER (make-cisim (scale 1.1 (bitmap "dragon.png"))
                             (make-vc 100 100)
                             (make-vc 0 0)
                             (make-vc 0 0)))
(define DÜŞMAN (make-cisim (scale 0.700 (bitmap "düşman.png"))
                           (make-vc 400 100)
                           (make-vc -13 0)
                           (make-vc 0 0)))
(define ÖDÜL (make-cisim (scale 0.600 (bitmap "DragonEgg.png"))
                         (make-vc 600 (random 100 500))
                         (make-vc -10 0)
                         (make-vc 0 0)))
(define SKOR 0)

  
(define MERMİ (make-mermi false (make-cisim (bitmap "flame.png")
                               (make-vc 0 0)
                               (make-vc 5 0)
                               (make-vc 0 0))))


  
                            



;mermi-ilerleme: mermi  -> mermi
(define (mermi-ilerleme m) (mermi-yok (make-mermi (mermi-görünüyor m) (cisim-ilerleme (mermi-cisim m)))))  

(define örnek-dünya-1 (make-dünya KARAKTER DÜŞMAN ÖDÜL 1 50 MERMİ))

;mermi-yok: mermi--> mermi
;mermiyi ekranın dışına çıkınca görünmez yapar

(define (mermi-yok m)
  (cond
    [(> (vc-x (cisim-yer (mermi-cisim m))) 1100)
     (make-mermi false (mermi-cisim m))]
    [else m]))
    
                   



                                  



;background-seç sayı-->görsel
;sayıya göre background belirler

(check-expect (background-seç 1) BACKGROUND-SCENE-01)
(check-expect (background-seç 2) BACKGROUND-SCENE-02)
(check-expect (background-seç 3) BACKGROUND-SCENE-03)

(define (background-seç seviye)
  (cond
    [(<= seviye 1) BACKGROUND-SCENE-01]
    [(<= seviye 2) BACKGROUND-SCENE-02]
    [else BACKGROUND-SCENE-03]))


;dünya-bölüm dünya --> dünya

(define (dünya-bölüm w)
  (cond
    [(>= (dünya-skor w) 200) (make-dünya (dünya-karakter w) (dünya-düşman w) (dünya-ödül w) 3 (dünya-skor w) (dünya-mermi w))]
    [(>= (dünya-skor w) 100) (make-dünya (dünya-karakter w) (dünya-düşman w) (dünya-ödül w) 2 (dünya-skor w) (dünya-mermi w))]
    (else w)))
                                  
                                              
                                  
                     
                     
(define (dünya-ilerleme w) (make-dünya
                            (cisim-ilerleme (dünya-karakter w))
                            (soldan-sağa-dönüş (cisim-ilerleme (dünya-düşman w)) (image-width BACKGROUND-01))
                            (soldan-sağa-dönüş (cisim-ilerleme (dünya-ödül w)) (image-width BACKGROUND-01))
                            
                            (dünya-seviye w)
                            
                            (dünya-skor w)
                            (mermi-ilerleme(dünya-mermi w))))


(define game-over (place-image (text "OYUN BİTTİ\n YENİDEN BAŞLAMAK İÇİN 'G' TUŞUNA BASIN"  50 "white") 550 240(rectangle 1500 1000 "solid" "black")))



;dünya --> görsel
;dünya alıp background verir

(define (dünya-çiz w)
(cond
  [(< (dünya-skor w) 0) game-over]
  [else 
  (place-flame (dünya-mermi w)
                                   (place-cisim (dünya-ödül w)
                                   (place-cisim (dünya-düşman w)
                                   (place-image (text (number->string
                                   (dünya-skor w)) 50 "black") 550 40
                                   (place-cisim (dünya-karakter w)
                                   (background-seç (dünya-seviye w)))))))]))

;place-flame mermi sahne-->sahne
;doğru değilse sahneyi döndürür

(define (place-flame m s)
  (cond
    [(mermi-görünüyor m ) (place-cisim (mermi-cisim m) s)]
    [else s]))

;soldan-sağa-dönüş: cisim sayı-->cisim
;soldan çıkan cismi sağdan rastlantısal bir yükseklikte geri getirir


(define (soldan-sağa-dönüş c g)
  (cond
    [(< (vc-x (cisim-yer c)) -100) (make-cisim (cisim-imaj c) (make-vc g (random 10 500)) (cisim-hız c) (cisim-ivme c))]
    [else c]))

                                                           
;oyuncu-güncelle: cisim x y-->cisim
;cismin x ve y yer koordinatlarıyla hareketini sağlar

(define (oyuncu-güncelle c x y)
  (make-cisim (cisim-imaj c)
              (make-vc (+ x (vc-x (cisim-yer c)))
                       (+ y (vc-y (cisim-yer c))))
                       (cisim-hız c)
                       (cisim-ivme c)))

;uzaklık cisim cisim--> sayı

(define (uzaklık v) (sqrt (+ (sqr (vc-x v))
                             (sqr (vc-y v)))))




                                  
;cisim-çarptı-mı? cisim cisim--> mantıksal
;iki cisim arasındaki mesafenin yarıçaplarının toplamından az olup olmadığına karar verir.

                                             
                                             
(define (cisim-çarptı-mı? c1 c2) (< (uzaklık (-vc (cisim-yer c1) (cisim-yer c2))) (/ (+ (image-height (cisim-imaj c1)) (image-height (cisim-imaj c2))) 2))) 

;dünya-kontrol dünya--> dünya

(define (dünya-kontrol w) (dünya-bölüm (çarpışma-kontrol (dünya-ilerleme w))))

;çarpışma-kontrol dünya --> dünya

(define (çarpışma-kontrol w)
  (cond 
    [(cisim-çarptı-mı? (dünya-karakter w) (dünya-düşman w))
     (make-dünya (dünya-karakter w)
                 (cisim-ilerleme (make-cisim (cisim-imaj (dünya-düşman w)) (make-vc -100 0) (cisim-hız (dünya-düşman w)) (cisim-ivme (dünya-düşman w))))                           
                 (dünya-ödül w)
                 (dünya-seviye w)
                 (- (dünya-skor w) 20)
                 (dünya-mermi w))]
    [(cisim-çarptı-mı? (mermi-cisim (dünya-mermi w)) (dünya-düşman w))
     (make-dünya (dünya-karakter w)
                 (cisim-ilerleme (make-cisim (cisim-imaj (dünya-düşman w)) (make-vc -100 0) (cisim-hız (dünya-düşman w)) (cisim-ivme (dünya-düşman w))))                                            
                 (dünya-ödül w)
                 (dünya-seviye w)
                 (+ (dünya-skor w) 30)
                 (make-mermi false (mermi-cisim (dünya-mermi w))))]
     [(cisim-çarptı-mı? (dünya-karakter w) (dünya-ödül w))
     (make-dünya (dünya-karakter w)
                 (dünya-düşman w)
                 (cisim-ilerleme (make-cisim (cisim-imaj (dünya-ödül w)) (make-vc -100 0) (cisim-hız (dünya-ödül w)) (cisim-ivme (dünya-ödül w))))                                            
                
                  (dünya-seviye w)
                 (+ (dünya-skor w) 40)
                 (dünya-mermi w))]
    [else w]))
         
                                  
(define (dünya-tuş w t)
  (cond
    [(string=? t "w") (make-dünya (oyuncu-güncelle (dünya-karakter w) 0 -20)
                                  (dünya-düşman w)
                                  (dünya-ödül w)
                                  (dünya-seviye w)
                                  (dünya-skor w)
                                  (dünya-mermi w))]
    [(string=? t "s") (make-dünya (oyuncu-güncelle (dünya-karakter w) 0 20)
                                  (dünya-düşman w)
                                  (dünya-ödül w)
                                  (dünya-seviye w)
                                  (dünya-skor w)
                                  (dünya-mermi w))]
    [(string=? t "a") (make-dünya (oyuncu-güncelle (dünya-karakter w) -20 0 )
                                  (dünya-düşman w)
                                  (dünya-ödül w)
                                  (dünya-seviye w)
                                  (dünya-skor w)
                                  (dünya-mermi w))]
    [(string=? t "d") (make-dünya (oyuncu-güncelle (dünya-karakter w) 20 0 )
                                  (dünya-düşman w)
                                  (dünya-ödül w)
                                  (dünya-seviye w)
                                  (dünya-skor w)
                                  (dünya-mermi w))]  
    [(string=? t " ") (make-dünya (dünya-karakter w) 
                                  (dünya-düşman w)
                                  (dünya-ödül w) 
                                  (dünya-seviye w)
                                  (dünya-skor w)
                                  (make-mermi true (make-cisim (cisim-imaj (mermi-cisim (dünya-mermi w))) (+vc (make-vc 350 160) (cisim-yer (dünya-karakter w))) (cisim-hız (mermi-cisim (dünya-mermi w)))
                                                               (cisim-ivme (mermi-cisim (dünya-mermi w))))))]
    [(and (string=? t "g") (<= (dünya-skor w) 0)) örnek-dünya-1]
    
              [else w]))

                             
                       
                                    
              

(define (dünya-fare w x y m) w)

(define yaradılış örnek-dünya-1)


                                          
                                          


(big-bang yaradılış
  (on-tick dünya-kontrol (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare))





                             
