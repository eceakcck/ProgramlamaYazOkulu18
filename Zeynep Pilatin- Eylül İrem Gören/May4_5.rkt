;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname May4_5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Death Star Mission
;Eylül İrem Gören - Zeynep Pilatin
;4.8.18
; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)

(define FRAME-RATE 12)
(define BACKGROUND (scale 0.8 (bitmap "imaj/ds3.jpg")))
(define BACKGROUND-SCENE
(place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))
(define-struct dünya (oyuncu düşman hedef gizem skor))
(define GAME-OVER (bitmap "imaj/yoda.jpeg"))


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
; (place-cisim (make-cisim (circle 10 "solid" "yellow") (make-vc 10 10) (make-vc 99 99) (make-vc 99 99)) (square 100 "solid" "black"))
; (place-image/vc (circle 10 "solid" "yellow") (make-vc 10 10)  (square 100 "solid" "black")))

(define (place-cisim c sahne)
  (place-image/vc (cisim-imaj c) (cisim-yer c) sahne))


;+vc   vc vc --------I>I> vc
;İki vektörün toplamını hesaplamak
;İkisinin toplamı yeni vektör

(define-struct vc (x y))

(check-expect
 (+vc (make-vc 7 5) (make-vc 3 4))
 (make-vc 10 9))

(check-expect
 (+vc (make-vc 8 4) (make-vc 5 1))
 (make-vc 13 5))

(define (+vc v1 v2) (make-vc (+ (vc-x v1) (vc-x v2))
                             (+ (vc-y v1) (vc-y v2))))

;-vc   vc vc --------I>I> vc
;İki vektörün farkını hesaplamak
;İkisinin farkı yeni vektör

(check-expect
 (-vc (make-vc 7 5) (make-vc 3 4))
 (make-vc 4 1))

(check-expect
 (-vc (make-vc 18 7) (make-vc 5 1))
 (make-vc 13 6))

(define (-vc v1 v2) (make-vc (- (vc-x v1) (vc-x v2))
                             (- (vc-y v1) (vc-y v2))))

;veri-yapısı
;cisim
;imaj resim
;yer vc
;hız vc
;ivme vc
;cisimin parçalarının içeren bir veri yapısı oluşturur
(define-struct cisim
  ( imaj yer hız ivme ))

(define obje1
  (make-cisim (star 30 "solid" "silver") (make-vc 0 0) (make-vc 0 0) (make-vc 1 5)))
(define obje2
 (make-cisim (circle 20 "solid" "black") (make-vc 4 7) (make-vc 12 13) (make-vc -1 -3)))
(define obje3
 (make-cisim (circle 35 "solid" "yellow") (make-vc 3 7) (make-vc 10 14) (make-vc 1 3)))
(define obje4
 (make-cisim (circle 35 "solid" "yellow") (make-vc 3 7) (make-vc 10 14) (make-vc 1 3)))
(define obje5
 (make-cisim (triangle 27 "solid" "red") (make-vc 6 9) (make-vc 5 7) (make-vc 2 4)))


(define OYUNCU (make-cisim(bitmap "imaj/obi.png") (make-vc 525 400) (make-vc 0 0) (make-vc 0 0)))
(define DÜŞMAN (make-cisim(bitmap "imaj/dv18.png") (make-vc  1050 500) (make-vc -30 0) (make-vc 0 0)))
(define HEDEF (make-cisim(bitmap "imaj/r2d2.png")  (make-vc 0 200) (make-vc 30 0) (make-vc 0 0)))
(define SKOR 0)
(define ( gizemkonum w )(cisim-yer(dünya-oyuncu w)))
(define GİZEM (make-cisim(bitmap "imaj/ls.png")
                         (make-vc -100 -100)
                         (make-vc 0 0)
                         (make-vc 0 0))) 




;güvenli-sağ: sayı ------> mantıksal
;karakter oyun ekranının içinde mi?
(check-expect(güvenli-sağ? 450)(<= 450 (image-width BACKGROUND)))
(check-expect(güvenli-sağ? 780)(<= 780 (image-width BACKGROUND)))
(define (güvenli-sağ? x) (<= x (image-width BACKGROUND)))

;güvenli-sol: sayı -------->mantıksal
;karakter oyun ekranının içinde mi?
(check-expect(güvenli-sol? 900)(<= 900 (image-width (cisim-imaj DÜŞMAN))))
(check-expect(güvenli-sol? 128)(<= 128 (image-width (cisim-imaj DÜŞMAN))))
(define(güvenli-sol? x)(<= x (image-width (cisim-imaj DÜŞMAN))))

;ekranda mı
;Ekranın içinde olup olmadığını belirlerle
(define (ekranda x)
  (and (güvenli-sol? x) (güvenli-sağ? x))) 

;cisim-döndürme:
;cisim -----> cisim
;Cismin x kordinatı 0'dan küçükse x kordinatını tekrar ekran genişliğinde set ediyor
;(check-expect (cisim-döndürme (make-cisim (circle 10 "solid" "black") (make-vc -1 -1) (make-vc 10 3) (make-vc -1 -2)))
 ;             (make-cisim (circle 10 "solid" "black") (make-vc (image-width BACKGROUND) (random 0 (image-height BACKGROUND))) (make-vc 10 3) (make-vc -1 -2)))
(define (cisim-döndürme c)
  (cond
    ((< (vc-x(cisim-yer c)) 0)(make-cisim
                               (cisim-imaj c) (make-vc (image-width BACKGROUND) (random 0 (image-height BACKGROUND))) (cisim-hız c) (cisim-ivme c)))
    (else c)))

(define (cisim-döndürme2 c)
  (cond
   ((> (vc-x(cisim-yer c)) (image-width BACKGROUND))(make-cisim
                              (cisim-imaj c) (make-vc 0 (random 0 (image-height BACKGROUND))) (cisim-hız c) (cisim-ivme c)))
   (else c)))
              




    

(define (cisim-ilerleme cisim1)
  (make-cisim(cisim-imaj cisim1)
                  (+vc (cisim-hız cisim1) (cisim-yer cisim1)) (+vc (cisim-ivme cisim1) (cisim-hız cisim1)) (cisim-ivme cisim1)))
  


;(check-expect(yön-değiştir(
;cisim-çarptı-mı
;cisim -----> mantıksal
;(check-expect(cisim-çarptı-mı obje1) (<= (image-height BACKGROUND) (vc-y (cisim-yer obje1))))
;(check-expect(cisim-çarptı-mı obje2) (<= (image-height BACKGROUND) (vc-y (cisim-yer obje2))))
(define(cisim-çarptı-mı c)(and
                         (<= (image-height BACKGROUND) (vc-y (cisim-yer c)))
                        (< 0 (vc-y (cisim-ivme c)))))

;yön-değiştirme:
;cisim -------->> cisim
;cismin y'deki hızını ters yönde değiştirir
;(check-expect(yön-değiştirme((mak
(define(yön-değiştir c)(make-cisim (cisim-imaj c)
                 (cisim-yer c) (make-vc (vc-x(cisim-hız c)) (* -1 (vc-y (cisim-hız c))))
                  (cisim-ivme c)))
;cisim-hareket
;cisim----->mantıksal

(define(cisim-hareket cisim)(cond
                           [(cisim-çarptı-mı cisim) (yön-değiştir cisim)]
                           [else (cisim-ilerleme cisim)]))
                              

;dünya-ilerleme: cisim----->dünya



(define(dünya-ilerleme w)(make-dünya
                          (cisim-ilerleme(dünya-oyuncu w))
                          (cisim-döndürme(cisim-ilerleme(dünya-düşman w)))
                          (cisim-döndürme2(cisim-ilerleme(dünya-hedef w)))
                          (cisim-ilerleme(dünya-gizem w))
                          (dünya-skor w)))



;oyuncu-x-kontrol x t -----> x
(define (oyuncu-x-kontrol x t)
  (cond
    [(string=? t "left" )(- x 20)]
    [(string=? t "right")(+ x 20)]
    [else x]))

;oyuncu-y-kontrol y t ------> y
(define (oyuncu-y-kontrol y t)
  (cond
    [(string=? t "up" )(- y 20)]
    [(string=? t "down")(+ y 20)]
    [else y]))
    

;oyuncu-yer-kontrol vc t ----> vc t ---->vc
(define (oyuncu-yer-kontrol v t)
  (make-vc (oyuncu-x-kontrol (vc-x v)t) (oyuncu-y-kontrol (vc-y v)t)))
 

;oyuncu-hareket cisim(c) tuş(t) ---> cisim
(define (oyuncu-hareket c t)
  (make-cisim (cisim-imaj c)
              (oyuncu-yer-kontrol (cisim-yer c)t)
              (cisim-hız c)
              (cisim-ivme c)))

;mg-vc vc--> sayı
;vc vektörünün uzunluğunu hesaplamak
(check-expect(mg-vc (make-vc 3 4)) 5)
(define(mg-vc v)
  (sqrt (+ (sqr (vc-x v)) (sqr (vc-y v)))))

(define (çarptımı? c1 c2)(<= (mg-vc (-vc (cisim-yer c1) (cisim-yer c2)))50))
 
(define (dünya-çarpışma-1 w)(cond
                              ((çarptımı? (dünya-oyuncu w) (dünya-düşman w))
                              (make-dünya (dünya-oyuncu w)
                                          (make-cisim(bitmap "imaj/dv18.png") (make-vc (image-width BACKGROUND) (random 0 (image-height BACKGROUND)) ) (make-vc -30 0) (make-vc 0 0))
                                          (dünya-hedef w)
                                          (dünya-gizem w)
                                          (- (dünya-skor w) 20)))
                              (else w)))
(define (dünya-çarpışma-2 w)(cond
                              ((çarptımı? (dünya-oyuncu w) (dünya-hedef w))
                               (make-dünya (dünya-oyuncu w)
                                           (dünya-düşman w)
                                           (make-cisim(bitmap "imaj/r2d2.png") (make-vc 0 (random 0 (image-height BACKGROUND))) (make-vc 30 0) (make-vc 0 0))
                                           (dünya-gizem w)
                                           (+ 15 (dünya-skor w))))
                               (else w)))
(define (dünya-çarpışma-3 w)(cond
                              ((çarptımı? (dünya-gizem w) (dünya-düşman w))
                              (make-dünya (dünya-oyuncu w)
                                          (make-cisim(bitmap "imaj/dv18.png") (make-vc (image-width BACKGROUND) (random 0 (image-height BACKGROUND)) ) (make-vc -30 0) (make-vc 0 0))
                                          (dünya-hedef w)
                                          GİZEM
                                          (+ (dünya-skor w) 10)))
                              (else w)))
  

(define(dünya-kontrol w) 
  (dünya-çarpışma-1 (dünya-çarpışma-2 (dünya-çarpışma-3 (dünya-ilerleme w)))))


(define (tehlikehizi SKOR)
  ((cond
    ((> SKOR 50) 15)
    ((> SKOR 100) 30)
    ((> SKOR 150) 45)
    ((> SKOR 200) 60)
    ((> SKOR 250) 80)
    (else (+0)))))
    
                                                                                                  
;) 
;dünya----->dünya
 ;,(define (dünya-yeni w)
  ;(dünya-çarpışma-2(dünya-çarpışma-1 (dünya-ilerleme w))))
                         
                                    

;dünya-çiz:
;cisim------>cisim
;Dünyaya verilen cisimleri yerleştirir
;(check-expect(dünya-çiz )
    ;  (place-cisim (dünya-oyuncu obje1)
    ;  (place-cisim (dünya-düşman w)
    ;  (place-cisim (dünya-hedef w)
    ;  (place-cisim (dünya-skor w) BACKGROUND-SCENE))))))

;(check-expect(dünya-çiz obje5 obje4 obje3 obje2 obje1)
;   (place-cisim (dünya-oyuncu obje5)
;   (place-cisim (dünya-düşman obje4)
;   (place-cisim (dünya-hedef obje3)
;   (place-cisim (dünya-skor obje1) BACKGROUND-SCENE))))))

(define (dünya-çiz w)(cond
                       ((< (dünya-skor w) 0)
                       (place-image GAME-OVER
                                    (/ (image-width BACKGROUND) 2) (/ (image-height BACKGROUND) 2)
                       BACKGROUND))
                       
                       (else


                     (place-cisim (dünya-oyuncu w)
                     (place-cisim (dünya-düşman w)
                     (place-cisim (dünya-hedef w)
                     (place-cisim (dünya-gizem w)
                     (place-image (text (string-append "SKOR: " (number->string (dünya-skor w))) 20 "white")
                                  1300 25
                      BACKGROUND-SCENE))))))))


;dünya tuş --> gizem
(define (gizem-güncelle w t)(cond
                              ((string=? t " ")
                                  (make-cisim (cisim-imaj (dünya-gizem w))
                                              (cisim-yer (dünya-oyuncu w))
                                              (make-vc 30 0)
                                              (cisim-ivme (dünya-gizem w))))
                              (else (dünya-gizem w))))
                                              
  
  

;oyuncu-hareket cisim(c) tuş(t)

(define (dünya-tuş w t) (make-dünya (oyuncu-hareket (dünya-oyuncu w)t)
                                    (dünya-düşman w)
                                    (dünya-hedef w)
                                    (gizem-güncelle w t)
                                    (dünya-skor w)))
                                     
(define (dünya-fare w x y m) w)

;yaradılış: cisim---> dünya
;yaratılan dünya içinde bir cisim yaratıp çıktı olarak cismi verir
(define yaradılış (make-dünya OYUNCU DÜŞMAN HEDEF GİZEM SKOR))




(big-bang yaradılış
  (on-tick dünya-kontrol (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare))

