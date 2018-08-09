; Hüseyin Yağız DEVRE
; 04.08.2018
; hdevre22@my.uaa.k12.tr
;Creative Common Lisansı
;CC2
; bootstrapworld reactive (ish) racket versiyonu
#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require (only-in racket/gui/base play-sound))
(require test-engine/racket-tests)

; ***** vector/cisim image fonksiyonları
; ***** fonksiyonları buradan kopyalanabilir

; place-image/vc resim vc scene -> scene
; bir sahneye vectora göre bir imaj yerleştir
;(check-expect
; (place-image/vc (circle 10 "solid" "yellow") (make-vc 5 10) (square 30 "solid" "red"))
; (place-image/align (circle 10 "solid" "yellow") 5 10 "left" "top" (square 30 "solid" "red")))

(define (place-image/vc im v sahne)
  (place-image/align im (vc-x v) (vc-y v) "left" "top"  sahne))

; place-line/vc vc vc color scene -> scene
; add line from v1 to v2 to scene
;(check-expect
; (place-line/vc (make-vc 20 30) (make-vc 30 40) "black" (rectangle 200 100 "solid" "white"))
 ;(add-line (rectangle 200 100 "solid" "white") 20 30 30 40 "black"))

(define (place-line/vc v1 v2 renk sahne)
  (add-line sahne (vc-x v1) (vc-y v1) (vc-x v2) (vc-y v2) renk ))

; place-cisim cisim scene -> scene
; bir cisim kendi yer kordinatlarına göre sahneye yerleştir
(check-expect
 (place-cisim (make-cisim (circle 10 "solid" "yellow") (make-vc 10 10) (make-vc 99 99) (make-vc 99 99)) (square 100 "solid" "black"))
 (place-image/vc (circle 10 "solid" "yellow") (make-vc 10 10)  (square 100 "solid" "black")))

(define (place-cisim c sahne)
  (place-image/vc (cisim-imaj c) (cisim-yer c) sahne))

(define kaktüs1 (bitmap "imaj/kaktüs1.png"))
(define kaktüs2 (bitmap "imaj/kaktüs2.png"))
(define uçan (bitmap "imaj/ucan.png"))

;Cisim
;imaj:resim
;yer:vc
;hız:vc
;ivme:vc
(define-struct dünya ( c enemyc1 enemyc2 enemyc3 skor met1 hı ))


; +vc vcvc ---> vc
;iki vektörün toplamını hesaplamak

(define-struct vc (x y))
(define vektör (make-vc 3 9))
(vc-x vektör)

;(check-expect
;(+vc (make-vc 7 5 ) (make-vc 3 4 ))
;(make-vc 10 9))

;(check-expect
;(+vc (make-vc 12 2 ) (make-vc 8 9 ))
;(make-vc 20 11)) 

(define vector1 (make-vc 3 9))
(define vector2 (make-vc 6 18))

;cisim-ilerleme cisim--->cisim
;fizik kurallarına göre cisim 1 saniye ilerliyor

(define-struct cisim
  ( imaj yer hız ivme))

;(define obje1
;  (make-cisim (circle 20 "solid""blue") (make-vc 3 3 ) (make-vc 4 5 ) (make-vc -3 -7 ) ))

(define skor 1)
(define HIskor 0)
(define (yüksekSkor w)
  (cond
    ((<= (dünya-hı w) (dünya-skor w))(dünya-skor w))
    ((>  (dünya-hı w) (dünya-skor w)) (dünya-hı w))))
    

(define dino
  (make-cisim   (bitmap "imaj/indir2.png") (make-vc 100 165) (make-vc 0 0) (make-vc 0 1)))
(define enemy1kaktüs
  (make-cisim   (bitmap "imaj/kaktüs1.png") (make-vc (random 700 900) 135) (make-vc (random -19 -10) 0) (make-vc 0 0)))
(define enemy2kaktüs
  (make-cisim   (bitmap "imaj/kaktüs2.png") (make-vc (random 700 900) 135) (make-vc (random -19 -10) 0) (make-vc 0 0)))
(define enemy3uçan
  (make-cisim   (bitmap "imaj/ucan.png") (make-vc 700 (random 140 170)) (make-vc (random -19 -10) 0) (make-vc 0 0)))
(define meteor1
  (make-cisim   (bitmap "imaj/meteor11.png") (make-vc (random 100 600)-100) (make-vc 0 7) (make-vc 0 0)))

(define meteor3
  (make-cisim   (bitmap "imaj/meteor 3.png") (make-vc 300  0) (make-vc 0 -15 ) (make-vc 0 0)))


;(check-expect
;(+vc (make-vc 7 5 ) (make-vc 3 4 ))
;(make-vc 10 9))

(define (+vc v1 v2)
  (make-vc
   (+ (vc-x v1) (vc-x v2))
   (+ (vc-y v1) (vc-y v2))))

;(define obje2
; (make-cisim  (make-vc 4 2 ) (make-vc 3 6 ) (make-vc -9 -1 ) ))

;cisim-ilerleme cisim--->cisim
;fizik kurallarına göre cisim 1 saniye ilerliyor

;cisim-çarptı-mı?
;cisim--->mantıksal

;cisim-hareket
;cisim--->cisim
;cisim çarptıysa yönü değiştir
;çarptıysa cisim-ilerleme fonksiyonunu çağıırır

(define (yön-değiştir ci2)
  
  ( make-cisim  (cisim-imaj ci2)  (cisim-yer ci2)
                (make-vc (vc-x( cisim-hız ci2)) ( * -1 (vc-y (cisim-hız ci2)))) (cisim-ivme ci2))) 

(define (cisim-çarptı-mı c) 
  (and
   (<=(image-height BACKGROUND-SCENE) (vc-y (cisim-yer c)))
   (< 0 (vc-y(cisim-hız c )))))
 
(define (cisim-hareket ci1)
  (cond
    ((cisim-çarptı-mı ci1)  (yön-değiştir ci1))
    (else (cisim-ilerleme ci1))))
                   
;cisim-ilerleme cisim--->cisim
;fizik kurallarına göre cisim 1 saniye ilerliyor

;cisim--->cisim
;oyuncuyu hareket ettirir
(define (cisim-ilerleme ci)
  (make-cisim
   (cisim-imaj ci)
   (+vc (cisim-yer ci) (cisim-hız ci))
   (+vc (cisim-hız ci)( cisim-ivme ci))
   (cisim-ivme ci)))

(define FRAME-RATE 24)
(define (ArkaPlanDeğiştir w)
  (cond
    ((<= (dünya-skor w) 700) (rectangle 700 250 "solid""white"))
    ((> (dünya-skor w) 700) (rectangle 700 250 "solid""black"))))

(define (YazıDeğiştir w)
  (cond
    ((<= (dünya-skor w) 700) (text/font   " İnternet Bağlantısı Yok 2.0" 20 "black" "Gill Sans" 'swiss 'normal 'bold #f))
    ((> (dünya-skor w) 700) (text/font   " İnternet Bağlantısı Yok 2.0" 20 "gray" "Gill Sans" 'swiss 'normal 'bold #f))))

(define (YazıDeğiştir1 w)
  (cond
    ((<= (dünya-skor w) 700) "black" )
    ((> (dünya-skor w) 700)  "gray" )))

;(define (ay w)
; (cond
;  ((< (dünya-skor w) 700) (bitmap"imaj/ay")
;  ((>= (dünya-skor w) 700) (bitmap "imaj/ay" )))
  (define (moon w)
    (cond
     ((<= (dünya-skor w) 700) (rectangle 1 1 "solid""white"  ))
    ((> (dünya-skor w) 700)  (bitmap "imaj/ay.png"))))



;ArkaPlan resim ---> resim:
(define BACKGROUND (bitmap "imaj/ap.png"))
(define (BACKGROUND-SCENE w)
  (overlay/offset (overlay/offset(overlay/offset(overlay/offset(overlay/offset
                                                 BACKGROUND
                                                 0 -140
                                                 (bitmap "imaj/bulut.png"))
                                                300  -20
                                                (bitmap "imaj/bulut.png"))
                                 -300 -20
                                 (bitmap "imaj/bulut.png"))
                                 150 -50
                             (moon w)  )
                  0 0
                  (ArkaPlanDeğiştir w) ))

;dünya
;c :cisim
; dünya-ilerleme dünya--->dünya

(define (dünya-ilerleme w)
  (make-dünya (güvenliY(güvenliX (yerde-dur (cisim-ilerleme (dünya-c w)))))
              (tekrar( cisim-ilerleme (dünya-enemyc1 w)))
              (tekrar( cisim-ilerleme (dünya-enemyc2 w)))
              (tekrar2( cisim-ilerleme (dünya-enemyc3 w)))
              (+(dünya-skor w)1)
              (tekrar3( cisim-ilerleme (dünya-met1 w)))
              (yüksekSkor w)
             ))
 (define (game-over w1)
      (place-image (text/font  (string-append "HI SCORE: " (number->string (yüksekSkor w1))) 20 "LightSteelBlue"
             #f 'modern 'italic 'normal #f)
  350 20
                   (place-image (bitmap "imaj/kaybettiniz.jpg")  350 125
                  (rectangle 700 250 "solid""black"))))

(define (kazan w1)
   (place-image (text/font  (string-append "HI SCORE: " (number->string (yüksekSkor w1))) 20 "LightSteelBlue"
             #f 'modern 'italic 'normal #f)
 350 20
     (place-image (bitmap "imaj/kazandın.png")  350 125
                  (rectangle 700 250 "solid""black"))))

              
(define (dünya-çiz w1 )
  (cond
     ((>= (dünya-skor w1) 1400) (kazan w1) )
    ((<= (dünya-skor w1) 0) (game-over w1) )
  (else 
          (place-cisim (dünya-met1 w1)
               
                            (place-cisim (dünya-c w1)
                                         (place-cisim (dünya-enemyc1 w1)
                                                      (place-cisim (dünya-enemyc2 w1)
                                                                   (place-cisim (dünya-enemyc3 w1)
                                                                                (place-image (text/font  (string-append "HI SCORE: " (number->string (yüksekSkor w1))) 20 (YazıDeğiştir1 w1) "Gill Sans" 'swiss 'normal 'bold #f)  100 20
                                                                                             (place-image (text/font  (string-append "SCORE : " (number->string (dünya-skor w1))) 20 (YazıDeğiştir1 w1) "Gill Sans" 'swiss 'normal 'bold #f)  600 20
                                                                                                          (place-image (YazıDeğiştir w1)   330 235
                                                                                                                       (BACKGROUND-SCENE w1))))))))))))

(define (-vc v1 v2)
  (make-vc
   (- (vc-x v1) (vc-x v2))
   (- (vc-y v1) (vc-y v2))))

  

(define (uzaklık v)
  (sqrt(+(sqr ( vc-x v )) (sqr ( vc-y v)))))
  
(define (çarpıştı? oyuncu düşman)
  ( <= (uzaklık ( -vc (cisim-yer oyuncu) (cisim-yer düşman)))  (+(/ (image-height (cisim-imaj oyuncu)) 2)(/ (image-height (cisim-imaj düşman) ) 2) -20)) )

(define (dünyadaÇarpışma w)
  (cond
   
    ((çarpıştı? (dünya-c w) (dünya-enemyc1 w)) (begin (play-sound "ses/sonar.wav" true)true)(make-dünya (dünya-c w)(dünya-enemyc1 w)(dünya-enemyc2 w)(dünya-enemyc3 w)(- (dünya-skor w) 100000000000000000000000000000000 ) (dünya-met1 w)(dünya-hı w)))
    ((çarpıştı? (dünya-c w) (dünya-enemyc2 w)) (begin (play-sound "ses/sonar.wav" true)true)(make-dünya (dünya-c w)(dünya-enemyc1 w)(dünya-enemyc2 w)(dünya-enemyc3 w)(- (dünya-skor w) 100000000000000000000000000000000)(dünya-met1 w)(dünya-hı w)))
    ((çarpıştı? (dünya-c w) (dünya-enemyc3 w)) (begin (play-sound "ses/sonar.wav" true)true)(make-dünya (dünya-c w)(dünya-enemyc1 w)(dünya-enemyc2 w)(dünya-enemyc3 w)(- (dünya-skor w) 100000000000000000000000000000000 )(dünya-met1 w)(dünya-hı w)))
    ((çarpıştı? (dünya-c w) (dünya-met1 w)) (begin (play-sound "ses/sonar.wav"true)true)(make-dünya (dünya-c w)(dünya-enemyc1 w)(dünya-enemyc2 w)(dünya-enemyc3 w)(- (dünya-skor w) 100000000000000000000000000000000 ) (dünya-met1 w)(dünya-hı w)))
    (else w)))
  

(define (dünyaKontrol w)
  (dünyadaÇarpışma ( dünya-ilerleme w )))

; yerde-dur cisim ---> cisim
; Oyuncunun yere değip değmediğini gösterir 
(define (yerde-dur c) 
  (cond
    ((and (> (vc-y (cisim-hız c)) 0) (<= (vc-y (cisim-yer c)) 165)) 
     (make-cisim 
      (cisim-imaj c)
      (make-vc (vc-x (cisim-yer c)) (vc-y (cisim-yer c)))
      (make-vc 0 15)
      (make-vc 0 3))) 

    ( (> (vc-y (cisim-yer c)) 165) 
      (make-cisim 
       (cisim-imaj c)
       (make-vc (vc-x (cisim-yer c)) 165)
       (make-vc 0 0)
       (make-vc 0 0)))
    (else 
     (make-cisim
      (cisim-imaj c)
      (cisim-yer c)
      (cisim-hız c)
      (cisim-ivme c)))))

(define (güvenliX c)
  (cond
    ( (<= (vc-x (cisim-yer c)) 25)
      (make-cisim 
       (cisim-imaj c)
       (make-vc 25 (vc-y (cisim-yer c)))
      (make-vc 0 (vc-y (cisim-hız c)))
      (make-vc 0 (vc-y (cisim-ivme c)))))
    ( (>= (vc-x (cisim-yer c)) 625)
      (make-cisim 
       (cisim-imaj c)
       (make-vc 625 (vc-y (cisim-yer c)))
       (make-vc 0 (vc-y (cisim-hız c)))
        (make-vc 0 (vc-y (cisim-ivme c)))))
    (else 
     (make-cisim
      (cisim-imaj c)
      (cisim-yer c)
      (cisim-hız c)
      (cisim-ivme c)))))
;bugsız olması için 57 yapın
(define (güvenliY c)
  (cond
    ( (<= (vc-y (cisim-yer c)) 50)
      (make-cisim 
       (cisim-imaj c)
       (make-vc (vc-x (cisim-yer c)) 50)
       (make-vc 0 57)
        (make-vc 0 (vc-y (cisim-ivme c)))))
   
    (else 
     (make-cisim
      (cisim-imaj c)
      (cisim-yer c)
      (cisim-hız c)
      (cisim-ivme c)))))

; tekrar cisim ---> cisim
; düşmanları tekrarlı bir biçimde hareket ettirir
(define (tekrar c) 
  (cond
    ( (<= (vc-x (cisim-yer c)) 0) 
      (make-cisim 
       (cisim-imaj c)
       (make-vc (random 700 900) (vc-y (cisim-yer c)) )
       (make-vc(random -19 -10) 0)
       (cisim-ivme c)))
    (else 
     (make-cisim
      (cisim-imaj c)
      (cisim-yer c)
      (cisim-hız c)
      (cisim-ivme c)))))
(define (tekrar2 c) 
  (cond
    ( (<= (vc-x (cisim-yer c)) 0) 
      (make-cisim 
       (cisim-imaj c)
       (make-vc (random 700 900) (random 140 170) )
       (make-vc(random -19 -10) 0)
       (cisim-ivme c)))
    (else 
     (make-cisim
      (cisim-imaj c)
      (cisim-yer c)
      (cisim-hız c)
      (cisim-ivme c)))))
(define (tekrar3 c) 
  (cond
    ( (>= (vc-y (cisim-yer c)) 170) 
      (make-cisim 
       (cisim-imaj c)
       (make-vc (random 100 600) -100)  
       (make-vc 0 7)
       (cisim-ivme c)))
    (else 
     (make-cisim
      (cisim-imaj c)
      (cisim-yer c)
      (cisim-hız c)
      (cisim-ivme c)))))

(define (tekrar4 c) 
  (cond
    ( (>= (vc-y (cisim-yer c)) 170) 
      (make-cisim 
       (cisim-imaj c)
       (make-vc  0 (random 0 200))  
       (make-vc (random 5 30) (random 5 30))
       (cisim-ivme c)))
    (else 
     (make-cisim
      (cisim-imaj c)
      (cisim-yer c)
      (cisim-hız c)
      (cisim-ivme c)))))

;yukarı-hız-ver cisim -> cisim

(define (yukarı-hız-ver c)
  (make-cisim
   (bitmap "imaj/indir2.png")
   (cisim-yer c)
   (make-vc (vc-x (cisim-hız c)) -15)
   (cisim-ivme dino)))

(define (aşağı-hız-ver c)
  (make-cisim
   (bitmap "imaj/egilen.png")
   (make-vc (vc-x(cisim-yer c)) 165)
   (make-vc 0 0)
   (make-vc 0 0)))



(define (sol-hız-ver c)
  (cond
    ((= (vc-y (cisim-yer c)) 165) 
     (make-cisim
      (bitmap "imaj/indir2.png")
      (cisim-yer c)
      (make-vc -15 0)
      (make-vc 0 0)))
    (else c)))

(define (sağ-hız-ver c)
  (cond
    ((= (vc-y (cisim-yer c)) 165) 
     (make-cisim
      (bitmap "imaj/indir2.png")
      (cisim-yer c)
      (make-vc 15 0)
      (make-vc 0 0)))
    (else c)))


  
 
(define (dünya-tuş w t)
  (cond
    
    ((key=? t "up")
     (make-dünya 
      (yukarı-hız-ver (dünya-c w))
      (dünya-enemyc1 w)
      (dünya-enemyc2 w)
      (dünya-enemyc3 w)
      (dünya-skor w)
      (dünya-met1 w)
       (dünya-hı w)
      ))
    
    ((key=? t "down")
     (make-dünya 
      (aşağı-hız-ver (dünya-c w))
      (dünya-enemyc1 w)
      (dünya-enemyc2 w)
      (dünya-enemyc3 w)
      (dünya-skor w)
      (dünya-met1 w)
       (dünya-hı w)
      ))
    
    ((key=? t "right")
     (make-dünya 
      (sağ-hız-ver (dünya-c w))
      (dünya-enemyc1 w)
      (dünya-enemyc2 w)
      (dünya-enemyc3 w)
      (dünya-skor w)
      (dünya-met1 w)
      (dünya-hı w)
      ))
    ((key=? t "left")
     (make-dünya 
      (sol-hız-ver (dünya-c w))
      (dünya-enemyc1 w)
      (dünya-enemyc2 w)
      (dünya-enemyc3 w)
      (dünya-skor w)
      (dünya-met1 w)
      (dünya-hı w)
      ))
 
    ( (and (or ( <=(dünya-skor w) 0) ( >=(dünya-skor w) 1400)) (string=? t " ") )
     (make-dünya 
      (make-cisim
   (bitmap "imaj/indir2.png")
   (make-vc 100 165)
   (make-vc 0 0)
   (make-vc 0 0))
       (make-cisim   (bitmap "imaj/kaktüs1.png") (make-vc (random 700 900) 135) (make-vc (random -19 -10) 0) (make-vc 0 0))
     
      (make-cisim   (bitmap "imaj/kaktüs2.png") (make-vc (random 700 900) 135) (make-vc (random -19 -10) 0) (make-vc 0 0))
      
      (make-cisim   (bitmap "imaj/ucan.png") (make-vc 700 (random 140 170)) (make-vc (random -19 -10) 0) (make-vc 0 0))
      1
      (make-cisim   (bitmap "imaj/meteor11.png") (make-vc (random 100 600)-100) (make-vc 0 7) (make-vc 0 0))
      (dünya-hı w)
     ))
    

  

    (else w)))
     
(define (dünya-fare w x y m) w)

(define yaradılış (make-dünya
                   dino
                   enemy1kaktüs
                   enemy2kaktüs
                   enemy3uçan
                   skor
                   meteor1
                   HIskor
                  ))

(big-bang yaradılış
  
  (on-tick dünyaKontrol (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)

  (on-mouse dünya-fare))

;Merhaba Dünyaylılar
;Beni Liderinize Götürün
