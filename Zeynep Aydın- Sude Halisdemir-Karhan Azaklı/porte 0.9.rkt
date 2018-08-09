#lang racket
;CONTACT:
;Zeynep Aydın zeynepaydin2009@gmail.com 
;Zeynep Sude Halisdemir zsude219@gmail.com 
;Karhan Azaklı karhanazakli01@gmail.com
;04.08.2018
;Creative Commons License CC BY 2.0

; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)
(require (only-in racket/gui/base play-sound))
(require test-engine/racket-tests)

(define FRAME-RATE 12)
(define BACKGROUND (bitmap "imaj/backback.png"))
(define YENİLGİ (place-image (bitmap "imaj/notalar.png")
                                430 500
                                (rectangle 936 655 "solid" "maroon")))
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))
;vc
; x sayı
; y sayı
(define-struct vc (x y))
(define vec (make-vc 6 8))
(define vec2 (make-vc 3 7))
;+vc vc vc --> vc
;iki vektörün toplamını hesaplar.
(check-expect (+vc (make-vc 7 5) (make-vc 4 8)) (make-vc 11 13))
(check-expect (+vc (make-vc 2 5) (make-vc 1 9)) (make-vc 3 14))
(define (+vc vc1 vc2) (make-vc (+ (vc-x vc1) (vc-x vc2)) (+ (vc-y vc1) (vc-y vc2))))

;-vc vc vc --> vc
;iki vektörün birbirinden çıkarımını hesaplar.
(check-expect (-vc (make-vc 3 8) (make-vc 2 8)) (make-vc 1 0))
(check-expect (-vc (make-vc 6 3) (make-vc 5 1)) (make-vc 1 2))
(define (-vc vc1 vc2) (make-vc (- (vc-x vc1) (vc-x vc2)) (- (vc-y vc1) (vc-y vc2))))
;nota-getir sayı--> cisim
;fonksiyon çağrıldığında tanımlanmış random nota getirir.
(check-expect (nota-getir 7) SOL)
(check-expect (nota-getir 4) RE)
(define (nota-getir x )
  (cond
    ((= x 0) (nota-getir  (random 8)))
    ((= x 1) LA)
    ((= x 2) Sİ)
    ((= x 3) DO)
    ((= x 4) RE)
    ((= x 5) Mİ)
    ((= x 6) FA)
    ((= x 7) SOL)))

;cisim
;imaj:resim
;yer:vc
;hız:vc
;ivme:vc
(define-struct cisim (açıklama imaj ses yer hız ivme))
(define LA  (make-cisim "La" (rotate 315(ellipse 20 35 "solid" "gold")) "notases/la.wav" (make-vc 930   (/ (image-height BACKGROUND) 2)) (make-vc -8 0) (make-vc 0 0)))
(define Sİ (make-cisim "Si" (rotate 315 (ellipse 20 35 "solid" "brown"))"notases/ti.wav"  (make-vc 930 (-  (/ (image-height BACKGROUND) 2) 25)) (make-vc -8 0) (make-vc 0 0) ))
(define DO (make-cisim "Do" (rotate 315 (ellipse 20 35 "solid" "pink"))"notases/do.wav" (make-vc 930 (- (/ (image-height BACKGROUND) 2) 50))  (make-vc -8 0) (make-vc 0 0)))
(define RE (make-cisim "Re" (rotate 315 (ellipse 20 35 "solid" "cornflowerblue"))"notases/re.wav" (make-vc 930 (+ 100 (/ (image-height BACKGROUND) 2))) (make-vc -8 0) (make-vc 0 0)))
(define Mİ (make-cisim "Mi" (rotate 315 (ellipse 20 35 "solid" "red")) "notases/mi.wav"(make-vc 930 (+ (/ (image-height BACKGROUND) 2) 75)) (make-vc -8 0) (make-vc 0 0)))
(define FA (make-cisim "Fa" (rotate 315 (ellipse 20 35 "solid" "maroon"))"notases/fa.wav" (make-vc 930 (+ 50 (/ (image-height BACKGROUND) 2))) (make-vc -8 0) (make-vc 0 0)))
(define SOL (make-cisim "Sol" (rotate 315 (ellipse 20 35 "solid" "green")) "notases/sol.wav"(make-vc 930 (+  (/ (image-height BACKGROUND) 2) 25)) (make-vc -8 0) (make-vc 0 0)))
(define OYUNCU (make-cisim "müzisyen" (rectangle 40 15 "outline" "black") "notases/glass.wav"(make-vc 250  (/ (image-height BACKGROUND) 2)) (make-vc 0 0) (make-vc 0 0)))
(define ÜÇGEN (make-cisim "üçgen" (triangle 30  "solid" "red" )"notases/sol.wav" (make-vc 930 140) (make-vc -4 0) (make-vc 0 0)))
(define SKOR 0)
;dünya
;a:cisim(nota)
;b:cisim(nota)
;c:cisim(nota)
;o:cisim(oyuncu)
;s:cisim(skor)
(define-struct dünya (a b c o s))

(define EARTH2 (make-dünya ÜÇGEN Sİ DO OYUNCU SKOR))
(define oyuncu-genişliği 40)
(define ekran-genişliği 936)
(define ekran-yüksekliği 655)
(define oyuncu-yüksekliği 15)
(define çarpışma-aralığı (+ 10 (/ oyuncu-genişliği 2)))
(define TEHLİKE1 (nota-getir (random 8)))
(define TEHLİKE2 (nota-getir (random 8)))
(define ÖDÜL (nota-getir (random 8)))
(define EARTH (make-dünya TEHLİKE1 TEHLİKE2 ÖDÜL OYUNCU SKOR))

; mag-vc vc -> Sayı
; Vc vektörünün uzunluğunu hesaplar.
(check-expect (mag-vc (make-vc 3 4)) 5)
(check-expect (mag-vc (make-vc 6 8)) 10)
(define (mag-vc v) (sqrt (+ (sqr (vc-x v)) (sqr (vc-y v)))))
                       
; cisim-çarpışma? : cisim cisim -> Mantıksal
; Ne kadar uzak olduklarına bakıp çarpışıp çarpışmadıklarına karar veririz.
(check-expect (cisim-çarpışma? ÜÇGEN LA) false)
(check-expect (cisim-çarpışma? LA Sİ) true)
(define (cisim-çarpışma? c c2)
    (>= çarpışma-aralığı (mag-vc (-vc (cisim-yer c) (cisim-yer c2 ))))) 

;cisim-ilerleme cisim --> cisim
;fizik kurallarına göre cismin 1 saniyede ilerlemesini gösterir.
(check-expect (cisim-ilerleme LA)
 (make-cisim "La" (rotate 315 (ellipse 20 35 "solid" "gold")) "notases/la.wav"(+vc (cisim-yer LA) (cisim-hız LA)) (+vc (cisim-hız LA) (cisim-ivme LA)) (cisim-ivme LA)))

(check-expect (cisim-ilerleme  Sİ) 
(make-cisim "Si" (rotate 315 (ellipse 20 35 "solid" "brown")) "notases/ti.wav"(+vc (cisim-yer Sİ) (cisim-hız Sİ)) (+vc (cisim-hız Sİ) (cisim-ivme Sİ)) (cisim-ivme Sİ)))

(define (cisim-ilerleme a)
(make-cisim (cisim-açıklama a)(cisim-imaj a)(cisim-ses a)(+vc (cisim-yer a) (cisim-hız a)) (+vc (cisim-hız a) (cisim-ivme a)) (cisim-ivme a)))

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

; place-cisim cisim sahne -> sahne
; bir cisim kendi yer kordinatlarına göre sahneye yerleştir
(check-expect
 (place-cisim (make-cisim "circle" (circle 10 "solid" "yellow") "notases/sol.wav" (make-vc 10 10) (make-vc 99 99) (make-vc 99 99)) (square 100 "solid" "black"))
 (place-image/vc (circle 10 "solid" "yellow") (make-vc 10 10)  (square 100 "solid" "black")))

(define (place-cisim c sahne)
  (place-image/vc (cisim-imaj c) (cisim-yer c) sahne))

;==============================================
;nota-baştan dünya --> mantıksal
;Yer vektörünün x koordinatı 0 olan bir notayı başa döndürür.
(define (nota-baştan w) (<= (vc-x (cisim-yer (dünya-a w))) 0))

  
;dünya-ilerleme dünya --> dünya
;verilmiş olan dünyanın bir sonraki pozisyonunu belirler.
(check-expect (dünya-ilerleme EARTH) (cond
                             [(nota-baştan EARTH) (make-dünya  (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (dünya-o EARTH)
                                                    (- (dünya-s EARTH) 50))]
              (else (make-dünya (cisim-ilerleme (dünya-a EARTH))
                          (cisim-ilerleme (dünya-b EARTH))
                          (cisim-ilerleme (dünya-c EARTH))
                          (cisim-ilerleme (dünya-o EARTH))
                          (dünya-s EARTH)))))

(check-expect (dünya-ilerleme EARTH2) (cond
                             [(nota-baştan EARTH2) (make-dünya  (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (dünya-o EARTH2)
                                                    (- (dünya-s EARTH2) 50))] 
              (else (make-dünya (cisim-ilerleme (dünya-a EARTH2))
                          (cisim-ilerleme (dünya-b EARTH2))
                          (cisim-ilerleme (dünya-c EARTH2))
                          (cisim-ilerleme (dünya-o EARTH2))
                          (dünya-s EARTH2)))))

(define (dünya-ilerleme w) (cond
                             [(nota-baştan w) (make-dünya  (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (dünya-o w)
                                                    (- (dünya-s w) 50))]
                             
             (else (make-dünya (cisim-ilerleme (dünya-a w))
                          (cisim-ilerleme (dünya-b w))
                          (cisim-ilerleme (dünya-c w))
                          (cisim-ilerleme (dünya-o w))
                          (dünya-s w)))))

;çarpışma dünya--> dünya
;bir dünyadaki çarpışan iki cismin çarpışma hareketlerini kontrol eder.
(check-expect (çarpışma EARTH) (make-dünya TEHLİKE1 TEHLİKE2 ÖDÜL OYUNCU SKOR))
(check-expect (çarpışma EARTH2) (make-dünya ÜÇGEN Sİ DO OYUNCU SKOR))
(define (çarpışma w) (cond
                     [(cisim-çarpışma? (dünya-o w) (dünya-a w))
                     (begin (play-sound (cisim-ses (dünya-a w)) true)
                            (make-dünya (nota-getir (random 8))  
                                                    (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (dünya-o w)
                                                    (+ 10 (dünya-s w))))]
                                   


                                 [(or (cisim-çarpışma? (dünya-o w) (dünya-b w)) 
                                      (cisim-çarpışma? (dünya-o w) (dünya-c w)))
                                  (begin (play-sound "notases/sonar.wav" true))
                                       (make-dünya  (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (nota-getir (random 8))
                                                    (dünya-o w)
                                                    (- (dünya-s w) 10))]

                                 [else w]))


;dünya-yeni dünya--> dünya
;İçinde çarpışma bulunan bir dünyanın hareketlerini kontrol eder.
(check-expect (dünya-yeni EARTH) (çarpışma (dünya-ilerleme EARTH)))
(check-expect (dünya-yeni EARTH2) (çarpışma (dünya-ilerleme EARTH2)))
(define (dünya-yeni w) (çarpışma (dünya-ilerleme w)))


                                        

;dünya-çiz dünya--> sahne
;dünyayı çiz.
(check-expect (dünya-çiz EARTH) (cond [(< (dünya-s EARTH) 0) (place-image (text/font "GAME OVER" 100 "white"  #f 'modern 'italic 'normal #f)
                                                                          (- (/ ekran-genişliği 2) 50) (-(/ ekran-yüksekliği 2)50)
                                                                          YENİLGİ)]
                                                                      
  (else 
                                (place-cisim (dünya-a EARTH)
                                (place-cisim (dünya-b EARTH)
                                (place-cisim (dünya-c EARTH)
                                
                                (place-cisim (dünya-o EARTH)
                                (place-image (text/font (cisim-açıklama (dünya-a EARTH)) 50 "maroon"
             #f 'modern 'italic 'normal #f)
                                             400 150
                                (place-image (text/font "CREDITS" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                             40 40
                                (place-image (text/font "Zeynep Aydın" 15 "maroon"
             #f 'modern 'italic 'normal #f)              
                                             55 60
                                 (place-image (text/font "Karhan Azaklı" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                              60 80
                                 (place-image (text/font "Zeynep Sude Halisdemir" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                              100 100
                                  (place-image (text/font "04.08.2018" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                               45.25 625               
                                (place-image (text/font (string-append "PORTE         "  (number->string (dünya-s EARTH))) 50 "maroon"
             #f 'modern 'italic 'normal #f)
                                              600 25
                                              BACKGROUND))))))))))))))

(check-expect (dünya-çiz EARTH2) (cond [(<  (dünya-s EARTH2) 0) (place-image (text/font "GAME OVER" 100 "white"  #f 'modern 'italic 'normal #f)
                                                           (- (/ ekran-genişliği 2) 50) (/ ekran-yüksekliği 2)
                                                           YENİLGİ)]
                                                                      
  (else  
                                (place-cisim (dünya-a EARTH2)
                                (place-cisim (dünya-b EARTH2)
                                (place-cisim (dünya-c EARTH2)
                                
                                (place-cisim (dünya-o EARTH2)
                                (place-image (text/font (cisim-açıklama (dünya-a EARTH2)) 50 "maroon"
             #f 'modern 'italic 'normal #f)
                                              400 150
                                  (place-image (text/font "CREDITS" 15 "maroon"
             #f 'modern 'italic 'normal #f)            
                                             40 40
                                 (place-image (text/font "Zeynep Aydın" 15 "maroon"
             #f 'modern 'italic 'normal #f)              
                                             55 60
                                 (place-image (text/font "Karhan Azaklı" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                              60 80
                                 (place-image (text/font "Zeynep Sude Halisdemir" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                              100 100
                                  (place-image (text/font "04.08.2018" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                               45.25 625               
                                 (place-image (text/font (string-append "PORTE         " (number->string (dünya-s EARTH2))) 50 "maroon"
             #f 'modern 'italic 'normal #f)
                                              600 25
                                              BACKGROUND))))))))))))))
  

(define (dünya-çiz w) (cond [(<  (dünya-s w) 0) (place-image (text/font "GAME OVER" 100 "white"  #f 'modern 'italic 'normal #f)
                                                           (- (/ ekran-genişliği 2) 50) (/ ekran-yüksekliği 2)
                                                           YENİLGİ)]
                                                                      
  (else (place-cisim (dünya-a w)
                                (place-cisim (dünya-b w)
                                (place-cisim (dünya-c w)
                                (place-cisim (dünya-o w)
                                             (place-image (text/font (cisim-açıklama (dünya-a w)) 50 "maroon"
             #f 'modern 'italic 'normal #f)
                                              400 150
                                 (place-image (text/font "CREDITS" 15 "maroon"
             #f 'modern 'italic 'normal #f)            
                                             40 40
                                             
                                  (place-image (text/font "Zeynep Aydın" 15 "maroon"
             #f 'modern 'italic 'normal #f)              
                                             55 60
                                 (place-image (text/font "Karhan Azaklı" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                              60 80
                                 (place-image (text/font "Zeynep Sude Halisdemir" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                              100 100
                                  (place-image (text/font "04.08.2018" 15 "maroon"
             #f 'modern 'italic 'normal #f)
                                              45.25 625            
                               (place-image (text/font (string-append  "PORTE         " (number->string (dünya-s w))) 50 "maroon"
             #f 'modern 'italic 'normal #f)
                                             600 25
                                             BACKGROUND))))))))))))))
;cisim-hareket cisim sayı sayı--> cisim
;tek tuş hamlesinde cismin ne kadar hareket edeceğini hesaplar.
(check-expect (cisim-hareket LA 3 4) (make-cisim (cisim-açıklama LA)(cisim-imaj LA) (cisim-ses LA)(make-vc (+ 3 (vc-x (cisim-yer LA)))
                                                                                             (+ 4 (vc-y (cisim-yer LA)))) (cisim-hız LA) (cisim-ivme LA)))
(check-expect (cisim-hareket Sİ 5 4) (make-cisim (cisim-açıklama Sİ)(cisim-imaj Sİ)(cisim-ses Sİ) (make-vc (+ 5 (vc-x (cisim-yer Sİ)))
                                                                                             (+ 4 (vc-y (cisim-yer Sİ)))) (cisim-hız Sİ) (cisim-ivme Sİ)))
(define (cisim-hareket c x y)
              (make-cisim (cisim-açıklama c)(cisim-imaj c)(cisim-ses c) (make-vc (+ x (vc-x (cisim-yer c))) (+ y (vc-y (cisim-yer c)))) (cisim-hız c) (cisim-ivme c)))

;dünya-tuş dünya tuş--> dünya
;Verilmiş olan tuş komutuyla oyuncuyu istenen yönde ve hızda hareket ettirir. 
(check-expect (dünya-tuş EARTH "up")(make-dünya (dünya-a EARTH) (dünya-b EARTH) (dünya-c EARTH) (cisim-hareket (dünya-o EARTH) 0 -15) (dünya-s EARTH)))
(check-expect (dünya-tuş EARTH2 "up")(make-dünya (dünya-a EARTH2) (dünya-b EARTH2) (dünya-c EARTH2) (cisim-hareket (dünya-o EARTH2) 0 -15) (dünya-s EARTH2)))
(define (dünya-tuş w t)
(cond
  ((string=? t "up") (make-dünya (dünya-a w) (dünya-b w) (dünya-c w) (cisim-hareket (dünya-o w) 0 -15) (dünya-s w)))
  ((string=? t "down") (make-dünya (dünya-a w) (dünya-b w) (dünya-c w) (cisim-hareket (dünya-o w) 0 15) (dünya-s w)))
  ((string=? t "left") (make-dünya (dünya-a w) (dünya-b w) (dünya-c w) (cisim-hareket (dünya-o w) -15 0) (dünya-s w)))
  ((string=? t "right") (make-dünya (dünya-a w) (dünya-b w) (dünya-c w) (cisim-hareket (dünya-o w) 15 0) (dünya-s w)))
  ((string=? t " ") (make-dünya (dünya-a w) (dünya-b w) (dünya-c w) (dünya-o w) SKOR))
  (else w)))

(define (dünya-fare w x y m) w)

(define yaradılış (make-dünya (nota-getir (random 8)) (nota-getir (random 8)) (nota-getir (random 8))   OYUNCU SKOR))

(big-bang yaradılış
  (on-tick dünya-yeni (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare))
 




