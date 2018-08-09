;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname oyunumuz) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;; OYUNUMUZ ;;;;;;;;;;;;;;;;;;;;;;;;

; bootstrapworld reactive (ish) racket versiyonu
(require 2htdp/universe)
(require 2htdp/image)

(define FRAME-RATE 12)
(define BACKGROUND (bitmap "RESİMLER/mutfak (2).jpg"))
(define BACKGROUND-SCENE
  (place-image/align BACKGROUND  0 0 "left" "top" (empty-scene (image-width BACKGROUND) (image-height BACKGROUND))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Ekran Özellikleri

(define EKRAN-GENİŞLİĞİ 900)

(define EKRAN-YÜKSEKLİĞİ 675)



;Oyuncu Özellikleri

(define OYUNCU-BOYU 150)

(define OYUNCU-GENİŞLİĞİ 150)



;;;Resimler

(define OYUNCU-RESMİ (bitmap "RESİMLER/unnamed (1).png"))

(define TEHLİKE-RESMİ (bitmap "RESİMLER/unnamed (2).png"))

(define HEDEF-RESMİ (bitmap "RESİMLER/unnamed (3).png"))






;;;Tehlike Özellikleri

(define TEHLİKE-BOYU 123)

(define TEHLİKE-GENİŞLİĞİ 150)



;;;Hedef Özellikleri

(define HEDEF-BOYU 56)

(define HEDEF-GENİŞLİĞİ 65)



;;;text :  metin -> görsel
;;;number -> string :  sayı -> metin






;vc
;(vc (x y))-> vektör

(define-struct vc (x y))



;cisim
;resim + vc-hız + vc-konum + vc-ivme -> cisim

(define-struct cisim (resim konum hız ivme))



;;;Oyuncu, Tehlike ve Hedef

(define OYUNCU (make-cisim OYUNCU-RESMİ (make-vc (+ 30 (/ (- EKRAN-GENİŞLİĞİ OYUNCU-BOYU) 2)) (- EKRAN-YÜKSEKLİĞİ OYUNCU-BOYU)) (make-vc 0 0) (make-vc 0 0)))

(define TEHLİKE (make-cisim TEHLİKE-RESMİ (make-vc 0 0)(make-vc 0 25)(make-vc 0 0)))

(define HEDEF (make-cisim HEDEF-RESMİ (make-vc 0 0)(make-vc 0 20)(make-vc 0 0)))

(define SKOR 0)



;(+vc vektör vektör)-> vektör

(check-expect(+vc (make-vc 3 5)(make-vc 4 5))(make-vc 7 10))

(define (+vc vector1 vector2) (make-vc (+ (vc-x vector1) (vc-x vector2))(+ (vc-y vector1) (vc-y vector2))))


;(-vc vektör vektör)-> vektör

(check-expect(-vc (make-vc 5 7)(make-vc 4 5))(make-vc 1 2))

(define (-vc vector1 vector2) (make-vc (- (vc-x vector1) (vc-x vector2))(- (vc-y vector1) (vc-y vector2))))


;;;dünya
;;;hedef + tehlike + oyuncu + skor

(define-struct dünya (hedef tehlike oyuncu skor))

(define dünyam (make-dünya HEDEF TEHLİKE OYUNCU SKOR))






;cisim-ilerleme
;cisim -> cisim

(define (cisim-ilerleme cismim) (make-cisim
                                 (cisim-resim cismim)
                                 (+vc (cisim-hız cismim)(cisim-konum cismim))
                                 (+vc (cisim-hız cismim)(cisim-ivme cismim))
                                 (cisim-ivme cismim)))

;vc-uzunluk
;vector -> sayı

(define (vc-uzunluk v)(sqrt (+ (sqr (vc-x v))(sqr (vc-y v)))))

;;cisim-mesafe
;cisim cisim -> sayı

(define (cisim-mesafe c1 c2)(vc-uzunluk (-vc (cisim-konum c1)(cisim-konum c2))))


;;çarptı-mı
;cisim cisim -> mantıksal

(define (çarptı-mı c1 c2)(<= (cisim-mesafe c1 c2) (/ (+ (image-height (cisim-resim  c1)) (image-height (cisim-resim  c2))) 3)))

;;tehlike-çarpışma
;dünya -> dünya
;hedef ve oyuncu çarptığında ne olacağını belirler

(define (tehlike-çarpışma w)(cond
                              [(çarptı-mı (dünya-oyuncu w) (dünya-tehlike w))
                               (make-dünya
                                (dünya-hedef w)
                                (alttakini-döndür (dünya-tehlike w) 0)
                                (dünya-oyuncu w)
                                (- (dünya-skor w) 50))] 
                              [else w]))

(define (hedef-çarpışma w)(cond
                              [(çarptı-mı (dünya-oyuncu w) (dünya-hedef w))
                               (make-dünya
                                (alttakini-döndür (dünya-hedef w) 0)
                                (dünya-tehlike w)
                                (dünya-oyuncu w)
                                (+ (dünya-skor w) 25))]
                              [else w]))
;;dünya-yeni
;dünya -> dünya
;dünyadaki çarpışmaları belirler

(define (dünya-yeni w)(tehlike-çarpışma (hedef-çarpışma (dünya-ilerleme w))))

;;;;;;;;;;;;;;;; KOPYALAMA ;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
  (place-image/vc (cisim-resim c) (cisim-konum c) sahne))




; vektör testleri
(define v1 (make-vc 150 150))
(define v2 (make-vc 120 30))
(define origin (make-vc 0 0))


; vektör toplam gösterisi
(place-line/vc origin (+vc v1 v2) "green"
                                     (place-line/vc origin v1 "pink"
                                                    (place-line/vc v1 (+vc v1 v2)  "yellow" (rectangle 300 225 "solid" "black"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define yaradılış (make-dünya HEDEF TEHLİKE OYUNCU SKOR))


;dünya-ilerleme
;dünya -> dünya
;dünyadaki cisimleri ilerletir

(check-expect (dünya-ilerleme dünyam) (make-dünya (cisim-ilerleme HEDEF) (cisim-ilerleme TEHLİKE) (cisim-ilerleme  OYUNCU) SKOR))

(define (dünya-ilerleme w) (make-dünya                            
                            (alttakini-döndür (cisim-ilerleme (dünya-hedef w))  EKRAN-YÜKSEKLİĞİ)
                            (alttakini-döndür (cisim-ilerleme (dünya-tehlike w))  EKRAN-YÜKSEKLİĞİ)
                            (cisim-ilerleme  (dünya-oyuncu w))
                            (dünya-skor w)))

;dünya-çiz
;dünya -> görsel
;bir dünya çizer

(check-expect (dünya-çiz dünyam) (place-cisim (dünya-hedef dünyam)
                      (place-cisim (dünya-tehlike dünyam)
                      (place-cisim (dünya-oyuncu dünyam)
                      (place-image (text(string-append "SKOR: " (number->string (dünya-skor dünyam))) 20 "white") 800 50           
                       BACKGROUND-SCENE)))))


(define (dünya-çiz w) (cond
                        ((< (dünya-skor w) 0) (overlay (bitmap "RESİMLER/mutfak (3).jpg")(rectangle 900 675 "solid" "black")))
                        ((>= (dünya-skor w) 500)(overlay (bitmap "RESİMLER/mutfak (4).jpg")))
                        (else                       
                        (place-cisim (dünya-hedef w)
                      (place-cisim (dünya-tehlike w)
                      (place-cisim (dünya-oyuncu w)
                      (place-image (text(string-append "SKOR: " (number->string (dünya-skor w))) 20 "white") 800 50           
                       BACKGROUND-SCENE)))))))


(define
  (dünya-tuş w t)
  (make-dünya
   (dünya-hedef w)
   (dünya-tehlike w)
   (make-cisim
    (cisim-resim OYUNCU)
    (make-vc (+  (vc-x (cisim-konum (dünya-oyuncu w))) (cond [(string=? t "right") 50]
                                                             [(string=? t "left") -50]
                                                             [else 0])) (vc-y (cisim-konum (dünya-oyuncu w))) )(cisim-hız (dünya-oyuncu w))(cisim-ivme (dünya-oyuncu w)))
   (cond
     ((and(<(dünya-skor w) 0) (string=? t " ")) 0)
     (else (dünya-skor w)))))

;alttakini-döndür cisim sayı -> cisim
;tehlike ve hedf alta indiğide tekrar üstten çıkar

(define (alttakini-döndür c limit)
  (cond
    [(> (vc-y (cisim-konum c)) limit) (make-cisim (cisim-resim c) (make-vc (random EKRAN-GENİŞLİĞİ) 0) (cisim-hız c) (cisim-ivme c))]
    [else c]))



;güvenli-sağ? :  sayı -> mantıksal
;oyuncunun ekranın sağ tarafından çıkmasını engeller

(define (güvenli-sağ? c)(<= (vc-x (cisim-konum c)) (- EKRAN-GENİŞLİĞİ (/ OYUNCU-GENİŞLİĞİ 2))))



;güvenli-sol? :  sayı -> mantıksal
;oyuncunun ekranın sol tarafından çıkmasını engeller

(define (güvenli-sol? c)(>= (vc-x (cisim-konum c)) (/ OYUNCU-GENİŞLİĞİ 2)))

;;güvenli-sol-sağ
;cisim -> mantıksl

(define (güvenli-sol-sağ c)(and (güvenli-sağ? c) (güvenli-sol? c) ))


;;mesafe
;cisim cisim -> sayı

;(define (mesafe c1 c2)(sqrt (+ (- (vc-x (cisim-konum c1)) (vc-x (cisim-konum c2))) (- (vc-y (cisim-konum c1)) (- (vc-y (cisim-konum c2)))))))


;çarıştı-mı
;cisim cisim -> mantıksal

;(define (çarpıştı-mı c1 c2)(<= (mesafe c1 c2) 50))



(define (dünya-fare w x y m) w)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (cisim-çarptı-mı? c)(vc-y (cisim-konum)))

(define (yön-değiştir c)(make-cisim (cisim-resim c) (cisim-konum c) (make-vc (vc-x(cisim-hız c))(* -1 (vc-y cisim-hız c)))(cisim-ivme c)))

(define (cisim-hareket c)(cond[(cisim-çarptı-mı? c)(yön-değiştir c)][else (cisim-ilerleme c)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(big-bang yaradılış
  (on-tick dünya-yeni (/ 1.0 FRAME-RATE))
  (on-draw dünya-çiz)
  (on-key dünya-tuş)
  (on-mouse dünya-fare))

;;KALANLAR
;; güvenli-sağ & sol
;; Oyunun ismi ve senin ismin eklenecek
;; resim istersen resimleri değiştir


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




