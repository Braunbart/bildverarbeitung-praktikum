#lang racket
(require vigracket)
(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width plt-image-width)
                    (image-height plt-image-height)))

(define (resizeimage-percent img prozent [interpolation 2])
    (let* ((p (/ prozent 100))
           (new_w (round (* (image-width img) p)))
           (new_h (round (* (image-height img) p))))
        (resizeimage img new_w new_h interpolation)))

;(define orig (load-image "/Volumes/students/home/4schande/Bildverarbeitung/photos/originals/IMG_0777.JPG"))
;(define img (resizeimage-percent orig 25))
(define img (load-image "/Volumes/students/home/4schande/Bildverarbeitung/photos/smaller/IMG_0777.JPG"))

;(show-image img "Das neue Bild")

(define work_img (image->blue img))

(define canny_img (cannyedgeimage work_img 2.0 11.0 255.0)); TODO maybe automatisch werte finden --> Auch bei Zuschneiden2 ändern!!

;Aus teaching-demo.rkt kopiert:
;----BBOX----
(define (findBBox x y pixel bbox)
  (when (>  (car pixel) 0.0)
    (begin
      (when (> x (vector-ref bbox 2))    (vector-set! bbox 2 x))
      (when (< x (vector-ref bbox 0))    (vector-set! bbox 0 x))
      (when (> y (vector-ref bbox 3))    (vector-set! bbox 3 y))
      (when (< y (vector-ref bbox 1))    (vector-set! bbox 1 y)))))

;Damit der Kasten Angezeigt werden kann:
(define (overlay-bboxes img bboxes colors)
  (if (empty? bboxes)
       (image->plt-image img)
       (let ((bbox  (car bboxes))
             (color (car colors)))
         (underlay/xy (overlay-bboxes img (cdr bboxes) (cdr colors))
                      (- (vector-ref bbox 0) 1)
                      (- (vector-ref bbox 1) 1)
                      (rectangle (+ (- (vector-ref bbox 2) (vector-ref bbox 0)) 2)
                                 (+ (- (vector-ref bbox 3) (vector-ref bbox 1)) 2)
                                 'outline color)))))


;Vektor (x-min y-min x-max y-max)
(define canny_bbox (vector (image-width img) (image-height img) 0 0))

(void (image-for-each-pixel (curryr findBBox canny_bbox)  canny_img))

;(show-image (plt-image->image (overlay-bboxes img (list canny_bbox) '(green))) "canny-bbox")

;Aus teaching-demo.rkt kopiert:
;----Zuschneiden1----
(define canny_crop (subimage img  (vector-ref canny_bbox 0) (vector-ref canny_bbox 1) (vector-ref canny_bbox 2) (vector-ref canny_bbox 3)))
;(show-image canny_crop "Zuschnitt 1")

;Aus teaching-demo.rkt kopiert:
;----Drehen----
(define (findFirstPixelInRow img x1 x2 row)
  (let ((not_found 
         (= (apply max (image-ref img x1 row)) 
            0)))
    (if not_found
        (if (= x1 x2)
            #f
            (findFirstPixelInRow
              img 
              (+ x1 (sgn (- x2 x1))) x2 row))
        x1)))

;Breite der vorher bestimmten BBox
(define canny_bbox-width (-  (vector-ref canny_bbox 2)  (vector-ref canny_bbox 0)))

;Linken und rechten Rand der Ecke bestimmen
(define canny_left  (- (findFirstPixelInRow canny_img (vector-ref canny_bbox 0) (vector-ref canny_bbox 2) (vector-ref canny_bbox 1)) (vector-ref canny_bbox 0)))
(define canny_right (- (findFirstPixelInRow canny_img (vector-ref canny_bbox 2) (vector-ref canny_bbox 0) (vector-ref canny_bbox 1)) (vector-ref canny_bbox 0)))

;Mittelwert zwischen linken und rechten Rand von Ecke
;(define canny_pos (/ (+ canny_left canny_right) 2))
(define canny_pos (+ canny_right 23)) ;findFirstPixelInRow verfehlt noch die exakte Eckenposition daher bisher manuell zurecht gerückt

;Daraus und der width den Winkel berechnen
(define canny_angle  (/ (* (atan  (- canny_bbox-width canny_pos) canny_pos ) -180) pi))

;Vektor FÜR SPÄTER
;(define canny_bbox2 (vector (image-width canny_crop) (image-height canny_crop) 0 0))

;Drehen um Winkel
(define canny_crop-rotated (rotateimage canny_crop canny_angle 1))

;(show-image canny_crop-rotated  "Rotiert")

;Aus teaching-demo.rkt kopiert:
;----Zuschneiden2----
(define canny_bbox2 (vector (image-width canny_crop) (image-height canny_crop) 0 0))
;Neue BBOX finden
(void (image-for-each-pixel (curryr findBBox canny_bbox2)  (cannyedgeimage canny_crop-rotated 2.0 11.0 255.0)))
;An Neuer BBOX Zuschneiden
(define canny_crop2 (subimage canny_crop-rotated  (vector-ref canny_bbox2 0) (vector-ref canny_bbox2 1) (vector-ref canny_bbox2 2) (vector-ref canny_bbox2 3)))

(show-image canny_crop2  "Final Zugeschnitten")
;(save-image canny_crop2 "/Volumes/students/home/4schande/Bildverarbeitung/Zugeschnitten.png")

;----Felder durchgehen (Prozentual)----
(define spielfeld (vector '('(9) 'goal '(49.83 6))
                          
                          '('(2 18) 'black '(6.5 11.63)) '('(1 3) 'black '(12 11.63)) '('(2 4) 'black '(17.3 11.63)) '('(3 5) 'black '(22.7 11.63))
                          '('(4 6) 'black '(28 11.63)) '('(5 7) 'black '(33.5 11.63)) '('(6 8) 'black '(38.9 11.63)) '('(7 9) 'black '(44.3 11.63))
                          '('(0 8 10) 'red '(49.83 11.63)) '('(9 11) 'black '(55.2 11.63)) '('(10 12) 'black '(60.6 11.63)) '('(11 13) 'black '(66 11.63))
                          '('(12 14) 'black '(71.45 11.63)) '('(13 15) 'black '(76.8 11.63)) '('(14 16) 'black '(82.2 11.63)) '('(15 17) 'black '(87.6 11.63))
                          '('(16 19) 'black '(93 11.63))

                          '('(1 20) 'black '(6.5 17.07)) '('(17 36) 'black '(93 17.07))

                          '('(18 21) 'black '(6.5 22.38)) '('(20 22) 'black '(12 22.38)) '('(21 23) 'black '(17.3 22.38)) '('(22 24) 'black '(22.7 22.38))
                          '('(23 25) 'black '(28 22.38)) '('(24 26) 'black '(33.5 22.38)) '('(25 27) 'black '(38.9 22.38)) '('(26 28) 'black '(44.3 22.38))
                          '('(27 29 37) 'red '(49.83 22.38)) '('(28 30) 'black '(55.2 22.38)) '('(29 31) 'black '(60.6 22.38)) '('(30 32) 'black '(66 22.38))
                          '('(31 33) 'black '(71.45 22.38)) '('(32 34) 'black '(76.8 22.38)) '('(33 35) 'black '(82.2 22.38)) '('(34 36) 'black '(87.6 22.38))
                          '('(19 35) 'black '(93 22.38))

                          '('(28 40) 'red '(49.83 27.48))

                          '('(39 43) 'black '(38.9 33.16)) '('(38 40) 'black '(44.3 33.16)) '('(37 39 41) 'red '(49.83 33.16)) '('(40 42) 'black '(55.2 33.16))
                          '('(41 44) 'black '(60.6 33.16))

                          '('(38 47) 'black '(38.9 38.75)) '('(42 51) 'black '(60.6 38.75))

                          '('(46 54) 'black '(28 44.14)) '('(45 47) 'black '(33.5 44.14)) '('(43 46 48) 'red '(38.9 44.14)) '('(47 49) 'black '(44.3 44.14))
                          '('(48 50) 'black '(49.83 44.14)) '('(49 51) 'black '(55.2 44.14)) '('(44 50 52) 'red '(60.6 44.14)) '('(51 53) 'black '(66 44.14))
                          '('(52 55) 'black '(71.45 44.14))

                          '('(45 58) 'black '(28 49.82)) '('(53 66) 'black '(71.45 49.82))

                          '('(57 69) 'black '(17.3 55.13)) '('(56 58) 'black '(22.7 55.13)) '('(54 57 59) 'black '(28 55.13)) '('(58 60) 'black '(33.5 55.13))
                          '('(59 61 70) 'black '(38.9 55.13)) '('(60 62) 'black '(44.3 55.13)) '('(61 63) 'black '(49.83 55.13)) '('(62 64) 'black '(55.2 55.13))
                          '('(63 65 71) 'black '(60.6 55.13)) '('(64 66) 'black '(66 55.13)) '('(55 65 67) 'black '(71.45 55.13)) '('(66 68) 'black '(76.8 55.13))
                          '('(67 72) 'black '(82.2 55.13))

                          '('(56 75) 'black '(17.3 60.64)) '('(60 79) 'black '(38.9 60.64)) '('(64 83) 'black '(60.6 60.64)) '('(68 87) 'black '(82.2 60.64))

                          '('(74 90) 'red '(6.5 66.23)) '('(73 75) 'black '(12 66.23)) '('(69 74 76) 'black '(17.3 66.23)) '('(75 77) 'black '(22.7 66.23))
                          '('(76 78 91) 'red '(28 66.23)) '('(77 79) 'black '(33.5 66.23)) '('(70 78 80) 'black '(38.9 66.23)) '('(79 81) 'black '(44.3 66.23))
                          '('(80 82 92) 'red '(49.83 66.23)) '('(81 83) 'black '(55.2 66.23)) '('(71 82 84) 'black '(60.6 66.23)) '('(83 85) 'black '(66 66.23))
                          '('(84 86 93) 'red '(71.45 66.23)) '('(85 87) 'black '(76.8 66.23)) '('(72 86 88) 'black '(82.2 66.23)) '('(87 89) 'black '(87.6 66.23))
                          '('(88 94) 'red '(93 66.23))

                          '('(73 95) 'black '(6.5 71.83)) '('(77 99) 'black '(28 71.83)) '('(81 103) 'black '(49.83 71.83)) '('(85 107) 'black '(71.45 71.83))
                          '('(89 111) 'black '(93 71.83))

                          '('(90 96) 'black '(6.5 77.18)) '('(95 97) 'black '(12 77.18)) '('(96 98 112 113 114 115 116) 'black '(17.3 77.18))
                          '('(97 99) 'black '(22.7 77.18)) '('(91 98 100) 'black '(28 77.18)) '('(99 101) 'black '(33.5 77.18))
                          '('(100 102 117 118 119 120 121) 'black '(38.9 77.18)) '('(101 103) 'black '(44.3 77.18)) '('(92 102 104) 'black '(49.83 77.18))
                          '('(103 105) 'black '(55.2 77.18)) '('(104 106 122 123 124 125 126) 'black '(60.6 77.18)) '('(105 107) 'black '(66 77.18))
                          '('(93 106 108) 'black '(71.45 77.18)) '('(107 109) 'black '(76.8 77.18)) '('(108 110 127 128 129 130 131) 'black '(82.2 77.18))
                          '('(109 111) 'black '(87.6 77.18)) '('(94 110 112) 'black '(93 77.18))

                          '('(97) 'start_red '(17.3 82.69)) '('(97) 'start_red '(11 87.23)) '('(97) 'start_red '(23.8 87.23)) '('(97) 'start_red '(13.4 95.1))
                          '('(97) 'start_red '(21.4 95.1))

                          '('(101) 'start_green '(38.9 82.69)) '('(101) 'start_green '(32.5 87.23)) '('(101) 'start_green '(45.5 87.23))
                          '('(101) 'start_green '(35 95.1)) '('(101) 'start_green '(43 95.1))

                          '('(105) 'start_yellow '(60.6 82.69)) '('(105) 'start_yellow '(54.3 87.23)) '('(105) 'start_yellow '(67.3 87.23))
                          '('(105) 'start_yellow '(56.7 95.1)) '('(105) 'start_yellow '(64.7 95.1))

                          '('(109) 'start_blue '(82.2 82.69)) '('(109) 'start_blue '(76 87.23)) '('(109) 'start_blue '(89 87.23))
                          '('(109) 'start_blue  '(78.5 95.1)) '('(109) 'start_blue '(86.5 95.1))
                          ))

;Vektor der Aktuellen Spielzustand repräsentiert
(define spielstand (make-vector 132 'empty))

(define (prozentZuPixel prozentlist)
  (list (inexact->exact (round (* (/(car prozentlist)100) (image-width canny_crop2))))
        (inexact->exact (round (* (/(cadr prozentlist)100) (image-height canny_crop2))))))

;Errechnet Durchschnittliche Farbe um Pixelkoordinate (+/- 5)
(define (getFeldColor pixellist)
  (map (lambda (x) (/ x 121))
       (image-reduce + (subimage canny_crop2
                                 (- (car pixellist) 5)
                                 (- (cadr pixellist) 5)
                                 (+ (car pixellist) 6)
                                 (+ (cadr pixellist) 6)) 0)))

(define (getFeldzustand feldnummer)
  (getFeldColor (prozentZuPixel (cadr (caddr (vector-ref spielfeld feldnummer))))))