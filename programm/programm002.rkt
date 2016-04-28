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

;(show-image orig "Das neue Bild")

(define work_img (image->blue img))

(define canny_img (cannyedgeimage work_img 2.0 11.0 255.0)); TODO maybe automatisch werte finden

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


;Vektor (x-max y-max x-min y-min)
(define canny_bbox (vector (image-width img) (image-height img) 0 0))

(void (image-for-each-pixel (curryr findBBox canny_bbox)  canny_img))

;(show-image (plt-image->image (overlay-bboxes img (list canny_bbox) '(green))) "canny-bbox")

;Aus teaching-demo.rkt kopiert:
;----Zuschneiden1----
(define canny_crop (subimage img  (vector-ref canny_bbox 0) (vector-ref canny_bbox 1) (vector-ref canny_bbox 2) (vector-ref canny_bbox 3)))
(show-image canny_crop "Zuschnitt 1")

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
(define canny_pos canny_right)

;Daraus und der width den Winkel berechnen
(define canny_angle  (/ (* (atan  (- canny_bbox-width canny_pos) canny_pos ) -180) pi))

;Vektor FÜR SPÄTER
;(define canny_bbox2 (vector (image-width canny_crop) (image-height canny_crop) 0 0))

;Drehen um Winkel
(define canny_crop-rotated (rotateimage canny_crop canny_angle 1))

;(show-image canny_crop-rotated  "Rotiert")

;Felder durchgehen (Prozentual)