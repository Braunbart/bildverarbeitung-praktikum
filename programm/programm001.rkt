#lang racket
(require vigracket)

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
(define threshhold 150.0) ; TODO: als paramter oder coole function für automatisch finden
(show-image (image-map (lambda (x) (if (> x threshhold) 255.0 0.0)) work_img))



;Spielfeld erkennen

;Spielfeld drehen

;Spielfeld Ausschneiden

;Felder durchgehen (Prozentual)