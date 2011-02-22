;;;; 
;;;; Usage:
;;;;    cp svg-to-consumable.scm ~/.gimp-2.6/scripts/
;;;;    gimp --no-interface --batch "(svg-to-consumable \"/home/sjcarbon/local/src/cvs/go-dev/amigo/amigo/images/star.svg\" 60 60)" --batch "(gimp-quit 0)"
;;;;

;;;
(define (svg-to-consumable filename width height)
  (let* ((image (car (file-svg-load RUN-NONINTERACTIVE
                                    filename "" 90.0 width height 0)))
         (drawable (car (gimp-image-get-active-drawable image)))
         (good-filename-part (car (strbreakup filename ".")))
         (png-filename (string-append good-filename-part ".png"))
         (gif-filename (string-append good-filename-part ".gif")))
    (file-png-save RUN-NONINTERACTIVE image drawable png-filename "" 0 9 0 0 0 1 1)
    ;; Have to switch to index before saving to GIF to avoid an error.
    (gimp-image-convert-indexed image NO-DITHER MAKE-PALETTE 256 FALSE FALSE "")
    (file-gif-save RUN-NONINTERACTIVE image drawable gif-filename "" TRUE FALSE 0 0)))

;; ;; TODO: Register into Gimp menus.
;; (script-fu-register "svg-to-consumable"
;;                     _"<Image>/Tools/SVG to PNG and GIF..."
;;                     "Convert a base SVG image into consumable formats (i.e. PNG and GIF)."
;;                     "Seth Carbon <sjcarbon@berkeleybop.org>"
;;                     "Seth Carbon"
;;                     "2010/2/1"
;;                     "RGBA SVG"
;;                     SF-IMAGE "Image" 0
;;                     SF-DRAWABLE "Drawable" 0
;;                     SF-ADJUSTMENT _"Width" (16 0 100 1 10 0 0)
;;                     SF-ADJUSTMENT _"Height" (16 0 100 1 10 0 0))
