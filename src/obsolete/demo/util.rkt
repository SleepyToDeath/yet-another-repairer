#lang rosette

(require pict slideshow/code)

(define (save-pict pict filename)
	(if pict 
		(send (pict->bitmap pict) save-file filename 'jpeg 100) #f))

(provide save-pict)
