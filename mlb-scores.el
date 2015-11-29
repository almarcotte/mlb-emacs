;;; mlb-scores.el -- mlb scores

;;; Commentary:
;; Gives access to MLB scores in emacs by downloading the data from mlb.com

;;; Code:

(defun mlb/download-file (year month day)
  "Download the grid json file for the given YEAR, MONTH and DAY."
  (interactive)
  (let ((file-url (format "http://gd2.mlb.com/components/game/mlb/%s/grid.json" (mlb/format-date year month day)))
	 (file-tmp (concat temporary-file-directory (format "mlb-%04d-%02d-%02d.json" year month day))))
    (request file-url
	     :parser (lambda ()
		       (let ((json-object-type 'alist))
			 (json-read)))
	     :success (function*
		       (lambda (&key data &allow-other-keys)
			 (let* (
				(game (elt (assoc-default 'data data) 0))
				(a (assoc-default 'game game))
				(b (assoc-default 'id a))
				)
			   (message "%s" b)
			   )
			 
			 ))
	     :status-code
	     '((400 . (lambda (&rest _) (message "Got 400.")))
	       (418 . (lambda (&rest _) (message "Got 418."))))
	     :complete
	     )
    )
  )


(defun mlb/format-date (year month day)
  "Formats the given YEAR, MONTH and DAY as a path on mlb.com."
  (let* ((a (format "year_%04d/month_%02d/day_%02d" year month day)))
    a)
  )


(mlb/download-file 2015 06 10)

;;; mlb-scores.el ends here
 
