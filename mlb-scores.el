;;; mlb-scores.el -- mlb scores

;;; Commentary:
;; Gives access to MLB scores in emacs by downloading the data from mlb.com

;;; Code:

(defun mlb/download-file (year month day)
  "Download the grid json file for the given YEAR, MONTH and DAY."
  (require 'request)
  (let ((file-url (format "http://gd2.mlb.com/components/game/mlb/%s/grid.json" (mlb/format-date year month day))))
    (request file-url
	     :parser (lambda ()
		       (let ((json-object-type 'alist))
			 (json-read)))
	     :success (function*
		       (lambda (&key data &allow-other-keys)
			 (let* (
				(games (elt (assoc-default 'data data) 0))
				(game (assoc-default 'game games))
				(home-teams (cl-map 'list
					       (lambda (alist) (cdr (assoc 'home_team_name alist)))
					       game))
				(away-teams (cl-map 'list
						    (lambda (alist) (cdr (assoc 'away_team_name alist)))
						    game))
				(home-score (cl-map 'list
						    (lambda (alist) (cdr (assoc 'home_score alist)))
						    game))
				(away-score (cl-map 'list
						    (lambda (alist) (cdr (assoc 'away_score alist)))
						    game))
				(status (cl-map 'list
						    (lambda (alist) (cdr (assoc 'status alist)))
						    game))
				(inning (cl-map 'list
						    (lambda (alist) (cdr (assoc 'inning alist)))
						    game))
				)
			   (switch-to-buffer "*mlb*")
			   (erase-buffer)
			   (let* ((home (cl-mapcar 'list home-teams home-score))
				 (away (cl-mapcar 'list away-teams away-score))
				 (innings (cl-mapcar 'list inning status))
				 (matchups (cl-mapcar 'list home away))
				 (games (cl-mapcar 'list matchups innings)))
			     (dolist (el games)
			       (insert (format "%S\n" el))))
			   )
			 ))
	     :status-code
	     '((400 . (lambda (&rest _) (message "Got 400.")))
	       (418 . (lambda (&rest _) (message "Got 418.")))
	       (404 . (lambda (&rest _) (message "Got 404."))))
	     :complete
	     (message "Done.")
	     :error
	     (function* (lambda (&key error-thrown &allow-other-keys&rest _)
			  (message "Error: %S" error-thrown)
			  (kill-buffer "*mlb*")))
	     )
    )
  )

(defun mlb/format-date (year month day)
  "Formats the given YEAR, MONTH and DAY as a path on mlb.com."
  (let* ((a (format "year_%s/month_%s/day_%s" year month day)))
    a)
  )

(defun mlb/get-scores (year month day)
  "Get the YEAR, MONTH and DAY from the user and fetches the scores for that date."
  (interactive (list
		(read-string "Year:")
		(read-string "Month:")
		(read-string "Day:")))
  (mlb/download-file year month day)
  )

(defun mlb/today ()
  "Get the scores for today's games."
  (interactive)
  (let ((y (format-time-string "%Y"))
	(m (format-time-string "%m"))
	(d (format-time-string "%d")))
    (message "Getting games for %s/%s/%s" y m d)
    (mlb/get-scores y m d)
    )
  )

(provide 'mlb-scores)
;;; mlb-scores.el ends here
 
