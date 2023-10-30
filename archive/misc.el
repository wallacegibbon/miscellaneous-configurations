(defun read-file (filename)
  "Read the full contents from FILENAME and return the result as a string"
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min) (point-max))))


(defun read-file-trim (filename)
  "Read contents from FILENAME and trim the spaces at the beginning and end"
  (replace-regexp-in-string
   "^[ \t\n]*\\(.*[^ \t\n]\\)[ \t\n]*$"
   "\\1"
   (read-file filename)))


(defun read-number-from-file (filename)
  (string-to-number (read-file filename)))


(defun power-info ()
  (list (read-number-from-file
	 "/sys/class/power_supply/axp288_fuel_gauge/charge_now")
	(read-number-from-file
	 "/sys/class/power_supply/axp288_fuel_gauge/charge_full")
	(read-file-trim
	 "/sys/class/power_supply/axp288_fuel_gauge/status")))


(defun thermal-cpu ()
  (read-number-from-file "/sys/class/thermal/thermal_zone0/temp"))


(defun power-info-string ()
  (let* ((v-info (power-info))
	 (now (car v-info))
	 (max (cadr v-info))
	 (status (caddr v-info)))
    (format "%.2f%% (%s)"
	    (* 100 (/ now max 1.0)) status)))


(defun thermal-cpu-string ()
  (format "%.2f 'C" (/ (thermal-cpu) 1000.0)))


(defun misc-info-show ()
  (interactive)
  (message "CPU temperature: %s, Power: %s"
	   (thermal-cpu-string)
	   (power-info-string)))

