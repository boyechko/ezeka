(in-package :lishp-user)
(use-package :iterate)

(defun next-unused-slug ()
  (iter (for n from 0 to 999)
        (while (probe-file (make-pathname :name (format nil "~3,'0D" n) :type "txt")))
        (finally (return (format nil "~3,'0D" n)))))

(defun zettelp (filename)
  "Returns T if the filename matches my format for Zettel: 000[-A[-00]]."
  (multiple-value-bind (start end)
      (ppcre:scan "[0-9]{3}((-[A-Z])(-[0-9]{2})*)*" filename)
    (if (and start (= (- end start) (length filename)))
        t
        nil)))

(defun rename-note-and-fix-title (file new-name)
  
  )

(defun rename-notes ()
  (map-sorted-directory
   #'(lambda (file)
       (unless (or (zettelp (pathname-name file))
                   (not (string-equal (pathname-type file) "txt")))
         (let ((next-slug (next-unused-slug))
               (current-name (file-namestring file)))
           (if (y-or-n-p "Rename ~S to ~S?" current-name next-slug)
               (rename-note-and-fix-title file next-slug)
               (progn
                 (princ "Enter new name: ")
                 (let ((new-name (read-line)))
                   (if (and (not (zerop (length new-name)))
                            (y-or-n-p "Rename ~S to ~S?" current-name next-slug))
                       (rename-note-and-fix-title file new-name)
                       (format t "~&Skipping ~S...~%" current-name))))))
         (princ (pathname-filename file))
         (terpri)))
   #p"./"
   #'<
   #'(lambda (file) (sb-posix:stat-mtime (sb-posix:stat file)))))
