(load "~/quicklisp/setup.lisp")
;;(load "music-gen.lisp")

(ql:quickload '(:hunchentoot :postmodern :cl-json))

;;(defpackage :web-api
;;  (:use :cl :hunchentoot :music-gen :postmodern))

(defpackage :web-api
  (:use :cl :hunchentoot :postmodern))

(in-package :web-api)

(defun get-env-port (default-port)
  "Retrieve the port number from the environment variable `PORT` or use the default."
  (let ((env-port (uiop:getenv "PORT")))
    (if env-port
        (parse-integer env-port)
        default-port)))

(defun connect-to-db ()
  "Establish a connection to the PostgreSQL database using environment variables."
  (let ((host (or (uiop:getenv "PG_HOST") "localhost"))
        (port (or (uiop:getenv "PG_PORT") "5432"))
        (db (or (uiop:getenv "PG_DB") "testdb"))
        (user (or (uiop:getenv "PG_USER") "user"))
        (pass (or (uiop:getenv "PG_PASS") "password")))
    (postmodern:connect-toplevel db user pass host :port (parse-integer port))))

(defun disconnect-from-db ()
  "Disconnect from the PostgreSQL database."
  (postmodern:disconnect-toplevel *db-connection*))

;; Create table for melodies if it doesn't exist
(defun ensure-melody-table ()
  "Ensure the melodies table exists."
  (postmodern:execute
   "CREATE TABLE IF NOT EXISTS melodies (
      id SERIAL PRIMARY KEY,
      root_frequency FLOAT NOT NULL,
      melody TEXT NOT NULL
    );"))

;; Save a melody to the database
(defun save-melody (root-frequency melody)
  "Save the generated melody to the database."
  (postmodern:execute
   "INSERT INTO melodies (root_frequency, melody) VALUES ($1, $2)"
   root-frequency
   (format nil "~{~a~^,~}" melody)))

;; Fetch all melodies from the database
(defun fetch-melodies ()
  "Fetch all melodies from the database."
  (postmodern:query "SELECT id, root_frequency, melody FROM melodies"))

;; REST API endpoint to generate and save melody
(defparameter *simple-tune-handler*
  (define-easy-handler (simple-tune :uri "/simple-tune/:root") ()
    (let* ((root (parse-integer (getf *params* :root)))
          (major-scale-intervals '(0 2 4 5 7 9 11 12))
          (major-scale (generate-scale root major-scale-intervals))
          (melody-length 16)
          (melody (generate-melody major-scale melody-length)))
      (save-melody root melody)
      (setf (header-out "Content-Type") "application/json")
      (json-encode-plist `(:status "success" :melody ,melody)))))

;; REST API endpoint to list all melodies
(defparameter *list-melodies-handler*
  (define-easy-handler (list-melodies :uri "/melodies") ()
    (let ((melodies (fetch-melodies)))
      (setf (header-out "Content-Type") "application/json")
      (json-encode-plist `(:status "success" :melodies ,melodies)))))

;; REST API endpoints for CRUD operations on melodies
(defparameter *melodies-handler*
  (define-easy-handler (melodies-handler :uri "/melodies") ()
    (case (hunchentoot:request-method *request*)
      (:post
      (let* ((payload (cl-json:decode-json-from-string (hunchentoot:raw-post-data *request*)))
              (root-frequency (getf payload :root_frequency))
              (melody (getf payload :melody)))
        (if (and root-frequency melody)
            (progn
              (postmodern:execute "INSERT INTO melodies (root_frequency, melody) VALUES ($1, $2)"
                                  root-frequency melody)
              (setf (header-out "Content-Type") "application/json")
              (json-encode-plist `(:status "success" :message "Melody created"))))
            (progn
              (setf (header-out "Content-Type") "application/json")
              (json-encode-plist `(:status "error" :message "Invalid payload"))))))
      (t
      (progn
        (setf (header-out "Content-Type") "application/json")
        (json-encode-plist `(:status "error" :message "Method not allowed"))))))

(defun handle-get-melody (id)
  "Fetch and return the melody by ID."
  (let ((parsed-id (parse-integer id)))
    (let ((melody (first (postmodern:query "SELECT * FROM melodies WHERE id = $1" parsed-id))))
      (if melody
          (progn
            (setf (header-out "Content-Type") "application/json")
            (json-encode-plist `(:status "success" :melody ,melody)))
          (progn
            (setf (header-out "Content-Type") "application/json")
            (json-encode-plist `(:status "error" :message "Melody not found")))))))

(defun handle-update-melody (id)
  "Update the melody by ID."
  (let* ((parsed-id (parse-integer id))
         (payload (cl-json:decode-json-from-string (hunchentoot:raw-post-data *request*)))
         (root-frequency (getf payload :root_frequency))
         (melody (getf payload :melody)))
    (if (and root-frequency melody)
        (progn
          (postmodern:execute "UPDATE melodies SET root_frequency = $1, melody = $2 WHERE id = $3"
                              root-frequency melody parsed-id)
          (setf (header-out "Content-Type") "application/json")
          (json-encode-plist `(:status "success" :message "Melody updated"))))
        (progn
          (setf (header-out "Content-Type") "application/json")
          (json-encode-plist `(:status "error" :message "Invalid payload")))))

(defun handle-delete-melody (id)
  "Delete the melody by ID."
  (let ((parsed-id (parse-integer id)))
    (postmodern:execute "DELETE FROM melodies WHERE id = $1" parsed-id)
    (setf (header-out "Content-Type") "application/json")
    (json-encode-plist `(:status "success" :message "Melody deleted"))))

(defun handle-unsupported-method ()
  "Handle unsupported HTTP methods."
  (setf (header-out "Content-Type") "application/json")
  (json-encode-plist `(:status "error" :message "Method not allowed")))

(defparameter *melody-handler*
  (define-easy-handler (melody-handler :uri "/melodies/:id") ()
    (let ((id (hunchentoot:parameter :id)))
      (case (hunchentoot:request-method *request*)
        (:get (handle-get-melody id))
        (:put (handle-update-melody id))
        (:delete (handle-delete-melody id))
        (t (handle-unsupported-method))))))

(defun start-server (&key (port (get-env-port 8080)))
  "Start the REST API server on the specified port, defaulting to 8080."
  (connect-to-db)
  (ensure-melody-table)
  (setf hunchentoot:*dispatch-table*
        (list *simple-tune-handler*
              *list-melodies-handler*
              *melodies-handler*
              *melody-handler*))
  (start (make-instance 'hunchentoot:easy-acceptor :port port))
  (format t "Server running on port ~a~%" port))
