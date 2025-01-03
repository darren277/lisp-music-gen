(load "~/quicklisp/setup.lisp")
(load "src/music-gen.lisp")

(ql:quickload '(:hunchentoot :postmodern :cl-json))

(defpackage :web-api
  (:use :cl :hunchentoot :postmodern)
  (:import-from :music-gen :generate-scale :generate-melody)
)

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
  (postmodern:disconnect-toplevel))

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
  (handler-case
    (let ((results (postmodern:query "SELECT * FROM melodies")))
      (setf (hunchentoot:content-type*) "application/json")
      (cl-json:encode-json-to-string 
       `(:status "success" 
         :data ,results)))
    (error (e)
      (setf (hunchentoot:content-type*) "application/json")
      (cl-json:encode-json-to-string 
       `(:status "error" 
         :message "Failed to fetch melodies"
         :details ,(format nil "~A" e))))))

;; REST API endpoint to generate and save melody
(defun fetch-root ()
  "Fetch and validate the root parameter."
  (let ((root (hunchentoot:parameter :root)))
    (if root
        (parse-integer root)
        (error "Root parameter is missing or invalid."))))

(defun generate-major-scale (root-freq)
  "Generate a major scale based on the root frequency."
  (let ((major-scale-intervals '(0 2 4 5 7 9 11 12)))
    (generate-scale root-freq major-scale-intervals)))

(defun generate-random-melody (scale length)
  "Generate a random melody of the given length from the scale."
  (generate-melody scale length))

(defun save-generated-melody (root melody)
  "Save the generated melody to the database."
  (save-melody root melody))

(defun build-json-response (status message &optional melody)
  "Build a JSON response with a status, message, and optional melody."
  (if melody
      (cl-json:encode-json-to-string `(:status ,status :message ,message :melody ,melody))
      (cl-json:encode-json-to-string `(:status ,status :message ,message))))

(hunchentoot:define-easy-handler (simple-tune :uri "/simple-tune/:root") ()
  (format t "Entering simple-tune handler...~%")
  (handler-case
      (let* ((root-str (car (last (cl-ppcre:split "/" (hunchentoot:script-name*)))))
             (root-freq (parse-integer root-str))
              (major-scale (progn
                            (format t "Generating scale with root: ~a~%" root-freq)
                            (generate-major-scale root-freq)))
              (melody-length 16)
              (melody (progn
                        (format t "Generating melody from scale: ~a~%" major-scale)
                        (generate-random-melody major-scale melody-length))))
        (format t "Saving melody: ~a~%" melody)
        (save-generated-melody root-freq melody)
        (setf (header-out "Content-Type") "application/json")
        (format t "Returning melody response: ~a~%" melody)
        (build-json-response "success" "Melody generated successfully" melody))
    (error (e)
      (format t "Error occurred in simple-tune handler: ~a~%" e)
      (setf (header-out "Content-Type") "application/json")
      (build-json-response "error" (princ-to-string e)))))

;; REST API endpoints for CRUD operations on melodies
(defun handle-unsupported-method ()
  "Handle unsupported HTTP methods."
  (setf (header-out "Content-Type") "application/json")
  (cl-json:encode-json-to-string `(:status "error" :message "Method not allowed")))

(defun handle-post-melody ()
  "Handle the POST request to create a new melody."
  (handler-case
    (let* ((raw-data (hunchentoot:raw-post-data :want-stream nil))
           (payload (cl-json:decode-json-from-string 
                     (if (typep raw-data '(vector (unsigned-byte 8)))
                         (babel:octets-to-string raw-data)
                         raw-data)))
           ;; Use assoc instead of getf for alist access
           (root-frequency (cdr (assoc :root--frequency payload)))  ; Note double hyphen
           (melody (cdr (assoc :melody payload))))
      (format t "Decoded payload: ~A~%" payload)
      (format t "Root frequency: ~A~%" root-frequency)
      (format t "Melody: ~A~%" melody)
      
      (if (and root-frequency melody)
          (handler-case
            (progn
              (let ((result (first (postmodern:query 
                           "INSERT INTO melodies (root_frequency, melody) 
                            VALUES ($1, $2) 
                            RETURNING id, root_frequency, melody" 
                           root-frequency 
                           melody))))
                (setf (hunchentoot:content-type*) "application/json")
                (cl-json:encode-json-to-string 
                 `(:status "success" 
                   :message "Melody created"
                   :data, result))))
            (error (e)
              (setf (hunchentoot:content-type*) "application/json")
              (cl-json:encode-json-to-string 
               `(:status "error" 
                 :message "Database error occurred" 
                 :details ,(format nil "~A" e)))))
          (progn
            (setf (hunchentoot:content-type*) "application/json")
            (cl-json:encode-json-to-string
             `(:status "error"
               :message "Invalid payload - root_frequency and melody are required")))))
    (error (e)
      (setf (hunchentoot:content-type*) "application/json")
      (cl-json:encode-json-to-string 
       `(:status "error" 
         :message "Request processing failed" 
         :details ,(format nil "~A" e))))))

(hunchentoot:define-easy-handler (melodies :uri "/melodies") ()
  (case (hunchentoot:request-method*)
    (:get (fetch-melodies))
    (:post (handle-post-melody))
    (otherwise 
     (setf (hunchentoot:content-type*) "application/json")
     (setf (hunchentoot:return-code*) 405)
     (cl-json:encode-json-to-string 
      `(:status "error" 
        :message "Method not allowed")))))

(defun handle-get-melody (id)
  "Fetch and return the melody by ID."
  (let ((parsed-id (parse-integer id)))
    (let ((melody (first (postmodern:query "SELECT * FROM melodies WHERE id = $1" parsed-id))))
      (if melody
          (progn
            (setf (header-out "Content-Type") "application/json")
            (cl-json:encode-json-to-string `(:status "success" :melody ,melody)))
          (progn
            (setf (header-out "Content-Type") "application/json")
            (cl-json:encode-json-to-string `(:status "error" :message "Melody not found")))))))

(defun handle-update-melody (id)
  "Update the melody by ID."
  (handler-case
    (let* ((raw-data (hunchentoot:raw-post-data :want-stream nil))
           (payload (cl-json:decode-json-from-string 
                     (if (typep raw-data '(vector (unsigned-byte 8)))
                         (babel:octets-to-string raw-data)
                         raw-data)))
           (root-frequency (cdr (assoc :root--frequency payload)))
           (melody (cdr (assoc :melody payload)))
           (parsed-id (parse-integer id)))
      (if (and root-frequency melody)
          (handler-case
            (progn
              (let ((result (first (postmodern:query 
                            "UPDATE melodies 
                             SET root_frequency = $1, melody = $2 
                             WHERE id = $3 
                             RETURNING id, root_frequency, melody"
                            root-frequency melody parsed-id))))
                (if result
                    (progn
                      (setf (hunchentoot:content-type*) "application/json")
                      (cl-json:encode-json-to-string 
                       `(:status "success" 
                         :message "Melody updated"
                         :data ,result)))
                    (progn
                      (setf (hunchentoot:content-type*) "application/json")
                      (setf (hunchentoot:return-code*) 404)
                      (cl-json:encode-json-to-string 
                       `(:status "error" 
                         :message "Melody not found"))))))
            (error (e)
              (setf (hunchentoot:content-type*) "application/json")
              (setf (hunchentoot:return-code*) 500)
              (cl-json:encode-json-to-string 
               `(:status "error" 
                 :message "Database error occurred" 
                 :details ,(format nil "~A" e)))))
          (progn
            (setf (hunchentoot:content-type*) "application/json")
            (setf (hunchentoot:return-code*) 400)
            (cl-json:encode-json-to-string 
             `(:status "error" 
               :message "Invalid payload - root_frequency and melody are required")))))
    (error (e)
      (setf (hunchentoot:content-type*) "application/json")
      (setf (hunchentoot:return-code*) 400)
      (cl-json:encode-json-to-string 
       `(:status "error" 
         :message "Request processing failed" 
         :details ,(format nil "~A" e))))))

(defun handle-delete-melody (id)
  "Delete the melody by ID."
  (let ((parsed-id (parse-integer id)))
    (postmodern:execute "DELETE FROM melodies WHERE id = $1" parsed-id)
    (setf (header-out "Content-Type") "application/json")
    (cl-json:encode-json-to-string `(:status "success" :message "Melody deleted"))))


(hunchentoot:define-easy-handler (melody-handler :uri "/melodies/:id") ()
  (let ((id (car (cl-ppcre:all-matches-as-strings "\\d+" (hunchentoot:script-name*)))))
    (case (hunchentoot:request-method*)
      (:get (handle-get-melody id))
      (:put (handle-update-melody id))
      (:delete (handle-delete-melody id))
      (otherwise 
       (setf (hunchentoot:content-type*) "application/json")
       (setf (hunchentoot:return-code*) 405)
       (cl-json:encode-json-to-string 
        `(:status "error" 
          :message "Method not allowed"))))))

(hunchentoot:define-easy-handler (catch-all :uri "/*") ()
  (let ((response `(:status "error" :message "Unhandled request")))
    (format t "Response plist: ~a~%" response)
    (setf (header-out "Content-Type") "application/json")
    (cl-json:encode-json-to-string response)))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(setf hunchentoot:*dispatch-table*
  (list 
        (hunchentoot:create-regex-dispatcher "^/simple-tune/(\\d+)$" 'simple-tune)
        (hunchentoot:create-regex-dispatcher "^/melodies/([0-9]+)$" 'melody-handler)
        (hunchentoot:create-prefix-dispatcher "/melodies" 'melodies)
        (hunchentoot:create-prefix-dispatcher "/yo" 'say-yo)
        (hunchentoot:create-prefix-dispatcher "/" 'catch-all)
  ))

(defun start-server (&key (port (get-env-port 8080)))
  "Start the REST API server on the specified port, defaulting to 8080."
  (connect-to-db)
  (ensure-melody-table)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))
  (format t "Dispatch table: ~a~%" hunchentoot:*dispatch-table*)
  (format t "Server running on port ~a~%" port)
  (format t "Server running on port 5648")
  ;; Prevent SBCL from exiting
  (loop (sleep 1)))

(start-server)
