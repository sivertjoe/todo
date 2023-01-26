#!/usr/bin/env sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-tui)
(ql:quickload :cl-store)

(defpackage #:todo
  (:use :cl :cl-tui))
(in-package #:todo)

(defvar *cursor* 0)
(defvar *items* '())
(defconstant +path+ (merge-pathnames #p".todo" (user-homedir-pathname)))

(defclass item ()
  ((str :initarg :str :accessor str)
   (done :initarg :done :accessor done)))

(defmethod build-string (item has-cursor?)
  (let ((cursor (if has-cursor? ">" " "))
        (done (if (done item) "x" " ")))
    (format nil "~a * [~a] ~a" cursor done (str item))))

(defmethod flip (item)
  (setf (done item) (not (done item))))

(defun store ()
  (cl-store:store *items* +path+))

(defun loader ()
  (if (probe-file +path+)
    (cl-store:restore +path+)
    (loop
      for x from 1 to 30
      collect (make-instance 'item
                             :str (format nil "lesson ~a" x)
                             :done nil))))

(defun main-render (&key frame h w)
  (loop 
    for item in *items*
    for h from 0
    do (put-text frame h 0 (build-string item (eql h *cursor*))))
  (put-text frame (1- h) 0 "Press `Esc` or `Q` to quit.."))

(defun move-cursor (f)
  (setf *cursor* (mod (funcall f *cursor*) (length *items*))))

(defun start-cursor ()
  (or (position-if (lambda (item) (not (done item))) *items*) 0))

(defun main ()
  (setf *items* (loader))
  (setf *cursor* (start-cursor))
  (with-screen ()
    (loop
      (refresh)
       (let ((key (read-key)))
         (cond 
           ((or (eql key #\Esc) (eql key #\q)) (return))
           ((or (eql key #\j) (eql key :key-down)) (move-cursor #'1+))
           ((or (eql key #\k) (eql key :key-up)) (move-cursor #'1-))

           ((or (eql key #\Newline) (eql key #\x))
            (progn
              (flip (nth *cursor* *items*))
              (store))))))))


;; (sb-ext:save-lisp-and-die "lessons" :toplevel #'main :executable t)

(define-frame main (simple-frame :render #'main-render) :on :root)

(main)
