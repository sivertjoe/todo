#!/usr/bin/env sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-tui)
(ql:quickload :cl-store)

(defpackage #:todo
  (:use :cl :cl-tui))
(in-package #:todo)

(defvar *cursor* 0)
(defvar *cursor-c* 0)
(defvar *lists* '())

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
  (cl-store:store *lists* +path+))

(defun loader ()
  (if (probe-file +path+)
    (cl-store:restore +path+)
    (loop for x from 0 below 3
          for l = '()
          collect
            (loop
              for x from 1 to 30
              collect (make-instance 'item 
                                     :str (format nil "lesson ~a" x)
                                    :done nil)))))

(defun main-render (&key frame h w id)
  (draw-box frame)
  (let* ((title (format nil "Series ~a" id))
        (offset   (- (/ w 2) (/ (length title) 2))))
    (put-text frame 0 2 title)

    (loop 
      for item in (nth id *lists*)
      for h from 1
      do (put-text frame h 2 (build-string item (and (= id *cursor-c*) (= h (1+ *cursor*))))))))

(defun text-render (&key frame h w)
  (put-text frame 0 0 "Press `Esc` or `Q` to quit.."))

(defmacro generate (n container)
  `(-generate 0 ,n ,container))

(defmacro -generate (n high container)
  `(when (/= ,n ,high)
     (define-frame ,(gensym) (simple-frame :render (lambda (&rest kwargs) (apply #'main-render :id ,n kwargs))) :on ,container)
     (-generate (1+ ,n) ,high ,container)))

(defun move-cursor (f)
  (setf *cursor* (mod (funcall f *cursor*) (length (nth *cursor-c* *lists*)))))

(defun move-cursor-c (f)
  (setf *cursor-c* (mod (funcall f *cursor-c*) (length *lists*))))

(defun start-cursor (col)
  (or (position-if (lambda (item) (not (done item))) (nth col *lists*)) 0))

(defun start-cursor-c ()
  (or (loop for li in *lists*
        for index from 0
        when (find-if (lambda (item) (not (done item))) li)
        return index)
      0))

(defun main ()
  (setf *lists* (loader))
  (setf *cursor-c* (start-cursor-c))
  (setf *cursor* (start-cursor *cursor-c*))
  (with-screen ()
    (loop
      (display)
       (let ((key (read-key)))
         (cond 
           ((or (eql key #\Esc) (eql key #\q)) (return))
           ((or (eql key #\j) (eql key :key-down)) (move-cursor #'1+))
           ((or (eql key #\k) (eql key :key-up)) (move-cursor #'1-))
           ((or (eql key #\l) (eql key :key-right)) (move-cursor-c #'1+))
           ((or (eql key #\h) (eql key :key-left)) (move-cursor-c #'1-))

           ((or (eql key #\Newline) (eql key #\x))
            (progn
              (flip (nth *cursor* (nth *cursor-c* *lists*)))
              (store))))))))

;; (sb-ext:save-lisp-and-die "lessons" :toplevel #'main :executable t)

(define-frame container (container-frame) :on :root)
(define-frame lessons (container-frame :split-type :horizontal) :on container)
(generate 3 lessons)
(define-frame text (simple-frame :render #'text-render) :on container :h 1)

(main)
