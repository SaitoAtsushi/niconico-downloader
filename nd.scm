#!/usr/bin/env gosh
;;; -*- coding: utf-8; mode:gauche; -*-
;;; Author: SAITO Atsushi

;;: set your account
(define *mail* "Set your email address.")
(define *password* "Set your password.")

;;; set your filesystem encode
(define *fsencode*
  (cond-expand (gauche.os.windows 'Shift_JIS)
               (else 'utf8)))

(define-syntax use-each
  (syntax-rules ()
    [(_ a ...) (begin (use a) ...)]))

(use-each srfi-1 srfi-27 srfi-43 rfc.uri rfc.http sxml.ssax sxml.sxpath
          gauche.charconv gauche.parameter gauche.parseopt gauche.vport
          gauche.uvector gauche.sequence util.match text.progress www.cgi)

(define waiting-time (make-parameter 300))
(define shuffle-list (make-parameter #f))
(define economy-deny (make-parameter #f))

(define (abstruct-cookie header reg)
  (rxmatch-substring
   (any (compose reg cadr)
        (filter ($ string=? "set-cookie" $ car $) header))))

(define-class <nico> ()
  ((user-session)
   (history)))

(define-method nico-login ((self <nico>) mail password)
  (receive (status header body)
      (http-post "secure.nicovideo.jp" "/secure/login?site=niconico"
                 `(("mail" ,mail) ("password" ,password))
                 :secure #t
                 :no-redirect #t)
    (slot-set! self 'user-session
               (abstruct-cookie header #/user_session=user_session_[^;]+/))
    status))

(define-method nico-prewatch ((self <nico>) video-id)
  (receive (status header body)
      (http-get "www.nicovideo.jp" #`"/watch/,|video-id|"
                :cookie (~ self 'user-session))
    (slot-set! self 'history
               (abstruct-cookie header #/nicohistory=[^;]+/ ))
    status))

(define (get-info video-id)
  (ssax:xml->sxml
   (open-input-string
    (string-incomplete->complete
     (receive (status header body)
         (http-get "flapi.nicovideo.jp" #`"/api/getthumbinfo?v=,|video-id|")
       body)
     :omit))
   '()))

(define-syntax define-accessor
  (syntax-rules ()
    [(_ name path) (define name (if-car-sxpath path))]))

(define-accessor title '(nicovideo_thumb_response thumb title *text*))
(define-accessor type  '(nicovideo_thumb_response thumb movie_type *text*))
(define-accessor sizehigh '(nicovideo_thumb_response thumb size_high *text*))
(define-accessor sizelow '(nicovideo_thumb_response thumb size_low *text*))

(define-method get-threadkey ((self <nico>) video-id)
  (receive (status header body)
      (http-get "flapi.nicovideo.jp"
                `("/api/getthreadkey"
                  (thread ,video-id)))
    (if-let1 m (#/threadkey=([^&]+)&force_184=(.+)/ body)
      (values (m 1) (m 2)))))

(define-method get-url ((self <nico>) video-id)
  (receive (status header body)
      (http-get "flapi.nicovideo.jp"
                #`"/api/getflv?v=,|video-id|&as3=1"
                :cookie (~ self 'user-session))
    (^p (display body p))
    (rxmatch-case body
      (#/deleted=1/ (#f) (print video-id " is deleted.") #f)
      (#/url=([^&]+)/ (#f m) (uri-decode-string m))
      (#/url=&/ (#f m) (uri-decode-string m))
      (#/error=access_locked/ (#f)
       (print "Access Locked. Challenge Later. " (print video-id))
       (sys-sleep (waiting-time))
       (get-url self video-id))
      (else (error "Invalid URL-info : " body video-id))
      )))

(define (uri-split uri)
  (let1 m (#/:\/\/([^\/]+)(\/.+)/ uri)
    (values (m 1) (m 2))))

(define path-cleanup (cut regexp-replace-all #/[\\\/:*?\"<>\|\t]/ <> "_"))

(define fsencode (cut ces-convert <> (gauche-character-encoding) *fsencode*))

(define-class <progress-output-port> (<buffered-output-port>)
  ((port :init-keyword :port)
   (max-value :init-keyword :max-value)
   (header :init-keyword :header)
   (progress :init-keyword :progress)))

(define-method initialize ((self <progress-output-port>) initargs)
  (next-method self
    (cons*
     :flush (^[v f]
              (rlet1 size (size-of v)
                (write-block v (~ self 'port))
                ((~ self 'progress) 'inc size)))
     :close (^[] ((~ self 'progress) 'finish))
     :progress
     (make-text-progress-bar :max-value (get-keyword :max-value initargs)
                             :header (get-keyword :header initargs)
                             :header-width 11
                             :num-width 18
                             :bar-width 35)
     initargs)))

(define (info->size info economy)
  (string->number ((if economy sizelow sizehigh) info)))

(define (make-filename prefix video-id title type)
  (fsencode
   (path-cleanup
    #`",|prefix|,|video-id| ,|title|.,|type|")))

(define-syntax call-with-output-progress-port
  (syntax-rules ()
    ((_ port (options ...) body ...)
     (let*-values (((port) (make <progress-output-port> options ...))
                   (result (begin body ...)))
       (close-output-port port)
       (apply values result)))))

(define-method nico-download-http ((self <nico>) id url tempfile size)
  (call-with-output-file tempfile
    (^p
     (call-with-output-progress-port port (:port p :max-value size :header id)
       (receive (domain path) (uri-split url)
         (http-get domain path
                   :cookie #`",(~ self 'user-session); ,(~ self 'history)"
                   :sink port
                   :flusher (^[p header]
                              (flush p)
                              (regexp-replace "x-shockwave-flash"
                                (rxmatch->string #/[^\/]+$/
                                  (cgi-get-parameter "content-type" header))
                                "swf"))))))))

(define-method nico-download ((self <nico>) id)
  (and-let* ((info (get-info id))
             (title (title info))
             (video-url (get-url self id))
             (economy (^[] (#/low$/ video-url)))
             (prefix (if (economy) "[Economy]" ""))
             (size (info->size info (economy)))
             (tempfile #`",|prefix|,|id|.part"))
    (unless (and (economy) (economy-deny))
      (nico-prewatch self id)
      (receive (status header type)
          (nico-download-http self id video-url tempfile size)
        (if (string=? "200" status)
            (if (= (sys-stat->size (sys-stat tempfile))
                   (string->number (cgi-get-parameter "content-length" header)))
                (let1 filename (make-filename prefix id title type)
                  (sys-rename tempfile filename)))
            (sys-unlink tempfile)
            )))))

(define (parse-nico-uri uri)
  (cond ((#/((?:sm|nm|so)?\d+)(?:\?.+)?$/ uri) => (cut <> 1))
        (else (error "invalid video-id : " uri))))

(define (usage cmd)
  (print "usage: " cmd " [options] [args]")
  (print "\
Options:
  --wait  second    waiting time in case of retry. (default=300sec)
  --listfile file   reading list from file.
  --economydeny     deny when economy-time.
  --info            download with metadata. (not implement yet.)
  --shuffle         shuffle list before download.
  --help            show usage.
")
  (exit))

(define file->list (cut call-with-input-file <> (pa$ port->list read-line)))

(define random-source (make-random-source))

(define (shuffle-if-needed ls)
  (random-source-randomize! random-source)
  (if (shuffle-list) (shuffle ls random-source) ls))

(define (main args)
  (let-args (cdr args)
      ((wait  "w|wait=i" 300 => waiting-time)
       (shuffle? "s|shuffle" => (cut shuffle-list #t))
       (listfile  "l|listfile=s"  #f)
       (deny? "d|economydeny" => (cut economy-deny #t))
       (help  "h|help" => (cut usage (car args)))
       . targets)
    (let ((targets (if listfile (file->list listfile) targets))
          (nico-obj (make <nico>)))
      (if (null? targets)
          (usage (car args))
          (begin
            (nico-login nico-obj *mail* *password*)
            (for-each ($ nico-download nico-obj $ parse-nico-uri $)
                      (shuffle-if-needed targets))))
      0)))
