;; Copyright (C) 2005-2009 by Guillaume Germain, All Rights Reserved.
;; File: "termite.scm"

;; this is the main file for the Termite system

(export
 ;; Termite "primordials"
 self ! ? ?? !? on make-node spawn pid? 
 spawn-link remote-spawn remote-spawn-link
 ;; Useful
 make-tag current-node
 ;; Process linking for error propagation
 inbound-link outbound-link full-link
 ;; Wrap Gambit's I/O
 spawn-output-port spawn-input-port
 ;; Migration
 migrate-task migrate/proxy 
 ;; Useful condition reporting/logging procedures
 warning debug info
 ;; Node stuff
 node-init node? node-host node-port 
 ;; Nameserver mechanism
 ;; make-nameserver-node By Per: This doesn't seem to be defined?
 ;; Publishing and resolving names for services
 publish-service unpublish-service resolve-service remote-service
 ;; default init and node names for convenience
 node1 node2
 ;; *termite-nameserver-port* By Per: This doesn't seem to be defined?
 *termite-cookie*
 ;; Useful
 ping
 
 (re-export: uuid
	     otp/gen_server
	     data
	     deftype
	     recv
	     match))

(import termite_core
        recv
        data
        otp/gen_event
        match)
(declare
  (standard-bindings)
  (extended-bindings)
  (block))

;; ----------------------------------------------------------------------------
;; Services

;; LINKER (to establish exception-propagation links between processes)
(define linker
  (spawn
	(lambda ()
	  (let loop ()
		(recv
		  (('link from to)
		   (cond
			 ((process? from)
			  (process-links-set! from (cons to (process-links from)))) ;;;;;;;;;;
			 ((upid? from)
			  (! (remote-service 'linker (upid-node from))
				 (list 'link from to)))
			 (else
			   (warning "in linker-loop: unknown object"))))
		  (msg
			(warning "linker ignored message: " msg)))
		(loop)))
    name: 'termite-linker))


;; Remote spawning
;; the SPAWNER answers remote-spawn request
(define spawner
  (spawn
	(lambda ()
	  (let loop ()
		(recv
		  ((from tag ('spawn thunk links name))
		   (! from (list tag (spawn thunk links: links name: name))))

		  (msg
			(warning "spawner ignored message: " msg)))
		(loop)))
    name: 'termite-spawner))

;; the PUBLISHER is used to implement a mutable global env. for
;; process names
(define publisher
  (spawn 
	(lambda ()
	  (define dict (make-dict))

	  (let loop ()
		(recv
		  (('publish name pid)
		   (dict-set! dict name pid))

		  (('unpublish name pid)
		   (dict-set! dict name))

		  ((from tag ('resolve name))
		   (! from (list tag (dict-ref dict name))))

		  (msg
			(warning "puslisher ignored message: " msg)))

		(loop)))
    name: 'termite-publisher))

(define (publish-service name pid)
  (! publisher (list 'publish name pid)))

(define (unpublish-service name pid)
  (! publisher (list 'unpublish name pid)))

;; This should probably only used internally
(define (resolve-service name #!optional host)
  (!? publisher (list 'resolve name)))

;; * Get the pid of a service on a remote node 'node' which has been
;; published with |publish-service| to the name 'service-name'.
(define (remote-service service-name node)
  (make-upid service-name node))


;; ----------------------------------------------------------------------------
;; Links

;; * Link another process 'pid' /to/ the current one: any exception
;; not being caught by the remote process and making it crash will be
;; propagated to the current process.
(define (inbound-link pid)
  (! linker (list 'link pid (self))))


;; * Link bidirectionally the current process with another process
;; 'pid': any exception not being caught in any of the two processes
;; will be propagated to the other one.
(define (full-link pid)
  (inbound-link  pid)
  (outbound-link pid))


;; ----------------------------------------------------------------------------
;; Erlang/OTP-like behavior for "generic servers" and "event handlers"

;; (include "otp/gen_server.scm") By Per: Not needed
;; (include "otp/gen_event.scm") By Per: Not needed


;; build a trivial event handler with no state, only invoking a
;; callback on any event
(define (make-simple-event-handler callback initial-state)
  (make-event-handler
   ;; INIT
   (lambda (args)
     initial-state)
   ;; NOTIFY
   (lambda (event state)
     (callback event state))
   ;; CALL
   (lambda (args state)
     (values (void) state))
   ;; TERMINATE
   (lambda (reason state)
     (void))))


;; ----------------------------------------------------------------------------
;; Migration

;; Task moves away, lose identity
(define (migrate-task node)
  (call/cc
	(lambda (k)
	  (remote-spawn node (lambda () (k #t)))
	  (halt!))))

;; Task moves away, leave a proxy behind
(define (migrate/proxy node)
  (define (proxy pid)
	(let loop ()
	  (! pid (?))
	  (loop)))
  (call/cc
	(lambda (k)
	  (proxy
		(remote-spawn-link node (lambda () (k #t)))))))


;; ----------------------------------------------------------------------------
;; A logging facility for Termite

;; (Ideally, this should be included with the services, but the
;; writing style is much different.  Eventually, the other services
;; might use similar style.)

(define (report-event event port)
  (match event
    ((type who messages)
     (with-output-to-port port
       (lambda ()
         (newline)
         (display "[")
         (display type)
         (display "] ")
         (display (formatted-current-time))
         (newline)
         (display who)
         (newline)
         (for-each (lambda (m) (display m) (newline)) messages)
         (force-output))))
    (_ (display "catch-all rule invoked in reporte-event")))
  port)

(define file-output-log-handler
  (make-event-handler
	;; init
	(lambda (args)
	  (match args
		((filename) 
         (open-output-file (list path: filename
                                 create: 'maybe
                                 append: #t)))))
	;; event
	report-event
	;; call
	(lambda (term port)
	  (values (void) port))
	;; shutdown
	(lambda (reason port)
	  (close-output-port port))))


;; 'type' is a keyword (error warning info debug)
(termite-log-fun
 (lambda (type message)
   (event-manager:notify logger (list type (self) message))))

(define logger 
  (let ((logger (event-manager:start name: 'termite-logger)))
	(event-manager:add-handler logger
                                   (make-simple-event-handler
                                    report-event
                                    (current-error-port)))
	(event-manager:add-handler logger
                                   file-output-log-handler
                                   "_termite.log")
	logger))


(define ping-server
  (spawn 
	(lambda ()
	  (let loop ()
		(recv
		  ((from tag 'ping) 
		   (! from (list tag 'pong)))
		  (msg (debug "ping-server ignored message" msg)))
		(loop)))
    name: 'termite-ping-server))

(define (ping node #!optional (timeout 1.0))
  (!? (remote-service 'ping-server node) 'ping timeout 'no-reply))

;; ----------------------------------------------------------------------------
;; Initialization

(process-links-set! (self) '())

(define (node-init node)
  (start-tcp-server (node-port node) start-messenger)
  (set! current-node (lambda () node))
  (publish-external-services)
  'ok)

(define (publish-external-services)
  ;; --------------------
  ;; Services
  
  ;; publishing the accessible exterior services
  ;; (essentially, opening the node to other nodes)
  (publish-service 'spawner spawner)
  (publish-service 'linker linker)
  (publish-service 'ping-server ping-server))


;; Some convenient definitions
(define node1 (make-node "localhost" 3001))
(define node2 (make-node "localhost" 3002))

