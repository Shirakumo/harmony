#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-pulse-cffi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.pulse.cffi)
  (:use #:cl #:cffi)
  (:export
   #:libpulse
   #:libpulse-simple
   #:sample-format
   #:stream-direction
   #:channel-position
   #:channel-map
   #:channel-map-channels
   #:channel-map-map
   #:sample-spec
   #:sample-spec-format
   #:sample-spec-rate
   #:sample-spec-channels
   #:buffer-attr
   #:buffer-attr-maxlength
   #:buffer-attr-length
   #:buffer-attr-prebuf
   #:buffer-attr-minreq
   #:buffer-attr-fragsize
   #:strerror
   #:simple-new
   #:simple-write
   #:simple-drain
   #:simple-flush
   #:simple-free))
(in-package #:org.shirakumo.fraf.harmony.drains.pulse.cffi)

(define-foreign-library libpulse
  (t (:default "libpulse")))

(define-foreign-library libpulse-simple
  (t (:default "libpulse-simple")))

(use-foreign-library libpulse)
(use-foreign-library libpulse-simple)

(defconstant +channels-max+ 32)

(defctype size_t #+x86-64 :uint64 #+x86 :uint32)

(defcenum sample-format
  :u8
  :alaw
  :ulaw
  :s16le
  :s16be
  :float32le
  :float32be
  :s32le
  :s32be
  :s24le
  :s24be
  :s24-32le
  :s24-32be
  :max
  :invalid)

(defcenum stream-direction
  :no-direction
  :playback
  :record
  :upload)

(defcenum channel-position
  :invalid
  :mono
  :front-left
  :front-right
  :front-center
  :rear-center
  :rear-left
  :rear-right
  :lfe
  :front-left-of-center
  :front-right-of-center
  :side-left
  :side-right
  :aux0
  :aux1
  :aux2
  :aux3
  :aux4
  :aux5
  :aux6
  :aux7
  :aux8
  :aux9
  :aux10
  :aux11
  :aux12
  :aux13
  :aux14
  :aux15
  :aux16
  :aux17
  :aux18
  :aux19
  :aux20
  :aux21
  :aux22
  :aux23
  :aux24
  :aux25
  :aux26
  :aux27
  :aux28
  :aux29
  :aux30
  :aux31
  :top-center
  :top-front-left
  :top-front-right
  :top-front-center
  :top-rear-left
  :top-rear-right
  :top-rear-center
  :max)

(defcstruct (channel-map :class channel-map :conc-name channel-map-)
  (channels :uint8)
  (map channel-position :count #.+channels-max+))

(defcstruct (sample-spec :class sample-spec :conc-name sample-spec-)
  (format sample-format)
  (rate :uint32)
  (channels :uint8))

(defcstruct (buffer-attr :class buffer-attr :conc-name buffer-attr-)
  (maxlength :uint32)
  (length :uint32)
  (prebuf :uint32)
  (minreq :uint32)
  (fragsize :uint32))

(defcfun (strerror "pa_strerror") :string
  (error :int))

(defcfun (simple-new "pa_simple_new") :pointer
  (server :string)
  (name :string)
  (direction stream-direction)
  (device :string)
  (stream-name :string)
  (sample-spec :pointer)
  (channel-map :pointer)
  (buffer-attributes :pointer)
  (error :pointer))

(defcfun (simple-write "pa_simple_write") :int
  (simple :pointer)
  (data :pointer)
  (bytes size_t)
  (error :pointer))

(defcfun (simple-drain "pa_simple_drain") :int
  (simple :pointer)
  (error :pointer))

(defcfun (simple-flush "pa_simple_flush") :int
  (simple :pointer)
  (error :pointer))

(defcfun (simple-free "pa_simple_free") :void
  (simple :pointer))
