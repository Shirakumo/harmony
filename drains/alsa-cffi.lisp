#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-alsa-cffi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.alsa.cffi)
  (:use #:cl #:cffi)
  (:export
   #:libasound
   #:pcm-stream
   #:pcm-format
   #:pcm-access
   #:pcm-open
   #:pcm-set-params
   #:pcm-writei
   #:pcm-recover
   #:pcm-pause
   #:pcm-close
   #:strerror))
(in-package #:org.shirakumo.fraf.harmony.drains.alsa.cffi)

(define-foreign-library libasound
  (t (:default "libasound")))

(use-foreign-library libasound)

(defcenum pcm-stream
  :playback
  :capture)

(defcenum pcm-format
  :unknown
  :s8
  :u8
  :s16-le
  :s16-be
  :u16-le
  :u16-be
  :s24-le
  :s24-be
  :u24-le
  :u24-be
  :s32-le
  :s32-be
  :u32-le
  :u32-be
  :float-le
  :float-be
  :float64-le
  :float64-be
  :iec958-subframe-le
  :iec958-subframe-be
  :mu-law
  :a-law
  :ima-adpcm
  :mpeg
  :gsm
  :special
  :s24-3le
  :s24-3be
  :u24-3le
  :u24-3be
  :s20-3le
  :s20-3be
  :u20-3le
  :u20-3be
  :s18-3le
  :s18-3be
  :u18-3le
  :u18-3be
  :s16
  :u16
  :s24
  :u24
  :s32
  :u32
  :float
  :float64
  :iec958-subframe)

(defcenum pcm-access
  :mmap-interleaved
  :mmap-noninterleaved
  :mmap-complex
  :rw-interleaved
  :rw-noninterleaved)

(defcfun (pcm-open "snd_pcm_open") :int
  (pcm :pointer)
  (name :string)
  (stream pcm-stream)
  (mode :int))

(defcfun (pcm-set-params "snd_pcm_set_params") :int
  (pcm :pointer)
  (format pcm-format)
  (access pcm-access)
  (channels :uint)
  (rate :uint)
  (soft-resample :int)
  (latency :uint))

(defcfun (pcm-writei "snd_pcm_writei") :long
  (pcm :pointer)
  (buffer :pointer)
  (frames :ulong))

(defcfun (pcm-recover "snd_pcm_recover") :int
  (pcm :pointer)
  (err :int)
  (silent :int))

(defcfun (pcm-pause "snd_pcm_pause") :int
  (pcm :pointer)
  (enable :int))

(defcfun (pcm-close "snd_pcm_close") :int
  (pcm :pointer))

(defcfun (strerror "snd_strerror") :string
  (error :int))
