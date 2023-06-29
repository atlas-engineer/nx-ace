;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+nyxt-2
(defpackage #:nx-ace
  (:use #:cl))
#-nyxt-2
(nyxt:define-package #:nx-ace
  (:use #:cl
        #+(or nyxt-2 3-pre-release-1 3-pre-release-2 3-pre-release-3 3-pre-release-4 3-pre-release-5 3-pre-release-6)
        #:nyxt/editor-mode
        #-(or nyxt-2 3-pre-release-1 3-pre-release-2 3-pre-release-3 3-pre-release-4 3-pre-release-5 3-pre-release-6)
        #:nyxt/mode/editor)
  (:import-from
   #+(or 3-pre-release-1 3-pre-release-2 3-pre-release-3 3-pre-release-4 3-pre-release-5 3-pre-release-6)
   #:nyxt/passthrough-mode
   #-(or 3-pre-release-1 3-pre-release-2 3-pre-release-3 3-pre-release-4 3-pre-release-5 3-pre-release-6)
   #:nyxt/mode/passthrough
   #:passthrough-mode))
