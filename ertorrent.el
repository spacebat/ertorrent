;;; ertorrent.el --- an rtorrent xml-rpc interface and ui

;; Copyright (C) 2013 Andrew Kirkpatrick

;; Author:     Andrew Kirkpatrick <ubermonk@gmail.com>
;; URL:        https://github.com/spacebat/ertorrent
;; Created:    07 Apr 2013
;; Keywords:   convenience, lisp
;; Version:    0.1
;; Package-Requires: (tabulated-list xrc)

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You may have received a copy of the GNU General Public License
;; along with this program.  If not, see:
;; <http://www.gnu.org/licenses/>.

;; Local Variables:
;; lexical-binding: t
;; End:

;;; Commentary:

;; Sick of the clunkiness of the rtorrent CLI interface and the rtgui web interface? This library gives you the chance to be sick of an emacs interface to rtorrent.

;; For example, the code is currently extremely incomplete and buggy.

;; The xrc library is available at https://github.com/spacebat/xrc

;;; Code:

(require 'tabulated-list)
(require 'xrc)

(defvar ertorrent-default-url "http://localhost/RPC2"
  "The XML-RPC interface URL to use by default for rtorrent")

(defvar ertorrent-xrc-endpoint nil)
(defvar ertorrent-xrc-caller nil)

(defun ertorrent--list-torrents ()
  "List of torrents in `tabulated-list-mode' format on an rtorrent instance."
  ;; '((meh ["one" "two" "three" "four"]))
  (message "buffer is %s" (current-buffer))
  (message "ertorrent-xrc-endpoint is %s" ertorrent-xrc-endpoint)
  (message "ertorrent-xrc-caller is %s" ertorrent-xrc-caller)
  
  (loop for torrent in (ertorrent-get-list ertorrent-xrc-caller :context "main")
        for hash = (ertorrent-torrent-val torrent 'hash)
        if hash
        collect
        (list hash
              (vector (ertorrent-torrent-val torrent 'name)
                      (ertorrent-torrent-status-string torrent)
                      (format "%s" (ertorrent-torrent-percent-complete torrent))
                      (format "%s" (ertorrent-torrent-val torrent 'left-bytes)))))
  )

(define-derived-mode
  ertorrent-list-mode tabulated-list-mode "RTorrent torrent list"
  "Major mode for listing torrents on an rtorrent instance."
  (setq tabulated-list-entries 'ertorrent--list-torrents
        tabulated-list-format [("Torrent Name" 60 nil)
                               ("Status" 10 nil)
                               ("Done" 10 nil)
                               ("Remain" 10 nil)
                               ;; ("Size" 10 nil)
                               ;; ("Down" 10 nil)
                               ;; ("Up" 10 nil)
                               ;; ("Seeded" 10 nil)
                               ;; ("Ratio" 10 nil)
                               ;; ("Peers" 10 nil)
                               ])
  (tabulated-list-init-header))

;;;###autoload
(defun ertorrent-list-torrents (&optional url)
  "List torrents on an rtorrent instance."
  (interactive
   ;; "sRTorrent URL: "
   (list (read-string "RTorrent URL: " ertorrent-default-url))
   )
  (with-current-buffer (get-buffer-create (format "*rtorrent %s*" url))
    (setq lexical-binding t)
    (setq ertorrent-xrc-endpoint (xrc-make-endpoint :url url))
    (setq ertorrent-xrc-caller (xrc-caller ertorrent-xrc-endpoint))
    (message "in buffer %s" (current-buffer))
    (message "set ertorrent-xrc-endpoint to %s" ertorrent-xrc-endpoint)
    (message "set ertorrent-xrc-caller to %s" ertorrent-xrc-caller)
    (ertorrent-list-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(defconst ertorrent--torrent-fields
  '(base-filename
    base-path
    bytes-done
    chunk-size
    chunks-hashed
    complete
    completed-bytes
    completed-chunks
    connection-current
    connection-leech
    connection-seed
    creation-date
    directory
    down-rate
    down-total
    free-diskspace
    hash
    hashing
    ignore-commands
    left-bytes
    local-id
    local-id-html
    max-file-size
    message
    peers-min
    name
    peer-exchange
    peers-accounted
    peers-complete
    peers-connected
    peers-max
    peers-not-connected
    priority
    priority-str
    ratio
    size-bytes
    size-chunks
    size-files
    skip-rate
    skip-total
    state
    state-changed
    tied-to-file
    tracker-focus
    tracker-numwant
    tracker-size
    up-rate
    up-total
    uploads-max
    is-active
    is-hash-checked
    is-hash-checking
    is-multi-file
    is-open
    is-private))

(defvar ertorrent--torrent-fields-str nil)

(defvar ertorrent--sym-str (make-hash-table))

;; populate the strings and symbol to string map
(setq ertorrent--torrent-fields-str
      (loop for sym in ertorrent--torrent-fields
            for str = (replace-regexp-in-string "-" "_" (symbol-name sym))
            collect (puthash sym str ertorrent--sym-str)))


(defstruct (ertorrent-torrent (:constructor ertorrent--make-torrent))
  data)

(defun make-ertorrent-torrent (&rest args)
  (let* ((torrent (ertorrent--make-torrent :data (make-hash-table)))
         (data (ertorrent-torrent-data torrent))
         extras)
    (loop for (key val) on args by 'cddr do
          (if (gethash key ertorrent--sym-str)
              (puthash key val data)
            (push key extras)))
    (when extras
      (message "Extra keys passed to make-ertorrent-torrent: %s" extras))
    torrent))

(defun* ertorrent-get-list (xrcaller &key (context "main"))
  (let ((responses
         (funcall xrcaller
                  'd.multicall
                  (append (list context)
                          (mapcar (lambda (str)
                                    (concat "d." (if (string-match "^is_" str) "" "get_") str "="))
                                  ertorrent--torrent-fields-str)))))
    (loop for response in responses
          collect (apply 'make-ertorrent-torrent
                         (loop for key in ertorrent--torrent-fields
                               for val in response
                               collect key
                               collect val)))))

(defun ertorrent-torrent-val (torrent field)
  (gethash field (ertorrent-torrent-data torrent)))

(defun ertorrent-torrent-val-checked (torrent field)
  (if (gethash field ertorrent--sym-str)
      (gethash field (ertorrent-torrent-data torrent))
    (error "Unknown torrent field '%s'" field)))


;; $retarr[$index]['percent_complete']=@floor(($retarr[$index]['completed_bytes'])/($retarr[$index]['size_bytes'])*100);
(defun ertorrent-torrent-percent-complete (torrent)
  (if (ertorrent-torrent-hashing-p torrent)
      (* (round (ertorrent-torrent-val torrent 'chunks-hashed)
                (ertorrent-torrent-val torrent 'size-chunks))
         100)
    (* (floor (ertorrent-torrent-val torrent 'completed-bytes)
              (ertorrent-torrent-val torrent 'size-bytes))
       100)))

;; $retarr[$index]['bytes_diff']=($retarr[$index]['size_bytes']-$retarr[$index]['completed_bytes']);
(defun ertorrent-torrent-bytes-diff (torrent)
  (- (ertorrent-torrent-val torrent 'size-bytes)
     (ertorrent-torrent-val torrent 'completed-bytes)))

;; assuming this is a number
(defun ertorrent-torrent-active-p (torrent)
  (not (zerop (ertorrent-torrent-val torrent 'is-active))))

(defun ertorrent-torrent-complete-p (torrent)
  (not (zerop (ertorrent-torrent-val torrent 'complete))))

(defun ertorrent-torrent-hashing-p (torrent)
  (not (zerop (ertorrent-torrent-val torrent 'hashing))))

(defun ertorrent-torrent-status-string (torrent)
  (cond
   ((not (ertorrent-torrent-active-p torrent))
    "Stopped")
   ((ertorrent-torrent-val torrent 'complete)
    "Complete")
   ((and (ertorrent-torrent-active-p torrent)
         (string= (ertorrent-torrent-val torrent 'connection-current) "leech"))
    "Leeching")
   ((and (ertorrent-torrent-active-p torrent)
         (ertorrent-torrent-complete-p))
    "Seeding")
   ((ertorrent-torrent-hashing-p torrent)
    "Hashing")
   (t
    "Unknown")))

;; if ($retarr[$index]['is_active']==0) $retarr[$index]['status_string']="Stopped";
;; if ($retarr[$index]['complete']==1) $retarr[$index]['status_string']="Complete";
;; if ($retarr[$index]['is_active']==1 && $retarr[$index]['connection_current']=="leech") $retarr[$index]['status_string']="Leeching";
;; if ($retarr[$index]['is_active']==1 && $retarr[$index]['complete']==1) $retarr[$index]['status_string']="Seeding";
;; if ($retarr[$index]['hashing']>0) {
;;    $retarr[$index]['status_string']="Hashing";
;;    $retarr[$index]['percent_complete']=@round(($retarr[$index]['chunks_hashed'])/($retarr[$index]['size_chunks'])*100);
;; }


(provide 'ertorrent)

;;; ertorrent.el ends here

