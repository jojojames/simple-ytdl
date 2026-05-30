;;;; -*- lexical-binding: t; -*-
;;; simple-ytdl.el --- Minimal yt-dlp wrapper for Emacs

;;; Commentary:
;;
;; A tiny convenience layer over the `yt-dlp' command-line tool.
;; Exposes two interactive entry points:
;;
;;   `simple-ytdl-download'            - prompt for a URL and download it.
;;   `simple-ytdl-download-clipboard'  - download the URL on the clipboard.
;;
;; Downloads run asynchronously; progress is appended to the
;; `*simple-ytdl*' buffer.  Files are saved under
;; `simple-ytdl-download-dir' at the best available quality (yt-dlp picks
;; the best video + best audio streams and muxes them into mp4, which
;; requires `ffmpeg' on PATH).
;;
;; Any URL supported by yt-dlp will work (Reddit, YouTube, Twitter,
;; Vimeo, TikTok, ...); see `yt-dlp --list-extractors' for the full set.

;;; Code:

(defgroup simple-ytdl nil
  "Minimal yt-dlp wrapper."
  :group 'external
  :prefix "simple-ytdl-")

(defcustom simple-ytdl-download-dir "~/Downloads"
  "Directory into which downloads are saved.
Created on demand if it does not already exist."
  :type 'directory
  :group 'simple-ytdl)

(defcustom simple-ytdl-program "yt-dlp"
  "Name of, or path to, the yt-dlp executable."
  :type 'string
  :group 'simple-ytdl)

(defcustom simple-ytdl-cookies-from-browser "firefox"
  "Browser to pull cookies from, or nil to disable.
When non-nil, passed to yt-dlp as `--cookies-from-browser', which
is required for sites that gate content behind a login (e.g.
some Reddit posts).  Accepts any value yt-dlp recognises:
`firefox', `chrome', `chromium', `brave', `edge', `safari', etc."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'simple-ytdl)

(defcustom simple-ytdl-extra-args nil
  "Additional command-line arguments passed to yt-dlp.
Appended after `simple-ytdl''s built-in arguments, so they can be
used to override the defaults (e.g. to cap resolution with
`(\"-S\" \"res:1080\")')."
  :type '(repeat string)
  :group 'simple-ytdl)

(defcustom simple-ytdl-watch-interval 1.0
  "How often, in seconds, `simple-ytdl-watch-mode' polls the clipboard."
  :type 'number
  :group 'simple-ytdl)

(defconst simple-ytdl--buffer-name "*simple-ytdl*"
  "Name of the buffer that accumulates yt-dlp output.")

(defun simple-ytdl--clipboard-url ()
  "Return the URL on the system clipboard, or nil if there isn't one.
Whitespace is trimmed; the value is returned only if it begins with
`http://' or `https://'."
  (when-let* ((raw (ignore-errors
                     (gui-get-selection 'CLIPBOARD 'STRING)))
              (url (string-trim raw)))
    (and (string-match-p "\\`https?://" url) url)))

(defun simple-ytdl--read-url ()
  "Prompt for a URL, defaulting to the clipboard when it holds one."
  (let ((default (simple-ytdl--clipboard-url)))
    (read-string (if default
                     (format "URL (default %s): " default)
                   "URL: ")
                 nil nil default)))

(defun simple-ytdl--start (url)
  "Kick off an async yt-dlp download of URL.
Output is appended to `simple-ytdl--buffer-name', which is also
displayed.  Signals a user-error if the yt-dlp executable cannot
be found on `exec-path'."
  (unless (executable-find simple-ytdl-program)
    (user-error "%s not found on exec-path" simple-ytdl-program))
  (let* ((dir (expand-file-name simple-ytdl-download-dir))
         (default-directory (file-name-as-directory dir))
         (buf (get-buffer-create simple-ytdl--buffer-name))
         (args (append (list "--no-playlist"
                             "-f" "bv*+ba/b"
                             "--merge-output-format" "mp4"
                             "-o" "%(title).200B [%(id)s].%(ext)s")
                       (when simple-ytdl-cookies-from-browser
                         (list "--cookies-from-browser"
                               simple-ytdl-cookies-from-browser))
                       simple-ytdl-extra-args
                       (list url))))
    (make-directory dir t)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n$ cd %s\n$ %s %s\n"
                        dir
                        simple-ytdl-program
                        (mapconcat #'shell-quote-argument args " "))))
      (special-mode))
    (let ((proc (apply #'start-process
                       "simple-ytdl" buf simple-ytdl-program args)))
      (set-process-sentinel
       proc
       (lambda (p event)
         (when (memq (process-status p) '(exit signal))
           (message "simple-ytdl: %s %s"
                    (process-name p) (string-trim event))))))
    (display-buffer buf)))

;;;###autoload
(defun simple-ytdl-download (url)
  "Download the video at URL into `simple-ytdl-download-dir'.
Interactively, prompt for the URL, offering the clipboard
contents as the default when they look like an http(s) URL.
Accepts any URL yt-dlp can handle."
  (interactive (list (simple-ytdl--read-url)))
  (when (or (null url) (string-empty-p url))
    (user-error "No URL given"))
  (simple-ytdl--start url))

;;;###autoload
(defun simple-ytdl-download-clipboard ()
  "Download the URL currently on the system clipboard, without prompting.
Signals a user-error if the clipboard does not hold an http(s) URL."
  (interactive)
  (let ((url (simple-ytdl--clipboard-url)))
    (unless url
      (user-error "No http(s) URL on the clipboard"))
    (simple-ytdl--start url)))

(defvar simple-ytdl--watch-timer nil
  "Active polling timer for `simple-ytdl-watch-mode', or nil.")

(defvar simple-ytdl--watch-last nil
  "Last clipboard value seen by `simple-ytdl-watch-mode'.
Used to suppress re-downloading the same URL on every tick, and
seeded at mode-enable time so whatever is already on the clipboard
is not pulled.")

(defun simple-ytdl--watch-tick ()
  "Poll the clipboard and download any newly observed http(s) URL.
Called on the `simple-ytdl-watch-interval' timer.  Compares the
raw clipboard contents against `simple-ytdl--watch-last' so that
identical repeat copies are ignored; the URL itself is filtered
through `simple-ytdl--clipboard-url' so non-URL clipboard changes
are silently skipped."
  (let ((raw (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING))))
    (unless (equal raw simple-ytdl--watch-last)
      (setq simple-ytdl--watch-last raw)
      (when-let* ((url (simple-ytdl--clipboard-url)))
        (message "simple-ytdl: auto-downloading %s" url)
        (simple-ytdl--start url)))))

;;;###autoload
(define-minor-mode simple-ytdl-watch-mode
  "Toggle automatic downloading of URLs copied to the clipboard.

When enabled, the system clipboard is polled every
`simple-ytdl-watch-interval' seconds; any new value that looks
like an http(s) URL is passed to yt-dlp via `simple-ytdl--start'.

The clipboard value at mode-enable time is recorded as the
baseline and is *not* downloaded, so toggling the mode while a
URL is already on the clipboard is safe."
  :global t
  :lighter " ytdl-watch"
  :group 'simple-ytdl
  (when (timerp simple-ytdl--watch-timer)
    (cancel-timer simple-ytdl--watch-timer)
    (setq simple-ytdl--watch-timer nil))
  (when simple-ytdl-watch-mode
    (setq simple-ytdl--watch-last
          (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING))
          simple-ytdl--watch-timer
          (run-with-timer simple-ytdl-watch-interval
                          simple-ytdl-watch-interval
                          #'simple-ytdl--watch-tick))))

(provide 'simple-ytdl)
;;; simple-ytdl.el ends here
