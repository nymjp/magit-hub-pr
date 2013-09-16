;;; magit-hub-pr.el --- GitHub Pull Request
;; Copyright (C) 2013 YAMAMOTO, N.
;; Version: 0.0.1
;; Keywords: tools
;;
;; magit-hub-pr is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; magit-hub-pr is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with magit-hub-pr.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You can send GutHub pull request from magit

;;; Code:

(require 'magit)
(require 'url)

(defgroup magit-hub-pr nil
  "GitHub Pull Request"
  :prefix "magit-hub-pr-"
  :group 'tools)

(defcustom magit-hub-pr-executable "hub"
  "The name of the hub executable."
  :group 'magit-hub-pr
  :type 'string)

(defvar magit-hub-pr-prefix-key (kbd "H"))

(magit-key-mode-add-group 'hub-pr)
(let ((group (magit-key-mode-options-for-group 'hub-pr)))
  (setcdr group (list '(switches) '(arguments)))
  (setcdr
   (assoc 'actions group)
   '(("p" "Pull Request" magit-hub-pr-log-edit)
     ("i" "Pull Request on Issue" magit-hub-pr-request-issue))
   )
  (setcdr
   (assoc 'switches group)
   '(
     ("-n" "noop" "--noop")
     ))
  )

;; (magit-key-mode-insert-action
;;  'hub-pr "i" "attach Issue" 'magit-hub-pr-request-issue)
;; (magit-key-mode-insert-switch
;;  'hub-pr "-n" "noop" "--noop")
(magit-key-mode-generate 'hub-pr)

(defun magit-hub-pr-run-hub-async-with-input (input cmd opts &rest args)
  (let* ((preopts  (and (member "--noop" opts) (list "--noop")))
         (postopts (delete "--noop" opts))
         (hubargs (append preopts (list cmd) postopts args))
         (magit-process-connection-type nil)) ;; no-pager が効かないようなので。
         (message "Running %s %s" magit-hub-pr-executable (mapconcat 'identity hubargs " "))
    (magit-run* (append (list magit-hub-pr-executable) magit-git-standard-options hubargs)
                nil nil nil t input)))

(defun magit-hub-pr-run-hub-async (cmd opts &rest args)
    (apply 'magit-hub-pr-run-hub-async-with-input nil cmd opts args))

(defun magit-hub-pr-host()
  (or (getenv "GITHUB_HOST")
      (magit-get "hub" "host")
      "github.com"))

(defun magit-hub-pr-get-remote ()
  (let ((ghhost (magit-hub-pr-host))
        (remotes (magit-git-lines "remote")))
    (loop for remote in remotes
          for url  = (magit-get "remote" remote "url")
          for host = (caddr (magit-hub-pr-parse-url url))
          if (string= host ghhost) collect remote)))

(defun magit-hub-pr-read-remote ()
  (let* ((remotes (magit-hub-pr-get-remote))
         (def (or (car (member "upstream" remotes))
                  (car (member "origin" remotes))
                  (magit-guess-remote)))
         (reply (magit-completing-read
                 "Remote: " remotes
                 nil
                 nil nil nil def)))
    (if (string= reply "")
        (error "no github remote found.")
      reply)))

(defun magit-hub-pr-parse-url (urlstr)
  (let ((url (url-generic-parse-url urlstr)))
    (unless (url-host url)
      (setq url
            (url-generic-parse-url
             (concat "ssh://"
                     (replace-regexp-in-string ":" "/" urlstr)))))
    (let ((host (url-host url)) (path (url-filename url))
          (user) (repos))
      (when (string-match "\\([^/]+\\)/\\([^/]+\\)[.]git$" path)
        (setq user (match-string 1 path)
              repos (match-string 2 path))
        (list user repos host)))))

(magit-define-command hub-pr-request-issue ()
  (interactive)
  (let* ((remote (magit-hub-pr-read-remote))
         (branch (magit-read-remote-branch remote nil "master"))
         (issue (read-from-minibuffer "Issue number: "))
         (base (magit-hub-pr-parse-url
                (magit-get "remote" remote "url"))))
    (or (string-match "^[[:blank:]]*[[:digit:]]+[[:blank:]]*$" issue)
        (error "Invalid issue number."))
    (magit-hub-pr-run-hub-async
     "pull-request"
     magit-custom-options
     "-b" (concat (car base) "/" (cadr base) ":" branch)
     "-i" issue
     )))

;;; hub-pr Log edit mode
(defvar magit-hub-pr-log-edit-buffer-name "*magit-hub-pr-edit-log*"
  "Buffer name for composing commit messages.")

(defvar magit-hub-pr-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magit-hub-pr-log-edit-commit)
    (define-key map (kbd "C-x #") 'magit-hub-pr-log-edit-commit)
    (define-key map (kbd "M-p") 'log-edit-previous-comment)
    (define-key map (kbd "M-n") 'log-edit-next-comment)
    (define-key map (kbd "C-c C-k") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-c C-]") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-x C-s") (lambda ()
                                      (interactive)
                                      (message "Not saved. Use C-c C-c to finalize this commit message.")))
    map))

(define-derived-mode
  magit-hub-pr-log-edit-mode text-mode "Magit Hub PR Log Edit")

(magit-define-command hub-pr-log-edit ()
  (interactive)
  (let* ((remote (magit-hub-pr-read-remote))
         (branch (magit-read-remote-branch remote nil "master"))
         (base (magit-hub-pr-parse-url
                 (magit-get "remote" remote "url")))
         (magit-buf (current-buffer))
         (buf (get-buffer-create magit-log-edit-buffer-name)))
    (magit-log-edit-set-field
     'base
     (concat (car base) "/" (cadr base) ":" branch))
    (and (member "--noop" magit-custom-options)
         (magit-log-edit-set-field 'noop "yes"))
    (setq magit-pre-log-edit-window-configuration
          (current-window-configuration))
    (pop-to-buffer buf)
    (magit-hub-pr-log-edit-mode)
    (make-local-variable 'magit-buffer-internal)
    (setq magit-buffer-internal magit-buf)
    (message "Type C-c C-c to pull request (C-c C-k to cancel).")))

(defun magit-hub-pr-log-edit-commit ()
  (interactive)
  (let* ((fields (magit-log-edit-get-fields))
         (base (cdr (assq 'base fields)))
         (noop (equal (cdr (assq 'noop fields)) "yes"))
         (msgfile (concat (magit-git-dir) "PULL_REQUEST_MSG")))
    (magit-log-edit-set-fields nil)
    (magit-log-edit-cleanup)
    (magit-hub-pr-run-hub-async-with-input
     (current-buffer)
     "pull-request"
     (and noop (list "--noop"))
     "-b" base
     "-F" "-"
     )
    ;; cleanup
    (erase-buffer)
    (let ((magit-buf magit-buffer-internal))
      (bury-buffer)
      (set-buffer magit-buf))
    (when magit-pre-log-edit-window-configuration
      (set-window-configuration magit-pre-log-edit-window-configuration)
      (setq magit-pre-log-edit-window-configuration nil))))

(when magit-hub-pr-prefix-key
  (define-key magit-mode-map magit-hub-pr-prefix-key 'magit-key-mode-popup-hub-pr))

(provide 'magit-hub-pr)

;;; TODO:
;; * more options
;; * change access library from "hub" to "gh.el"
;; * support comment create

;;; magit-hub-pr.el ends here
