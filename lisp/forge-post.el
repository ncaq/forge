;;; forge-post.el --- Post support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'markdown-mode)

(require 'forge)

;;; Options

(defcustom forge-post-mode-hook
  '(visual-line-mode
    turn-on-flyspell)
  "Hook run after entering Forge-Post mode."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type 'hook
  :options '(visual-line-mode
             turn-on-flyspell))

(defcustom forge-buffer-draft-p nil
  "Whether new pull-requests start out as drafts by default.

The buffer-local value is use to keep track of the draft status
of the current pull-request."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'boolean)

;;; Class

(defclass forge-post (forge-object) () :abstract t)

;;; Query

(cl-defmethod forge-get-parent ((post forge-post))
  (forge-get-topic post))

(cl-defmethod forge-get-repository ((post forge-post))
  (forge-get-repository (forge-get-topic post)))

;;; Utilities

(cl-defmethod forge-get-url ((post forge-post))
  (forge--format post (let ((topic (forge-get-parent post)))
                        (cond ((forge--childp topic 'forge-issue)
                               'issue-post-url-format)
                              ((forge--childp topic 'forge-pullreq)
                               'pullreq-post-url-format)))))

(cl-defmethod forge-browse :after ((post forge-post))
  (oset (forge-get-topic post) unread-p nil))

;;; Sections

(defun forge-post-at-point ()
  (magit-section-value-if '(issue pullreq post)))

(defun forge-comment-at-point ()
  (and (magit-section-value-if '(post))
       (let ((post (oref (magit-current-section) value)))
         (and (or (forge-pullreq-post-p post)
                  (forge-issue-post-p post))
              post))))

(defun forge-topic-at-point ()
  (or (magit-section-value-if '(issue pullreq))
      (and-let* ((branch (magit-branch-at-point))
                 (n (magit-get "branch" branch "pullRequest")))
        (forge-get-pullreq (string-to-number n)))
      (and-let* ((rev (magit-commit-at-point)))
        (forge--pullreq-from-rev rev))
      (forge--issue-by-forge-short-link-at-point)
      (forge--pullreq-by-forge-short-link-at-point)))

(defun forge-current-topic ()
  (or (forge-topic-at-point)
      (and (derived-mode-p 'forge-topic-mode)
           forge-buffer-topic)
      (and (derived-mode-p 'forge-topic-list-mode)
           (forge-get-topic (tabulated-list-get-id)))))

(defun forge--pullreq-from-rev (rev)
  (and-let* ((repo    (forge-get-repository nil))
             (refspec (oref repo pullreq-refspec))
             (name    (magit-rev-name rev (cadr (split-string refspec ":")))))
    (save-match-data
      (and (string-match "\\([0-9]*\\)\\([~^][0-9]*\\)?\\'" name)
           (forge-get-pullreq (string-to-number (match-string 0 name)))))))

;;; Utilities

(cl-defmethod forge--format ((post forge-post) slot &optional spec)
  (forge--format (forge-get-topic post) slot
                 `(,@spec (?I . ,(oref post number)))))

;;; Mode

(defvar-keymap forge-post-mode-map
  "C-c C-e"                                #'forge-post-dispatch
  "C-c C-c"                                #'forge-post-submit
  "<remap> <evil-save-and-close>"          #'forge-post-submit
  "<remap> <evil-save-modified-and-close>" #'forge-post-submit
  "C-c C-k"                                #'forge-post-cancel
  "<remap> <kill-buffer>"                  #'forge-post-cancel
  "<remap> <ido-kill-buffer>"              #'forge-post-cancel
  "<remap> <iswitchb-kill-buffer>"         #'forge-post-cancel
  "<remap> <evil-quit>"                    #'forge-post-cancel)

(transient-define-prefix forge-post-dispatch ()
  "Dispatch a post creation command."
  ["Settings"
   :if (lambda () (string-prefix-p "new-" (file-name-nondirectory buffer-file-name)))
   ("d" "Set draft"     forge-topic-set-draft)
   ("a" "Set assignees" forge-topic-set-assignees)
   ("l" "Set labels"    forge-topic-set-labels)
   ("m" "Set milestone" forge-topic-set-milestone)
   ("r" "Set reviewers" forge-topic-set-reviewers)]
  ["Act"
   ("C-c" "Submit" forge-post-submit)
   ("C-k" "Cancel" forge-post-cancel)])

(define-derived-mode forge-post-mode gfm-mode "Forge-Post" "")

(defvar-local forge--buffer-base-branch nil)
(defvar-local forge--buffer-head-branch nil)
(defvar-local forge--buffer-post-object nil)
(defvar-local forge--buffer-issue nil)
(defvar-local forge--submit-post-function nil)
(defvar-local forge--cancel-post-function nil)
(defvar-local forge--pre-post-buffer nil)

(defun forge--prepare-post-buffer (filename &optional header source target)
  (let ((file (convert-standard-filename
               (expand-file-name (concat "magit/posts/" filename)
                                 (magit-gitdir)))))
    (make-directory (file-name-directory file) t)
    (let ((prevbuf (current-buffer))
          (resume (and (file-exists-p file)
                       (> (file-attribute-size (file-attributes file)) 0)))
          (buf (find-file-noselect file)))
      (with-current-buffer buf
        (forge-post-mode)
        (when header
          (magit-set-header-line-format header))
        (setq forge--pre-post-buffer prevbuf)
        (when resume
          (forge--display-post-buffer buf)
          (when (magit-read-char-case "A draft already exists.  " nil
                  (?r "[r]esume editing existing draft")
                  (?d "[d]iscard draft and start over" t))
            (erase-buffer)
            (setq resume nil)))
        (when (and (not resume) (string-prefix-p "new" filename))
          (let-alist (forge--topic-template
                      (forge-get-repository t)
                      (if source 'forge-pullreq 'forge-issue))
            (cond
             (.url
              (browse-url .url)
              (forge-post-cancel)
              (setq buf nil)
              (message "Using browser to visit %s instead of opening an issue"
                       .url))
             (t
              (cond
               ((and source
                     (length= (car (magit-rev-diff-count source target)) 1))
                (magit-rev-insert-format "%B" source)
                (when .text
                  (insert "\n------\n\n" .text)))
               (.text (insert .text)))
              (when .draft     (setq forge-buffer-draft-p    .draft))
              (when .labels    (setq forge--buffer-labels    .labels))
              (when .assignees (setq forge--buffer-assignees .assigness))
              )))))
      buf)))

(defun forge--display-post-buffer (buf)
  (magit-display-buffer buf #'display-buffer))

(defun forge-post-cancel ()
  "Cancel the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--cancel-post-function))
      (funcall fn forge--buffer-post-object)
    (magit-mode-bury-buffer 'kill)))

(defun forge-post-submit ()
  "Submit the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--submit-post-function))
      (funcall fn
               (forge-get-repository forge--buffer-post-object)
               forge--buffer-post-object)
    (error "forge--submit-post-function is nil")))

(defun forge--post-submit-callback ()
  (let* ((file    buffer-file-name)
         (editbuf (current-buffer))
         (prevbuf forge--pre-post-buffer)
         (topic   (ignore-errors (forge-get-topic forge--buffer-post-object)))
         (repo    (forge-get-repository topic)))
    (lambda (value headers status req)
      (run-hook-with-args 'forge-post-submit-callback-hook
                          value headers status req)
      (delete-file file t)
      (let ((dir (file-name-directory file)))
        (unless (cddr (directory-files dir nil nil t))
          (delete-directory dir nil t)))
      (when (buffer-live-p editbuf)
        (with-current-buffer editbuf
          (magit-mode-bury-buffer 'kill)))
      (with-current-buffer
          (if (buffer-live-p prevbuf) prevbuf (current-buffer))
        (if (and topic
                 (forge--childp repo 'forge-github-repository)
                 (or (and (fboundp 'forge-pullreq-p)
                          (forge-pullreq-p topic))
                     (oref repo selective-p)))
            (forge--pull-topic repo topic)
          (forge-pull))))))

(defun forge--post-submit-errorback ()
  (lambda (error &rest _)
    (error "Failed to submit post: %S" error)))

;;; Notes

(defclass forge-note (forge-post) ())

(defvar-keymap forge-note-section-map
  "<remap> <magit-edit-thing>" #'forge-edit-topic-note)

(defun forge--save-note (_repo topic)
  (let ((value (string-trim (buffer-substring-no-properties
                             (point-min)
                             (point-max)))))
    (oset topic note (if (equal value "") nil value)))
  (delete-file buffer-file-name t)
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (cddr (directory-files dir nil nil t))
      (delete-directory dir)))
  (let ((prevbuf forge--pre-post-buffer))
    (magit-mode-bury-buffer 'kill)
    (when (buffer-live-p prevbuf)
      (magit-refresh))))

;;; _
(provide 'forge-post)
;;; forge-post.el ends here
