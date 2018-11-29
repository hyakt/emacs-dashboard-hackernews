;;; dashboard-hackernews.el --- Hacker News Client for dashboard  -*- lexical-binding: t -*-

;;; Commentary:
;; Check Hacker News on Emacs.

;;; Code:
(require 'json)
(require 'request)

(add-to-list 'dashboard-item-generators  '(hackernews . dashboard-insert-hackernews))
(add-to-list 'dashboard-items '(hackernews) t)


(defconst dashboard-hackernews-number-of-items 10
  "Number of show items of hackernews.")

(defconst dashboard-hackernews-api-version "v0"
  "Currently supported version of the Hacker News API.")

(defconst dashboard-hackernews-api-top-format
  (format "https://hacker-news.firebaseio.com/%s/topstories.json"
          dashboard-hackernews-api-version)
  "Format of targeted Hacker News API URLs.")

(defconst dashboard-hackernews-site-item-format "https://news.ycombinator.com/item?id=%s"
  "Format of Hacker News website item URLs.")

(defconst dashboard-hackernews-article "v0"
  "Currently supported version of the Hacker News API.")

(defun dashboard-hackernews--get-ids ()
  (request
   dashboard-hackernews-api-top-format
   :type "GET"
   :params '()
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (dashboard-hackernews--get-site-item (append data nil))))))

(defun dashboard-hackernews--get-site-item (ids)
  (dotimes (i dashboard-hacker)
    (print (pop ids))))

(dashboard-hackernews--get-ids)

(defun hackernews--retrieve-items ()
  "Retrieve items associated with current buffer."
  (let* ((items  (hackernews--get :items))
         (reg    (hackernews--get :register))
         (nitem  (length items))
         (offset (car reg))
         (ids    (cdr reg)))
    (dotimes-with-progress-reporter (i nitem)
        (format "Retrieving %d %s..."
                nitem (hackernews--feed-name (hackernews--get :feed)))
      (aset items i (hackernews--read-contents
                     (hackernews--item-url (aref ids (+ offset i))))))))

(defun dashboard-insert-reddit-list (title list)
  "Render REDDIT-LIST title and items of LIST."
  (when (car list)
    (insert title )
    (mapc (lambda (el)
            (setq url (nth 1 (split-string el "__")) )
            (setq title (nth 0 (split-string el "__")) )
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
                                      (browse-url ,url))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"    
                           title
                           ))
          list)))

(defun dashboard-insert-hackernews (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (if (> list-size 0 )
      (progn
        (setq file-path "/tmp/dashboard_reddit.json")
        (condition-case nil
            (delete-file file-path)
          (error nil))
        (url-copy-file "https://news.ycombinator.com/"  file-path)
        (setq reddit-list (mapcar (lambda (entry)
                                    (format "^%s\t  %s__%s" (let-alist entry .data.score ) (let-alist entry .data.title ) (let-alist entry .data.url )))
                                        ;(concat (let-alist entry .data.title ) (concat " - " (let-alist entry .data.url ))))
                                  (let-alist (json-read-file  file-path) .data.children)))


        (when (dashboard-insert-reddit-list
               "Recent Posts to /r/emacs:"
               (dashboard-subseq reddit-list 0 list-size))) 
                                        ;(dashboard-insert--shortcut "p" "Recent Posts:")
        )
    ))

