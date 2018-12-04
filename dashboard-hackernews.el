;;; dashboard-hackernews.el --- Hacker News Client for dashboard; -*- lexical-binding: t -*-;

;;; Commentary:
;; Check Hacker News on Emacs.

;;; Code:

(require 'request)
(require 'dashboard)
(require 'dashboard-widgets)

(add-to-list 'dashboard-item-generators  '(hackernews . dashboard-hackernews-insert))
(add-to-list 'dashboard-items '(hackernews) t)

(defvar dashboard-hackernews-items ())

(defconst dashboard-hackernews-api-version "v0"
  "Currently supported version of the Hacker News API.")

(defconst dashboard-hackernews-api-top-format
  (format "https://hacker-news.firebaseio.com/%s/topstories.json"
          dashboard-hackernews-api-version)
  "Format of targeted Hacker News API URLs.")

(defconst dashboard-hackernews-site-item-format
  (format "https://hacker-news.firebaseio.com/%s/item"
          dashboard-hackernews-api-version)
  "Format of Hacker News website item URLs.")

(defun dashboard-hackernews-get-ids (callback)
  "Get hackernews ids, and execute CALLBACK function."
  (request
   dashboard-hackernews-api-top-format
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall callback data)))))

(defun dashboard-hackernews-get-item (callback id)
  "Get hackernews article from ID, and execute CALLBACK function."
  (request
   (format "%s/%s.json" dashboard-hackernews-site-item-format id)
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall callback data)))))

(defun dashboard-hackernews-insert-list (list-display-name list)
  "Render LIST-DISPLAY-NAME and items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
                                      (browse-url ,(cdr (assoc 'url el))))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (format "[%3d] %s" (cdr (assoc 'score el)) (cdr (assoc 'title el)))))
          list )))

(defun dashboard-hackernews-insert (list-size)
  "Add the list of LIST-SIZE items from hackernews."
  (dashboard-hackernews-get-ids
   (lambda (ids)
     (dotimes (i list-size)
       (dashboard-hackernews-get-item
        (lambda (item) (push item dashboard-hackernews-items)) (elt ids i)))))
  (when (dashboard-hackernews-insert-list "Hackernews:"
                                          (dashboard-subseq dashboard-hackernews-items 0 list-size))))

;;; dashboard-hackernews.el ends here
