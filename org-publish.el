#!/usr/bin/emacs --script
;; Lots of code were taken from:
;; - https://gitlab.com/to1ne/blog/-/blob/master/elisp/publish.el
;; - https://github.com/dmacvicar/site.org/blob/master/publish.el

;; I don't want this script, especially package, to mess my Emacs directory.
(setq user-emacs-directory (expand-file-name ".cache/" default-directory))
(load-file (expand-file-name "elisp/no-littering.el" default-directory))

(setq package-user-dir (with-no-warnings (no-littering-expand-var-file-name "package/")))
(require 'package)
(package-initialize)

(unless package-archive-contents
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))

(dolist (pkg '(org-plus-contrib htmlize))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'cl-lib)
(require 'sh-script)
(require 'ox)
(require 'ox-publish)
(require 'ox-rss)

(defvar carbs--src-directory (expand-file-name "src/" default-directory)
  "Directory for most of the static webpage content")

(defvar carbs--publish-directory (expand-file-name "docs/" default-directory)
  "Root directory of the published website")

(defvar carbs--news-directory (expand-file-name "news/" default-directory)
  "Directory for news posts.")

(defvar carbs--blog-directory (expand-file-name "blog/" default-directory)
  "Directory for blog posts.")

(defvar carbs--template-directory (expand-file-name "templates/" default-directory)
  "Directory for all types of templates")

(defun carbs--insert-template (file)
  "Insert FILE from `carbs--template-directory'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file carbs--template-directory))
    (buffer-string)))

(defun carbs/org-html-publish-to-html (plist filename pub-dir)
  "Wrapper function that adds publishing date as a subtitle.

PLIST contains the properties, FILENAME the source file and
  PUB-DIR the output directory."
  (let ((project (cons 'rw plist)))
      (plist-put plist :subtitle
                 (carbs/format-date-subtitle filename project))
    (org-html-publish-to-html plist filename pub-dir)))

(defun carbs/org-html-publish-blog-index (plist filename pub-dir)
  "Wrapper function to publish only the index file to html.
PLIST contains the properties, FILENAME the source file and PUB-DIR the output
directory."
  (if (equal "index.org" (file-name-nondirectory filename))
      (org-html-publish-to-html plist filename pub-dir)))

(defun carbs/format-rss-feed-entry (entry style project)
  "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
                (title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* %s\n" title))
             (org-set-property "RSS_PERMALINK" link)
             (org-set-property "PUBDATE" date)
             (insert-file-contents file)
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun carbs/format-rss-feed (title list)
  "Generate RSS feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n"
          "#+DESCRIPTION: a simple Linux distribution\n\n"
          (org-list-to-subtree list 1 '(:icount "" :istart ""))))

(defun carbs/org-rss-publish-to-rss (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed."
      (org-rss-publish-to-rss plist filename pub-dir))

(defun carbs/format-date-subtitle (file project)
  "Format the date found in FILE of PROJECT."
  (format-time-string "posted on %Y-%m-%d" (org-publish-find-date file project)))

(defun carbs--pre/postamble-format (type)
  "Return the content for the pre/postamble of TYPE."
  `(("en" ,(carbs--insert-template (format "%s.html" type)))))

(defun carbs/org-publish-sitemap (title list)
  "Generate sitemap as a string, having TITLE.
LIST is an internal representation for the files to include, as
returned by `org-list-to-lisp'."
  (let ((filtered-list (cl-remove-if (lambda (x)
                                       (and (sequencep x) (null (car x))))
                                     list)))
    (concat (carbs--insert-template "blog.org")
            (org-list-to-org filtered-list) "\n")))

(defun carbs/org-publish-sitemap-entry (entry style project)
  "Format for sitemap ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (unless (equal entry "404.org")
    (format "[[file:%s][%s]] /%s/"
            entry
            (org-publish-find-title entry project)
            (carbs/format-date-subtitle entry project))))

(defun carbs/org-publish-news (title list)
  "Generate sitemap as a string, having TITLE.
LIST is an internal representation for the files to include, as
returned by `org-list-to-lisp'."
  (let ((filtered-list (cl-remove-if (lambda (x)
                                       (and (sequencep x) (null (car x))))
                                     list)))
    (concat "#+TITLE: News Index\n\n"
            (org-list-to-subtree filtered-list 1 '(:istart "" :icount "")) "\n")))

(defun carbs/org-publish-news-latest (title list)
  "Generate sitemap as a string, having TITLE.
LIST is an internal representation for the files to include, as
returned by `org-list-to-lisp'."
  (let* ((filtered-list (cl-remove-if (lambda (x)
                                       (and (sequencep x) (null (car x))))
                                     list))
        (latest-posts (seq-subseq filtered-list 0 (min (length filtered-list) 6))))
    (concat
    (org-list-to-subtree latest-posts 1 '(:istart "" :icount "")) "\n")))

(defun carbs/org-publish-news-entry (entry style project)
  "Format for sitemap ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
                (date (format-time-string "%b %d, %Y" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" link date))
             (insert-file-contents file)
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(setq user-full-name "Cem Keylan"
      user-mail-address "root@carbslinux.org"
      org-publish-timestamp-directory (with-no-warnings (no-littering-expand-var-file-name "timestamps/"))
      org-html-doctype "html5"
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\">"
      org-html-head-include-scripts nil
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-head-include-default-style nil
      org-html-html5-fancy t
      org-html-htmlize-output-type 'css
      org-export-with-toc nil
      org-export-with-section-numbers nil

      org-publish-project-alist
      (list
       (list "news"
             :author ""
             :base-directory carbs--news-directory
             :html-preamble t
             :html-postamble t
             :html-preamble-format (carbs--pre/postamble-format 'preamble)
             :html-postamble-format (carbs--pre/postamble-format 'postamble)
             :publishing-directory (expand-file-name "news/" carbs--publish-directory)
             :publishing-function '(org-html-publish-to-html org-ascii-publish-to-ascii)
             :exclude (regexp-opt '("index.org" "news.org" "latest-news.org"))
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-style 'list
             :sitemap-title "Carbs Linux news"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'carbs/org-publish-news
             :sitemap-format-entry 'carbs/org-publish-news-entry)
       (list "news-rss-generate"
             :base-directory carbs--news-directory
             :publishing-directory carbs--publish-directory
             :publishing-function 'ignore
             :exclude (regexp-opt '("index.org" "news.org" "latest-news.org"))
             :html-link-home "https://carbslinux.org/news"
             :auto-sitemap t
             :sitemap-filename "news.org"
             :sitemap-style 'list
             :sitemap-title "Carbs Linux news"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'carbs/format-rss-feed
             :sitemap-format-entry 'carbs/format-rss-feed-entry)
       (list "latest-news"
             :base-directory carbs--news-directory
             :publishing-directory carbs--publish-directory
             :publishing-function 'ignore
             :exclude (regexp-opt '("index.org" "news.org" "latest-news.org"))
             :auto-sitemap t
             :sitemap-filename "latest-news.org"
             :sitemap-style 'list
             :sitemap-title "Carbs Linux news"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'carbs/org-publish-news-latest
             :sitemap-format-entry 'carbs/org-publish-news-entry)
       (list "news-rss-publish"
             :base-directory carbs--news-directory
             :exclude "."
             :include '("news.org")
             :rss-extension "xml"
             :recursive t
             :html-link-home "https://carbslinux.org/news"
             :publishing-directory carbs--publish-directory
             :publishing-function 'carbs/org-rss-publish-to-rss)
       (list "home"
             :base-directory carbs--src-directory
             :html-preamble t
             :html-postamble t
             :html-preamble-format (carbs--pre/postamble-format 'preamble)
             :html-postamble-format (carbs--pre/postamble-format 'postamble)
             :publishing-directory carbs--publish-directory
             :publishing-function 'org-html-publish-to-html)
       (list "blog-index-publish"
             :base-directory carbs--blog-directory
             :publishing-directory (expand-file-name "blog/" carbs--publish-directory)
             :publishing-function 'carbs/org-html-publish-blog-index
             :html-preamble t
             :html-postamble t
             :html-preamble-format (carbs--pre/postamble-format 'preamble)
             :html-postamble-format (carbs--pre/postamble-format 'postamble)
             :html-link-org-files-as-html t
             :exclude (regexp-opt '("rss.org" "index.org"))
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-style 'list
             :sitemap-title "Carbs Linux blog"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'carbs/org-publish-sitemap
             :sitemap-format-entry 'carbs/org-publish-sitemap-entry)
       (list "blog"
             :base-directory carbs--blog-directory
             :publishing-directory (expand-file-name "blog/" carbs--publish-directory)
             :publishing-function 'carbs/org-html-publish-to-html
             :html-preamble t
             :html-postamble t
             :html-preamble-format (carbs--pre/postamble-format 'preamble)
             :html-postamble-format (carbs--pre/postamble-format 'postamble)
             :exclude (regexp-opt '("rss.org" "index.org"))
             :html-link-org-files-as-html t)
       (list "blog-generate-rss-sitemap"
             :base-extension "org"
             :base-directory carbs--blog-directory
             :html-link-home "https://carbslinux.org/blog"
             :exclude (regexp-opt '("rss.org" "index.org"))
             :publishing-function 'ignore
             :publishing-directory (expand-file-name "blog/" carbs--publish-directory)
             :rss-extension "xml"
             :auto-sitemap t
             :html-link-org-files-as-html t
             :sitemap-filename "rss.org"
             :sitemap-title "Carbs Linux blog"
             :sitemap-style 'list
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'carbs/format-rss-feed
             :sitemap-format-entry 'carbs/format-rss-feed-entry)
       (list "blog-publish-rss-sitemap"
             :base-directory carbs--blog-directory
             :rss-extension "xml"
             :recursive t
             :exclude "."
             :include '("rss.org")
             :publishing-directory carbs--publish-directory
             :publishing-function 'carbs/org-rss-publish-to-rss
             :html-link-home "https://carbslinux.org/blog"
             :html-link-use-abs-url t)
       (list "plaintext"
             :author ""
             :base-directory carbs--src-directory
             :publishing-directory carbs--publish-directory
             :publishing-function 'org-ascii-publish-to-ascii
             :base-extension "org"
             :exclude (regexp-opt '("rss.org"))
             :recursive t)))

(org-publish-all)
