;; website2org.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL:
;; Version: 0.1.1
;; Package-Requires: ((emacs "26"))
;; Keywords: html websites orgmode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; website2org.el allows to turn any website into a minimal orgmode
;; buffer or .org file.

;; 0.1.1
;; - Added <ol> and removed <input> and <time>; many small changes

(defvar website2org-wget-cmd "wget -q ")
(defvar website2org-cache-filename "~/website2org-cache.html")

;; Turn website2org-additional-meta nil if not applicable. This is for
;; use in orgrr (https://github.com/rtrppl/orgrr).
(defvar website2org-additional-meta "#+roam_tags: website orgrr-project") 

(defvar website2org-directory org-directory) ;; directories must end with / 
(defvar website2org-filename-time-format "%Y%m%d%H%M%S")


(defun website2org ()
  "Use the URL at point or an entered URL and initiate 
website2org-url-to-org. Creates an org-file in website2org-directory."
  (interactive)
  (let ((url (or 
              (thing-at-point-url-at-point)
              (org-element-property :raw-link (org-element-context))
              (read-string "Please enter a URL: "))))
    (website2org-url-to-org url)))

(defun website2org-temp ()
  "Use the URL at point or an entered URL and initiate 
website2org-url-to-org. Results will be presented in a buffer."
  (interactive)
  (let ((url (or 
              (thing-at-point-url-at-point)
              (org-element-property :raw-link (org-element-context))
              (read-string "Please enter a URL: "))))
    (website2org-to-buffer url)))

(defun website2org-url-to-org (url)
 "Creates an Orgmode document from a URL."
  (website2org-create-local-cache-file url)
  (let* ((content (website2org-load-file website2org-cache-filename))
	 (title (website2org-process-html content "title"))
	 (org-content (website2org-process-html content "content"))
	 (time (format-time-string website2org-filename-time-format))
         (filename)
	 (final))
    (setq filename (concat website2org-directory time "-" (replace-regexp-in-string "[\"':;\s\\\/]" "_" title)))
    (website2org-delete-local-cache-file)
    (find-file (concat filename ".org"))
    (insert (concat "#+title: " time "-" (replace-regexp-in-string "[\(\)]" "-" title) "\n"))
    (when website2org-additional-meta
      (insert (concat website2org-additional-meta "\n")))
    (insert (concat "#+roam_key: " url "\n\n"))
    (insert org-content)
    (goto-char (point-min))))

(defun website2org-to-buffer (URL)
  "Returns the text from a html-file."
  (interactive)
  (website2org-create-local-cache-file URL)
  (let* ((content (website2org-load-file website2org-cache-filename))
	 (title (website2org-process-html content "title"))
	 (org-content (website2org-process-html content "content"))
	 (final))
    (website2org-delete-local-cache-file)
    (setq final (concat "#+roam_key: " URL "\n\n" org-content))
    (setq final (concat "#+title: " title "\n" final))
    (with-current-buffer (get-buffer-create "website2org")
      (erase-buffer)
      (switch-to-buffer "website2org")
      (insert final)))
  (website2org-prepare-findings-buffer "website2org"))

(defun website2org-prepare-findings-buffer (buffer)
 "Preparing the orgrr findings buffer."
 (with-current-buffer buffer
   (org-mode))
 (let ((window (get-buffer-window buffer)))
   (select-window window)
   (goto-char (point-min))
   (org-next-visible-heading 1)
   (deactivate-mark)))

(defun website2org-create-local-cache-file (URL)
  "Uses wget to download a website into a local cache file."
    (shell-command (concat website2org-wget-cmd "\"" URL "\"" " -O " website2org-cache-filename ) t))

(defun website2org-delete-local-cache-file ()
  "Deletes the website2org local cache file."
  (delete-file website2org-cache-filename))

(defun website2org-load-file (filename)
  "Returns the plain html of a html-file."
  (let ((content))
    (with-temp-buffer
      (insert-file-contents filename)
      (setq content (buffer-substring-no-properties (point-min)(point-max))))
    content))

(defun website2org-process-html (content what)
  "Main function to transform html into minimal org."
  (let* ((processed-content)
	 (return)
	 (case)
	 (error-in-log)
	 (title))
    (setq content (website2org-cleanup-remove-footer content)) 
    (with-temp-buffer 
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "\\(<blockquote\\)\\s-*\\([^\0]+?\\)\\(</blockquote>\\)" nil t)
	(when (match-string 0)
	  (let* ((replacement (replace-regexp-in-string "<p\\s-*[^>]*>" " " (match-string 0)))
		 (replacement (replace-regexp-in-string "</p>" " " replacement)))
	    (replace-match replacement t t))))
      (goto-char (point-min))
      (while (re-search-forward "\\(<p[\s>]\\|<pre[\s>]\\|<blockquote\\|<h1\\|<h2\\|<h3\\|<ul\\|<ol\\|<title\\)\\s-*\\([^\0]+?\\)\\(</p>\\|</pre>\\|</blockquote>\\|</h1>\\|</h2>\\|</h3>\\|</ul>\\|</ol>\\|</title>\\)" nil t)
	(when (match-string 0)
	  (setq case (match-string 0))
	  (when (and (or (string-match-p "<h1" case)
			 (string-match-p "<h2" case)
			 (string-match-p "<h3" case))
		     (not (string-match-p "<img" case)))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (and (string-match-p "<ul[\s>]" case)
		     (not (string-match-p ".png" case))
		     (not (string-match-p ".jpg" case)))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (and (string-match-p "<ol[\s>]" case)
		     (not (string-match-p ".png" case))
		     (not (string-match-p ".jpg" case)))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (string-match-p "<pre[\s>]" case)
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (string-match-p "<blockquote" case)
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (string-match-p "<p[\s>]" case)
	    (setq processed-content (concat processed-content "\n\n" case "\n\n"))))))
    (setq processed-content (website2org-cleanup-remove-header processed-content))
    (setq processed-content (website2org-cleanup-html-tags processed-content))
    (setq title (website2org-return-title content))
    (setq processed-content (website2org-html-to-org processed-content))
    (setq processed-content (website2org-cleanup-org-weird-characters processed-content))
    (setq processed-content (website2org-cleanup-org processed-content))
    (when (string-equal what "title")
      (setq return title))
    (when (string-equal what "content")
      (setq return (string-trim processed-content)))
  return))

(defun website2org-cleanup-remove-footer (content)
  "Removes the footer from a HTML document." 
  (let ((result))
    (with-temp-buffer 
      (insert content)
      (let ((lines (split-string (buffer-string) "\n" t))
	    (footer nil))
	(erase-buffer)
	(dolist (line lines)
      	  (when (and (string-match-p "<[!-]*footer[^>]*>" line)
		     (not (string-match-p "title" line)))
	    (setq footer 1))
	  (when (not footer)
	    (insert line)))
        (setq result (buffer-substring-no-properties (point-min)(point-max)))))))


(defun website2org-return-title (content)
  "Returns the title of a HTML document."
  (let ((title)
	(title-p))
    (with-temp-buffer 
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "\\(<title>\\)\\s-*\\([^\0]+?\\)\\(</title>\\)" nil t)
	(when (and (match-string 0)
		   (not title-p))
	  (setq title (match-string 0))
	  (setq title-p 1))
	(replace-match "" t t)))
    (setq title (replace-regexp-in-string "<title>" "" title))
    (setq title (replace-regexp-in-string "</title>" "" title))
    (setq title (replace-regexp-in-string "[ \n]+" " " title))
    (setq title (website2org-cleanup-org-weird-characters title))
    (setq title (string-trim title))
    title))


(defun website2org-cleanup-html-tags (content)
  "Minimalizing HTML tags."
  (setq content (replace-regexp-in-string "<p\s\\([^>]*\\)>" "<p>" content))
  (setq content (replace-regexp-in-string "<em\s\\([^>]*\\)>" "<em>" content))
  (setq content (replace-regexp-in-string "<i\s\\([^>]*\\)>" "<i>" content))
  (setq content (replace-regexp-in-string "<img\s\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "\\(<figure[\s>]\\)\\s-*\\([^\0]+?\\)\\(</figure>\\)" "" content))
  (setq content (replace-regexp-in-string "<h1\\([^>]*\\)>" "<h1>" content))
  (setq content (replace-regexp-in-string "<h2\\([^>]*\\)>" "<h2>" content))
  (setq content (replace-regexp-in-string "<h3\\([^>]*\\)>" "<h3>" content))
  (setq content (replace-regexp-in-string "<ul\\([^>]*\\)>" "<ul>" content))
  (setq content (replace-regexp-in-string "<li\\([^>]*\\)>" "<li>" content))
  (setq content (replace-regexp-in-string "<pre\\([^>]*\\)>" "<pre>" content))
  (setq content (replace-regexp-in-string "<blockquote\\([^>]*\\)>" "<blockquote>" content))
  (setq content (replace-regexp-in-string "<ol\\([^>]*\\)>" "<ol>" content))
  (setq content (replace-regexp-in-string "<time\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</time\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<input\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</input\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<pre>.*<code>" "<pre>" content))
  (setq content (replace-regexp-in-string "</code>.*</pre>" "</pre>" content))
  (setq content (replace-regexp-in-string "<strong\\([^>]*\\)>" "<strong>" content))
  (setq content (replace-regexp-in-string "<span\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</span\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<div\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</div\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<svg\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</svg\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<button\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</button\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<path\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</path\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "\\(?:\u3000+\\)" "" content)))

(defun website2org-cleanup-remove-header (content)
  "Removes everything before the first H1 headline."
    (with-temp-buffer 
      (insert content)
      (goto-char (point-min))
      (let ((h1-p))
      (while (not (eobp))
	(let ((line (thing-at-point 'line t)))
	  (when (string-match-p "<h1.*?>" line)
	    (setq h1-p t))
	  (when (not h1-p)
	    (delete-region (line-beginning-position) (line-end-position)))
	(forward-line)))
      (setq content (buffer-substring-no-properties (point-min)(point-max))))))	

(defun website2org-html-to-org-via-pandoc (content)
  "Turns the filtered HTML content into clean Orgmode content.
Currently this function is not needed/used."
    (with-temp-buffer 
      (insert content)
      (shell-command-on-region (point-min) (point-max) "pandoc -f html -t org --wrap=preserve" t t)
    (setq content (buffer-substring-no-properties (point-min)(point-max)))))
   
(defun website2org-html-to-org (content)
  "Turns the filtered HTML content into clean Orgmode content."
 (setq content (replace-regexp-in-string "<h1>" "* " content))
 (setq content (replace-regexp-in-string "</h1>" "" content))
 (setq content (replace-regexp-in-string "<h2>" "** " content))
 (setq content (replace-regexp-in-string "</h2>" "" content))
 (setq content (replace-regexp-in-string "<h3>" "*** " content))
 (setq content (replace-regexp-in-string "</h3>" "" content))
 (setq content (replace-regexp-in-string "<p>" "" content))
 (setq content (replace-regexp-in-string "</p>" "" content))
 (setq content (replace-regexp-in-string "<strong>" "**" content))
 (setq content (replace-regexp-in-string "</strong>" "**" content))
 (setq content (replace-regexp-in-string "<b>" "" content))
 (setq content (replace-regexp-in-string "</b>" "" content))
 (setq content (replace-regexp-in-string "<i>" "" content))
 (setq content (replace-regexp-in-string "</i>" "" content))
 (setq content (replace-regexp-in-string "<em>" " /" content))
 (setq content (replace-regexp-in-string "</em>" "/ " content))
 (setq content (replace-regexp-in-string "<ul>" "" content))
 (setq content (replace-regexp-in-string "</ul>" "\n" content))
 (setq content (replace-regexp-in-string "<ol>" "" content))
 (setq content (replace-regexp-in-string "</ol>" "\n" content))
 (setq content (replace-regexp-in-string "</li>" "</li>\n" content))
 (setq content (replace-regexp-in-string "<li>" "- " content))
 (setq content (replace-regexp-in-string "</li>" "" content))
 (setq content (replace-regexp-in-string "<code>" "=" content))
 (setq content (replace-regexp-in-string "</code>" "=" content))
 (setq content (replace-regexp-in-string "<pre>" "#+BEGIN_SRC\n" content))
 (setq content (replace-regexp-in-string "</pre>" "\n#+END_SRC" content))
 (setq content (replace-regexp-in-string "<blockquote>" "#+BEGIN_QUOTE\n" content))
 (setq content (replace-regexp-in-string "</blockquote>" "\n#+END_QUOTE" content))
 (setq content (replace-regexp-in-string "<br>" "\n" content))
 ;; transforming links
 (with-temp-buffer 
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "<a[\s\t].*+href=['\"]\\([^'\"]+\\)['\"][^>]*>\\([^<]+\\)</a>" nil t)
	(let ((url (match-string 1))
	      (text (match-string 2)))
	  (setq url (replace-regexp-in-string "^#" "*" url))
	  (replace-match (format "[[%s][%s]]" url text) t t)))
      (goto-char (point-min))
      (while (re-search-forward "<a[\s\t].*+href=['\"]\\([^'\"]+\\)['\"][^>]*></a>" nil t)
	  (replace-match "" t t))
 ;; this is to remove the traces of <strong> in <a href>     
      (goto-char (point-min))
      (while (re-search-forward "\\[\\*\\*" nil t)
	  (replace-match "\[" t t))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\]" nil t)
	  (replace-match "\]" t t))
      (buffer-substring-no-properties (point-min) (point-max))))

(defun website2org-cleanup-org-weird-characters (content)
  "Cleaning-up weird characters in the Orgmode content."
  (setq content (replace-regexp-in-string "&quot;" "\"" content))
  (setq content (replace-regexp-in-string "&ldquo;" "\"" content))
  (setq content (replace-regexp-in-string "&rdquo;" "\"" content))
  (setq content (replace-regexp-in-string "&rsquo;" "'" content))
  (setq content (replace-regexp-in-string "&nbsp;" " " content))
  (setq content (replace-regexp-in-string "&gt;" ">" content))
  (setq content (replace-regexp-in-string "&#8220;" "\"" content))
  (setq content (replace-regexp-in-string "&#8221;" "\"" content))
  (setq content (replace-regexp-in-string "[\t\r]+" " " content))
  (setq content (replace-regexp-in-string "&bull;" "•" content))
  (setq content (replace-regexp-in-string " " " " content))
  (setq content (replace-regexp-in-string "&#\\(?:8217\\|039\\);" "'" content)))

(defun website2org-cleanup-org (content)
  "Final clean-up of the Orgmode content."
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
;; This was something Pandoc did a lot. Perhaps not necessary anymore.
      (while (re-search-forward "^:PROPERTIES:\n\\([^:]*:.*\n\\):END:\n?" nil t)
	(replace-match "\n"))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line (thing-at-point 'line t)))
	  (when (string-match-p "\\[\\[data:image" line)
	    (delete-region (line-beginning-position) (line-end-position)))
	  (when (string-match-p "\s*<source" line)
	    (delete-region (line-beginning-position) (line-end-position)))
	  (when (string-match-p "\s*<a\s" line)
	    (delete-region (line-beginning-position) (line-end-position)))
	  (when (string-match-p "\s*<title" line)
	    (delete-region (line-beginning-position) (line-end-position)))
	  (when (string-match-p "\s*<tool-tip" line)
	    (delete-region (line-beginning-position) (line-end-position)))
	  (forward-line)))	     
      (setq content (buffer-substring-no-properties (point-min)(point-max))))
;; proper punctuation  
    (setq content (replace-regexp-in-string "/\s\\([,;.:!?)]\\)" "/\\1" content)) 
    (setq content (replace-regexp-in-string "\\([(]\\)\s/" "\\1/" content))
;; no empty lines that just start with \  
    (setq content (replace-regexp-in-string "[\\]*$" "" content)) 
;; no empty lines that just start with * 
    (setq content (replace-regexp-in-string "[\*]* $" "" content))
;; no empty lines that just start with *  
    (setq content (replace-regexp-in-string "* $" "" content))
;; no new line starts with a space
    (setq content (replace-regexp-in-string "^\s*" "" content)) 
;; no more than one space 
    (setq content (replace-regexp-in-string "\s\\{2,\\}" "\s" content))
;; no empty lines that just start with - 
    (setq content (replace-regexp-in-string "^- $\\|^-$" "" content))
;; no more than one empty line
    (setq content (replace-regexp-in-string "\n\\{2,\\}" "\n\n" content)))


(provide 'website2org)
