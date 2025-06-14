;;; website2org.el --- Turn any website into a minimal orgmode buffer or .org file -*- lexical-binding: t -*-

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: https://github.com/rtrppl/website2org
;; Version: 0.3.4
;; Package-Requires: ((emacs "26"))
;; Keywords: comm

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

;; website2org.el is a tool to turn a website into a minimal orgmode
;; buffer or .org file.
;;
;; 0.3.4
;; - Improved line-breaks for new <p> blocks
;;
;; 0.3.3
;; - Fix for <wow-image> and <u style...>; fix for [] in links
;;
;; 0.3.2
;; - Fix for <dl> blocks
;;
;; 0.3.1
;; - Removing class=anchor links from headlines; fix for repeating 
;; paragraphs when headlines are dropped in unfinished paragraphs; more fixes
;; for bold and italics
;;
;; 0.3
;; - Fixes for detecting links in documents; `website2org-dired-file-to-org' 
;; transforms marked HTML files in `Dired' into Orgmode documents and places 
;; them into `website2org-directory' 
;;
;; 0.2.13
;; - Added `website2org-visual-fill-column-mode-p' configuration;
;; `website2org-to-buffer' now creates read-only buffer; improvements
;; in display of quotation marks before links 
;;
;; 0.2.12
;; - Fixed Substack double loading content bug (by deleting all scripts
;; before processing the HTML)
;;
;; 0.2.11
;; - Switching from `wget' to `curl' as standard tool; better handling 
;; for Unicode escape sequences
;;
;; 0.2.10 
;; - Improved handling of HTML tags in links
;;
;; 0.2.9
;; - Ensured that there is at least one space between a word and a 
;; link
;;
;; 0.2.8
;; - Added option to press the spacebar to scroll in `website2org-temp' 
;;
;; 0.2.7
;; - Added support for `elfeed-show-mode' + added minor mode for 
;; `website2org-temp' (press "q" to exit)
;;
;;; Code:

(require 'org)
(require 'xml)
(require 'shr)

(defvar website2org-datatransfer-tool-cmd "curl -L ") ;; alternatively use "wget -q "
(defvar website2org-datatransfer-tool-cmd-mod " -o ") ;; alternatively use "wget -q " "-o ") ;; for wget use "-O "
(defvar website2org-cache-filename "~/website2org-cache.html")
(defvar website2org-visual-fill-column-mode-p t)

;; Turn website2org-additional-meta nil if not applicable. This is for
;; use in orgrr (https://github.com/rtrppl/orgrr).
(defvar website2org-additional-meta "#+roam_tags: website") 

(defvar website2org-directory org-directory) ;; directories must end with / 
(defvar website2org-filename-time-format "%Y%m%d%H%M%S")

(defvar website2org-archive nil)
(defvar website2org-archive-url "https://archive.today/") 

(defun website2org ()
  "Use the URL at point or an entered URL and initiate 
website2org-url-to-org. Creates an org-file in website2org-directory."
  (interactive)
  (let ((url (or 
              (thing-at-point-url-at-point)
	      (get-text-property (point) 'shr-url)
	      (when (derived-mode-p 'org-mode)
		(org-element-property :raw-link (org-element-context)))
              (read-string "Please enter a URL: "))))
    (website2org-url-to-org url)))

(defun website2org-temp ()
  "Use the URL at point or an entered URL and initiate 
website2org-url-to-org. Results will be presented in a buffer."
  (interactive)
  (let ((url (or 
              (thing-at-point-url-at-point)
	      (get-text-property (point) 'shr-url)
	      (when (derived-mode-p 'org-mode)
		(org-element-property :raw-link (org-element-context)))
              (read-string "Please enter a URL: "))))
    (website2org-to-buffer url)))

;;;###autoload
(defun website2org-url-to-org (url &optional dummy file noselect title-nodate)
  "Creates an Orgmode document from an URL or a file."
  (let ((final-filename)
	(filename))
    (with-temp-buffer
      (when (not file)
	(website2org-create-local-cache-file url))
      (let* ((content (website2org-load-file (or file website2org-cache-filename)))
	     (url (or url (website2org-return-URL content)))
	     (title (website2org-process-html content "title" url))
	     (org-content (website2org-process-html content "content" url))
	     (time (format-time-string website2org-filename-time-format)))      
	(when (and website2org-archive
		   (not file))
	  (shell-command (concat "open " website2org-archive-url url)))
	(setq filename (replace-regexp-in-string "[\"\|',.:;?\s\\\/]" "_" title))
	(when (> (length filename) 100)
	  (setq filename (substring title 0 100)))
	(setq filename (replace-regexp-in-string "_\\{2,\\}" "_" filename))
	(setq filename (concat website2org-directory time "-" filename))
	(when (not file)
	  (website2org-delete-local-cache-file)
	  (let ((buffer (if noselect
			    (find-file-noselect (concat filename ".org"))
			  (find-file (concat filename ".org")))))
	    (with-current-buffer buffer
	      (if title-nodate
		  (insert (concat "#+title: " (replace-regexp-in-string "[\(\)]" "-" title) "\n"))
		(insert (concat "#+title: " time "-" (replace-regexp-in-string "[\(\)]" "-" title) "\n")))
	      (when website2org-additional-meta
		(insert (concat website2org-additional-meta "\n")))
	      (insert (concat "#+roam_key: " url "\n\n"))
	      (insert org-content)
	      (goto-char (point-min))
	      (when noselect
		(save-buffer (current-buffer))
		(kill-buffer (current-buffer))))
	    (when file
	      (with-current-buffer (find-file-noselect (concat filename ".org"))
		 (if title-nodate
		     (insert (concat "#+title: " (replace-regexp-in-string "[\(\)]" "-" title) "\n"))
		   (insert (concat "#+title: " time "-" (replace-regexp-in-string "[\(\)]" "-" title) "\n")))
		(when website2org-additional-meta
		  (insert (concat website2org-additional-meta "\n")))
		(insert (concat "#+roam_key: " url "\n\n"))
		(insert org-content)
		(save-buffer (current-buffer))
		(kill-buffer (current-buffer))))))))
	(setq final-filename (concat filename ".org"))
	final-filename))

  
 ;;;###autoload
(defun website2org-dired-file-to-org ()
  "Transforms selected files into Orgmode files and places them
into `website2org-directory'."
  (interactive)
  (let* ((marked-files (dired-get-marked-files)))
    (dolist (file marked-files)
      (let* ((extension (file-name-extension file)))
	(when (string= extension "html")
	  (website2org-url-to-org nil nil file)
	  (message "Transforming... %s" file))
	(when (not (string= extension "html"))
	  (message "%s is not a HTML file." file))))
    (message "All HTML files transformed."))
  (revert-buffer)) 

;;;###autoload
(defun website2org-to-buffer (url &optional dummy)
  "Creates an Orgmode buffer from an URL."
  (with-temp-buffer
    (website2org-create-local-cache-file url)
    (let* ((content (website2org-load-file website2org-cache-filename))
	   (title (website2org-process-html content "title" url))
	   (org-content (website2org-process-html content "content" url))
	   (final))
      (website2org-delete-local-cache-file)
    (setq final (concat "#+roam_key: " url "\n\n" org-content))
    (setq final (concat "#+title: " title "\n" final))
    (with-current-buffer (get-buffer-create "website2org")
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (switch-to-buffer "website2org")
      (insert final)
      (when website2org-visual-fill-column-mode-p
	(visual-fill-column-mode))
      (setq-local buffer-read-only t)))
  (website2org-prepare-findings-buffer "website2org")))

(defun website2org-prepare-findings-buffer (buffer)
 "Preparing the orgrr findings buffer."
 (with-current-buffer buffer
   (org-mode))
 (let ((window (get-buffer-window buffer)))
   (select-window window)
   (goto-char (point-min))
   (deactivate-mark)
   (website2org-temp-buffer-mode)))

(define-minor-mode website2org-temp-buffer-mode
  "A minor mode for the `website2org-temp' buffer."
  :lighter " website2org-temp-buffer-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
	    (define-key map (kbd "SPC") 'scroll-up-command)
            map))

(defun website2org-create-local-cache-file (URL)
  "Uses wget to download a website into a local cache file."
  (shell-command (concat website2org-datatransfer-tool-cmd "\"" URL "\"" website2org-datatransfer-tool-cmd-mod  website2org-cache-filename) t))

(defun website2org-load-file (filename)
  "Returns the plain html of a html-file."
  (let ((content))
    (with-temp-buffer
      (insert-file-contents filename)
      (setq content (buffer-string)))
    content))

(defun website2org-delete-local-cache-file ()
  "Deletes the website2org local cache file."
  (delete-file website2org-cache-filename))

(defun website2org-process-html (content what &optional og-url)
  "Main function to transform html into minimal org."
  (let* ((processed-content)
	 (return)
	 (case)
	 (error-in-log)
	 (title))
    (setq content (website2org-cleanup-remove-footer content))
    (setq content (replace-regexp-in-string "[\s =]*<script.*</script>[\s=]*" "" content))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "<img[ \t][^>]*?src=\\(['\"]?\\)\\([^'\" \t>]+\\)\\1[^>]*>" nil t)
	  (let* ((url (match-string 2))
		 (replacement (concat "\n<img src=\"" url "\"</img>"))) ;; creating a fake image end-tag here
	  (replace-match replacement t t)))
      (goto-char (point-min))
      (while (re-search-forward "\\(<blockquote\\)\\s-*\\([^\0]+?\\)\\(</blockquote>\\)" nil t)
	(when (match-string 0)
	  (let* ((replacement (replace-regexp-in-string "<p[^>]*>" " " (match-string 0)))
		 (replacement (replace-regexp-in-string "</p>" "\n" replacement))
		 (replacement (replace-regexp-in-string "<pre\\s-*[^>]*>" "\n\n#+BEGIN_SRC\n" replacement))
		 (replacement (replace-regexp-in-string "</pre>" "\n#+END_SRC\n\n" replacement))
		 (replacement (replace-regexp-in-string "<code[^>]*>" "" replacement))
		 (replacement (replace-regexp-in-string "</code>" "" replacement)))
	    (replace-match replacement t t))))
;; dealing with formated links that break paragraphs
      (goto-char (point-min))
      (while (re-search-forward "\\(<a\\)\\s-*\\([^\0]+?\\)\\(</a>\\)" nil t)
	(when (match-string 0)
	  (let* ((replacement (replace-regexp-in-string "<u[^>]*>" " " (match-string 0)))
		 (replacement (replace-regexp-in-string "</u>" "" replacement))
		 (replacement (replace-regexp-in-string "<em[^>]*>" "/" replacement))
		 (replacement (replace-regexp-in-string "</em>" "/" replacement))
		 (replacement (replace-regexp-in-string "<strong[^>]*>" "*" replacement))
		 (replacement (replace-regexp-in-string "</strong>" "*" replacement))
		 (replacement (replace-regexp-in-string "\\[" "(" replacement))
		 (replacement (replace-regexp-in-string "\\]" ")" replacement)))
	    (replace-match replacement t t))))
      (goto-char (point-min))
      (while (re-search-forward "\\(<p[\s>]\\|<blockquote\\|<pre\\|<h1\\|<h2\\|<h3\\|<li[\s>]\\|<title\\|<img\\|<dl\\)\\s-*\\([^\0]+?\\)\\(</p>\\|</blockquote>\\|</pre>\\|</h1>\\|</h2>\\|</h3>\\|</ul>\\|</ol>\\|</li>\\|</title>\\|</img>\\|</dl>\\)" nil t)
	(when (match-string 0)
	  (setq case (match-string 0))
	  (when (or (string-match-p "<h1" case)
		    (string-match-p "<h2" case)
		    (string-match-p "<h3" case))
	    (setq case (replace-regexp-in-string "<ul[^>]*>" "" case))
	    (setq case (replace-regexp-in-string "<ol[^>]*>" "" case))
	    (setq case (replace-regexp-in-string "<li[^>]*>" "" case))
	    (setq case (replace-regexp-in-string "\n" "" case))
	    (setq case (replace-regexp-in-string "</span>" "\n" case 1))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (and (string-match-p "<li[^>]*>" case)
		     (not (string-match-p "<img" case))
		     (not (string-match-p "<p" case)))
	    (setq case (replace-regexp-in-string "<ul[^>]*>" "\n" case))
	    (setq case (replace-regexp-in-string "<ol[^>]*>" "\n" case))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (string-match-p "<dl[^>]*>" case)
	    (setq case (replace-regexp-in-string "\n" " " case))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (string-match-p "<blockquote" case)
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (string-match-p "<pre" case)
	    (setq case (replace-regexp-in-string "<code[^>]*>" "" case))
	    (setq case (replace-regexp-in-string "</code>" "" case))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (and (string-match-p "<img[\s>]" case)
		     (not (string-match-p "<a " case)))
	    (setq processed-content (concat processed-content "\n\n" case "\n\n")))
	  (when (and (string-match-p "<p[\s>]" case)
		     (not (string-match-p "<h1" case))
		     (not (string-match-p "<h2" case))
		     (not (string-match-p "<h3" case))
		     (not (string-match-p "<img" case)))
	    (setq processed-content (concat processed-content "\n\n" (replace-regexp-in-string "\n" " " case) "\n\n"))))))
    (setq processed-content (website2org-cleanup-remove-header processed-content))
    (setq processed-content (website2org-cleanup-html-tags processed-content))
    (setq title (website2org-return-title content))
    (setq processed-content (website2org-html-to-org processed-content og-url))
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
      (goto-char (point-min))
      (if (re-search-forward "<[!-]*footer[^>]*>" nil t 1)
	  (progn
	    (when (string-match-p "header" (match-string 0))
	      (re-search-forward "<[!-]*footer[^>]*>" nil t 1)
	      (setq result (buffer-substring-no-properties (point-min) (match-beginning 0))))
	    (when (not (string-match-p "header" (match-string 0)))
              (setq result (buffer-substring-no-properties (point-min) (match-beginning 0)))))
        (setq result (buffer-substring-no-properties (point-min) (point-max))))
    result)))

(defun website2org-return-title (content)
  "Returns the title of a HTML document."
  (let ((title)
	(title-p))
    (with-temp-buffer 
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "\\(<title[^>]*>\\)\\s-*\\([^\0]+?\\)\\(</title>\\)" nil t)
	(when (and (match-string 0)
		   (not title-p))
	  (setq title (match-string 0))
	  (setq title-p 1))
	(replace-match "" t t)))
    (setq title (replace-regexp-in-string "<title[^>]*>" "" title))
    (setq title (replace-regexp-in-string "</title>" "" title))
    (setq title (replace-regexp-in-string "[ \n]+" " " title))
    (setq title (website2org-cleanup-org-weird-characters title))
    (setq title (string-trim title))
    title))

(defun website2org-return-URL (content)
  "Returns the URL of a HTML document based on canonical link."
  (let ((URL)
	(URL-p))
    (with-temp-buffer 
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "\\(<link rel=\"canonical\" href=\"\\)\\([^\"]*\\)" nil t)
	(when (and (match-string 1)
		   (not URL-p))
	  (setq URL (match-string 2))
	  (setq URL-p 1))))
    URL))

(defun website2org-cleanup-html-tags (content)
  "Minimalizing HTML tags."
  (setq content (replace-regexp-in-string "<p\\s-\\([^>]*\\)>" "<p>" content))
  (setq content (replace-regexp-in-string "<em\s\\([^>]*\\)>" "<em>" content))
  (setq content (replace-regexp-in-string "<i\s\\([^>]*\\)>" "<i>" content))
  (setq content (replace-regexp-in-string "\\(<figure[\s>]\\)\\s-*\\([^\0]+?\\)\\(</figure>\\)" "" content))
  (setq content (replace-regexp-in-string "[\s =]*<script.*</script>[\s=]*" "" content))
  (setq content (replace-regexp-in-string "\\(<include-fragment[\s>]\\)\\s-*\\([^\0]+?\\)\\(</include-fragment>\\)" "" content))
  (setq content (replace-regexp-in-string "<h1\\([^>]*\\)>" "\n\n<h1>" content))
  (setq content (replace-regexp-in-string "<h2\\([^>]*\\)>" "\n\n<h2>" content))
  (setq content (replace-regexp-in-string "<h3\\([^>]*\\)>" "\n\n<h3>" content))
  (setq content (replace-regexp-in-string "<h4\\([^>]*\\)>" "\n\n<h4>" content))
  (setq content (replace-regexp-in-string "<h5\\([^>]*\\)>" "\n\n<h5>" content))
  (setq content (replace-regexp-in-string "<ul\\([^>]*\\)>" "\n\n<ul>" content))
  (setq content (replace-regexp-in-string "<ol\\([^>]*\\)>" "\n\n<ol>" content))
  (setq content (replace-regexp-in-string "<li\\([^>]*\\)>" "<li>" content))
  (setq content (replace-regexp-in-string "<dl\\([^>]*\\)>" "<dl>" content))
  (setq content (replace-regexp-in-string "<dt\\([^>]*\\)>" "<dt>" content))
  (setq content (replace-regexp-in-string "<dd\\([^>]*\\)>" "<dd>" content))
  (setq content (replace-regexp-in-string "<pre\\([^>]*\\)>" "<pre>" content))
  (setq content (replace-regexp-in-string "<blockquote\\s-\\([^>]*\\)>" "\n#+BEGIN_QUOTE\n" content))
  (setq content (replace-regexp-in-string "</blockquote>" "\n#+END_QUOTE\n" content))
  (setq content (replace-regexp-in-string "<pre>" "\n\n#+BEGIN_SRC\n" content))
  (setq content (replace-regexp-in-string "</pre>" "\n#+END_SRC" content))
  (setq content (replace-regexp-in-string "<u style\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<wow-image.*</wow-image>" "" content))
  (setq content (replace-regexp-in-string "<time\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</time\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<font\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</font\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<!--\\([^>]*\\)-->" "" content))
  (setq content (replace-regexp-in-string "<picture\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<input\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</input\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<form\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</form\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<label\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</label\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<pre>.*<code>" "<pre>" content))
  (setq content (replace-regexp-in-string "</code>.*</pre>" "</pre>" content))
  (setq content (replace-regexp-in-string "<strong\\([^>]*\\)>" "<strong>" content))
  (setq content (replace-regexp-in-string "<span\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</span\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<div\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<meta[^>]*>" "" content))
  (setq content (replace-regexp-in-string "</meta\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<small[^>]*>" "" content))
  (setq content (replace-regexp-in-string "</small\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<mark[^>]*>" "*" content))
  (setq content (replace-regexp-in-string "</mark\\([^>]*\\)>" "*" content))
  (setq content (replace-regexp-in-string "<abbr[^>]*>" "" content))
  (setq content (replace-regexp-in-string "</abbr\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<svg\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</svg\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<button\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</button\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "<path\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "</path\\([^>]*\\)>" "" content))
  (setq content (replace-regexp-in-string "\\(?:\u3000+\\)" "" content)))

(defun website2org-cleanup-remove-header (content)
  "Removes everything before the first H1 headline."
  (let ((h1-p))
    (with-temp-buffer 
    (insert content)
    (goto-char (point-min))
    (when (string-match-p "<h1" content)
      (while (not (eobp))
	(let ((line (thing-at-point 'line t)))
	  (when (string-match-p "<h1.*?>" line)
	    (setq h1-p t))
	  (when (not h1-p)
	    (delete-region (line-beginning-position) (line-end-position)))
	(forward-line))))
      (setq content (buffer-substring-no-properties (point-min)(point-max))))))	

(defun website2org-html-to-org-via-pandoc (content)
  "Turns the filtered HTML content into clean Orgmode content.
Currently this function is not needed/used."
    (with-temp-buffer 
      (insert content)
      (shell-command-on-region (point-min) (point-max) "pandoc -f html -t org --wrap=preserve" t t)
    (setq content (buffer-substring-no-properties (point-min)(point-max)))))
   
(defun website2org-html-to-org (content og-url)
  "Turns the filtered HTML content into clean Orgmode content."
 (setq content (replace-regexp-in-string "<p>" "\n\n" content))
 (setq content (replace-regexp-in-string "</p>" "\n" content))
 (setq content (replace-regexp-in-string "<strong>" "*" content))
 (setq content (replace-regexp-in-string "[ ]*</strong>" "* " content))
 (setq content (replace-regexp-in-string "\s*__\\*\\*" "*" content))
 (setq content (replace-regexp-in-string "\\*\\*\s," "*, " content))
 (setq content (replace-regexp-in-string "<b>" "*" content))
 (setq content (replace-regexp-in-string "[ ]*</b>" "*" content))
 (setq content (replace-regexp-in-string "<kbd>" "*" content))
 (setq content (replace-regexp-in-string "[ ]*</kbd>" "*" content))
 (setq content (replace-regexp-in-string "<dl>" "" content))
 (setq content (replace-regexp-in-string "</dl>" "" content))
 (setq content (replace-regexp-in-string "<dt>" "- " content))
 (setq content (replace-regexp-in-string "</dt>" ": " content))
 (setq content (replace-regexp-in-string "<dd>" "" content))
 (setq content (replace-regexp-in-string "</dd>" "\n" content))
 (setq content (replace-regexp-in-string "<i>" "" content))
 (setq content (replace-regexp-in-string "</i>" "" content))
 (setq content (replace-regexp-in-string "<kbd>" "~" content))
 (setq content (replace-regexp-in-string "</kbd>" "~" content))
 (setq content (replace-regexp-in-string "<em>" " /" content))
 (setq content (replace-regexp-in-string "[ ]*</em>" "/ " content))
 (setq content (replace-regexp-in-string "\\*\\* \/" " */" content))
 (setq content (replace-regexp-in-string "\/ \\*\\*" "/* " content))
 (setq content (replace-regexp-in-string "<h1>" "* " content))
 (setq content (replace-regexp-in-string "</h1>" "" content))
 (setq content (replace-regexp-in-string "<h2>" "** " content))
 (setq content (replace-regexp-in-string "</h2>" "" content))
 (setq content (replace-regexp-in-string "<h3>" "*** " content))
 (setq content (replace-regexp-in-string "</h3>" "" content))
 (setq content (replace-regexp-in-string "<h4>" "**** " content))
 (setq content (replace-regexp-in-string "</h4>" "" content))
 (setq content (replace-regexp-in-string "<u>" "" content)) ;; no support for U because it messes with org-links
 (setq content (replace-regexp-in-string "</u>" "" content))
 (setq content (replace-regexp-in-string "<cite\\([^>]*\\)>" " /" content))
 (setq content (replace-regexp-in-string "</cite>" "/ " content))
 (setq content (replace-regexp-in-string "<header>" "" content))
 (setq content (replace-regexp-in-string "</header>" "" content))
 (setq content (replace-regexp-in-string "<center>" "" content))
 (setq content (replace-regexp-in-string "</center>" "" content))
 (setq content (replace-regexp-in-string "<tt>" "" content))
 (setq content (replace-regexp-in-string "</tt>" "" content))
 (setq content (replace-regexp-in-string "<aside>" "" content))
 (setq content (replace-regexp-in-string "</aside>" "\n" content))
 (setq content (replace-regexp-in-string "<ul>" "" content))
 (setq content (replace-regexp-in-string "</ul>" "\n" content))
 (setq content (replace-regexp-in-string "<ol>" "" content))
 (setq content (replace-regexp-in-string "</ol>" "\n" content))
 (setq content (replace-regexp-in-string "</li>" "</li>\n" content))
 (setq content (replace-regexp-in-string "\\([^\n]\\)\\(<li>\\)" "\\1\n- " content))
 (setq content (replace-regexp-in-string "<li>" "- " content))
 (setq content (replace-regexp-in-string "</li>" "\n" content))
 (setq content (replace-regexp-in-string "<code\\([^>]*\\)>" "=" content))
 (setq content (replace-regexp-in-string "</code>" "=" content))
 (setq content (replace-regexp-in-string "<samp\\([^>]*\\)>" "=" content))
 (setq content (replace-regexp-in-string "</samp>" "=" content))
 (setq content (replace-regexp-in-string "<source\\([^>]*\\)>" "=" content))
 (setq content (replace-regexp-in-string "<var\\([^>]*\\)>" "" content))
 (setq content (replace-regexp-in-string "</var>" "" content))
 (setq content (replace-regexp-in-string "<div>" "" content))
 (setq content (replace-regexp-in-string "</div>" "" content))
 (setq content (replace-regexp-in-string "<sup\\([^>]*\\)>" "" content))
 (setq content (replace-regexp-in-string "</sup>" "" content))
 (setq content (replace-regexp-in-string "<pre>" "#+BEGIN_SRC\n" content))
 (setq content (replace-regexp-in-string "</pre>" "\n#+END_SRC" content))
 (setq content (replace-regexp-in-string "<blockquote>" "#+BEGIN_QUOTE\n" content))
 (setq content (replace-regexp-in-string "</blockquote>" "\n#+END_QUOTE" content))
 (setq content (replace-regexp-in-string "<br>" "\n" content))
 (setq content (replace-regexp-in-string "<br/>" "\n" content))
 (setq content (replace-regexp-in-string "<br\s/>" "\n" content))
 ;; transforming links
 (with-temp-buffer 
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "<a[ \t][^>]*?href=\\(['\"]?\\)\\([^'\" \t>]+\\)\\1[^>]*>\\([^<]+\\)</a>" nil t)
	  (let* ((url (match-string 2))
		 (url (website2org-fix-relative-links url og-url))
		 (url (replace-regexp-in-string "^#" "*" url))
		 (text (match-string 3))
		 (text (replace-regexp-in-string "[\n\t]" "" text))
		 (text (replace-regexp-in-string "=" " " text)) ; removed because of fontification glitch
		 (text (replace-regexp-in-string "^[ \t]+" "" text))
		 (anchor))
	    (when (string-match-p "class=anchor" (match-string 0))
	      (replace-match "" t t)
	      (setq anchor t))
	    (when (and (not (string-match-p "#fn" url))
		       (not anchor))
	      (replace-match (format "[[%s][%s]]" url text) t t))
	    (when (string-match-p "#fn" url)
	      (replace-match (format " [fn:%s]" text) t t))))
      (goto-char (point-min))
      (while (re-search-forward "<a[\s\t].*+href=['\"]\\([^'\"]+\\)['\"][^>]*></a>" nil t)
	  (replace-match "" t t))
      (goto-char (point-min))
;; transforming images into links
      (while (re-search-forward "<img[ \t].*?src=['\"]\\([^'\"]+\\)['\"][^>]*</img>" nil t)
	(let* ((url (match-string 1))
	       (text (file-name-nondirectory url))
	       (url (website2org-fix-relative-image-links url og-url))
	       (text (replace-regexp-in-string "[+_-]" " " text)))
	  (setq url (replace-regexp-in-string "^#" "*" url))
	  (replace-match (format "(/image:/ [[%s][%s]])" url text) t t)))
      (goto-char (point-min))
 ;; this is to remove the traces of <strong> in <a href>     
      (goto-char (point-min))
      (while (re-search-forward "\\[\\*\\*" nil t)
	  (replace-match "\[" t t))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\]" nil t)
	  (replace-match "\]" t t))
      (goto-char (point-min))
      (while (re-search-forward "\\(#+BEGIN_SRC\\)\\s-*\\([^\0]+?\\)\\(#+END_SRC\\)" nil t)
	(when (match-string 0)
	  (let* ((begin-tag (match-string 1))
		 (content (match-string 2))
		 (end-tag (match-string 3))
		 (processed-content (split-string content "\n")))
	    (replace-match (concat begin-tag processed-content end-tag) on t t))))
      (goto-char (point-min))
      (while (re-search-forward "\\(^- \\)\\s-*\\([^\0]+?\\)\\(#+END_SRC\\)" nil t)
	(when (match-string 0)
	  (let* ((begin-tag (match-string 1))
		 (content (match-string 2))
		 (end-tag (match-string 3))
		 (processed-content (split-string content "\n")))
	    (replace-match (concat begin-tag processed-content end-tag) on t t))))
      (buffer-substring-no-properties (point-min) (point-max))))

(defun website2org-fix-relative-image-links (url og-url)
  "Turns relative image URLs into complete image URLs."
  (let ((path (file-name-directory og-url)))
    (when (not (string-prefix-p "http" url))
      (when (string-prefix-p "/" url)
	(setq path (replace-regexp-in-string "\\(http.*?//.*?/\\).*" "\\1" path))
        (setq url (string-remove-prefix "/" url)))
      (when (string-prefix-p "\"" path)
        (setq path (string-remove-prefix "\"" path)))
      (setq url (concat path url)))
    url))

(defun website2org-fix-relative-links (url og-url)
  "Turns relative URLs into complete URLs."
  (when (or (not (string-suffix-p "/" og-url))
	    (not (string-suffix-p ".html" og-url))
	    (not (string-suffix-p ".htm" og-url)))
    (setq og-url (concat og-url "/")))
  (let* ((path (file-name-directory og-url))
	 (path (replace-regexp-in-string "\\(http.*?//.*?/\\).*" "\\1" path)))  
    (when (and (not (string-prefix-p "http" url))
	       (not (string-prefix-p "//" url)))
      (when (and (string-prefix-p "/" url)
		 (not (string-prefix-p "//" url)))
        (setq url (concat path (string-remove-prefix "/" url)))))
    (when (string-prefix-p "\"" path)
      (setq path (string-remove-prefix "\"" path))
      (setq url (concat path url)))
    (when (string-prefix-p "//" url)
      (setq url (concat "https:" url)))
    url))

(defun website2org-cleanup-org-weird-characters (content)
  "Cleaning-up weird characters in the Orgmode content."
  (setq content (replace-regexp-in-string "&quot;" "\"" content))
  (setq content (replace-regexp-in-string "&#91;" "[" content))
  (setq content (replace-regexp-in-string "&#93;" "]" content))
  (setq content (replace-regexp-in-string "&copy;" "©" content))
  (setq content (replace-regexp-in-string "&mdash;" "—" content))
  (setq content (replace-regexp-in-string "&mldr;" "..." content))
  (setq content (replace-regexp-in-string "&ldquo;" "\"" content))
  (setq content (replace-regexp-in-string "&rdquo;" "\"" content))
  (setq content (replace-regexp-in-string "&rsquo;" "'" content))
  (setq content (replace-regexp-in-string "&lsquo;" "'" content))
  (setq content (replace-regexp-in-string "\\u2019s" "'" content))
  (setq content (replace-regexp-in-string "&#x27;" "'" content))
  (setq content (replace-regexp-in-string "&#39;" "'" content))
  (setq content (replace-regexp-in-string "&nbsp;" " " content))
  (setq content (replace-regexp-in-string "&gt;" ">" content))
  (setq content (replace-regexp-in-string "&lt;" "<" content))
  (setq content (replace-regexp-in-string "&#8211;" "–" content))
  (setq content (replace-regexp-in-string "&#8222;" "„" content))
  (setq content (replace-regexp-in-string "&#8220;" "“" content))
  (setq content (replace-regexp-in-string "&#8221;" "”" content))
  (setq content (replace-regexp-in-string "&#8216;" "‘" content))
  (setq content (replace-regexp-in-string "[\t\r]+" " " content))
  (setq content (replace-regexp-in-string "&bull;" "•" content))
  (setq content (replace-regexp-in-string " " " " content))
  (setq content (replace-regexp-in-string "&amp;" "&" content))
  (setq content (replace-regexp-in-string "&#\\(?:8217\\|039\\);" "’" content))
  (setq content (xml-substitute-special content))
  (setq content (replace-regexp-in-string
   "\\\\u\\([0-9A-Fa-f]\\{4\\}\\)"
   (lambda (m)
     (char-to-string (string-to-number (match-string 1 m) 16)))
   content)))


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
	  (when (string-match-p "\s*<style" line)
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
    (setq content (replace-regexp-in-string "\\*\s\\([,;.:!?)]\\)" "*\\1" content))
    (setq content (replace-regexp-in-string "\\([(]\\)\s/" "\\1/" content))
;; always at least one space before links
    (setq content (replace-regexp-in-string "\\([^ \(]\\)\\[\\[" "\\1 [[" content))
;; no empty lines that just start with \  
    (setq content (replace-regexp-in-string "[\\]*$" "" content)) 
;; no empty lines that just start with * 
    (setq content (replace-regexp-in-string "^[\*>]* $" "" content))
    (setq content (replace-regexp-in-string "^[\*>]*$" "" content))
;; no new line starts with a space
    (setq content (replace-regexp-in-string "^\s*" "" content)) 
;; no more than one space 
    (setq content (replace-regexp-in-string "\s\\{2,\\}" "\s" content))
;; no empty lines that just start with - or = 
    (setq content (replace-regexp-in-string "^- $\\|^-$" "" content))
    (setq content (replace-regexp-in-string "^= $\\|^=$" "" content))
;; no more than one empty line
    (setq content (replace-regexp-in-string "\n\\{2,\\}" "\n\n" content))
;; proper italics
    (setq content (replace-regexp-in-string " \/[ ]*$" "/" content))
;; proper italics for links
    (setq content (replace-regexp-in-string " \/ \\[\\[" " /[[" content))
    (setq content (replace-regexp-in-string "\\]\\[\/[ ]*" "][/" content))
;; proper separatuion of links from italics or strong 
    (setq content (replace-regexp-in-string "\]\]\\*" "]] *" content))
    (setq content (replace-regexp-in-string "[\“] \\[\\[" "\“[[" content))
    (setq content (replace-regexp-in-string "[\"] \\[\\[" "\"[[" content))
;; no empty line before END_SRC
    (setq content (replace-regexp-in-string "^\n#\\+END_SRC" "#+END_SRC" content))
;; no empty line before END_QUOTE
    (setq content (replace-regexp-in-string "^\n#\\+END_QUOTE" "#+END_QUOTE" content))
;; no empty line after BEGIN_QUOTE
    (setq content (replace-regexp-in-string "^#\\+BEGIN_QUOTE\n\n" "#+BEGIN_QUOTE\n" content))
;; remains a TODO
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "\\(^-.*\n\\)\\(^[ \t]*\n\\)\\(^-.*\n\\)" nil t)
	(when (match-string 0)
	  (let* ((replacement (concat (match-string 1) (match-string 3))))
	    (replace-match replacement t t)))
	(forward-line -1))
      (setq content (buffer-substring-no-properties (point-min)(point-max)))))

(provide 'website2org)

;;; website2org.el ends here
