;;; flymake-elisp-config.el --- Setup load-path for flymake on Emacs Lisp mode  -*- Lexical-binding: t; -*-

;; Copyright (C) 2022-2023  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: lisp

;; Version: 1.0.1
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/ROCKTAKEY/flymake-elisp-config

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Table of Contents
;; _________________

;; 1. flymake-elisp-config: Setup `load-path' for flymake on Emacs Lisp mode
;; 2. How to Use?
;; 3. Add configurer
;; .. 1. Define predicate
;; .. 2. Define configurer
;; 4. License


;; [https://img.shields.io/github/tag/ROCKTAKEY/flymake-elisp-config.svg?style=flat-square]
;; [https://img.shields.io/github/license/ROCKTAKEY/flymake-elisp-config.svg?style=flat-square]
;; [https://img.shields.io/codecov/c/github/ROCKTAKEY/flymake-elisp-config.svg?style=flat-square]
;; [https://img.shields.io/github/workflow/status/ROCKTAKEY/flymake-elisp-config/test/master.svg?style=flat-square]


;; [https://img.shields.io/github/tag/ROCKTAKEY/flymake-elisp-config.svg?style=flat-square]
;; <https://github.com/ROCKTAKEY/flymake-elisp-config>

;; [https://img.shields.io/github/license/ROCKTAKEY/flymake-elisp-config.svg?style=flat-square]
;; <file:LICENSE>

;; [https://img.shields.io/codecov/c/github/ROCKTAKEY/flymake-elisp-config.svg?style=flat-square]
;; <https://codecov.io/gh/ROCKTAKEY/flymake-elisp-config?branch=master>

;; [https://img.shields.io/github/workflow/status/ROCKTAKEY/flymake-elisp-config/test/master.svg?style=flat-square]
;; <https://github.com/ROCKTAKEY/flymake-elisp-config/actions>


;; 1 flymake-elisp-config: Setup `load-path' for flymake on Emacs Lisp mode
;; ========================================================================

;;   Default `load-path' for flymake on Emacs Lisp mode can be set through
;;   `elisp-flymake-byte-compile-load-path', but it is just a global
;;   variable.  When you are editing init.el, flymake should use all the
;;   `load-path'.  When you update some packages, `load-path' also should
;;   be updated.  When you are editing your package, flymake should use
;;   paths provided by cask or keg.

;;   This package provides three features:
;;   - Customizable variable `flymake-elisp-config-load-path-getter', which
;;     is a `FUNCTION' returning `load-path' for flymake.
;;   - Automatic setting of `load-path' for flymake by
;;     `flymake-elisp-config-auto-mode'.
;;   - Manual setting of it by configurers named
;;     `flymake-elisp-config-as-*'.


;; 2 How to Use?
;; =============

;;   Just write in init.el:
;;   ,----
;;   | ;; Make `load-path' for flymake customizable through `flymake-elisp-config-load-path-getter'.
;;   | (flymake-elisp-config-global-mode)
;;   | ;; Automatically set `load-path' for flymake.
;;   | (flymake-elisp-config-auto-mode)
;;   `----

;;   If automatical setting is wrong, you can use
;;   `flymake-elisp-config-as-*' commands to change `load-path' for flymake
;;   manually.
;;   `flymake-elisp-config-as-config'
;;         Emacs configuration file such as init.el
;;   `flymake-elisp-config-as-keg'
;;         Emacs Lisp project managed by `keg'.
;;   `flymake-elisp-config-as-cask'
;;         Emacs Lisp project managed by `cask'.
;;   `flymake-elisp-config-as-default'
;;         Default Emacs Lisp file.  It uses same `load-path' as default
;;         flymake.


;; 3 Add configurer
;; ================

;;   When you want to use another `load-path' getter, you can define
;;   `configurer'.  It can be achived by three steps:
;;   1. Define predicate
;;   2. Define configurer
;;   3. Register to `flymake-elisp-config-auto-configurer-alist'


;; 3.1 Define predicate
;; ~~~~~~~~~~~~~~~~~~~~

;;   Define predicate function, which takes `BUFFER' as an argument, and
;;   which return `non-nil' only when `flymake' should use your new
;;   configurer.


;; 3.2 Define configurer
;; ~~~~~~~~~~~~~~~~~~~~~

;;   Define configurer function, which takes `BUFFER' as an argument, and
;;   which sets `flymake-elisp-config-load-path-getter' in `BUFFER'.

;;   The value of `flymake-elisp-config-load-path-getter' should be a
;;   function which takes `BUFFER' as an argument, and returns list of
;;   path, like `load-path', which is used by `flymake' on `BUFFER'.


;; 4 License
;; =========

;;   This package is licensed by GPLv3. See [LICENSE].


;; [LICENSE] <file:LICENSE>

;;; Code:

(require 'subr-x)
(require 'project)
(require 'flymake)

(defgroup flymake-elisp-config ()
  "Setup `load-path' for flymake on Emacs Lisp mode."
  :group 'flymake
  :prefix "flymake-elisp-config-"
  :link '(url-link "https://github.com/ROCKTAKEY/flymake-elisp-config"))


;;; Initializer of a project manager
(defvar-local flymake-elisp-config--initializer-process nil
  "Process of `flymake-elisp-config--initializer'.
It is nil when no process run.")

(defun flymake-elisp-config--initializer-sentinel (buffer process-sentinel process event)
  "Sentinel assosiated with process of `flymake-elisp-config--initializer'.
BUFFER is a buffer to initialize.  PROCESS-SENTINEL will run in the end of
this function.

The requirement of PROCESS-SENTINEL is same as `set-process-sentinel'.
PROCESS and EVENT is also same as the arguments of SENTINEL
of `set-process-sentinel'."
  (when (and
         (buffer-live-p buffer)
         (not (process-live-p process)))
    (with-current-buffer buffer
      (setq flymake-elisp-config--initializer-process nil))
    (let ((process-buf (process-buffer process))
          (program (car (process-command process)))
          (project-dir (with-current-buffer buffer (project-root (project-current)))))
      (message "`flymake-elisp-config' initializer process of \"%s\" in \"%s\" %s"
               program project-dir (substring event nil -1))
      (when (string= event "finished\n")
        (kill-buffer process-buf)
        (with-current-buffer buffer
          (funcall process-sentinel process event))))))

(defun flymake-elisp-config--initializer (buffer program program-args &optional process-sentinel)
  "Initialize elisp BUFFER for flymake by running PROGRAM with PROGRAM-ARGS.
After initializeation, Call PROCESS-SENTINEL.
The requirement of PROCESS-SENTINEL is same as `set-process-sentinel'."
  (with-current-buffer buffer
    (let* ((project-dir (project-root (project-current)))
           (process-name (concat "flymake-elisp-config--initializer "  program " - " project-dir))
           (process-buf (generate-new-buffer (concat " *" process-name "*")))
           (process (apply #'start-process
                           process-name process-buf program program-args)))
      (set-process-sentinel process
                            (apply-partially #'flymake-elisp-config--initializer-sentinel buffer process-sentinel))
      (setq flymake-elisp-config--initializer-process process))))


;;; `flymake-elisp-config-mode'

(defcustom flymake-elisp-config-load-path-getter #'flymake-elisp-config-get-load-path-default
  "Function which returns `load-path' used by flymake in current elisp buffer.
The default value is used as fallback."
  :group 'flymake-elisp-config
  :local t
  :type 'function)

(defun flymake-elisp-config-get-load-path (buffer)
  "Get `load-path' for flymake in Emacs Lisp BUFFER."
  (funcall flymake-elisp-config-load-path-getter buffer))

(defun flymake-elisp-config-byte-compile (report-fn &rest args)
  "A Flymake backend for elisp byte compilation.
This is just a wrapper of `elisp-flymake-byte-compile' to override
 `elisp-flymake-byte-compile-load-path'.

REPORT-FN and ARGS are directly passed to `elisp-flymake-byte-compile'."
  ;; NOTE: Remove the process object when process is already finished.
  (unless flymake-elisp-config--initializer-process
    (let ((elisp-flymake-byte-compile-load-path
           (flymake-elisp-config-get-load-path (current-buffer))))
      (elisp-flymake-byte-compile report-fn args))))

;;;###autoload
(define-minor-mode flymake-elisp-config-mode
  "Provide configurable `load-path' with flymake in Emacs Lisp mode.
Set getter function of `load-path' to `flymake-elisp-config-load-path-getter'."
  :group 'flymake-elisp-config
  (if flymake-elisp-config-mode
      (setq-local flymake-diagnostic-functions
                  (mapcar (lambda (arg)
                            (if (eq arg #'elisp-flymake-byte-compile)
                                #'flymake-elisp-config-byte-compile
                              arg))
                          flymake-diagnostic-functions))
    (setq-local flymake-diagnostic-functions
                (mapcar (lambda (arg)
                          (if (eq arg #'flymake-elisp-config-byte-compile)
                              #'elisp-flymake-byte-compile
                            arg))
                        flymake-diagnostic-functions))))

;;;###autoload
(define-globalized-minor-mode flymake-elisp-config-global-mode
  flymake-elisp-config-mode
  flymake-elisp-config-mode)


;;; `flymake-elisp-config-auto-mode'

(defcustom flymake-elisp-config-auto-configurer-alist
  '((flymake-elisp-config-config-p . flymake-elisp-config-as-config)
    (flymake-elisp-config-keg-p . flymake-elisp-config-as-keg)
    (flymake-elisp-config-cask-p . flymake-elisp-config-as-cask))
  "Alist to determine a flymake configurer in elisp buffer.
Each element is `(PRED . CONFIGURER)'.
PRED is a function which takes buffer to configure as argument,
and return non-nil when CONFIGURER should be used as CONFIGURER of BUFFER.
CONFIGURER takes buffer to configure as argument,
and it should set `flymake-elisp-config-load-path-getter' in BUFFER.

See also `flymake-elisp-config-load-path-getter'."
  :group 'flymake-elisp-config
  :type '(alist :key-type function :value-type function))

(make-obsolete
 'flymake-elisp-config-auto-load-path-getter-alist
 "Use `flymake-elisp-config-auto-configurer-alist' instead (but incompatible)."
 "1.0.0")

(defun flymake-elisp-config-auto-configure ()
  "Automatically configure `load-path' for flymake on current Emacs Lisp buffer.
This function scans `flymake-elisp-config-auto-configurer-alist'
to determines the flymake CONFIGURER, which sets `load-path' getter."
  (funcall
   (or
    (seq-some
     (lambda (cons)
       (let ((pred (car cons))
             (configurer (cdr cons)))
         (and (funcall pred (current-buffer))
              configurer)))
     flymake-elisp-config-auto-configurer-alist)
    #'ignore)
   (current-buffer)))

;;;###autoload
(define-minor-mode flymake-elisp-config-auto-mode
  "Configure flymake appropriately in Emacs Lisp file.
`flymake-elisp-config-global-mode' should be turned on to use this minor mode."
  :global t
  :group 'flymake-elisp-config
  (unless flymake-elisp-config-global-mode
    (user-error "`flymake-elisp-config-global-mode' should be turned on when you use `flymake-elisp-config-auto-mode'"))
  (if flymake-elisp-config-auto-mode
      (add-hook 'emacs-lisp-mode-hook #'flymake-elisp-config-auto-configure)
    (remove-hook 'emacs-lisp-mode-hook #'flymake-elisp-config-auto-configure)))


;;; Default `load-path' getter

(defun flymake-elisp-config-get-load-path-default (_)
  "Get `elisp-flymake-byte-compile-load-path'."
  elisp-flymake-byte-compile-load-path)

;;;###autoload
(defun flymake-elisp-config-as-default ()
  "Current buffer file is regarded as usual Emacs Lisp file.
`load-path' used by flymake is provided by
`elisp-flymake-byte-compile-load-path'."
  (interactive)
  (flymake-elisp-config-mode)
  (setq flymake-elisp-config-load-path-getter #'flymake-elisp-config-get-load-path-default))


;;; `load-path' getter for Emacs configuration file

(defun flymake-elisp-config-get-load-path-config (_)
  "Get `load-path' for flymake in Emacs configuration file."
  (append elisp-flymake-byte-compile-load-path
          load-path))

(defcustom flymake-elisp-config-config-file-name-regexp-list
  (list (concat (regexp-opt
                 '("init.el"
                   ".emacs"
                   ".emacs.el"))
                "$"))
  "Regexp list whose element matches Emacs configuration file, like init.el."
  :group 'flymake-elisp-config
  :type '(repeat string))

(defun flymake-elisp-config-config-file-p (file current-directory regexp-list)
  "Return non-nil if FILE in CURRENT-DIRECTORY is Emacs configuration file.
Each element of REGEXP-LIST, a regular expression, matches Emacs configuration
files."
  (let ((file-fullname (expand-file-name file current-directory)))
    (seq-some
     (lambda (regexp)
       (string-match-p regexp file-fullname))
     regexp-list)))

(defun flymake-elisp-config-config-p (buffer)
  "Return non-nil if BUFFER file is Emacs configuration file.
Each element of REGEXP-LIST, a regular expression, matches Emacs configuration
files."
  (with-current-buffer buffer
    (when-let* ((file (buffer-file-name))
                (current-directory default-directory)
                (file-fullname (expand-file-name file current-directory))
                (regexp-list flymake-elisp-config-config-file-name-regexp-list))
      (seq-some
       (lambda (regexp)
         (string-match-p regexp file-fullname))
       regexp-list))))

;;;###autoload
(defun flymake-elisp-config-as-config (buffer)
  "BUFFER file is regarded as Emacs configuration file by flymake.
`load-path' used by flymake is provided by
`flymake-elisp-config-get-load-path-config'."
  (interactive
   (list (current-buffer)))

  (with-current-buffer buffer
    (flymake-elisp-config-mode)
    (setq flymake-elisp-config-load-path-getter #'flymake-elisp-config-get-load-path-config)))


;;; `load-path' getter for project managed by `cask'

(defvar-local flymake-elisp-config-load-path-cask-cache nil
  "Cache for `flymake-elisp-config-get-load-path-cask'.")

(defun flymake-elisp-config-get-load-path-cask (_)
  "Get `load-path' for flymake in Emacs Lisp package file managed by `cask'.
Because \"cask load-path\" is sometimes late, it return only cache.
You can refresh cache by `flymake-elisp-config-get-load-path-cask-refresh'.
It also runs when the buffer initialized."
  (append elisp-flymake-byte-compile-load-path
          flymake-elisp-config-load-path-cask-cache))

(defun flymake-elisp-config-get-load-path-cask-get-from-cask ()
  "Return `load-path' for flymake in package file from `cask'."
  (let ((default-directory (project-root (project-current))))
    (split-string
     (car (last (split-string (shell-command-to-string "cask load-path"))))
     (path-separator))))

(defun flymake-elisp-config-get-load-path-cask-refresh (buffer)
  "Refresh cache for `load-path' in elisp BUFFER under a `cask'-managed project."
  (interactive
   (list (current-buffer)))
  (message "Refresh load-path for flymake by \"cask load-path\"...")
  (let ((process-buf (generate-new-buffer " *flymake-elisp-config - cask load-path*")))
    (set-process-sentinel
     (start-process "flymake-elisp-config - cask load-path" process-buf "cask" "load-path")
     `(lambda (process event)
        (unless (string= event "finished\n")
          (user-error "Somehow \"cask load-path\" failed"))

        (setq flymake-elisp-config-load-path-cask-cache
              (split-string
               (car (last (split-string
                           (with-current-buffer (process-buffer process)
                             (buffer-substring-no-properties (point-min) (point-max))))))
               (path-separator)))
        (kill-buffer (process-buffer process))
        (with-current-buffer ,buffer
          (flymake-start t))
        (message "Refresh load-path for flymake by \"cask load-path\"...done")))))

(defun flymake-elisp-config-cask-p (buffer)
  "Return non-nil if BUFFER is in the project managed by `cask'."
  (with-current-buffer buffer
    (when-let* ((project (project-current))
                (root (project-root project)))
      (locate-file "Cask" (list root)))))

;;;###autoload
(defun flymake-elisp-config-as-cask (buffer)
  "BUFFER file is regarded as a `cask'-managed project by flymake.
`load-path' used by flymake is provided by
`flymake-elisp-config-get-load-path-cask'."
  (interactive
   (current-buffer))

  (unless (executable-find "cask")
    (user-error "`cask' executable is not found.  Please install it and run M-x `flymake-elisp-config-as-cask'"))

  (flymake-elisp-config--initializer
   buffer
   "cask" '("install")
   `(lambda (_process _event)
      (if (string= event "finished\n")
          (flymake-elisp-config-get-load-path-cask-refresh ,buffer)
        (user-error "Somehow \"cask install\" failed"))))

  (with-current-buffer buffer
    (flymake-elisp-config-mode)
    (setq flymake-elisp-config-load-path-getter #'flymake-elisp-config-get-load-path-cask)))


;;; `load-path' getter for project managed by `keg'

(defun flymake-elisp-config-get-load-path-keg (buffer)
  "Return `load-path' in Emacs Lisp BUFFER under a project managed by `keg'."
  (append elisp-flymake-byte-compile-load-path
          (with-current-buffer buffer
            (let ((default-directory (project-root (project-current))))
              (split-string
               (car (last (split-string (shell-command-to-string "keg load-path"))))
               (path-separator))))))

(defun flymake-elisp-config-keg-p (buffer)
  "Return non-nil if BUFFER is in the project managed by `keg'."
  (with-current-buffer buffer
    (when-let* ((project (project-current))
                (root (project-root project)))
      (locate-file "Keg" (list root)))))

;;;###autoload
(defun flymake-elisp-config-as-keg (buffer)
  "BUFFER file is regarded as a `keg'-managed project by flymake.
`load-path' used by flymake is provided by
`flymake-elisp-config-get-load-path-keg'."
  (interactive
   (list (current-buffer)))

  (unless (executable-find "keg")
    (user-error "`keg' executable is not found.  Please install it and run M-x `flymake-elisp-config-as-keg'"))

  (message "Run \"keg install\"...")
  (flymake-elisp-config--initializer
   buffer
   shell-file-name
   (list
    (cond
     ((string-match-p "cmd.exe$" shell-file-name) "/c")
     ((string-match-p "powershell.exe$" shell-file-name) "-Command")
     (t "-c"))
    "keg install")
   `(lambda (_process event)
      (if (string= event "finished\n")
          (progn
            (message "Run \"keg install\"...done")
            (with-current-buffer ,buffer
              (flymake-start t)))
        (user-error "Somehow \"keg install\" failed"))))

  (with-current-buffer buffer
    (flymake-elisp-config-mode)
    (setq flymake-elisp-config-load-path-getter #'flymake-elisp-config-get-load-path-keg)))

(provide 'flymake-elisp-config)
;;; flymake-elisp-config.el ends here
