;;; flymake-elisp-config-test.el --- Test for flymake-elisp-config

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

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

;; Test for flymake-elisp-config

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'flymake-elisp-config)

(ert-deftest flymake-elisp-config-config-file-p ()
  (let ((regexp-list
         flymake-elisp-config-config-file-name-regexp-list))
   (should (flymake-elisp-config-config-file-p
           "init.el"
           "~/.emacs.d"
           regexp-list))
   (should (flymake-elisp-config-config-file-p
           ".emacs"
           "~/"
           regexp-list))
   (should (flymake-elisp-config-config-file-p
           ".emacs.el"
           "~/"
           regexp-list))
   (should (flymake-elisp-config-config-file-p
           "init.el"
           "~/dotfiles/emacs/.emacs.d"
           regexp-list))
   (should-not (flymake-elisp-config-config-file-p
                "flymake-elisp-config.el"
                "~/flymake-elisp-config"
                regexp-list))))

(provide 'flymake-elisp-config-test)
;;; flymake-elisp-config-test.el ends here
