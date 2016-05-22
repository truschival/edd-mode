;;; edd-mode.el --- Major Mode for Electronic Device Description

;; Based on actionscript-mode.el, ardunio-mode.el
;;
;; Author: Thomas Ruschival (thomas@ruschival.de)
;; Created:  19 May 2016
;; Keywords: languages


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(require 'cc-mode)

(eval-when-compile
  (require 'cl)
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cc-menus))

(eval-and-compile
  (c-add-language 'edd-mode 'c++-mode))

(c-lang-defconst c-primitive-type-kwds
  edd (append '("FLOAT" 	"BOOL"
				"INDEX"		"ARRAY_INDEX"
				"ASCII" "BIT_ENUMERATED" "DATE"
				"UNSIGNED_INTEGER")
			  (c-lang-const c-primitive-type-kwds)))


(c-lang-defconst c-constant-kwds
  edd (append
           '("READ" "WRITE" "TRUE" "FALSE"
			 "DEVICE" "OFFLINE" "DYNAMIC" "LOCAL" "TABLE"
			 "SCOPE" "LARGE" "ONLINE" "DIALOG" "PAGE"
			  "VERTICAL" "XX_LARGE" "INPUT"
			 )
           (c-lang-const c-constant-kwds)))


(c-lang-defconst c-modifier-kwds
  edd (append
	   '( "CONTAINED"
		  "HANDLING"  "PRIVATE" )
  (c-lang-const c-modifier-kwds)))

;; Statements followed directly by a block of code
(c-lang-defconst c-block-stmt-1-kwds
  edd (append
	   '("ITEMS" "TYPE" "DEFINITION"
		 "REQUEST" "REPLY" "TRANSACTION"
         "OPERATION" "RESPONSE_CODES" "VECTORS")
	(c-lang-const c-block-stmt-1-kwds)))

;; Statements followed directly by a Parentesis and block of code
(c-lang-defconst c-block-stmt-2-kwds
  edd (append
	'("VALIDITY" "VISIBILITY" "IF" "ELSE" )
	(c-lang-const c-block-stmt-2-kwds)))

;; Declaration of types
(c-lang-defconst c-type-list-kwds
  edd (append
	   '("ARRAY" "INTERFACE" "COMMAND" "MENU" "METHOD"
		 "GRID" "IMAGE" "TEMPLATE" "VARIABLE"
		 "SOURCE" "AXIS" "WAVEFORM" "COLLECTION")
	   (c-lang-const c-type-list-kwds)))

;; Free standing statements (break, continue...)
(c-lang-defconst c-simple-stmt-kwds
  edd (append
           '("random"
             )
           (c-lang-const c-simple-stmt-kwds)))

;; This allows the classes after the "LIKE" in the class declartion to be
;; fontified.
(c-lang-defconst c-typeless-decl-kwds
  edd '("LIKE" "OF"))

;; We map all attribute keywords of EDDL to other-keywords
(c-lang-defconst c-other-kwds
  edd (append
	   '(
		 "ACCESS"
		 "CLASS"
		 "HANDLING"
		 "TYPE"
		 "PATH"
		 "LABEL"
		 "STYLE"
		 "HELP"
		 "MIN_VALUE"
		 "MAX_VALUE"
		 "DEFAULT_VALUE"
		 "CONSTANT_UNIT"
		 "DISPLAY_FORMAT"
	;; Chart Attributes
		 "MEMBERS"
		 "WIDTH"
		 "HEIGHT"
	;; Source Attributes
		 "LINE_COLOR"
		 "X_AXIS"
		 "Y_AXIS"
	;; Attributes for COMMANDS
		 "NUMBER"
		 "BLOCK"
		 "OPERATION"
		 "COLUMNBREAK"
		 )
	   (c-lang-const c-other-kwds)))

(defconst edd-font-lock-keywords-1 (c-lang-const c-matchers-1 edd)
  "Minimal highlighting for Edd mode.")

(defconst edd-font-lock-keywords-2 (c-lang-const c-matchers-2 edd)
  "Fast normal highlighting for Edd mode.")

(defconst edd-font-lock-keywords-3 (c-lang-const c-matchers-3 edd)
  "Accurate normal highlighting for Edd mode.")

(defvar edd-font-lock-keywords edd-font-lock-keywords-3
  "Default expressions to highlight in EDD mode.")

(defvar edd-mode-syntax-table nil
  "Syntax table used in edd-mode buffers.")
(or edd-mode-syntax-table
    (setq edd-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table edd))))

(defvar edd-mode-abbrev-table nil
  "Abbreviation table used in edd-mode buffers.")

(c-define-abbrev-table 'edd-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trigger
  ;; reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar edd-mode-map
  (let ((map (c-make-inherited-keymap)))
    ;; Add bindings which are only useful for Edd
    map)
  "Keymap used in edd-mode buffers.")

(easy-menu-define edd-menu edd-mode-map "Edd Mode Commands"
  (cons "Edd" (c-lang-const c-mode-menu edd)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.edd\\'" . edd-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dd\\'" . edd-mode))

;;;###autoload
(defun edd-mode ()
  "Major mode for editing Edd code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `edd-mode-hook'.

Key bindings:
\\{edd-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table edd-mode-syntax-table)
  (setq major-mode 'edd-mode
        mode-name "edd"
        local-abbrev-table edd-mode-abbrev-table
        abbrev-mode t
        imenu-generic-expression cc-imenu-c-generic-expression)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars edd-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'edd-mode)
  (easy-menu-add edd-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'edd-mode-hook)
  (c-update-modeline)
  ;;(add-custom-keyw)
 )

(defun edd-run-edd ()
  (interactive)
  (start-file-process "edd" () edd-executable (buffer-file-name)))

(provide 'edd-mode)
;;; edd-mode.el ends here
