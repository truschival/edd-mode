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

;; EDD mandatory elements and default menus
(setq edd-ident-kwds
	  '(
		"MANUFACTURER" "DEVICE_TYPE" "DEVICE_REVISION" "DD_REVISION"
		"device_root_menu" "diagnostic_root_menu"
		"download_to_device_root_menu"
		"mantenance_root_menu" "offline_root_menu" "process_variables_root_menu"
		"root_menu" "upload_from_device_root_menu"
		"view_menu"
		))

;; EDD communication functions
(setq edd-comm-cmds
	  '("abortTransferPort" "browseIdentity" "closeTransferPort"
		"ext_send_command" "ext_send_command_trans" "fGetByte"
		"get_more_status" "get_transfer_status" "openTransferPort"
		"readItemFromDevice" "send" "send_command"
		"send_command_trans" "send_trans" "writeItemToDevice"
		"ReadCommand" "WriteCommand"
		))

;; EDD built-in functions
(setq edd-built-in-functions
	  '("ACKNOWLEDGE" "BUILD_MESSAGE" "DELAY" "LOG_MESSAGE"
		"MenuDisplay" "PUT_MESSAGE" "SELECT_FROM_LIST"
		"SELECT_FROM_MENU" "abort" "add_abort_method" "dictionary_string"
		"discard_on_exit" "fgetval" "fsetval" "get_enum_string"
		"get_rspcode_string" "get_status_code_string"
		"igetval" "isOffline" "isetval" "lassign"
		"pop_abort_method" "process_abort" "push_abort_method"
		"put_message" "remove_abort_method" "remove_all_abort_methods"
		"save_values" "sgetval" "ssetval" "strcmp" "strlen" "strlwr"
		"strmid" "strstr" "strtrim" "strupr"
		"GET_LOCAL_VAR_VALUE" "GET_DEV_VAR_VALUE"
		"get_local_var_value" "get_dev_var_value"
		"get_dictionary_string"
	    ;; method related
		"DD_ITEM" "DD_STRING" "DICT_ID" "DICTSTRID"
		"ITEM_ID" "MEMBER_ID" "METHOD_ID" "METHODID"
		"VARID"
		;; list
		"ListInsert" "ListDeleteElementAt"
		))

;; EDD Deprecated built-ins
;; See SPEC500 Annex A "Archaic Builtins"
(setq edd-deprecated-keywds
	  '("dassign"  "assign_double"
		"fassign" "assign_float"
		"iassign" "assign_int"
		"vassign" "assign_var"
		"float_value" "fvar_value"
		"int_value" "ivar_value"
		)
	  )

;; EDD Built-In response/return values
(setq edd-built-in-response-codes
	  '("BLTIN_SUCCESS"
		"BLTIN_NO_MEMORY"
		;; Built-in return codes
		"BI_ABORT"	"BI_COMM_ERR" "BI_CONTINUE" "BI_ERROR"
		"BI_NO_DEVICE" "BI_PORT_IN_USE" "BI_RETRY"
		"BI_SUCCESS"
		;; Edd errors
		"DATA_ENTRY_ERROR" "DATA_ENTRY_WARNING"
		"MISC_ERROR" "MISC_WARNING"
		"MODE_ERROR" "PROCESS_ERROR"
		"SUCCESS"
		))

;; Pre-defined attribute values
(setq edd-attribute-value-kwds
	  '(;; pre-defined classes (values for CLASS attribute)
		"ALARM" "ANALOG_INPUT" "ANALOG_OUTPUT" "COMPUTATION"
		"CONTAINED" "CORRECTION" "DEVICE" "DIAGNOSITC"
		"DIGITAL_INPUT" "DIGITAL_OUTPUT" "DISCRETE_INPUT" "DISCRETE_OUTPUT"
		"DYNAMIC" "FREQUENCY_INPUT" "FREQUENCY_OUTPUT"
		"HART" "INPUT" "LOCAL" "LOCAL_DISPLAY" "OPERATE" "OPTIONAL"
		"OUTPUT" "SERVICE" "TEMPORARY" "TUNE" "USER_INTERFACE"
		"ANALOG_CHANNEL" "DISCRETE" "FREQUENCY" "MODE" "RANGE" "HIDDEN"
		;; BIT_ENUMERATED classes
		"ERROR" "IGNORE_IN_HOST" "INFO" "WARNING"
		"DATA" "HARDWARE" "MISC" "PROCESS" "SOFTWARE"
		"CORRECTABLE" "SELF_CORRECTING" "UNCORRECTABLE"
		"DETAIL" "SUMMARY" "COMM_ERROR"
		"MORE"
		;; Output-Classes
		"AUTO" "MANUAL" "GOOD" "BAD"
		;; Style
		"DIALOG" "WINDOW" "PAGE" "GROUP" "TABLE"
		;; Formatting elements
		"COLUMNBREAK" "ROWBREAK" "SEPARATOR"
		;; Access values
		"OFFLINE" "ONLINE"
		;; size attributes values
		"XX_SMALL" "X_SMALL" "SMALL" "MEDIUM" "LARGE" "X_LARGE" "XX_LARGE"
		;; Grid orientation
		"HORIZONTAL" "VERTICAL"
		;; Chart Types
		"GAUGE" "SCOPE" "STRIP" "SWEEP" "VERICAL_BAR" "HORIZONTAL_BAR"
		;; Axis scaling
		"LINEAR" "LOGARITHMIC"
		;; Waveform types
		"XT" "YT" "XY"
		;; GUI Qualifiers
		"NO_LABEL" "NO_UNIT" "READ_ONLY" "DISPLAY_VALUE"
		))


;; EDD defined types
(c-lang-defconst c-primitive-type-kwds
  edd (append '("ARRAY_INDEX" 	"ASCII" "BITSTRING" "BIT_ENUMERATED" "BOOL"
				"DATE" "DATE_AND_TIME" "DD_STRING" "DOUBLE" "DURATION"
				"ENUMERATED" "FLOAT"  "INTEGER" "OCTET"
				"PACKED_ASCII" "PASSWORD" "TIME" "TIME_FORMAT"
				"TIME_SCALE" "TIME_VALUE" "UNSIGNED_INTEGER")
			  (c-lang-const c-primitive-type-kwds)))

;; Constants are also used for defined attribute values
(c-lang-defconst c-constant-kwds
  edd (append
           '( "FALSE" "TRUE"
			  "INPUT" "LOCK_UNLOCK"
			  "READ"    "WRITE"
			  "INLINE"
			 )
           (c-lang-const c-constant-kwds)))

(c-lang-defconst c-modifier-kwds
  edd (append
	   '("ADD"
		 "DELETE" "REDEFINE")
	   edd-ident-kwds
	  (c-lang-const c-modifier-kwds)))

;; Statements followed directly by a block of code
;; In EDD: attribute statements with braces
(c-lang-defconst c-block-stmt-1-kwds
  edd (append
	   '("DEFINITION" "ELEMENTS" "ITEMS" "ITEMS"
		 "KEY_POINTS" "MEMBERS" "OPERATION" "REDEFINITIONS"
         "REPLY" "REQUEST" "RESPONSE_CODES" "TRANSACTION"
		 "TYPE" "VECTORS"
		 "ELSE"
		 ;; Variable actions
		 "POST_EDIT_ACTIONS" "PRE_EDIT_ACTIONS"
		 "POST_READ_ACTIONS" "PRE_READ_ACTIONS"
		 "POST_WRITE_ACTIONS" "PRE_WRITE_ACTIONS"
		 ;; Chart / Waveform actions + brace attributes
		 "INIT_ACTIONS"  "REFRESH_ACTIONS"
		 "EXIT_ACTIONS" "X_VALUES" "Y_VALUES"
		 )
	(c-lang-const c-block-stmt-1-kwds)))

;; Statements followed directly by a Parentesis and block of code
(c-lang-defconst c-block-stmt-2-kwds
  edd (append
	   '("VALIDITY" "VISIBILITY" "IF" "SELECT")
	   (c-lang-const c-block-stmt-2-kwds)))

;; Keywords introducing colon terminated labels in blocks.
(c-lang-defconst c-label-kwds
  edd (append
	   '("case" "CASE")
	   (c-lang-const c-label-kwds)))

;; Declaration of types c-class-decl-kwds or c-type-list-kwds
(c-lang-defconst c-class-decl-kwds
  edd (append
	   '(
		 "ARRAY" "AXIS" "BLOB" "CHART" "COLLECTION" "COMMAND" "ITEM_ARRAY"
		 "EDIT_DISPLAY" "FILE" "GRAPH" "GRID" "IMAGE" "LIST" "MENU"
		 "METHOD" "PLUGIN" "REFRESH" "SOURCE" "TEMPLATE"
		 "UNIT" "VARIABLE" "WAVEFORM" "WRITE_AS_ONE" "VARIABLE_LIST"
	   )
	   (c-lang-const  c-class-decl-kwds)))

;; Free standing statements (break, continue...)
(c-lang-defconst c-simple-stmt-kwds
  edd (append
	   '("IMPORT" "EVERYTHING")
	   (c-lang-const c-simple-stmt-kwds)))

;; This allows the classes after the "LIKE" in the class declartion to be
;; fontified.
(c-lang-defconst c-typeless-decl-kwds
  edd '("LIKE" "OF"))

;; EDDL other-keywords
(c-lang-defconst c-other-kwds
  edd (append
	   '("CHILD" "FIRST" "LAST" "NEXT" "NULL" "PARENT" "PREV" "SELF")
	   (c-lang-const c-other-kwds)))

;;------------------------------------------------------------------------------
;; Field bus specifc keywords
;;------------------------------------------------------------------------------

(setq edd-HART-kwds
	  '(
		"STANDARD" "_STATUS" "_TABLES" "_UNIVERSAL" "_COMMON_PRACTICE"
		"ABORT_ON_ALL_COMM_STATUS" "ABORT_ON_ALL_DEVICE_STATUS" "ABORT_ON_ALL_RESPONSE_CODES"
		"ABORT_ON_COMM_ERROR" "ABORT_ON_COMM_STATUS" "ABORT_ON_DEVICE_STATUS" "ABORT_ON_NO_DEVICE"
		"ABORT_ON_RESPONSE_CODE"
		"IGNORE_ALL_COMM_STATUS" "IGNORE_ALL_DEVICE_STATUS" "IGNORE_ALL_RESPONSE_CODES"
		"IGNORE_COMM_ERROR" "IGNORE_COMM_STATUS" "IGNORE_DEVICE_STATUS" "IGNORE_NO_DEVICE"
		"IGNORE_RESPONSE_CODE"
		"RETRY_ON_ALL_COMM_STATUS" "RETRY_ON_ALL_DEVICE_STATUS" "RETRY_ON_ALL_RESPONSE_CODES"
		"RETRY_ON_COMM_ERROR" "RETRY_ON_COMM_STATUS" "RETRY_ON_DEVICE_STATUS"
		"RETRY_ON_NO_DEVICERETRY_ON_RESPONSE_CODE"
		"XMTR_ABORT_ON_ALL_COMM_STATUS" "XMTR_ABORT_ON_ALL_DATA" "XMTR_ABORT_ON_ALL_DEVICE_STATUS"
		"XMTR_ABORT_ON_ALL_RESPONSE_CODES" "XMTR_ABORT_ON_COMM_ERROR" "XMTR_ABORT_ON_COMM_STATUS"
		"XMTR_ABORT_ON_DATA" "XMTR_ABORT_ON_DEVICE_STATUS" "XMTR_ABORT_ON_NO_DEVICE"
		"XMTR_ABORT_ON_RESPONSE_CODE" "XMTR_IGNORE_ALL_COMM_STATUS" "XMTR_IGNORE_ALL_DATA"
		"XMTR_IGNORE_ALL_DEVICE_STATUS" "XMTR_IGNORE_ALL_RESPONSE_CODES" "XMTR_IGNORE_COMM_ERROR"
		"XMTR_IGNORE_COMM_STATUS" "XMTR_IGNORE_DATA" "XMTR_IGNORE_DEVICE_STATUS" "XMTR_IGNORE_NO_DEVICE"
		"XMTR_IGNORE_RESPONSE_CODE" "XMTR_RETRY_ON_ALL_COMM_STATUS" "XMTR_RETRY_ON_ALL_DATA"
		"XMTR_RETRY_ON_ALL_DEVICE_STATUS" "XMTR_RETRY_ON_ALL_RESPONSE_CODES" "XMTR_RETRY_ON_COMM_ERROR"
		"XMTR_RETRY_ON_COMM_STATUS" "XMTR_RETRY_ON_DATA" "XMTR_RETRY_ON_DEVICE_STATUS"
		"XMTR_RETRY_ON_NO_DEVICE" "XMTR_RETRY_ON_RESPONSE_CODE"
		"display_response_status" "display_xmtr_status"
		"ext_send_command" "ext_send_command_trans"
		;; COLORS from macros.ddl
		"BLACK" "SILVER" "GRAY" "WHITE" "MAROON"
		"RED" "ORANGE" "PURPLE" "FUCHSIA" "GREEN" "LIME"
		"OLIVE" "YELLOW" "NAVY" "BLUE" "TEAL" "AQUA"
		;; Universal Variables & Commands - would be cool if we had a parser
		;; The following variables will be added as regexp later
		;;device_variable_code_[1-4]
		;;device_specific_status_[1-24]
		"comm_status" "config_change_counter" "date" "descriptor" "device_id"
		"final_assembly_number" "hardware_revision" "longTag"
		"max_num_device_variables" "message" "polling_address"
		"read_additional_device_status"	"read_device_variables_and_status"
		"read_dynamic_variable_classification" "read_dynamic_variables_and_pv_current"
		"read_final_assembly_number"
		"read_long_tag" "read_loop_configuration" "read_message"
		"read_pv" "read_pv_current_and_percent_range" "read_pv_output_info"
		"read_pv_sensor_info" "read_tag_descriptor_date" "read_unique_identifier"
		"read_unique_identifier_with_long_tag" "read_unique_identifier_with_tag"
		"request_preambles"
		"reset_configuration_change_flag" "response_code" "response_preambles"
		"software_revision" "tag" "time_stamp" "transmitter_revision"
		"universal_revision" "write_final_assembly_number" "write_long_tag"
		"write_message" "write_polling_address"
		"write_tag_descriptor_date"
		;;
		"background_period"  "device_status"
		"comm_status"  "device_icon"  "response_code"
		"loop_warning_variables" "upload_variables" "extended_device_status"
		"manufacturer_id" "private_label_distributo" "device_type"
		"write_protect" "physical_signaling_code" "loop_current_mode" "device_flags"
		"extended_fld_device_status"  "device_profile"
		"standardized_status_0" "standardized_status_1" "standardized_status_2" "standardized_status_3"
		"analog_channel_saturated1" "analog_channel_fixed1"
		;; From menu.ddl
		"root_menu" "device_setup" "process_variables"  "diag_service"
		"test_device" "calibration" "status_display" "basic_setup"
		"detailed_setup" "device_info" "construction_materials"
		"device_revisions" "measuring_elements" "menu_pres_sensor"
		"menu_sensor_info" "signal_conditioning" "output_conditioning"
		"analog_output" "hart_output" "frequency_output" "discrete_output"
		"review" "hot_key"
		;; PV1.ddl
		"loopCurrent" "loop_alarm_code" "loop_flags" "analog_io" "percentRange"
		"transfer_function" "upperRange_value" "lowerRange_value" "scaling"
		"primary_variable" "secondary_variable" "tertiary_variable"
		"quaternary_variable" "dynamic_variables" "scaling_units_relation"
		"scaling_wao"
		 ))

;; Attributes in Blocks (VARIABLE, Axis...)
(setq edd-attribute-kwds
	  '(
		;; Variable / Common
		"LABEL" "HELP"	"CLASS" "HANDLING"
		"STYLE"	"CONSTANT_UNIT" "ACCESS"  "PURPOSE"
		"READ_TIMEOUT"	 "WRITE_TIMEOUT" ;; "TYPE" is a block-statement
		;; Image
		"PATH" 	"LINK"
		;; Array
		"NUMBER_OF_ELEMENTS"
		;; LIST
		"COUNT"	"CAPACITY"
		;; Type Subattributes
		"DISPLAY_FORMAT" "EDIT_FORMAT"	"DEFAULT_VALUE"	"INITIAL_VALUE"
		"MIN_VALUE"	"MAX_VALUE"	"SCALING_FACTOR"
		;; style / formating elements
		"ORIENTATION"
		;; Chart Attributes
		"HEIGHT" "HIGH_HIGH_LIMIT" "HIGH_LIMIT" "LOW_LIMIT"
		"LOW_LOW_LIMIT"	"WIDTH"	"LENGTH" "CYCLE_TIME"
		;; Source Attributes
		"LINE_COLOR" "LINE_TYPE" "EMPHASIS"	"X_AXIS" "Y_AXIS"
		;; AXIS / Waveform
		"SCALING" "X_INITIAL" "Y_INITIAL"
		"X_INCREMENT" "NUMBER_OF_POINTS"
		;; Attributes for COMMANDS
		"NUMBER" "BLOCK" "OPERATION" "SLOT"
		"SUBSLOT" "INDEX"
		)
	  )


;;------------------------------------------------------------------------------
;; Menu entries & Commands
;;------------------------------------------------------------------------------
(defgroup EDD nil "EDD mode customizations"
  :group 'languages)


;;------------------------------------------------------------------------------
;; Custom faces for EDD Specifics
;;------------------------------------------------------------------------------

;; Face for attributes
(defface edd-attribute-face
  '((t (:inherit font-lock-builtin-face :foreground "gold"  :weight bold )))
  "Face for highlighting Attributes of edd elements."
  :group 'EDD)

(defface edd-attribute-value-face
  '((t (:inherit font-lock-builtin-face :foreground "LimeGreen" :weight semi-bold )))
  "Face for highlighting attribute value keywords")

(defface edd-special-face
  '((t (:inherit font-lock-builtin-face :foreground "IndianRed" :weight bold )))
  "Face for highlighting mandatory info, return codes, and pre-defined menus.")

(defface edd-HART-specific-face
  '((t (:inherit font-lock-constant-face :foreground "DeepSkyBlue" )))
  "Face for highlighting 'HART Specific keywords'.")


(defface edd-deprecated-face
  '((t (:inherit font-lock-function-name-face :underline (:color "yellow" :style wave) )))
  "Face for highlighting deprecated symbols and functions")


;; Important to make the new face a  variable
;; (http://emacs.stackexchange.com/questions/3584/how-do-i-specify-a-custom-face-with-font-lock-defaults)
(defvar edd-special-face  'edd-special-face)
(defvar edd-attribute-face  'edd-attribute-face)
(defvar edd-attribute-value-face  'edd-attribute-value-face)
(defvar edd-HART-specific-face 'edd-HART-specific-face)
(defvar edd-deprecated-face  'edd-deprecated-face)


;------------------------------------------------------------------------------
;; Bind keyword lists to font-lock faces
;;------------------------------------------------------------------------------

(font-lock-add-keywords
 'edd-mode
 (list
  ;; HART Keywords
  (list (concat "\\<\\("
  				(regexp-opt (append edd-HART-kwds edd-built-in-response-codes) t)
  				"\\)\\>")
		1 'edd-HART-specific-face)
  ;; manual regexp for device_specific_status
  (list (concat "\\<\\("
  				"device_\\(specific_status\\|variable_code\\)_[0-9]+"
  				"\\)\\>")
  		1 'edd-HART-specific-face)

  ;; Attributes
  (list (concat "\\<\\("
				(regexp-opt edd-attribute-kwds t)
				"\\)\\>")
		1 'edd-attribute-face)
  ;; Attribute values
  (list (concat "\\<\\("
				(regexp-opt edd-attribute-value-kwds t)
				"\\)\\>")
		1 'edd-attribute-value-face)
  ;; Special elements
  (list (concat "\\<\\("
				(regexp-opt edd-ident-kwds t)
				"\\)\\>")
		1 'edd-special-face)
  ;; Built-in functions
  (list (concat "\\<\\("
				(regexp-opt (append edd-built-in-functions edd-comm-cmds) t)
				"\\)\\>")
		1 'font-lock-function-name-face)
  ;; Deprecated functions in warning face
  (list (concat "\\<\\("
				(regexp-opt edd-deprecated-keywds t)
				"\\)\\>")
		1 'edd-deprecated-face)
  ))

(font-lock-add-keywords
 'edd-mode '(
			 ("\\(Franz\\)" . 'font-lock-function-name-face)
			 )
 )


;;------------------------------------------------------------------------------

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
	("ELSE" "ELSE" c-electric-continued-statement 0)
   	("while" "while" c-electric-continued-statement 0)))

(defvar edd-mode-map
  (let ((map (c-make-inherited-keymap)))
    ;; Add bindings which are only useful for Edd
    map)
  "Keymap used in edd-mode buffers.")

(easy-menu-define edd-menu edd-mode-map "Edd Mode Commands"
  (cons "Edd" (c-lang-const c-mode-menu edd)))

;; Initialize Snippets
(defun init-edd-snippets ()
  ;; only provide snippets if yasnippet is installed
  (when (require 'yasnippet nil :noerror)
	;; Enter Yas
	(yas-minor-mode t)
	;; Where edd-mode.el is found (or if the buffer is evaluated)
	(defvar edd-mode-root
	  (file-name-directory (or load-file-name (buffer-file-name))))

	(let ((snip-dir (expand-file-name "snippets" edd-mode-root)))
	  (when (boundp 'yas-snippet-dirs)
		(add-to-list 'yas-snippet-dirs snip-dir t)
		(message snip-dir)
	  (yas-load-directory snip-dir)))
	)
  )


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
  (init-edd-snippets)
  (c-update-modeline)
  ;;(add-custom-keyw)
 )



(provide 'edd-mode)
;;; edd-mode.el ends here
