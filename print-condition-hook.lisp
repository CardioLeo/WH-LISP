; Author: Vincent Dardel (so far as I know)
; Date Created:
; Date Modified:
; Description:
; Usage:

; -------------------

(defun print-condition-hook (condition hook)
  "Print this error message (condition) and abort the current operation"
  (declare (ignore hook))
  (princ condition)
  (clear-input)
  (abort))

; enter this separately

*debugger-hook*

(setf *debugger-hook* #'print-condition-hook)

