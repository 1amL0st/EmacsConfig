; This is the only sample text that doens't make any sence to me
; And anybody else

(defun sum-of-two (first second)
"Sum of two value\n
This is another string"
	(+ first second))

; Function definition in eLisp
(defun print-value (value)
		"You should leave the documentation everywhere!"
    (message "Text %d" value))

(print-value 40)

; Declare a number of variables
(let ((first_var "This is")
	 (second_var 20)
	 (third_var "third var")))

; Using if
(if (equal 1 2) 
		(message "1 == 1")
	(message "1 != 2")
	)

(defun compare-two-numbers (first second)
	(if (equal first second)
			(message "%d == %d" first second)
		(if (> first second)
				(message "%d > %d" first second)
			(message "%d < %d" first second))))

; Using if with multiple statements if true
(if (equal "This" "Ths")
		(progn (message "This is the first one")
					 (message "This is the second one"))
	(message "Print this message if false")
	)

(defun move-cursor-forward ()
	(forward-char)
	(forward-char)
	(forward-char)
	(forward-char))

(save-excursion
	(move-cursor-forward)
)

(buffer-file-name)


