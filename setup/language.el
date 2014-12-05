;; Chinese support
(require-old-package 'eim)

(autoload 'eim-use-package "eim" "Another emacs input method")

(setq eim-use-tooltip nil)              ; don't use tooltip

(setq eim-punc-translate-p nil)         ; use English punctuation

(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "pinyin" "EIM Chinese Pinyin Input Method" "py.txt"
 'my-eim-py-activate-function)

(setq default-input-method "eim-py")

;; follow os x way
(defun my-eim-py-activate-function ()
  (add-hook 'eim-active-hook
	    (lambda ()
	      (let ((map (eim-mode-map)))
		(define-key eim-mode-map "[" 'eim-previous-page)
		(define-key eim-mode-map "]" 'eim-next-page)))))

(provide 'language)
