;; (configure-mode
;;  ruby-mode
;;  (message "I Love Maruko!")
;;  (message "I Love Maruko!"))

;; ;; (define-minor-mode)

;; (defmacro configure-mode (mode &rest body)
;;   "This macro is hard to describe."

;;   (let* ((mode-name (symbol-name mode))
;;          (hook-symbol (intern (concat mode-name "-hook"))))

;;     ;; Keyword arguments
;;     (while (keywordp (setq keyw (car body)))
;;       (setq body (cdr body))
;;       (pcase keyw
;;         (`:hook-symbol (setq hook-symbol (pop body)))))

;;     ;; Do
;;     (add-hook hook-symbol
;;               (lambda ()
;;                 (remove-hook hook-symbol (first (symbol-value hook-symbol)))
;;                 body
;;                 ))
;;     ))


