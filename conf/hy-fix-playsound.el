;; Some emacs doesn't have the ability to playsound
;; My emacs is one of them.

(require 'cl-lib)
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal))
             (string-match "darwin" (symbol-name system-type)))
  (defun play-sound-internal (sound)
    "Internal function for `play-sound' (which see)."
    (or (eq (car-safe sound) 'sound)
        (signal 'wrong-type-argument (list sound)))

    (destructuring-bind (&key file data volume device)
        (cdr sound)

      (and (or data device)
           (error "DATA and DEVICE arg not supported"))

      (apply #'start-process "OS X sound file playing." nil
             "afplay" (append
                       (list (expand-file-name file data-directory)))))))

(provide 'hy-fix-playsound)
