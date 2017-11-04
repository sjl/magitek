(ql:quickload '(:deploy :magitek))

;; (deploy:define-resource-directory assets "assets/")

(sb-ext:gc :full t)
(asdf:operate :build-op :magitek)
