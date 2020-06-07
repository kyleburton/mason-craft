;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((clojure-mode
  (krb-clj-cider-connect-args :host "localhost" :port "4123" :cljs-repl-type shadow)
  (cider-shadow-default-options . "app")
  (krb-clj-cider-connect-fn . cider-connect-cljs)))
