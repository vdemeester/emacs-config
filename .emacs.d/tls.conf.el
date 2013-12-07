;; TLS configuration

; Don't use an insecure connection by default!
(setq tls-program '("gnutls-cli -p %p %h"))
