(setq gnus-select-method 
      '(nnmaildir "Personnal" 
                  (directory "~/desktop/mails/main/")
                  (directory-files nnheader-directory-files-safe) 
                  (get-new-mail nil)))

