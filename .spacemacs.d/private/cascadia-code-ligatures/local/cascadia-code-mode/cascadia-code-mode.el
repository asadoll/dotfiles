(defun cascadia-code-mode--make-alist (list)
    "Generate prettify-symbols alist from LIST."
    (let ((idx -1))
      (mapcar
      (lambda (s)
        (setq idx (1+ idx))
        (let* ((code (+ #Xe100 idx))
            (width (string-width s))
            (prefix ())
            (suffix '(?\s (Br . Br)))
            (n 1))
      (while (< n width)
        (setq prefix (append prefix '(?\s (Br . Bl))))
        (setq n (1+ n)))
      (cons s (append prefix suffix (list (decode-char 'ucs code))))))
      list)))

  (defconst cascadia-code-mode--ligatures
    '("Fl" "Tl" "fl" "www" "--" "---" "-->" "-|" "->" "->>" "-<" "-<<" "-~" "{|"
      "[|" "]#" ".-" ".." "..." "..<" ".?" ".=" "::" ":::" "::=" ":=" ":>" ":<"
      ";;" "!!" "!!." "!=" "!==" "?." "?:" "??" "?=" "**" "***" "*>" "*/" "#("
      "#{" "#[" "#:" "#!" "#?" "##" "###" "####" "#=" "#_" "#_(" "/*" "/=" "/=="
      "/>" "//" "///" "/\\" "\\/" "_|_" "__" "&&" "|-" "->" "|}" "|]" "||" "||-"
      "|||>" "||=" "||>" "|=" "|=>" "|>" "$>" "++" "+++" "+>" "=:=" "=!=" "=="
      "===" "==>" "=>" "=>>" "=<<" "=/=" ">-" ">->" ">:" ">=" ">=>" ">>" ">>-"
      ">>=" ">>>" "<-" "<--" "<-|" "<->" "<-<" "<:" "<!--" "<*" "<*>" "<|" "<||"
      "<|||" "<|>" "<$" "<$>" "<+" "<+>" "<=" "<=|" "<==" "<==>" "<=>" "<=<"
      "<>" "<<" "<<-" "<<=" "<<<" "<~" "<~>" "<~~" "</" "</>" "~-" "~@" "~="
      "~>" "~~" "~~>" "^=" "%%"))
  (defvar cascadia-code-mode--old-prettify-alist)

  (defun cascadia-code-mode--enable ()
    "Enable Fira Code ligatures in current buffer."
    (setq-local cascadia-code-mode--old-prettify-alist prettify-symbols-alist)
    (setq-local prettify-symbols-alist (append (cascadia-code-mode--make-alist cascadia-code-mode--ligatures) cascadia-code-mode--old-prettify-alist))
    (prettify-symbols-mode t))

  (defun cascadia-code-mode--disable ()
    "Disable Fira Code ligatures in current buffer."
    (setq-local prettify-symbols-alist cascadia-code-mode--old-prettify-alist)
    (prettify-symbols-mode -1))
(defun cascadia-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
    (lambda (s)
      (setq idx (1+ idx))
      (let* ((code (+ #Xe100 idx))
          (width (string-width s))
          (prefix ())
          (suffix '(?\s (Br . Br)))
          (n 1))
    (while (< n width)
      (setq prefix (append prefix '(?\s (Br . Bl))))
      (setq n (1+ n)))
    (cons s (append prefix suffix (list (decode-char 'ucs code))))))
    list)))

(defconst cascadia-code-mode--ligatures
  '("Fl" "Tl" "fl" "www" "--" "---" "-->" "-|" "->" "->>" "-<" "-<<" "-~" "{|"
    "[|" "]#" ".-" ".." "..." "..<" ".?" ".=" "::" ":::" "::=" ":=" ":>" ":<"
    ";;" "!!" "!!." "!=" "!==" "?." "?:" "??" "?=" "**" "***" "*>" "*/" "#("
    "#{" "#[" "#:" "#!" "#?" "##" "###" "####" "#=" "#_" "#_(" "/*" "/=" "/=="
    "/>" "//" "///" "/\\" "\\/" "_|_" "__" "&&" "|-" "->" "|}" "|]" "||" "||-"
    "|||>" "||=" "||>" "|=" "|=>" "|>" "$>" "++" "+++" "+>" "=:=" "=!=" "=="
    "===" "==>" "=>" "=>>" "=<<" "=/=" ">-" ">->" ">:" ">=" ">=>" ">>" ">>-"
    ">>=" ">>>" "<-" "<--" "<-|" "<->" "<-<" "<:" "<!--" "<*" "<*>" "<|" "<||"
    "<|||" "<|>" "<$" "<$>" "<+" "<+>" "<=" "<=|" "<==" "<==>" "<=>" "<=<"
    "<>" "<<" "<<-" "<<=" "<<<" "<~" "<~>" "<~~" "</" "</>" "~-" "~@" "~="
    "~>" "~~" "~~>" "^=" "%%"))
(defvar cascadia-code-mode--old-prettify-alist)

(defun cascadia-code-mode--enable ()
  "Enable Cascadia Code ligatures in current buffer."
  (setq-local cascadia-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (cascadia-code-mode--make-alist cascadia-code-mode--ligatures) cascadia-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun cascadia-code-mode--disable ()
  "Disable Cascadia Code ligatures in current buffer."
  (setq-local prettify-symbols-alist cascadia-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode cascadia-code-mode
  "Cascadia Code ligatures minor mode"
  :lighter " Cascadia Code PL"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if cascadia-code-mode
      (cascadia-code-mode--enable)
    (cascadia-code-mode--disable)))

(provide 'cascadia-code-mode)
  (define-minor-mode cascadia-code-mode
    "Cascadia Code ligatures minor mode"
    :lighter " Cascadia Code PL"
    (setq-local prettify-symbols-unprettify-at-point 'right-edge)
    (if cascadia-code-mode
        (cascadia-code-mode--enable)
      (cascadia-code-mode--disable)))

(provide 'cascadia-code-mode)
