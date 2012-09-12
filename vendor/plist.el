;; Allow editing of binary .plist files.
;; Stolen straight from the EmacsWiki <http://www.emacswiki.org/emacs/MacOSXPlist>
;; Credit: Kevin Lynch

(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;; It is necessary to perform an update!
(jka-compr-update)
