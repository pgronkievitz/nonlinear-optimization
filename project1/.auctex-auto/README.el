(TeX-add-style-hook
 "README"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "presentation")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("babel" "polish")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "inputenc"
    "fontenc"
    "graphicx"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "amssymb"
    "capt-of"
    "hyperref"
    "babel"
    "biblatex")
   (LaTeX-add-labels
    "sec:orga8ecb61"
    "eq:1"
    "sec:orgd09a6d8"
    "eq:2"
    "sec:org4ef4f76"))
 :latex)

