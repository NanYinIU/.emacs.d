(defcustom org-directory (expand-file-name "~/Library/Mobile Documents/iCloud~com~logseq~logseq/Documents")
  "Set org directory."
  :group 'emacs
  :type 'string)

(defcustom python-virtualenv-path "~/.virtualenvs/nvim_py3_venv/bin/python"
  "Path to Python virtual environment interpreter."
  :group 'emacs
  :type 'string)

(defcustom python-virtualenv-dir "~/.virtualenvs/nvim_py3_venv"
  "Path to Python virtual environment directory."
  :group 'emacs
  :type 'string)

(defcustom rust-playground-dir "~/Develop/rust/playground"
  "Path to Rust playground directory."
  :group 'emacs
  :type 'string)

(defcustom completion-style 'default
  "Completion style to use."
  :group 'emacs
  :type '(choice (const default)
                 (const childframe)
                 (const minibuffer)))



(defcustom prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'emacs
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    (":PROPERTIES:"   . ?)
    (":ID:"           . ?🪪)
    (":END:"          . ?🔚)

    ("#+ARCHIVE:"     . ?📦)
    ("#+AUTHOR:"      . ?👤)
    ("#+CREATOR:"     . ?💁)
    ("#+DATE:"        . ?📆)
    ("#+DESCRIPTION:" . ?⸙)

    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+RESULTS:"     . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'emacs
  :type '(alist :key-type string :value-type (choice character sexp)))


(provide 'init-custom)
