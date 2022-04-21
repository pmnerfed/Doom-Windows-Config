;;; ../../../mnt/c/Users/PUNEET MADAAN/.doom.d/code/python.el -*- lexical-binding: t; -*-


(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(after! python-mode
  (appendq! +ligatures-extra-symbols
            '(:assign "⟵"
              :multiply "×"))
  (set-ligatures! 'python-mode
    ;; Functional
    :def "def"
    ;; Types
    :null "None"
    :true "True"
    :false "False"
    :int "int"
    :floar "float"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "and" :or "or"
    :for "for"
    :in "%in%"
    :return "return"
    ;; Other
    :assign "<-"
    :multiply "%*%"))
