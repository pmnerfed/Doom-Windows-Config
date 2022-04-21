;; -*- no-byte-compile: t; -*-
;;; org/packages.el

(package! org-pretty-tags)
(package! org-super-agenda)
(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "148aa124901ae598f69320e3dcada6325cdc2cf0")
(package! org-fragtog)
(package! org-pomodoro)
(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "87772a9469d91770f87bfa788580fca69b9e697a")
;; (package! org-ol-tree)
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")
(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "8cbbade1e3237200c2140741f39ff60176e703e7")
(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view") :pin "13314338d70d2c19511efccc491bed3ca0758170")

(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")) :pin "c93f6b61a8d3d2edcf07eda6220278c418d1054b")
(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'

(package! polymode)
