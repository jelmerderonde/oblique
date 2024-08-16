(ns user
  (:require
   [vlaaad.reveal :as r]
   [oblique.db]
   [clj-reload.core :as reload])
  (:import [oblique.db Entity]))

(reload/init
 {:dirs ["dev"]
  :no-reload '#{user
                io.github.humbleui.protocols
                io.github.humbleui.signal}
  :reload-hook 'after-ns-reload})

(r/defstream Entity [node]
  (r/horizontal
   (r/raw-string "oblique.db.Entity {" {:fill :object})
   (r/entries node)
   (r/raw-string "}" {:fill :object})))
