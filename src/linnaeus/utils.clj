(ns linnaeus.utils)

(defn db->amp
  [db]
  (Math/pow 10.0 (/ db 10.0)))
