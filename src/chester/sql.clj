(ns chester.sql
  (:require [clojure.java.jdbc :as j])
  )

(defn filters-to-where
  [params]
  ;; where this=? and (this=? or this=?) and ...
  ;; bindings
  (let [pairs (for [[k v] params]
                (cond
                  (nil? v)
                  [(format "%s is null" (name k))]

                  (coll? v)
                  [(format
                     "(%s)"
                     (clojure.string/join
                       " or "
                       (for [vv v]
                         (format "%s=?" (name k)))))
                   v]

                  :else
                  [(format "%s=?" (name k)) v]))]
    [(when-not (empty? pairs)
       (clojure.string/join " and " (map first pairs)))
     (->> pairs (map second) flatten (remove nil?))]))

(defn query-string
  [relation count? & [where order direction limit offset]]
  (format
    "select %s from %s %s %s %s %s"
    (if count? "count(*)" "*")
    (name relation)
    (if where
      (str "where " where)
      "")
    (if order
      (format "order by %s %s"
              (name order)
              (condp = (keyword direction)
                :asc "asc"
                :desc "desc"
                ""))
      "")
    (if limit
      (format "limit %s" limit)
      "")
    (if offset
      (format "offset %s" offset)
      "")))

(defn samling
  [db]
  (fn [relation & {:keys [count?
                          filters
                          order
                          direction
                          limit
                          offset]
                   :or {count? true direction "asc"}}]
    (let [[where bindings] (filters-to-where filters)
          qstr (query-string relation
                 false
                 where
                 order
                 direction
                 limit
                 offset)
          _ (println qstr bindings)
          results (j/with-db-transaction [conn db]
                    [(j/query conn
                       (flatten
                         [qstr
                          bindings]))
                     (when count?
                       (-> conn
                         (j/query
                           (flatten
                             [(query-string
                                relation count? where)
                              bindings]))
                         first))])]
      results)))

(defn ett
  [db]
  (fn [relation & {:keys [filters order offset]}]
    (ffirst ((samling db) relation :count? false
                                   :filters filters
                                   :order order
                                   :limit 1
                                   :offset offset))))

(defn foga-in!
  [db]
  (fn [relation data]
    (first
      (j/with-db-transaction [conn db]
        (j/insert! conn (keyword relation)
          data)))))

(defn ändra!
  [db]
  (fn [relation data & [filters]]
    (let [[where bindings] (filters-to-where filters)]
      (j/with-db-transaction [conn db]
        (j/update! conn (keyword relation)
          data
          (flatten [where bindings]))))))

(defn radera!
  [db]
  (fn [relation & [filters]]
    (let [[where bindings] (filters-to-where filters)]
      (j/with-db-transaction [conn db]
        (j/delete! conn (keyword relation)
          (flatten [where bindings]))))))

(defn dbfns
  [db]
  {:collect (samling db)
   :one (ett db)
   :insert! (foga-in! db)
   :update! (ändra! db)
   :delete! (radera! db)})
