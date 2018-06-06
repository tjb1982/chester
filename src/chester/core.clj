(ns chester.core
  (:require [clojure.java.jdbc :as j]
            [clj-yaml.core :as yaml]
            [clojure.data.codec.base64 :as b64]
            [ring.util.codec :refer [url-encode]]
            [cheshire.core :as json]
            [compojure.core :refer :all]
            [compojure.coercions :refer :all]
  ))

(defn collect*
  [relation handler]
  (fn [& [filters
          {:keys [order
                  per-page
                  page
                  direction]
           :or {page "1"
                per-page "50"}
           :as params}]]
    (let [page (as-int page)
          page (if (pos? page)
                 page 1)
          per-page (as-int per-page)
          offset (* (dec page) per-page)
          results
          (handler relation
                   :filters filters
                   :order order
                   :direction direction
                   :limit per-page
                   :offset offset)
          total (-> results second :count)
          next* (when (and total
                           (> total (* page per-page)))
                  (inc page))
          previous (when (> page 1)
                     (dec page))
          last* (and total (int (Math/ceil (/ total per-page))))]
      {:next next*
       :previous previous
       :last last*
       :count total
       :per-page per-page
       :current page
       :order order
       :direction direction
       :results (first results)})))

(defn one*
  [relation handler]
  (fn [& [filters]]
    (handler relation :filters filters)))

(defn insert!*
  [relation handler]
  (fn [data]
    (handler relation data)))

(defn update!*
  [relation handler]
  (fn [data filters]
    (handler relation data filters)))

(defn delete!*
  [relation handler]
  (fn [filters]
    (handler relation filters)))

(defn data-access-fns
  [relation {:keys [collect one insert! update! delete!]}]
  {:collect (collect* relation collect)
   :one (one* relation one)
   :insert! (insert!* relation insert!)
   :update! (update!* relation update!)
   :delete! (delete!* relation delete!)})

(defn coerce-select-params
  [filter-keys params]
  (let [filters (for [f filter-keys]
                  (let [[k t] (map keyword
                                (-> f
                                  name
                                  (clojure.string/split #"\|" 2)))]
                    [k t]))
        coerce (fn [t v]
                 (condp = t
                   :int (as-int v)
                   :uuid (as-uuid v)
                   :uuid* (java.util.UUID/randomUUID)
                   :bytes (b64/decode (.getBytes v))
                   :bool (= "true" v)
                   v))
        params (for [[k v] params
                     [f t] filters]
                 (let [k (if (and t (.endsWith (name t) "*")) f k)]
                   (when (= k f)
                     [k (if (coll? v)
                          (into (empty v) (map (partial coerce t) v))
                          (coerce t v))])))]
    (into {} (remove nil? params))))

(defn resource-uri
  [scheme host & [endpoint]]
  (str (name scheme)
       "://"
       host
       endpoint))

(defn related-resources
  [resource controller request]
  (let [related (:related-resources controller)
        ind-href (:individual-href controller)
        format-fun
        (fn [fks]
          (apply format
                 (flatten
                   [(first fks)
                    (map #(url-encode (or (% resource) "")) (drop 1 fks))])))]
    (assoc resource
      :href (when ind-href
              (resource-uri (-> request :scheme name)
                            (-> request :headers (get "host"))
                            (format-fun ind-href)))
      :related-resources
      (into {}
        (for [[relation fks] related]
          [relation
           (resource-uri
             (-> request :scheme name)
             (-> request :headers (get "host"))
             (format-fun fks))])))))
 
(defn do-post
  [request controller insert! body]
  (let [data (coerce-select-params
               (:insert-keys controller)
               body)
        ikeys (into #{}
                (->> (:insert-keys controller)
                  (remove #(-> % name (.endsWith "*")))
                  (map
                    #(-> % name
                      (clojure.string/split #"\|")
                      first keyword))))]
    (when (not= (set (keys body)) (set ikeys))
      (throw (chester.APIException.
               (format
                 (str "Posts to %s must contain values "
                      "for each of (and only) %s. %s")
                 (:uri request)
                 ikeys
                 body))))
    (-> data
      insert!
      (related-resources
        controller
        request))))

(defn method-handlers
  [request
   controller
   route*
   method
   params
   body
   {:keys [collect one insert! update! delete!]}]
  (condp = method
    :get
    (GET (:path route*) {params :params}
      (let [filters (coerce-select-params
                      (:filter-keys controller)
                      params)
            resp (if (:collection? route*)
                   (collect filters params)
                   (one filters))]
        (if (contains? resp :results)
          {:status 200 :body (assoc resp
                               :results
                               (map #(related-resources
                                      %
                                      controller
                                      request)
                                    (:results resp)))}
          (when-not (nil? resp)
            {:status 200 :body (related-resources
                                 resp
                                 controller
                                 request)}))))
    :post
    (POST (:path route*) {params :params}
      (let [fun (partial do-post request controller insert!)]
        {:status 201
         :body (if (and (not (map? body))
                        (coll? body)
                        (sequential? body))
                 (map fun body)
                 (fun body))}))
    :put
    (PUT (:path route*) {params :params}
      (let [ukeys (:update-keys controller)
            filters (coerce-select-params (:filter-keys controller) params)
            data (coerce-select-params (:insert-keys controller) body)]
        (when-not (every? #(contains? (set ukeys) %)
                          (set (keys body)))
          (throw (chester.APIException.
                   (format
                     "Puts to %s can only contain values from %s."
                     (:uri request)
                     ukeys))))
        {:status 200
         :body (update! data filters)}
      ))
    :delete
    (DELETE (:path route*) {params :params}
      (let [dkeys (:delete-keys controller)
            filters (coerce-select-params (:filter-keys controller) params)]
        (when-not (every? #(contains? (set dkeys) %)
                          (set (keys params)))
          (throw (chester.APIException.
                   (format
                     "Deletes to %s must contain values from %s"
                     (:uri request)
                     dkeys))))
        {:status 200 :body (delete! filters)}))))

(defn context-compile
  [ctrl dbfns]
  (apply routes
    (for [[label controller] ctrl]
      (context (:context controller) request
        (let [params (:params request)
              body (:body request)]
          (->>
            (for [route* (:routes controller)
                  method (:methods route*)]
              (method-handlers
                request
                controller
                route*
                method
                params
                body
                (data-access-fns
                  (:relation controller)
                  dbfns)))
            flatten
            (apply routes)))))))

(defn -main []
  (println "ok"))
