(ns chester.core-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [cheshire.core :as json]
            [ring.middleware.defaults :refer [wrap-defaults
                                              site-defaults
                                              api-defaults]]
            [chester.core :refer :all]))

(def controller
  {"Foo"
   {:context "/foos"
    :relation "foo"
    :individual-href ["localhost/foos/%s" :id]
    :routes [{:path "/"
              :methods #{:get :post}
              :collection? true}
             {:path "/:username"
              :methods #{:delete}}]
    :insert-keys #{:id|uuid* :username}
    :filter-keys #{:id|uuid :username}
    :delete-keys #{:username}
    }})

(def gen json/generate-string)

(defn req
  [& argv]
  (let [q (apply mock/request argv)
        body (when (:body q) (slurp (:body q)))]
    (assoc q :body (json/parse-string body true))))

(def dbfns
  {:collect (fn [& _] [[] {:count 0}])
   :one (fn [& _] {:foo "bar"})
   :insert! (fn [& _] {:username "test" :id "abc123"})
   :update! (fn [& _])
   :delete! (fn [& _])})

(defn get-controller [controller]
  (-> (context-compile controller dbfns)
      (wrap-defaults api-defaults)))

(deftest a-test
  (testing "coerce-select-params"
    (let [result (coerce-select-params
                   #{:id|uuid* :username}
                   {:username "foo"})]
      (-> result :id nil? not is))
    )
  (let [result (get-controller controller)]
    (testing "post route keys"
      (-> (req :post "/foos/" (gen {:username "lala"}))
        result :body (select-keys #{:id :href :related-resources})
        count (= 3) is))
    (testing "delete"
      (-> (req :delete "/foos/foo" "{}")
        result :status (= 200) is))
    (testing "post list"
      (-> (req :post "/foos/"
            (gen [{:username "lala"} {:username "lalalala"}]))
        result :body count (= 2) is))
    ))
