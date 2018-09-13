(ns chester.core-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [cheshire.core :as json]
            [ring.middleware.defaults :refer [wrap-defaults
                                              site-defaults
                                              api-defaults]]
            [chester.core :refer :all]
            [chester.sql :as sql]))

(def controller
  {"Foo"
   {:context "/foos"
    :relation :foo
    :individual-href ["localhost/foos/%s" :id]
    :related-resources {:lalala ["lalala/?bar=%s" :bar]}
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
   :one (fn [& _] {:foo "bar" :id 135 :username "bar"})
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

(deftest test-related-resources
  (testing "related-resources"
    (let [result (related-resources {:id 1 :username "x" :bar 1}
                                    (get controller "Foo")
                                    (req :get "/foos/"))]
      (-> result :related-resources :lalala (= "http://localhostlalala/?bar=1") is))
    (let [result (related-resources {:blah nil :id 1}
                                    (get controller "Foo")
                                    (req :get "/foos/"))]
      (-> result :related-resources empty? is))
    ))

(deftest test-sql
  (testing "sql string with null"
    (let [result (sql/filters-to-where {:foo nil :bar 1})]
      (-> result first (= "foo is null and bar=?") is)
      (-> result second (= '(1)) is))))

