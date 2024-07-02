(ns main
  (:require [clojure.data.json :as json]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.content-negotiation :as content-negotiation]
            [io.pedestal.test :as test]))

;; Response Helpers
(defn response
  [status body & {:as headers}]
  {:status status :body body :headers headers})

(def ok (partial response 200))
(def created (partial response 201))
(def accepted (partial response 202))
(def not-found (partial response 404))

(defn greet [request]
  (let [name-param (get-in request [:query-params :name])]
    (ok (str "Hello, " name-param "\n"))))

(defn echos [request]
  (ok request))

(defn get-in-path-params [context key]
  (get-in context [:request :path-params key]))

;; route handlers / interceptors
;; Todo Lists
(defn make-list [nm]
  {:name nm
   :items {}})

(defn make-list-item [nm]
  {:name nm
   :done? false})

(def list-create
  {:name :list-create
   :enter
   (fn [context]
     (let [nm (get-in context [:request :query-params :name] "Unamed List")
           new-list (make-list nm)
           db-id (str (gensym "1"))
           url (route/url-for :list-view :params {:list-id db-id})]
       (assoc context
              :response (created new-list "Location" url)
              :tx-data [assoc db-id new-list])))})

(defn find-list-by-id [dbval db-id]
  (get dbval db-id))

(def list-view
  {:name :list-view
   :enter
   (fn [context]
     (let [db-id (get-in-path-params context :list-id)
           dbval (get-in context [:request :database])
           the-list (when db-id
                      (find-list-by-id dbval db-id))]
       (cond-> context
         the-list (assoc :result the-list))))})

(defn find-list-items-by-ids [dbval list-id item-id]
  (get-in dbval [list-id :items item-id] nil))

(def list-item-view
  {:name :list-item-view
   :leave
   (fn [context]
     (let [list-id (get-in-path-params context :list-id)
           item-id (and list-id
                        (get-in-path-params context :item-id))
           dbval (get-in context [:request :database])
           item (and item-id
                     (find-list-items-by-ids dbval list-id item-id))]
       (cond-> context
         item (assoc :result item))))})

(defn list-item-add [dbval list-id item-id new-item]
  (if (contains? dbval list-id)
    (assoc-in dbval [list-id :items item-id] new-item)
    dbval))

(def list-item-create
  {:name :list-item-create
   :enter
   (fn [context]
     (if-let [list-id (get-in-path-params context :list-id)]
       (let [nm (get-in context [:request :query-params :name] "Unamed Item")
             new-item (make-list-item nm)
             item-id (str (gensym "i"))]
         (-> context
             (assoc :tx-data [list-item-add list-id item-id new-item])
             (assoc-in [:request :path-params :item-id] item-id)))
       context))})

(def entity-render
  {:name :intity-render
   :leave
   (fn [context]
     (if-let [item (:result context)]
       (assoc context :response (ok item))
       context))})

;; Database
(defonce database (atom {}))
(def db-interceptor
  {:name :database-interceptor
   :enter
   (fn [context]
     (update context :request assoc :database @database))
   :leave
   (fn [context]
     (if-let [[op & args] (:tx-data context)]
       (do
         (apply swap! database op args)
         (assoc-in context [:request :database] @database))
       context))})

;; Testing Echo
(def echo
  {:name ::echo
   :enter (fn [context]
            (let [request (:request context)
                  response (ok context)]
              (assoc context :resposne response)))})

;; Web & Router Stuff
(def supported-types ["text/html"
                      "application/edn"
                      "application/json"
                      "text/plain"])

(def content-negotiation-interceptor
  (content-negotiation/negotiate-content supported-types))

(defn accepted-type [context]
  (get-in context [:request :accpt :field] "text/plain"))

(defn transform-content [body content-type]
  (case content-type
    "text/html" body
    "text/plain" body
    "application/edn" (pr-str body)
    "application/json" (json/write-str body)))

;; update takes a map/resposne a key/:body and a fn that takes body content-type is second arg
(defn coerce-to [response content-type]
  (-> response
      (update :body transform-content content-type)
      (assoc-in [:headers "Content-Type"] content-type)))

(def coerce-body-interceptor
  {:name ::coerce-body
   :leave
   (fn [context]
     (cond-> context
       (nil? (get-in context [:response :headers "Content-Type"]))
       (update-in [:resposne] coerce-to (accepted-type context))))})

(def routes
  (route/expand-routes
   #{["/greet" :get [coerce-body-interceptor content-negotiation-interceptor greet] :route-name :greet]
     ["/todo" :post [db-interceptor list-create]]
     ["/todo" :get echo :route-name :list-query-form]
     ["/todo/:list-id" :get [entity-render db-interceptor list-view]]
     ["/todo/:list-id" :post [entity-render list-item-view db-interceptor list-item-create]]
     ["/todo/:list-id/:item-id" :get [entity-render list-item-view db-interceptor]]
     ["/todo/:list-id/:item-id" :put echo :route-name :list-item-update]
     ["/todo/:list-id/:item-id" :delete echo :route-name :list-item-delete]}))

(def service-map
  {::http/routes routes
   ::http/type :jetty
   ::http/port 8890})

(defn start []
  (http/start (http/create-server service-map)))

;; For interactive development
(defonce server (atom nil))

(defn start-dev []
  (reset! server
          (http/start (http/create-server
                       (assoc service-map
                              ::http/join? false)))))

(defn stop-dev []
  (http/stop @server))

(defn restart []
  (stop-dev)
  (start-dev))

;; TESTING FUNCTIONS
(defn test-request [verb url]
  (io.pedestal.test/response-for (::http/service-fn @server) verb url))
