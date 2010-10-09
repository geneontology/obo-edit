;;;;
;;;; 
;;;;
;; NOTE:
;; See: http://www.mail-archive.com/solr-user@lucene.apache.org/msg27945.html
;; Currently ~25s. for the rude 10000.
;; Currently ~6s. for the rude 5000.
;; Currently ~1s. for the rude 1000.
;; Fixable via filter?
;;   (time (join-test 100))

(ns goljur.core
  (:use [clojure.contrib.sql :as sql :only ()]
	[org.danlarkin [json :as json]]
	[clojure.contrib.def]))
(require '[clj-http.client :as client])
(require '[clojure.contrib.string :as string])

;;;
;;; Configurable variables.
;;;

;; Evaluate form with your own values.
;; TODO: move this into an external config file.
(let [mysql-host "localhost"
      mysql-port 3306
      mysql-name "go_latest_lite"
      mysql-user "sjcarbon"
      mysql-password ""
      lucene-path "/tmp/solr"
      solr-url "http://localhost:8080/solr/"]

  (def go-db {:classname "com.mysql.jdbc.Driver" ; must be in classpath
	      :subprotocol "mysql"
	      :subname (str "//" mysql-host ":" mysql-port "/" mysql-name)
	      :user mysql-user
	      :password mysql-password})

  (def golr-web {:url solr-url}))

;;;
;;; GO (lead) access.
;;;

(defn table-count
  "Return the row count of a table (passed as a string)."
  [table-name]
  (sql/with-connection go-db
    (sql/with-query-results results
      [(str "SELECT count(*) AS c FROM " table-name)]
      ;;results)))
      ;; BUG: if we don't force it here, it closes before eval?! Lazy?
      (get (first (doall results)) :c))))
      
(defn term-accs
  "List all terms accs."
  []
  (sql/with-connection go-db
    (sql/with-query-results results
      ["SELECT acc FROM term"]
      (map (fn [x] (get x :acc)) (doall results)))))

(defn n-random-term-accs
  "Return a list of n random numbers between 0 and n. An extra
argument will trigger cleaning the output to just include GO ids."
  ([n] (take n (shuffle (term-accs))))
  ([n clean-p] (filter (fn [x] (re-find #"^GO\:[0-9]{7}$" x))
		       (take n (shuffle (term-accs))))))

;; ;;;
;; ;;; Solr access.
;; ;;;

;; (defn golr-connect
;;   "Return a \"connection\" to Golr"
;;   []
;;   (solr/connect (get golr-web :url)))

;; ;; BUG: not working as expected--"neurogenesis" fine, but
;; ;; "neurogenesis OR pigment*" gets nothing...
;; (defn golr-query
;;   "Try and union a large chunk of the GO through Solr's web interface."
;;   [query-string]
;;   (with-connection (golr-connect)
;;     ;;(search query-string :rows 1)
;;     (search query-string)))

;;;
;;; Direct HTTP access.
;;;

(defnk golr-request
  "Mostly hardwired request at GO Solr."
  [:q "" :fq ""]
  [q fq]
  (let [in-params
	{
	 "explainOther" ""
	 "fl" "*,score"
	 "fq" fq
	 "hl.fl" ""
	 "indent" "on"
	 "q" q
	 "qt" "standard"
	 "rows" "1000000" ; "limit" to a million
	 "start" "0"
	 "version" "2.2"
	 "wt" "json"
	 }]
    (client/get (str (get golr-web :url) "select")
		{:query-params in-params})))

(defn extract-term-data
  "Extract the term information from a Solr JSON response."
  [resp]
  (get (get (json/decode-from-str (get resp :body)) :response) :docs))

;;(extract-term-data (try-request "neurogenesis")) ; count 4
;;(extract-term-data (try-request "acc:\"GO:0022008\"")) ; count 1
;;(extract-term-data (try-request "acc:\"GO:0022008\" OR acc:\"GO:0022007\"")) ; count 2

;;;
;;; Joint try.
;;;

(defn make-or-query-string
  "..."
  [n]
  (string/join " OR " (map (fn [x] (str "acc:\"" x "\""))
			   (n-random-term-accs n :clean))))

;; Too large a number gets 413. Fixed in jetty.xml with headerBufferSize.
;; (extract-term-data (golr-request :q (make-or-query-string 138)))
;; BUG: Too large a number gets 400. 
;; (extract-term-data (golr-request :q (make-or-query-string 1024)))
(defn join-test-naive
  "...?"
  [n]
  (try
    (count (extract-term-data (golr-request :q (make-or-query-string n))))
    (catch Exception e
      (swank.core/break)
      (prn e))))
;;...5600" OR acc:"GO:0042638"': too many boolean clauses
;;	at org.apache.lucene.queryParser.QueryParser.parse(QueryParser.java:215)
;; Fixed by temporarily raising maxBooleanClauses in solrconfig.xml

;; Unfortunately, not much of an improvement
(defn join-test-fq
  "...?"
  [n]
  (try
    (count (extract-term-data (golr-request :q "name:[a TO z]"
					    :fq (make-or-query-string n))))
    (catch Exception e
      (swank.core/break)
      (prn e))))

;; Very very bad, but fun.
(defn join-test-series
  "...?"
  [n]
  (let [series (n-random-term-accs n :clean)]
    (dotimes [x (count series)]
      (try
	(extract-term-data (golr-request :q (make-or-query-string 1)))
	(catch Exception e
	  (swank.core/break)
	(prn e))))))
  