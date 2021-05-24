(ns redditbot.core
  (:require [clj-http.client :as client]
            [cheshire.core :refer [parse-string]])
  (:gen-class))

(def options {:headers {"User-agent" "megacringe109100"}
              :query-params {:limit 100}})

(def url "http://www.reddit.com/r/Clojure.json")



(defn get-posts
  []
  (let [body (:body (client/get url options))
        parsed-body (parse-string body true)
        children (:children (:data parsed-body))]
    (map :data children)))

(defn good-post?
  [post]
  (> (:score post) 90))

(defn only-good-posts
  [posts]
  (filter good-post? posts))

(defn average-score
  [posts]
  (let [post-count (count posts)
        total-score (reduce + (map :score posts))]
    (float (/ total-score post-count))))

(defn post-count-helper
  [acc x]
  (update acc x (fnil inc 0)))

(defn total-score-helper
  [acc x]
  (update acc 
          (:author x) 
          (fnil (partial + (:score x)) 0)))

(defn author-total-score
  [posts]
  (reduce total-score-helper {} posts))


(defn author-post-count
  [posts]
  (let [authors (map :author posts)]
    (reduce post-count-helper {} authors)))


(defn links-posted-helper
  [acc x]
  (if (empty? (:selftext x))
    (conj acc (:url x))
    acc))

(defn links-posted 
  [posts]
  (reduce links-posted-helper [] posts))

(println (links-posted (get-posts)))

(def ui-choices
  "1: All posts
   2: Only the good ones
   3: Average score
   4: Author post count
   5: Gimme links
   Enter choice:\n")

(defn run-ui
  [choice posts]
  (if (empty? choice)
    (println "done.")
    (do (clojure.pprint/pprint (case choice
                                 "1" posts
                                 "2" (only-good-posts posts)
                                 "3" (average-score posts)
                                 "4" (author-post-count posts)
                                 "5" (links-posted posts)
                                 (println ui-choices)))
        (run-ui (read-line) posts))))


(defn -main
  [& args]
  (do (println ui-choices)))