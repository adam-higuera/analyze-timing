(ns analyze-timing.core)

(require 'clojure.set)
(use '(incanter core stats datasets io charts optimize))
(require 'clojure.math.numeric-tower)

(defn analyze-file [block-size n-processors root-path file-suffix]
  (with-open [r (clojure.java.io/reader (str root-path "str" block-size "timing" n-processors file-suffix))]
    (let [filter-func #(or (clojure.string/blank? %) (re-find #"CMA" %)
                           (re-find #"Rank" %) (re-find #"domain_size" %))
          times (map #'read-string (drop 4 (drop-while filter-func (line-seq r))))]
      (vector block-size n-processors (mean times) (sd times)))))

(defn get-data [{:keys [root-path d-sizes n-procs-list file-suffix]
                 :or {root-path "/Users/adamvh/Documents/Development/adi-prototype/"
                      d-sizes [10 25 50 100 250 500 1000 2000]
                      n-procs [9 16 25 36 49 64 81 100]
                      file-suffix ""}}]
  (dataset [:domain-size :n-procs :running-time :sigma-running-time]
   (for [block-size d-sizes n-procs n-procs-list]
     (analyze-file block-size n-procs root-path file-suffix))))

(defn strong-scaling-add-lines [chart data]
  (let [groups-map ($group-by :domain-size data)
        ks (keys groups-map)
        color-map [java.awt.Color/red java.awt.Color/blue java.awt.Color/green
                   java.awt.Color/yellow java.awt.Color/magenta]]
    (doseq [k ks]
      (let [n (.getDatasetCount (.getPlot chart))
            d (groups-map k)]
        (doto chart
          (add-lines ($ :n-procs d)
                     (map #(/ %1 (clojure.math.numeric-tower/expt %2 2) 100)
                          ($ :running-time d) ($ :domain-size d))
                     :series-label (str "D = " (:domain-size k) " (ATLAS)")
                     :legend true :points true)
          (set-stroke :series 0 :dataset n :dash 5)
          (set-stroke-color (nth color-map (- n 5)) :series 0 :dataset n))))
    (doto chart
      (set-stroke-color java.awt.Color/orange :series 0 :dataset 3)
      (set-stroke-color java.awt.Color/magenta :series 0 :dataset 4)
      (set-stroke-color java.awt.Color/orange :series 0 :dataset 8))
    chart))

;; (defn strong-scaling-plot [data block-sizes]
;;   (let [d (filter #(== (first %) (first block-sizes)) data)
;;         x (map #(log (nth % 1)) d)
;;         y (map #(/ (nth % 2) (clojure.math.numeric-tower/expt (first block-sizes) 2)
;;                    (if (< (first %) 250) 100 10)) d)
;;         plt (xy-plot x y
;;              :series-label (str "D = " (first block-sizes))
;;              :legend true)]
;;     (doseq [bs (rest block-sizes)]
;;       (let [d (filter #(== (first %) bs) data)]
;;         (add-lines plt
;;                    (map #(log (nth % 1)) d)
;;                    (map #(/ (nth % 2) (clojure.math.numeric-tower/expt bs 2)
;;                             (if (< (first %) 250) 100 10)) d)
;;                    :series-label (str "D = " bs)
;;                    :legend true)))
;;     (doto plt
;;       (set-title "Strong Scaling Numerical Experiment")
;;       (set-x-label "log(P)")
;;       (set-y-label "core-usec / cell / step"))
;;     plt))
(defn strong-scaling-plot [{:keys [root-path d-sizes n-procs-list file-suffix]
                            :or {root-path "/Users/adamvh/Documents/Development/adi-prototype/"
                                 d-sizes [10 25 50 100 250 500 1000 2000]
                                 n-procs-list [9 16 25 36 49 64 81 100]
                                 file-suffix ""}}]
  (let [d (get-data {:root-path root-path :d-sizes d-sizes :n-procs-list n-procs-list})]
    (xy-plot
     :n-procs
     (map #(/ %1 (clojure.math.numeric-tower/expt %2 1) 100) ($ :running-time d) ($ :domain-size d))
     :data d
     :group-by :domain-size
     :series-label #(str "D = " %)
     :legend true
     :log-x true
     :log-y true
     :points true
     :x-label "# Processors"
     :y-label "core-usec / cell / step")))

(defn message-strategy-comparison [big-message-root small-message-root domain-size]
  (let [d-big (get-data {:root-path big-message-root :d-sizes [domain-size]})
        d-small (get-data {:root-path small-message-root :d-sizes [domain-size]})
        plt (strong-scaling-plot d-big [domain-size])]
    (doto plt
      (set-stroke :dash 5)
      (set-title "Messaging Strategy Comparison (D=50)")
      (add-lines
       (map #(log (nth % 1)) d-small)
       (map #(/ (nth % 2) (clojure.math.numeric-tower/expt domain-size 2)
                (if (< (first %) 250) 10 10)) d-small)
       :series-label (str "D = " domain-size " (many small messages)")
       :legend true))
    plt))

(defn d-size-scaling-plot [data n-procs]
  (let [d (filter #(== (nth % 2) (first n-procs)) data)
        plt (xy-plot
             (map first d)
             (map #(/ (nth % 2) (clojure.math.numeric-tower/expt (first %) 2)
                      (if (< (first %) 250) 100 10)) d)
             :series-label (str (first n-procs) " Processors")
             :legend true)]
    (doseq [p (rest n-procs)]
      (let [d (filter #(== (nth % 1) p) data)]
        (add-lines plt
                   (map first d)
                   (map #(/ (nth % 2) (clojure.math.numeric-tower/expt (first %) 2)
                            (if (< (first %) 250) 100 10)) d)
                   :series-label (str p " Processors")
                   :legend true)))
    (doto plt
      (set-title "Strong Scaling Numerical Experiment")
      (set-x-label "D")
      (set-y-label "core-sec / cell / 10 steps"))))

(defn logp [theta x] (let [[a b] theta] (* a (clojure.math.numeric-tower/expt x b))))

(defn log-model [block-size data]
  (let [d (filter #(== (first %) block-size) data)
        x (map #(nth % 1) d)
        y (map #(/ (nth % 2) (clojure.math.numeric-tower/expt block-size 2)) d)]
    (non-linear-model logp y x [0.000003 0.0000005])))

(defn -main [& args]
  (let [data (get-data)]
    (view (strong-scaling-plot data 500))))

(defn make-plot []
  (let [d (get-data {:root-path "/Users/adamvh/Documents/Development/adi-prototype/usec2/"
                     :d-sizes [10 20 30 40 50]})
        plt
        (strong-scaling-add-lines
         (xy-plot
          :n-procs
          (map #(/ %1 (clojure.math.numeric-tower/expt %2 2) 100) ($ :running-time d) ($ :domain-size d))
          :data d
          :group-by :domain-size
          :series-label #(str "D = " %)
          :legend true
          :log-x true
          :points true
          :x-label "# Processors"
          :y-label "core-usec / cell / step")
         (get-data {:root-path "/Users/adamvh/Documents/Research/data/big_message_atlas/"
                    :d-sizes [10 20 30 40 50]
                    :file-suffix "atlas"}))]
    (.setLabelPaint (.getRangeAxis (.getPlot plt)) (new 
                                                    java.awt.Color 128 128 128)) 
    (.setLabelFont (.getRangeAxis (.getPlot plt)) (new java.awt.Font 
                                                       "Tahoma" java.awt.Font/BOLD 18)) 
    (.setTickLabelFont (.getRangeAxis (.getPlot plt)) (new java.awt.Font 
                                                           "Tahoma" java.awt.Font/BOLD 14)) 
    (.setTickLabelPaint (.getRangeAxis (.getPlot plt)) (new 
                                                        java.awt.Color 128 128 128))
    plt))

