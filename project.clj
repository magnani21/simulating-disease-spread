(ns disease-simulation)

(defn sir-model
  "Simulates disease spread using the SIR model."
  [population beta gamma days]
  (let [initial {:S (- population 1), :I 1, :R 0}]
    (reduce
      (fn [state _]
        (let [{:keys [S I R]} state
              new-infections (* beta S I (/ 1.0 population))
              new-recoveries (* gamma I)]
          {:S (- S new-infections)
           :I (+ I (- new-infections new-recoveries))
           :R (+ R new-recoveries)}))
      initial
      (range days))))

(defn save-results
  "Saves simulation results to a file."
  [results file-name]
  (spit file-name
        (clojure.string/join
         "\n"
         (map #(str (first %) "," (second %) "," (nth % 2)) results))))

(defn simulate-and-save
  "Runs the simulation and saves the results."
  []
  (let [population 1000
        beta 0.3
        gamma 0.1
        days 50
        results (map-indexed
                  (fn [day state]
                    [day (:S state) (:I state) (:R state)])
                  (sir-model population beta gamma days))]
    (save-results results "disease_simulation_results.csv")
    (println "Simulation complete. Results saved to disease_simulation_results.csv.")))

(simulate-and-save)