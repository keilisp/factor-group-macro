(ns factor-group-macro.core
  (:gen-class))

(defn get-filter-keys
  [bindings]
  (take-nth 2 (rest bindings)))

(defn group-by-keys
  [coll keys]
  (group-by #(select-keys % keys) coll))

(defn get-actual-bindings
  [coll bindings]
  (let [grouped-bindings (partition 2 bindings)]
    (into []
          (mapcat (fn [[binding key]] [binding (list key coll)]))
          grouped-bindings)))

(defmacro factor-group
  [data group-data bindings & body]
  `(let [groups# (->> '~bindings
                      get-filter-keys
                      (group-by-keys ~data))]
     (into '()
           (map (fn [~'group]
                  (let [~@(concat [['keys 'data] 'group
                                   group-data 'data]
                                  (get-actual-bindings 'keys bindings))]
                    ~@body))) groups#)))

(def all-patients
  '({:firstname "Adam"
     :lastname  "Smith"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Joseph"
     :lastname  "Goodman"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Werner"
     :lastname  "Ziegler"
     :diagnosis "COVID-19"
     :treated   false}
    {:firstname "Boris"
     :lastname  "Henry"
     :diagnosis "Healthy"
     :treated   false}
    {:firstname "Johnny"
     :lastname  "Grayhand"
     :diagnosis "COVID-76"
     :treated   false}))

(defn -main
  [& args]
  (factor-group
   all-patients
   patients-group

   [treated?     :treated
    disease-name :diagnosis]

   (println " начало обработки группы пациентов с диагнозом " disease-name
            (if treated? ", подвергавшихся лечению"
                ", НЕ подвергавшихся лечению"))

   (println " количество пациентов в группе - " (count patients-group))
   (println " фамилии пациентов - " (clojure.string/join ", " (map :lastname patients-group)))

   (count patients-group)))
