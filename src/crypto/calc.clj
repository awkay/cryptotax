(ns crypto.calc
  (:require
    [clojure-csv.core :as csv]
    [clojure.java.io :as io]
    [cljc.java-time.local-date :as ld]
    [cljc.java-time.duration :as duration]
    [clojure.string :as str])
  (:import
    (java.util Date Locale)
    (java.time ZoneId)
    (java.text DecimalFormat NumberFormat)))

(defn parse-number [^String amount]
  (try
    (let [^NumberFormat format (NumberFormat/getNumberInstance Locale/US)
          amount               (str/replace amount #"[^\d.,]" "")]
      (when (instance? DecimalFormat format)
        (.setParseBigDecimal ^DecimalFormat format true))
      (.parse format amount))
    (catch Exception _ 0.0M)))

(defn parse-date [^String d]
  (ld/parse d))

(defn load-transactions [filename]
  (let [{assets true
         sales  false} (group-by
                         (fn [{:keys [received]}] (boolean (some-> received (pos?))))
                         (sort-by :date
                           (map
                             (fn [[date-str income eth-recv usd-per-eth eth-sold :as r]]
                               (let [received   (parse-number eth-recv)
                                     sold       (parse-number eth-sold)
                                     unit-price (parse-number usd-per-eth)]
                                 (cond-> {:date       (parse-date date-str)
                                          :unit-price unit-price}
                                   (pos? received) (assoc :received received :remaining received)
                                   (pos? sold) (assoc :sold sold))))
                             (csv/parse-csv (slurp filename)))))]
    {:assets assets
     :sales  sales}))

(defn days-between [a b]
  (duration/to-days (duration/between (ld/at-start-of-day a) (ld/at-start-of-day b))))

(defn sell-asset
  "Returns the result of a sale, which is the age of the asset that was sold, the gain/loss, and the updated
   assets. The assets must be in date-sorted order, and will be returned in date-sorted order. A single sale
   might sell multiple assets, so the gains are to-many.

   {:assets updated-assets
    :date date-of-sale
    :gains [{:date-acquired date :eth-sold n :age ndays :amount number} ...]}
  "
  [assets {:keys [date unit-price sold] :as sale}]
  (if (empty? assets)
    {:gains  []
     :date   date
     :assets []}
    (loop [amount-to-sell   sold
           {purchase-date       :date
            remaining           :remaining
            purchase-unit-price :unit-price :as asset} (first assets)
           assets-remaining (rest assets)
           gains            []]
      (when (< (ld/to-epoch-day date) (ld/to-epoch-day purchase-date))
        (println "Out of sequence error:" sale asset))
      (let [age                (days-between purchase-date date)
            amount             (min remaining amount-to-sell)
            remainder-to-sell  (- amount-to-sell amount)
            remainder-of-asset (- remaining amount)
            gain               (* amount (- unit-price purchase-unit-price))
            gains              (conj gains {:date-acquired purchase-date
                                            :date-sold     date
                                            :eth-sold      amount
                                            :cost-basis    (* amount purchase-unit-price)
                                            :sale-price    (* amount unit-price)
                                            :amount        gain
                                            :age           age})
            assets             (if (> remainder-of-asset 0.0001M)
                                 (into [(assoc asset :remaining remainder-of-asset)] assets-remaining)
                                 assets-remaining)]
        (println date amount remainder-to-sell remainder-of-asset)
        (cond
          (empty? assets) {:gains  gains
                           :assets []
                           :date   date}

          (> remainder-to-sell 0.0001M) (recur remainder-to-sell (first assets) (rest assets) gains)

          :else {:gains  gains
                 :date   date
                 :assets assets})))))

(defn calculate-fifo-asset-gains [assets sales]
  (reduce
    (fn [{acc-assets :assets :as acc} sale]
      (println "SALE" sale)
      (let [{:keys [gains assets]} (sell-asset acc-assets sale)]
        (-> acc
          (assoc :assets assets)
          (update :gains into gains))))
    {:assets assets
     :gains  []}
    sales))

(comment

  (let [{:keys [assets
                sales]} (load-transactions "/Users/tonykay/Downloads/crypto.csv")
        {:keys [gains]}
        (calculate-fifo-asset-gains assets sales)]
    (spit  "/tmp/form.csv"
      (str
        "Description,Date Acquired,Date Sold,Sales Proceeds,Cost Basis\n"
        (str/join "\n"
         (keep
           (fn [{:keys [amount date-acquired date-sold cost-basis sale-price] :as item}]
             (when (= 2022 (ld/get-year date-sold))
               (str/join
                 ","
                 ["ETH Trade"
                  (str date-acquired)
                  (str date-sold)
                  (format "%.2f" sale-price)
                  (format "%.2f" cost-basis)])))
           gains))))
    )

  )
