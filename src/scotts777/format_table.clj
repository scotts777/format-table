(ns scotts777.format-table)

(def text-colors {:black          "\u001b[30m"
                  :red            "\u001b[31m"
                  :green          "\u001b[32m"
                  :yellow         "\u001b[33m"
                  :blue           "\u001b[34m"
                  :magenta        "\u001b[35m"
                  :cyan           "\u001b[36m"
                  :white          "\u001b[37m"
                  :bright-black   "\u001b[30;1m"
                  :bright-red     "\u001b[31;1m"
                  :bright-green   "\u001b[32;1m"
                  :bright-yellow  "\u001b[33;1m"
                  :bright-blue    "\u001b[34;1m"
                  :bright-magenta "\u001b[35;1m"
                  :bright-cyan    "\u001b[36;1m"
                  :bright-white   "\u001b[37;1m"})
(def text-reset "\u001b[0m")

(def ascii-pattern #"\u001b\[\d+;*\d*m")

(defn contains-many? [m & ks]
  (every? #(contains? m %) ks))

(defn ascii-color-code
  [row column type]
  (let [color-val (get column type)]
    (if (keyword? color-val)
      (get text-colors color-val)
      (if (string? color-val)
        color-val
        (if (fn? color-val)
          (color-val row)
          nil)))))

(defn adjust-ascii-colwidth
  [str-value col-width]
  (let [contained-ascii-codes (re-seq ascii-pattern str-value)
        ascii-char-count (reduce #(+ %1 (.length ^String %2)) 0 contained-ascii-codes)]
    (+ col-width ascii-char-count)))

(defn align-string
  [str-value align col-width]
  (let [adjusted-ascii-colwidth (adjust-ascii-colwidth str-value col-width)
        str-length (.length ^String str-value)
        free-space (- adjusted-ascii-colwidth str-length)]
    (case align
      :left (str str-value (apply str (repeat free-space \space)))
      :right (str (apply str (repeat free-space \space)) str-value)
      :center (let [first-length (quot free-space 2)
                    second-length (- free-space first-length)]
                (str (apply str (repeat first-length \space))
                     str-value
                     (apply str (repeat second-length \space))))
      (str str-value (apply str (repeat free-space \space))))))

(defn printable-string
  [string-value alignment col-width padding-length]
  (let [printable-value (align-string string-value alignment col-width)]
    (str (apply str (repeat padding-length \space)) printable-value )))

(defn print-header
  [columns]
  (let [header-strings (into [] (map #(printable-string (str (:name %)) (:align %) (:width %) (:padding %)) columns))
        underline-strings (into [] (map #(printable-string (apply str (repeat (.length ^String (str (:name %))) \-))
                                                           (:align %) (:width %) (:padding %)) columns))]
    (println (apply str header-strings))
    (println (apply str underline-strings))))

(defn format-value
  [row column]
  (let [value-fn (:value column)
        row-value (value-fn row)
        value-format (:format column)
        foreground-color (ascii-color-code row column :foreground)
        background-color (ascii-color-code row column :background)
        formatted-value (if value-format
                          (format value-format row-value)
                          (str row-value))
        foreground-formatted-value (if foreground-color
                                     (str foreground-color formatted-value text-reset)
                                     formatted-value)]
    foreground-formatted-value))

(defn print-row
  [row columns]
  (let [row-strings (into [] (map #(printable-string (format-value row %) (:align %)
                                                     (:width %) (:padding %))) columns)]
    (println (apply str row-strings))))

(defn validate-column
  [column]
  (if-not (contains-many? column :name :value)
    (throw (ex-info "column maps must contain both :name and :value keys" column))
    column))

(defn initialize-column-padding
  [column]
  (if (contains? column :padding)
    column
    (assoc column :padding 2)))

(defn initialize-column-width
  [column]
  (if (contains? column :width)
    column
    (assoc column :width 10)))

(defn normalize-column
  "either a column maps or keyword that will be normalized
to a column map"
  [column]
  (if (keyword? column)
    {:name    column
     :value   column
     :padding 2}
    (if (map? column)
      column
      (throw (ex-info "columns must only be keywords or maps" {:column column})))))

(defn adjust-alignment
  [column row]
  (if-not (contains? column :align)
    (let [value-fn (:value column)
          col-value (value-fn row)]
      (if (number? col-value)
        (assoc column :align :right)
        (assoc column :align :left)))
    column))

(defn do-nothing
  []
  (fn
    ([])
    ([result])
    ([result input]
     nil)))

(defn format-table
  "Prints a collection of maps in a textual table.
 ks a list of keywords that match map keys or maps themselves that define
 column configuration"
  ([columns]
   (fn [rf]
     (let [count (volatile! 1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [normalized-columns (into [] (map #(-> %
                                                      normalize-column
                                                      initialize-column-padding
                                                      initialize-column-width
                                                      (adjust-alignment, input))) columns)]
            (when (= @count 1) (print-header normalized-columns))
            (print-row input normalized-columns)
            (vreset! count (+ @count 1))
            (rf result input)))))))

  ([rows columns]
   (transduce (format-table columns) do-nothing rows)))