(defn naturals [n]
  {:pre [(integer? n)]}
  (lazy-seq
    (cons n (naturals (inc n)))))

(defn my-partial [bundle-size sequence]
  {:pre [(pos-int? bundle-size) (seq? sequence)]}
  (lazy-seq
    (if (empty? (take 1 sequence))
      (list )
      (cons (take bundle-size sequence) (my-partial bundle-size (drop bundle-size sequence))))))

(defn my-partial-1 [bundle-size bundles sequence acc]
  {:pre [(pos-int? bundle-size) (pos-int? (+ bundles 1)) (seq? sequence)]}
  (if (= 0 bundles)
    acc
    (recur bundle-size (dec bundles) (drop bundle-size sequence) (cons (take bundle-size sequence) acc))))

(println (my-partial-1 2 2 (range 10) (list )))

(def workers 4)
(def bundle-size 100)

;;stable
(defn p-filter [key-func sequence]
  {:pre [(ifn? key-func) (seq? sequence)]}
  ; insert (doall)
  (lazy-seq
    (if (empty? (take 1 sequence))
      (list )
      ;workers раз конкатнуть
      (concat (->> (my-partial bundle-size sequence)
                   (take workers)
                   ;(doall)                                  ;если закомментить, то время исполнения вырастет!
                   (map #(future (filter key-func %)))
                   (doall)
                   (map deref)
                   (doall)                                  ;если закомментить, то ничего не изменится
                   (reduce concat)) (p-filter key-func (drop (* workers bundle-size) sequence))))))

;filter сам ленивый, его нужно пинать

(defn lazyless-filter [keyfunc sequence acc]
  (if (empty? sequence)
    acc
    (if (keyfunc (first sequence))
      (recur keyfunc (rest sequence) (cons (first sequence) acc))
      (recur keyfunc (rest sequence) acc))))


(time (doall (lazyless-filter even? (range 400000) (list))) )


(defn p-filter-1 [key-func sequence]
  {:pre [(ifn? key-func) (seq? sequence)]}
  ; insert (doall)
  (lazy-seq
    (if (empty? (take 1 sequence))
      (list )
      ;workers раз конкатнуть
      (doall  (concat (->> (my-partial-1 bundle-size workers sequence (list))
                           (take workers)
                           (doall)                                  ;если закомментить, то время исполнения вырастет!
                           (map #(future (lazyless-filter key-func % (list))))
                           (doall)
                           (map deref)
                           (doall)                                  ;если закомментить, то ничего не изменится
                           (reduce concat)
                           (doall)) (doall ( p-filter key-func (drop (* workers bundle-size) sequence))))))))

(time (doall (filter even? (range 400000))))
(time (doall (p-filter-1 even? (range 400000))))

;(println (p-filter even? (list 1 2 3 4 5 6 7 8 9 10 11 12)))
;(println (take 10000 (p-filter even? (naturals 0))) )
;
;(defn all-primals [n sieved]
;  (lazy-seq
;    (cons n (all-primals
;              (first (filter #(not= 0 (mod % n)) sieved))
;              (filter #(not= 0 (mod % n)) sieved) ))))
;
;(defn primals []
;  (all-primals 2 (naturals 2)))
;
;(defn p-all-primals [n sieved]
;  (lazy-seq
;    (cons n (p-all-primals
;              (first (p-filter #(not= 0 (mod % n)) sieved))
;              (p-filter #(not= 0 (mod % n)) sieved)))))
;
;(defn p-primals []
;  (p-all-primals 2 (naturals 2)))
;
;
;(println (time (nth (primals) 111)))
;(println (time (nth (p-primals) 11)))
