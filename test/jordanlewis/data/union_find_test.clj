(ns jordanlewis.data.union-find-test
  (:use clojure.test
        jordanlewis.data.union-find))

(deftest test-union-find
  (let [set (-> (union-find 1 2 3 4 5 6)
                (union 1 2)
                (union 3 4)
                (union 4 5))]
    (testing "Missing elements have nil leaders."
      (is (= nil (get-canonical set 10))))
    (testing "Singleton sets are their own leaders."
      (is (= 6 (get-canonical set 6))))
    (testing "Singleton sets unioned with themselves are still their own leaders."
      (is (= 6 (get-canonical (union set 6 6) 6))))
    (testing "Unioning from both sides of size works as expected"
      (let [set (union set 1 3)
            set-left  (union set 1 4)
            set-right (union set 4 1)]
        (is (= 1 (set-left  1)))
        (is (= 1 (set-right 1)))))
    (testing "Connected singletons have the same leader."
      (let [a (get-canonical set 1)
            b (get-canonical set 2)
            c (get-canonical set 3)
            d (get-canonical set 4)
            e (get-canonical set 5)]
        (is (= a b))
        (is (= c d))
        (is (= c e))
        (is (not= b c))
        (let [set (union set 2 3)
              a (get-canonical set 1)
              c (get-canonical set 3)]
          (is (= a c 1)))))
    (testing "Seq returns all elements"
      (is (= 6 (count (seq set)))))
    (testing "Count-sets counts the number of connected components."
      (is (= 3 (count-sets set)))
      (is (= 2 (count-sets (union set 1 3)))))
    (testing "Count counts the number of total elements in the universe"
      (is (= 6 (count set)))
      (is (= 6 (count (union set 1 3)))))
    (testing "Conj adds new singletons"
      (let [set (conj set 7)]
        (is (= 7 (count set)))
        (is (= 4 (count-sets set)))
        (is (= 7 (count (union set 6 7))))
        (is (= 3 (count-sets (union set 6 7))))
        (is (= 7 (set 7)))
        (is (= 6 ((union set 6 7) 7)))))

    (testing "union-find is gettable"
      (is (= 1 (get set 2)))
      (is (= 1 (get set 1)))
      (is (= nil (get set 10)))
      (is (= :not-found (get set 10 :not-found))))

    (testing "union-find is a function"
      (is (= 1 (set 2)))
      (is (= 1 (set 1)))
      (is (= nil (set 10)))
      (is (= :not-found (set 10 :not-found))))

    (testing "supports meta"
      (is (= {:with :meta} (meta (with-meta set {:with :meta})))))

    (testing "equality works right"
      (is (= set set))
      (is (not= set (conj set 8)))
      (is (= (union set 5 6) (union set 6 5))))

    (testing "unioning a missing element is a no-op."
      (is (= set (union set 5 10))))))

(deftest test-transient-union-find
  (let [master-set (transient (-> (union-find 1 2 3 4 5 6)
                                  (union 1 2)
                                  (union 3 4)
                                  (union 4 5)))]
    (testing "Missing elements have nil leaders."
      (let [set (transient (union-find 1 2 3))]
        (is (= nil (get-canonical set 10)))))
    (testing "Singleton sets are their own leaders."
      (let [set (transient (union-find 1 2 3 6))]
       (is (= 6 (get-canonical set 6)))))
    (testing "Singleton sets unioned with themselves are still their own leaders."
      (let [set (transient (union-find 1 2 3 6))]
       (is (= 6 (get-canonical (union! set 6 6) 6))) )
      )
    (testing "Unioning from both sides of size works as expected"
      (let [set (transient (union-find 1 2 3 4))
            set (union! set 1 3)
            set-left  (union! set 1 4)
            set-right (union! set 4 1)]
        (is (= 1 (set-left  1)))
        (is (= 1 (set-right 1)))))
    (testing "Connected singletons have the same leader."
      (let [set master-set
            a (get-canonical set 1)
            b (get-canonical set 2)
            c (get-canonical set 3)
            d (get-canonical set 4)
            e (get-canonical set 5)]
        (is (= a b))
        (is (= c d))
        (is (= c e))
        (is (not= b c))
        (let [set (union! set 2 3)
              a (get-canonical set 1)
              c (get-canonical set 3)]
          (is (= a c 1)))))
    (testing "Count-sets counts the number of connected components."
      (let [set (transient (-> (union-find 1 2 3 4 5 6)
                (union 1 2)
                (union 3 4)
                (union 4 5)))]
        (is (= 3 (count-sets set)))
        (is (= 2 (count-sets (union! set 1 3))))))
    (testing "Count counts the number of total elements in the universe"
      (is (= 6 (count master-set))))
    (testing "Conj adds new singletons"
      (let [set (transient (-> (union-find 1 2 3 4 5 6)
                (union 1 2)
                (union 3 4)
                (union 4 5)))
            set (conj! set 7)
            set (conj! set 8)]
        (is (= 5 (count-sets set)))
        (is (= 8 (count set)))
        (is (= 4 (count-sets (union! set 6 7))))
        (is (= 8 (count set)))
        (is (= 8 (set 8)))
        (is (= 6 ((union! set 6 7) 7)))))

    (testing "union-find is gettable"
      (is (= 1 (get master-set 2)))
      (is (= 1 (get master-set 1)))
      (is (= nil (get master-set 10)))
      (is (= :not-found (get master-set 10 :not-found))))

    (testing "union-find is a function"
      (is (= 1 (master-set 2)))
      (is (= 1 (master-set 1)))
      (is (= nil (master-set 10)))
      (is (= :not-found (master-set 10 :not-found))))

    (testing "equality works right"
      (let [set-1 (transient (-> (union-find 1 2 3 4 5 6)
                (union 1 2)
                (union 3 4)
                (union 4 5)))
            set-2 (transient (-> (union-find 1 2 3 4 5 6)
                (union 1 2)
                (union 3 4)
                (union 4 5)))
            ]
        (is (not= set-1 set-2))
        )
        )
    (testing "unioning a missing element is a no-op."
      (is (= master-set (union! master-set 5 10))))
    (testing "persistent! works"
      (let [set (persistent! master-set)]
        (is (= set (persistent! (transient set))))))))
