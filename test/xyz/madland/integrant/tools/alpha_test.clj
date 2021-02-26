(ns xyz.madland.integrant.tools.alpha-test
  (:require [xyz.madland.integrant.tools.alpha :as ig.tools
             :refer [with-system with-init-times]]
            [integrant.core :as ig]
            [clojure.test :refer [deftest is]]))

(deftest get-test
  (is (= 123 (ig.tools/get {::foo 123} ::foo)))
  (is (= 123 (ig.tools/get {[::foo ::bar] 123} ::foo)))
  (is (= 123 (ig.tools/get {[::foo ::bar] 123} [::foo ::bar]))))

(deftest assoc-test
  (is (= {::foo 2}
         (ig.tools/assoc {::foo 1} ::foo 2)))
  (is (= {[::bar ::foo] 2}
         (ig.tools/assoc {[::bar ::foo] 1} ::foo 2)))
  (is (= {[::bar ::foo] 2}
         (ig.tools/assoc {[::bar ::foo] 1} [::bar ::foo] 2))))

(deftest assoc-in-test
  (is (= {[::foo ::bar] {[::foo ::bar] false, ::bar true}}
         (ig.tools/assoc-in {[::foo ::bar] {[::foo ::bar] false, ::bar false}}
                            [::foo ::bar]
                            true))))

(def sys-example
  {[::foo ::a] 1
   [::bar ::a] 1
   [::baz ::b] 2
   [::quux ::c] 2})

(deftest select-keys-test
  (is (= {[::foo ::a] 1
          [::bar ::a] 1}
         (ig.tools/select-keys sys-example [::a]))))

(deftest get-all-test
  (is (= [1 1] (ig.tools/get-all sys-example [::a]))))

(deftest assoc-all-test
  (is (= {[::foo ::a] :x
          [::bar ::a] :x
          [::baz ::b] 2
          [::quux ::c] 2}
         (ig.tools/assoc-all sys-example ::a :x))))

(deftest update-all-test
  (is (= {[::foo ::a] -10
          [::bar ::a] -10
          [::baz ::b] 2
          [::quux ::c] 2}
         (ig.tools/update-all sys-example ::a - 11))))

(def nested-example
  {[::foo ::a] {:foo 1}
   [::bar ::a] {:foo 1}
   [::baz ::b] 2
   [::quux ::c] 2})

(deftest assoc-all-test
  (is (= {[::foo ::a] {:foo :x}
          [::bar ::a] {:foo :x}
          [::baz ::b] 2
          [::quux ::c] 2}
         (ig.tools/assoc-in-all nested-example [::a :foo] :x))))

(deftest update-all-test
  (is (= {[::foo ::a] {:foo -10}
          [::bar ::a] {:foo -10}
          [::baz ::b] 2
          [::quux ::c] 2}
         (ig.tools/update-in-all nested-example [::a :foo] - 11))))

(defmethod ig/init-key ::foo [_ x] x)
(defmethod ig/init-key ::bar [_ x] x)
(defmethod ig/init-key ::x [_ x] x)

(deftest with-system-test
  (with-system [{::keys                     [foo]
                 {not-found ::baz
                  bar       [::baz ::quux]} ::bar
                 x                          [::x ::y]}
                {[::foo ::quux] 1
                 ::bar          {[::baz ::quux] 2}
                 [::x ::y]      3}]
    (is (= 1 foo))
    (is (= 2 bar))
    (is (= 3 x))
    (is (nil? not-found))))

(defmethod ig/init-key ::sleepy [_ {:keys [ms]}]
  (Thread/sleep ms))

(deftest with-init-times-test
  (let [logs (atom [])]
    (with-init-times #(swap! logs conj [%1 %2])
      (with-system [_ {[:init/foo ::sleepy] {:ms 50}
                       [:init/bar ::sleepy] {:ms 100 :foo (ig/ref :init/foo)}}]
        (let [[[foo-k foo-ms] [bar-k bar-ms]] @logs]
          (is (= [:init/foo ::sleepy] foo-k))
          (is (= [:init/bar ::sleepy] bar-k))
          (is (<= 50 foo-ms))
          (is (<= 100 bar-ms)))))))
