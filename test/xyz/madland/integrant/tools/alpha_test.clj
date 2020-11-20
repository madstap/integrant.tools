(ns xyz.madland.integrant.tools.alpha-test
  (:require [xyz.madland.integrant.tools.alpha :as ig.tools :refer [with-system]]
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
