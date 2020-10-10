echo "\n*** Clearing \"out\" directory"
rm -r out
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out
echo "\n*** Testing clojure"
clj -A:test
echo "\n*** Testing clojure 1.9"
clj -A:1.9:test
echo "\n*** Testing clojure 1.8"
clj -A:1.8:test
echo "\n*** Testing clojurescript, no optimizations"
clj -A:cljs-test:none
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out
echo "\n*** Testing clojurescript with clojure 1.9, no optimizations"
clj -A:1.9:cljs-test:none
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out
echo "\n*** Testing clojurescript with clojure 1.8, no optimizations"
clj -A:1.8:cljs-test:none
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out

echo "\n*** Testing clojurescript, simple optimizations"
clj -A:cljs-test:simple
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out

echo "\n*** Testing clojurescript with clojure 1.9, simple optimizations"
clj -A:1.9:cljs-test:simple
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out

echo "\n*** Testing clojurescript with clojure 1.8, simple optimizations"
clj -A:1.8:cljs-test:simple
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out

echo "\n*** Testing clojurescript, advanced compilation"
clj -A:cljs-test:advanced
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out

echo "\n*** Testing clojurescript with clojure 1.9, advanced optimizations"
clj -A:1.9:cljs-test:advanced
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out

echo "\n*** Testing clojurescript with clojure 1.8, advanced optimizations"
clj -A:1.8:cljs-test:advanced
echo "\n*** Clearing \"cljs-test-runner-out\" directory"
rm -r cljs-test-runner-out
