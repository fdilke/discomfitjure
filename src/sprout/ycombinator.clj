(ns sprout.ycombinator)

(defn ycombinator
  "Act on a 'metafunction' transforming functions A->B ;
  return a function A->B whose behaviour is unchanged by the transformation"
  [meta-fn]
  (letfn [(quine [] ; the quine maps quine-like objects to functions A->B
     (meta-fn #((quine) %))
  )] (quine)
))
