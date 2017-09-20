(ns runbld.pipeline
  (:require
   [runbld.io :as io]
   [runbld.util.debug :as debug]))

(defn symbol-name [fn-sym]
  (try
    `(:name (meta (var ~fn-sym)))
    (catch Exception _
      `(str ~fn-sym))))

(defmacro before [fn-sym]
  "Returns a middleware function that will run before the wrapped
  function runs.  The passed function is expected to take 1 argument,
  opts and must return opts when it is done.

  e.g.,
  (defun my-before-fn [opts] ...)
  (before my-before-fn)"
  `(fn [proc#]
     (fn [opts#]
       (let [name# ~(symbol-name fn-sym)
             res# (~fn-sym opts#)
             proc-res# (proc# res#)]
         proc-res#))))

(defmacro after [fn-sym]
  "Returns a middleware function that will run after the wrapped
  function runs.  The passed function is expected to take 1 argument,
  opts and must return opts when it is done.

  e.g.,
  (defun my-after-fn [opts] ...)
  (after my-after-fn)"
  `(fn [proc#]
     (fn [opts#]
       (let [name# ~(symbol-name fn-sym)
             res# (proc# opts#)
             fn-res# (~fn-sym res#)]
         fn-res#))))

(defmacro around [fn-sym]
  "Returns a middleware function that will run both before and after
  the wrapped function runs.  This one is different from before and
  after because the passed `opts-fn' is expected to take 2 arguments,
  proc and opts and is expected to call `proc' and return the results.

  e.g.,
  (defun my-around-fn [proc opts] ...)
  (around my-around-fn)"
  `(fn [proc#]
     (fn [opts#]
       (let [name# ~(symbol-name fn-sym)
             res# (~fn-sym proc# opts#)]
         res#))))

(defn make-pipeline
  "Wraps 'root-fn' in the middleware provided in middleware-list.  The
  following diagram illustrates the order of processing.  The list of
  middleware is on the left, and the order follows, starting at
  'before1' down until 'root function' is called and the proceeds back
  up until it ends at 'after1' and the final map is returned.

                                    ▲
       MIDDLEWARE      1. before1 │ │ 9. after1
         before1                  │ │
         after1        2. before2 │ │ 8. around
         before2                  │ │
         around        3.  around │ │ 7. after2
         after2                   │ │
         before3       4. before3 │ │ 6. after3
         after3                   │ │
                                  ▼
                          5. root function"
  [root-fn middleware-list]
  (reduce (fn wrap-proc* [proc middleware]
            (middleware proc))
          (fn [opts]
            (root-fn opts))
          (reverse middleware-list)))
