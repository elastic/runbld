(ns runbld.process
  (:require [runbld.schema :refer :all]
            [schema.core :as s])
  (:require [runbld.util.data :as data]
            [runbld.util.date :as date]
            [runbld.util.io :as io])
  (:import (runbld.schema ProcessResult)))

(defn exec* [pb outfile errfile]
  (with-open [out (java.io.PrintWriter. outfile)
              err (java.io.PrintWriter. errfile)]
    (let [start (System/currentTimeMillis)
          proc (.start pb)
          [out-bytes err-bytes] (io/spit-process
                                 (.getInputStream proc) out
                                 (.getErrorStream proc) err)
          exit-code (.waitFor proc)
          end (System/currentTimeMillis)]
      {
       :err-bytes @err-bytes
       :exit-code exit-code
       :millis-end end
       :millis-start start
       :out-bytes @out-bytes
       :status (if (pos? exit-code) "FAILURE" "SUCCESS")
       :time-end (date/ms-to-iso end)
       :time-start (date/ms-to-iso start)
       :took (- end start)
       :out-accuracy (data/bigdec
                      (data/safe-div (count (slurp outfile)) @out-bytes) 4)
       :err-accuracy (data/bigdec
                      (data/safe-div (count (slurp errfile)) @err-bytes) 4)})))

(s/defn exec :- ProcessResult
  ([program args scriptfile]
   (exec program args scriptfile (System/getProperty "user.dir")))
  ([program args scriptfile cwd]
   (let [scriptfile* (io/abspath scriptfile)
         dir (io/abspath-file cwd)
         stdoutfile (io/file dir ".stdout.log")
         stderrfile (io/file dir ".stderr.log")
         cmd (flatten [program args scriptfile*])
         pb (doto (ProcessBuilder. cmd)
              (.directory dir))
         res (exec* pb stdoutfile stderrfile)]
     (flush)
     (map->ProcessResult
      (merge
       res
       {:cmd cmd
        :cmd-source (slurp scriptfile)
        :out-file (str stdoutfile)
        :err-file (str stderrfile)})))))

(defn run [opts]
  (assoc opts
         :process
         (merge
          (:process opts)
          (exec
           (-> opts :process :program)
           (-> opts :process :args)
           (-> opts :process :scriptfile)
           (-> opts :process :cwd)))))
