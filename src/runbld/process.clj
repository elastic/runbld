(ns runbld.process
  (:require [runbld.util.date :as date]
            [runbld.util.io :as io]))

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
      {:start-millis start
       :time-start (date/ms-to-iso start)
       :end-millis end
       :time-end (date/ms-to-iso end)
       :took (- end start)
       :exit-code exit-code
       :status (if (pos? exit-code) "FAILURE" "SUCCESS")
       :out-bytes @out-bytes
       :err-bytes @err-bytes})))

(defn exec
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
     (merge
      res
      {:cmd cmd
       :cmd-source (slurp scriptfile)
       :out-file (str stdoutfile)
       :err-file (str stderrfile)}))))

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
