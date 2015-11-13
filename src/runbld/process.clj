(ns runbld.process
  (:require [runbld.util.date :as date]
            [runbld.util.io :as io]))

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
         start (System/currentTimeMillis)
         proc (.start pb)
         [out-bytes err-bytes] (io/spit-process
                                (.getInputStream proc)
                                (java.io.PrintWriter. stdoutfile)
                                (.getErrorStream proc)
                                (java.io.PrintWriter. stderrfile))
         exit-code (.waitFor proc)
         end (System/currentTimeMillis)]
     (flush)
     {:proc proc
      :cmd cmd
      :cmd-source (slurp scriptfile)
      :start-millis start
      :time-start (date/ms-to-iso start)
      :end-millis end
      :time-end (date/ms-to-iso end)
      :took (- end start)
      :exit-code exit-code
      :status (if (pos? exit-code) "FAILURE" "SUCCESS")
      :out-bytes @out-bytes
      :out-file (str stdoutfile)
      :err-bytes @err-bytes
      :err-file (str stderrfile)})))

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
