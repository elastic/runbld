(ns runbld.process
  (:require [runbld.util.date :as date]
            [runbld.util.io :as io]))

(defn exec
  ([program args scriptfile]
   (exec program args scriptfile (System/getProperty "user.dir")))
  ([program args scriptfile cwd]
   (let [scriptfile* (io/abspath scriptfile)
         dir (io/abspath-file cwd)
         cmd (flatten [program args scriptfile*])
         pb (doto (ProcessBuilder. cmd)
              (.directory dir)
              ;; Want the wrapping to be mostly transparent, so we send
              ;; stderr and stdout to the enclosing Java
              ;; environment. This also allows Jenkins console log to
              ;; work as expected.
              ;;
              ;; Later when we ship the output to S3 and ES we'll have
              ;; to multiplex the output from .getInputStream()
              ;; manually and won't be able to do this.
              ;;
              ;; http://docs.oracle.com/javase/8/docs/api/java/lang/Process.html#getInputStream--
              .inheritIO)
         start (System/currentTimeMillis)
         proc (.start pb)
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
      :status exit-code})))

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
