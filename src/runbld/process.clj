(ns runbld.process
  (:require [runbld.util.date :as date]))

(defn run [scriptfile]
  (let [cmd ["bash" "-x" scriptfile]
        pb (doto (ProcessBuilder. cmd)
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
     :start-millis start
     :time-start (date/ms-to-iso start)
     :end-millis end
     :time-end (date/ms-to-iso end)
     :took (- end start)
     :status exit-code}))
