(ns runbld.fs)

(defprotocol FileSystem
  (fs-mountpoint   [_] "/dev/md0")
  (fs-type         [_] "xfs")
  (fs-bytes-total  [_])
  (fs-bytes-used   [_])
  (fs-bytes-free   [_])
  (fs-percent-used [_])
  (fs-percent-free [_])
  )
