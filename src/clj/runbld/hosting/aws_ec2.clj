(ns runbld.hosting.aws-ec2
  (:require [runbld.schema :refer :all]
            [schema.core :as s]
            [slingshot.slingshot :refer [try+ throw+]])
  (:require [clj-http.client :as http]
            [runbld.hosting :refer [HostingProvider] :as hosting]))

(s/defn region-name
  "Given us-east-1c, we want us-east-1."
  ([az]
   (when-let [[_ reg _] (re-find #"(.+-.+-[0-9]+)([a-z]+)" az)]
     reg)))

(s/defn this-host?
  "Is this host in AWS EC2?"
  ([]
   (if-let [res (try+
                 (http/get "http://169.254.169.254/latest/meta-data/"
                           {:socket-timeout 500 :conn-timeout 500})
                 (catch org.apache.http.conn.ConnectTimeoutException e
                   false))]
     (boolean res))))

(defrecord AwsEc2Hosting [facts]
  HostingProvider
  (datacenter    [x]
    (-> x .facts :ec2_metadata :placement :availability-zone))

  (image-id      [x]
    (-> x .facts :ec2_metadata :image-id))

  (instance-id   [x]
    (-> x .facts :ec2_metadata :instance-id))

  (instance-type [x]
    (-> x .facts :ec2_metadata :instance-type))

  (hosting-provider [x]
    "aws-ec2")

  (region        [x]
    (region-name (hosting/datacenter x))))

(s/defn make
  ([facts]
   (AwsEc2Hosting. facts)))
