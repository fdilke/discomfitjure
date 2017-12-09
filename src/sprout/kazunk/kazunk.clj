(ns sprout.kazunk.kazunk
  (:use twitter oauth.client)
)

'  (:require [twitter :as twitter]
            [oauth.client :as oauth])

;; Make a OAuth consumer
(def oauth-consumer
  (oauth.client/make-consumer "WEnu4lnTW6JUJBvsqj3mYA" ;; consumer key
                       "JqLuo4RVin66TJiWgrd4iDnw13pWlb5wYGYMhaIrg" ;; consumer secret
                       "https://api.twitter.com/oauth/request_token"
                       "https://api.twitter.com/oauth/access_token"
                       "https://api.twitter.com/oauth/authorize"
                       :hmac-sha1))

(def oauth-access-token
  "21803358-ZMPPU3ZO293r5GexkDPkU6H9EtqfJHnx4qVYkgVHv")
(def oauth-access-token-secret
  "R0Ee6IraIaQZJmwDJYocC1iuhFXdWBPJlMhoDVgMo")

(defn latest-tweets [count]
  (twitter/with-https
    (twitter/with-oauth
      oauth-consumer
      oauth-access-token
      oauth-access-token-secret
      (twitter/home-timeline :count (str count)))))

(defn name-to-id [name]
  (:id_str ((twitter/lookup-users-by-name name) 0))
)

(defn id-to-name [id]
   (:screen_name  ((twitter/lookup-users-by-id id) 0))
)

(defn twitter-followers [name]
  (map (comp id-to-name str) (twitter/followers-of-name name))
)


