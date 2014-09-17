(ns tamarack.components.sidebar
  (:require [om.core :as om :include-macros true]
            [clojure.string :as string]
            [sablono.core :as html :refer-macros [html]]))

(defn- all-apps-items []
  (html
   [:ol
    [:li.active
     [:a {:href "#"} "Applications"]]]))

(defn- app-items [view app]
  (html
   [:ol.outer
    [:li
     [:a {:href "#"} "Applications"]
     [:ol
      [:li.active
       [:a {:href (string/join "/" [ "#/applications" (:name app)])} "Dashboard"]]]]]))

(defn- app-endpoint-items [view app endpoint]
  (html
   [:ol.outer
    [:li
     [:a {:href "#"} "Applications"]
     [:ol.outer
      [:li
       [:a {:href (string/join "/" [ "#/applications" (:name app)])} "Dashboard"]
       [:ol
        [:li.active
         [:a {:href (string/join "/" [ "#/applications" (:name app) app])}
          "Endpoint"]]]]]]]))

(defn component [app owner]
  (reify
    om/IDisplayName
    (display-name [_] "Sidebar")

    om/IRender
    (render [_]
      (case (:view app)
        :all-apps (all-apps-items)

        :app-dashboard
        (app-items (:view app) (:current-app app))

        :app-endpoint-overview
        (app-endpoint-items (:view app) (:current-app app) (:current-endpoint app))
        
        (html [:ol])))))
