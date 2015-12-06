(ns inspire.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :refer [<! chan put!]]
            [goog.date.DateTime :as dt]
            [pushy.core :as pushy]
            [goog.i18n.DateTimeFormat :as dtf]))

(enable-console-print!)


(defn set-location-hash! [h] (set! (-> js/window .-location .-hash) h))

(defn get-location-hash [] (-> js/window .-location .-hash (subs 1)))


(defn format-date
  [ts]
  (.format (goog.i18n.DateTimeFormat. 1)
           (js/Date. ts)))

(def db (sorted-map-by >))


(def sample-timelines {"jvuillermet" {1423094400000 {:title "Einstein: His Life and Universe", :media :book :link "www.amazon.com/Einstein-Life-Universe-Walter-Isaacson/dp/1442348062" :comment "How hard it was even for someone as smart as him ..."}
                                      1399680000000 {:title "Rich Hickey - Simple Made Easy", :media :video :link "http://www.infoq.com/presentations/Simple-Made-Easy" :refs [["zucker" 293939]]}
                                      1361318400000 {:title "Brett Victor - Inventing on Principle", :media :video :link "https://vimeo.com/36579366"}}
                       "brettfan" {1399680000000 {:title "Brett Victor - Inventing on Principle" :media :video :link "https://vimeo.com/36579366"}
                                   1361318400000 {:title "Rich Hickey - Simple Made Easy", :media :video :link "http://www.infoq.com/presentations/Simple-Made-Easy" :refs [["zucker" 293939]]}}
                       "randomco" {1399680000000 {:title "Brett Victor Random - Inventing on Principle" :media :video :link "https://vimeo.com/36579366"}
                                   1361318400000 {:title "Rich Hickey Random - Simple Made Easy", :media :video :link "http://www.infoq.com/presentations/Simple-Made-Easy" :refs [["zucker" 293939]]}}})

(defn with [db data] (merge db data))


(defonce app-state (atom {:show "jvuillermet" :timelines sample-timelines}))
(set! (.-onhashchange js/window) #(swap! app-state assoc :show (get-location-hash)))

(defn related [timelines link origin]
  (keep (fn [[u tl]]
          (when (and (not (= u origin)) (first (filter #(= (:link (second %)) link) tl)))
            u))
        timelines))

(defn current-user [] (:show @app-state))
(defn set-current-user! [user] (swap! app-state assoc :show user))

(defn get-val [id]
  (.-value (. js/document (getElementById id))))

(defn handle-submit [ev steps inspiration step input]
  (.preventDefault ev)
  (swap! inspiration assoc (first (step)) @input)
  (reset! input nil)
  (when (= (count @inspiration) (count steps))
    (swap! app-state assoc-in [:timelines (current-user) (.now js/Date)] (assoc @inspiration :media (keyword (:media @inspiration))))
    (reset! inspiration {})))

(defn add-entry []
  (let [steps [[:link "Add new inspirational link"] [:title "What's the title"] [:media "Is this a video, book or article"]
               [:comment "Optionally comment how that inspire you"]]
        inspiration (atom {})
        input (atom "")
        step #(get steps (count @inspiration))]
    (fn []
      [:form.ui.form.add-entry
        {:onSubmit #(handle-submit % steps inspiration step input)}
        [:span.add-input
          [:input#input.link {:type "text" :value @input :onChange #(reset! input (-> % .-target .-value)) :placeholder (second (step))}]
          (when-not (clojure.string/blank? @input)
            [:button.next.show.icon-right {:onClick #(handle-submit % steps inspiration step input)}])]])))

(defn header []
  [:header
    [:button.join "inspire others"]
    [:img.avatar {:src "https://pbs.twimg.com/profile_images/2141577891/186037_585958959_5778360_n_bigger.jpg"}]
    [:h1 (str "@" (current-user))]])

(defn timeline [user show? anim switch!]
  [:li {:className (str (when show? " selected ") anim)}
    (doall
      (for [[idx [date {:keys [title comment link media]}]] (map-indexed vector (with db (get-in @app-state [:timelines user])))]
        ^{:key date}
        [:div.cd-timeline-block
          [:div.cd-timeline-img {:className (str "cd-" (name media))}
            [:img {:src (str "image/" (name media) ".svg")}]]
          [:div.cd-timeline-content
            [:a {:href link :target "_blank"} [:h2 title user]]
            (when comment [:p comment])
            (let [related (related (:timelines @app-state) link user)]
              (when-not (empty? related)
                [:div.inspired
                  (doall (for [ref related]
                            ^{:key ref} [:span.cd-read-more {:onClick #(switch! ref (even? idx)) :href (str "#" ref)} ref]))]))
            [:span.cd-date (format-date date)]]]))])

(defn timelines []
  (let [next (atom nil) current (atom (current-user))
        animating (atom nil)
        flip (atom true)
        on-switch (fn [u direction]
                    (js/setTimeout
                      #(reset! animating nil)
                      500)
                    (reset! (if @flip next current) u)
                    (set-current-user! u)
                    (swap! flip not)
                    (reset! animating direction) 2)
        direction #(case [% %2] [false false] "right" [false true] "left" [true false] "left" [true true] "right")
        animation #(and (not (nil? @animating)) (str (if % "enter-" "leave-") (direction % @animating)))]
    (fn []
      (prn @current)
      (prn @next)
      [:section#cd-timeline.cd-container.cd-horizontal-timeline
        [:div.events-content
          [:ol
            [timeline @current @flip (animation @flip) on-switch]
            [timeline @next (not @flip) (animation (not @flip)) on-switch]]]])))


(defn entry []
  [:span
    [header]
    [add-entry]
    [timelines]])

; (defn entry []
;   (let [next (atom nil) current (atom (current-user))
;         animating (atom nil)
;         flip (atom true)
;         on-switch (fn [u direction] (prn u direction) (js/setTimeout #(reset! animating nil) 500) (reset! next u) (swap! flip not) (reset! animating direction) 2)
;         direction #(case [% %2] [false false] "right" [false true] "left" [true false] "left" [true true] "right")
;         animation #(and (not (nil? @animating)) (str (if % "enter-" "leave-") (direction % @animating)))]
;     (fn []
;       [:div.cd-horizontal-timeline
;         [:div.events-content
;           [:ol
;             [:li {:className (str (when @flip " selected ") (animation @flip))}
;               [page @current on-switch]]
;             [:li {:className (str (when (not @flip) " selected ") (animation (not @flip)))}
;               [page @next on-switch]]]]])))

(reagent/render-component [entry]
                          (. js/document (getElementById "app")))


(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
