(ns oblique-ui
  (:require [oblique.db :as db]
            [oblique.vclock :as clock]
            [io.github.humbleui.app :as app]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [io.github.humbleui.signal :as signal]
            [io.github.humbleui.window :as window])
  (:import
   [io.github.humbleui.jwm.skija LayerMetalSkija]
   [io.github.humbleui.skija ColorSpace]))

;; https://grayscale.design/app?lums=93.87,82.28,68.67,49.10,40.20,32.78,13.29,9.31,6.30,2.62,1.30,0.52&palettes=%23ffd804,%232ac42a,%232a94bd,%23db5f5f,%23000000&filters=0%7C0,0%7C0,0%7C0,0%7C0,0%7C0&names=,,,,&labels=,,,,

(defonce *state
  (signal/signal {}))

(defn empty-queue
  []
  (clojure.lang.PersistentQueue/EMPTY))

(defn first-n-chars
  [n uuid]
  (subs (str uuid) 0 n))

(defn min-chars-for-unique
  [uuids]
  (loop [n 1]
    (let [first-chars (->> uuids
                           (map #(first-n-chars n %))
                           (distinct))]
      (if (= (count uuids) (count first-chars))
        n
        (recur (inc n))))))

(def *eid-pool (signal/signal #{}))

(def *n-chars (signal/signal (min-chars-for-unique @*eid-pool)))

(defn new-eid
  []
  (let [eid (random-uuid)]
    (swap! *eid-pool conj eid)
    eid))

(defn init-state
  []
  (let [id1 (random-uuid)
        id2 (random-uuid)
        id3 (random-uuid)
        id4 (random-uuid)]
    (reset! *eid-pool #{})
    (reset! *state {1 {:id id1
                       :hid 1
                       :color 0xff37a6d3
                       :clock (clock/init id1)
                       :online true
                       :db (db/empty-db db/schema)
                       :send-queue (empty-queue)
                       :receive-queue (empty-queue)}
                    2 {:id id2
                       :hid 2
                       :color 0xffc12b2b
                       :clock (clock/init id2)
                       :online true
                       :db (db/empty-db db/schema)
                       :send-queue (empty-queue)
                       :receive-queue (empty-queue)}
                    3 {:id id3
                       :hid 3
                       :clock (clock/init id3)
                       :color 0xfff6d200
                       :online true
                       :db (db/empty-db db/schema)
                       :send-queue (empty-queue)
                       :receive-queue (empty-queue)}
                    4 {:id id4
                       :hid 4
                       :clock (clock/init id4)
                       :color 0xff26b326
                       :online true
                       :db (db/empty-db db/schema)
                       :send-queue (empty-queue)
                       :receive-queue (empty-queue)}})))

(defn validate-opvec
  [opvec]
  (let [[o e a & args] opvec]
    (assert (uuid? e))
    (assert (keyword? a))
    (assert (contains? db/schema a))))

(defn transact-op!
  "Opvec should be of form:
   [op e (nillable a) & args]"
  [state peer-id opvec]
  (validate-opvec opvec)
  (swap! state
         (fn [s]
           (let [clock (get-in s [peer-id :clock])
                 next-clock (clock/inc clock)
                 [o e a & args] opvec
                 op (apply db/op o e a clock args)]
             (-> s
                 (update-in [peer-id :db] db/with-op op)
                 (update-in [peer-id :send-queue] conj op)
                 (assoc-in [peer-id :clock] next-clock))))))

(defn receive-op
  [peer op]
  (-> peer
      (update :db db/with-op op)
      (update :clock clock/receive (:c op))))

(defn rand-network-send
  [state]
  (let [online-peers (->> (vals state)
                          (filter :online)
                          (filter (fn [peer] (not-empty (:send-queue peer)))))
        sending-peer (when (seq online-peers) (rand-nth online-peers))]
    (if-let [op (peek (:send-queue sending-peer))]
      (-> state
          (update-in [(:hid sending-peer) :send-queue] pop)
          (update-vals (fn [peer] (if (and (map? peer) (not= (:id sending-peer) (:id peer)))
                                    (update peer :receive-queue conj op)
                                    peer))))
      state)))

(defn network-receive
  [state]
  (let [online-peer-ids (->> (vals state)
                             (filter :online)
                             (filter (fn [peer] (not-empty (:receive-queue peer))))
                             (map :id)
                             (into #{}))]
    (update-vals state (fn [peer] (if (and (map? peer) (contains? online-peer-ids (:id peer)))
                                    (let [op (peek (:receive-queue peer))]
                                      (-> (receive-op peer op)
                                          (update :receive-queue pop)))
                                    peer)))))

(defonce stop-network? (atom false))

(defn stop-network-loop
  [] 
  (reset! stop-network? true)
  (tap> :stopped-network))

(defn start-network-loop
  []
  (reset! stop-network? false)
  (let [network-loop (Thread. (fn send-loop [] (loop []
                                                 (try
                                                   (swap! *state rand-network-send)
                                                   (catch Exception e (tap> [:send-error e])))
                                                 (try
                                                   (swap! *state network-receive)
                                                   (catch Exception e (tap> [:receive-error e])))
                                                 (Thread/sleep 1000)
                                                 (if @stop-network?
                                                   :done
                                                   (recur)))))]
    (.start network-loop)
    (tap> :started-network)
    :started))

(defonce *window
  (atom nil))

(ui/defcomp box
  ([child]
   (box {} child))
  ([opts child]
   (ui/with-resources [border (paint/stroke 0x40000000 (ui/scaled 1))]
     (fn render
       ([child]
        (render {} child))
       ([opts child]
        (let [{:keys [height] :or {height 100}} opts]
          [ui/rect {:paint border}
           [ui/size {:height height}
            child]]))))))

(ui/defcomp clock-display
  [clock]
  (let [counters (:counters clock)]
    [ui/grid {:cols 2 :rows 4}
     (apply concat
            (for [pid (range 1 5)]
              [[ui/padding {:padding 2} [ui/label pid]]
               [ui/padding {:padding 2} [ui/label (get counters (get-in @*state [pid :id]) 0)]]]))]))



(ui/defcomp op-display
  [op]
  [ui/rect {:paint (paint/fill 0x80FFDB2C)
            :radius 10}
   [ui/padding {:padding 10}
    [ui/label
     (:op op)
     " "
     (subs (str (:e op)) 0 @*n-chars)
     " "
     (:a op)
     " "
     (:args op)]]])

(defn select-peer
  [side peer-id]
  (case side
    :left (swap! *state assoc :left peer-id)
    :right (swap! *state assoc :right peer-id)
    :else nil))

(ui/defcomp peer-display
  [peer-id]
  (let [peer-state (get @*state peer-id)
        send-queue (:send-queue peer-state)
        receive-queue (:receive-queue peer-state)
        color (:color peer-state)
        online? (signal/signal (get-in @*state [peer-id :online]))]
    [ui/clickable {:on-click (fn [event]
                               (when (= :primary (:button event))
                                 (if (= #{:shift} (:modifiers event))
                                   (select-peer :right peer-id)
                                   (select-peer :left peer-id))))}
     [ui/rect {:paint (paint/stroke 0xff000033 (ui/scaled 1)) :radius 5}
      [ui/padding {:padding 2}
       [ui/column {:gap 5}
        [ui/padding {:padding 5}
         [ui/row {:gap 5}
          [ui/clip {:radius 5} [ui/rect {:paint (paint/fill color)} [ui/size {:height 10 :width 10}]]]
          [ui/label {:font-weight :bold} "Peer " peer-id]
          [ui/label {:font-slant :italic} (subs (str (get-in @*state [peer-id :id])) 0 8)]]]
        [clock-display (:clock peer-state)]
        [ui/label (str "Send queue: " (count send-queue))]
        [ui/label (str "Receive queue: " (count receive-queue))]
        #_[ui/size {:width 20}
           [ui/switch {:value-off false :value-on true :on-change (fn [v] (swap! *state assoc-in [peer-id :online] v)) :*value online?}]]]]]]))

(ui/defcomp tabs
  [*tab & pairs]
  [ui/row
   (for [[i [label value]] (map-indexed vector pairs)]
     ^{:stretch 1}
     [ui/clickable {:on-click (fn [e] (when (= :primary (:button e))
                                        (signal/reset! *tab value)))}
      [ui/rect {:paint (if (= @*tab value)
                         (paint/fill 0xff79c4e1)
                         (paint/fill 0xffb8e0ef))
                :radius [(if (zero? i) 5 0)
                         (if (= (count pairs) (inc i)) 5 0)
                         (if (= (count pairs) (inc i)) 5 0)
                         (if (zero? i) 5 0)]}
       [ui/padding {:padding 10}
        [ui/align {:x :center}
         [ui/label label]]]]])])

(ui/defcomp transact-view
  [side]
  (fn [side]
    (let [state @*state
          peer-id (get state side)
          peer-state (get state peer-id)]
      [ui/button {:on-click (fn [_] (transact-op! *state peer-id [:reg/set (new-eid) :register :hello]))} "Transact!"])))

(ui/defcomp queues-view
  [side]
  (fn [side]
    (let [state @*state
          peer-id (get state side)
          peer-state (get state peer-id)]
      [ui/column {:gap 10}
       ^{:stretch 1} [ui/column {:gap 5}
                      [ui/label "Send queue"]
                      [ui/vscroll
                       [ui/column {:gap 2}
                        (for [op (:send-queue peer-state)]
                          [op-display op])]]]
       ^{:stretch 1} [ui/column {:gap 5}
                      [ui/label "Receive queue"]
                      [ui/vscroll
                       [ui/column {:gap 2}
                        (for [op (:receive-queue peer-state)]
                          [op-display op])]]]])))

(ui/defcomp entities-view
  [_]
  [ui/label "entities"])

(ui/defcomp detail
  [side]
  (let [*control (signal/signal :transact)]
    (fn [side]
      [ui/column {:gap 5}
       (if (get @*state side)
         [ui/label {:font-weight :bold} (str "Peer " (get @*state side))]
         [ui/label {:font-slant :italic} (if (= :right side)
                                           "Shift-click to select a peer"
                                           "Click to select a peer")])
       [tabs *control
        ["Transact" :transact]
        ["Queues" :queues]
        ["Entities" :entities]]
       (if (get @*state side)
         (case @*control
           :transact [transact-view side]
           :queues ^{:stretch 1} [queues-view side]
           :entities ^{:stretch 1} [entities-view side])
         [ui/label "No peer selected"])])))

(defn app
  []
  [ui/padding {:padding 10}
   [ui/row {:gap 10}
    ^{:stretch 1}
    [ui/column {:gap 5}
     ^{:stretch 1}
     [peer-display 1]
     ^{:stretch 1}
     [peer-display 2]
     ^{:stretch 1}
     [peer-display 3]
     ^{:stretch 1}
     [peer-display 4]]
    ^{:stretch 4}
    [ui/column {:gap 10}
     ^{:stretch 1}
     [ui/rect {:paint (paint/fill 0x33333333)} "Header"]
     ^{:stretch 4}
     [ui/row {:gap 5}
      ^{:stretch 1} [detail :left]
      ^{:stretch 1} [detail :right]]]]])

(defonce *app
  (atom nil))

(reset! *app
        (ui/default-theme {}
                          (ui/make [app])))



(defn start-app
  [& args]
  (init-state)
  (start-network-loop)
  ;; setup window
  (ui/start-app!
   (let [opts   {:title    "Oblique UI"
                 :screen   (:id (first (app/screens)))
                 :width    800
                 :height   800
                 :x        :center
                 :y        :center}
         window (ui/window opts *app)]
      ;; TODO load real monitor profile
     (when (= :macos app/platform)
       (set! (.-_colorSpace ^LayerMetalSkija (.getLayer window)) (ColorSpace/getDisplayP3)))
     (reset! *window window))))

(defn redraw!
  "Requests a redraw on the next available frame."
  []
  (some-> @*window window/request-frame))

(defn before-ns-unload
  [] 
  (stop-network-loop))

(defn after-ns-reload
  []
  (start-network-loop))

(comment
  (start-app)
  (redraw!)
  (init-state)

  @stop-network?

  *state)

