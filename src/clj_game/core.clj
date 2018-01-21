(ns clj_game.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:plains {:desc "You are in a huge plain barren of life except for the waist-high grass.
              You remember nothing, but are dressed in nothing but clothes suitable for a peasent.
              You read a sign that states simply: Make the correct choices, or DIE. "
            :title "awaken.."
            :dir {:north :castle
                  :south :bog
                  :east :forest
                  :west :cave}
            :contents :literally-nothing
            :hidden "If you jump in every section, you might gain some knowledge by the end of this game. Maybe."}

   :bog {:desc "It's damp and gross here. You see something headed towards you, but it's too late! An alligator approaches. Run, or DIE "
            :title "enter the bog"
            :dir {:north :plains
                  :south nil
                  :east nil
                  :west nil}
            :contents #{}
            :hidden "There's always a bigger fish."}

   :castle {:desc "You walk up to the castle walls. The gate is locked, but there appears to be a key hole, if only you had the key... "
            :title "near the castle walls."
            :dir {:north :armory
                  :south :plains
                  :east nil
                  :west nil}
            :contents #{}
            :hidden "Wise man say: \"Forgiveness is divine, but never pay full price for late pizza.\""}

   :armory {:desc "It smells weird in here, but maybe there will be something in here to help you out in our next location... "
            :title " are in the armory"
            :dir {:north :dungeon
                  :south :castle
                  :east nil
                  :west nil}
            :contents :axe
            :hidden "There's usually armor here too. Guess this isn't too great of an armory."}

   :forest {:desc "Apart from a mildly dank smell and very large spiders, this seems to be just a normal forest. "
            :title "are enter the forest"
            :dir {:north nil
                  :south nil
                  :east nil
                  :west :plains}
            :contents #{}
            :hidden "What were you expecting to be here? Lembas bread? Elves?"}

   :dungeon {:desc "Upon arriving, it's really not as bad as you thought it'd be. There is a beam blocking the door to the tower though... How are you going to break that? "
            :title "left the armory, and are headed into the dungeon. "
            :dir {:north :tower
                  :south :armory
                  :east nil
                  :west nil}
            :contents #{}
            :hidden "Unfortunately, there's no Dragons, but THERE'S A TROLL. IN THE DUNGEON. I thought you'd ought to know. "}

   :tower {:desc "Collect the princess and move forward to win the game! "
           :title "are in the tower"
           :dir {:north :tower
                 :south :dungeon
                 :east nil
                 :west nil}
           :contents :princess
           :hidden "git gud"}

   :cave {:desc "It is very dark. You hear growling. It'd be best to leave before finding out what's making the noise. "
          :title "enter the cave"
          :dir {:north nil
                :south nil
                :east :plains
                :west nil}
          :contents :not-a-key
          :hidden "Infinity is a word used to describe our our incapability to comprehend the number of years our universe has left."}
   })

(def adventurer
  {:location :plains
   :inventory #{:lock :beam}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn breakbeam [player]
  (let [location (player :location)]
    (cond
      (contains? (-> player :inventory) :axe) (do (println "Speak hit and enter.") player)
      :else(do (println "You need to break the beam to go there!") player))))

(defn goodbyelock [player]
  (let [location (player :location)]
    (cond
      (contains? (-> player :inventory) :not-a-key) (do (println "Speak unlock and enter.") player)
      :else(do (println "You need a key to go there!") player))))

(defn winner [player]
  (println "Congrats! You won probably the most linear game in existence! Hope you had fun!")
  (. System exit 0))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (cond
      (nil? dest)(do (println "You can't go that way.") player)
      (and (= location :castle) (= dir :north)) (goodbyelock player)
      (and (= location :dungeon) (= dir :north)) (breakbeam player)
      (and (= location :tower) (= dir :north) (contains? (-> player :inventory) :princess)) (winner player)
      :else(assoc-in player [:location] dest))))

(defn turn-ctr [player]
  (update-in player [:turns] inc))

(defn pickup [player]
  (let [location (player :location)]
    (let [loot (-> the-map location :contents)]
      (update-in player [:inventory] #(conj % loot)))))


(defn show [player]
  (let [location (player :location)]
      (print (str "You have these items: " (-> player :inventory) ". "))
      (update-in player [:seen] #(conj % location))))

(defn EnDmYsUfFeRiNg [player]
  (println "You've chosen to end it all. GGWP.")
  (. System exit 0))

(defn mightaswell [player]
  (println "*jump*")
  (let [location (player :location)]
    (let [loot (-> the-map location :hidden)]
      (println loot)
      (update-in player [:seen] #(conj % location)))))


(defn hit [player]
  (let [location (player :location)]
  (cond
    (and (= location :dungeon) (contains? (-> player :inventory) :axe)) (and (do (println "*SMASH*") player) (assoc-in player [:location] :tower))
    :else(do (println "You need an axe to hit.") player))))

(defn unlock [player]
  (let [location (player :location)]
  (cond
    (and (= location :castle) (contains? (-> player :inventory) :not-a-key)) (and (do (println "*click*") player) (assoc-in player [:location] :armory))
    :else(do (println "You need a key to enter.") player))))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         [:forward] (go :north player)
         [:back] (go :south player)
         [:right] (go :east player)
         [:left] (go :west player)
         [:collect] (pickup player)
         [:show-inventory] (show player)
         (:or [:die] [:quit] [:q] [:exit]) (EnDmYsUfFeRiNg player)
         [:jump] (mightaswell player)
         [:hit] (hit player)
         [:unlock] (unlock player)

         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main
  "Let's hope this works..."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
