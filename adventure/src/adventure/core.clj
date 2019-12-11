(ns adventure.core
  (:gen-class))
(require '[clojure.string :as str])

;; Game data and flavor text
(def init-map
  {:entrance {:desc "\nThe locked :door behind you, you peer into the dimly-lit interior. The walls are peeling; the air is heavy. Spiderwebs line the dusty staircase. There are heavy wooden doors on both sides and a dark corridor in front of you."
              :title "in the entrance way"
              :dir {:north :kitchen, :up :hallway, :west :study, :east :workshop}
              :contents #{:door}}
   :study {:desc "\nAs the door slowly creaks open, you find yourself in an old study. Shelves full of moldy books line the walls. There is a desk with a :letter on it."
           :title "in the old study"
           :dir {:east :entrance}
           :contents #{:letter}}
   :workshop {:desc "\nThere are wooden tables and tools scattered all around the room. It must be a workshop. A :crumpled-paper by your feet catches your eye."
              :title "in the workshop"
              :dir {:west :entrance}
              :contents #{:crumpled-paper}}
   :kitchen {:desc "\nThe hall opens up into a decrepit but recognizable kitchen. A flickering light is cast from the living room to your right. Nearby, fragile-looking stairs lead into darkness. Your skin crawls upon seeing the dead :mice on the dining table."
             :title "in the kitchen area"
             :dir {:south :entrance, :east :living-room, :down :basement}
             :contents #{:mice}}
   :living-room {:desc "\nThe flickering light seems to be coming from the fire in the dusty :fireplace. It's open flames quiver slowly and sadly."
                 :title "in the living room"
                 :dir {:west :kitchen, :down :basement}
                 :contents #{:fireplace}}
   :basement {:desc "\nAs you descend the shaky stairs, you hear a weak snarling. When you reach the bottom, your eyes--trained to the darkness by now--lock on to two red-hot, glowing eyes. As you tense up, you realize it is just a very thin :dog chained to a stake. It is quietly gnawing on what looks to be a :crowbar."
              :title "in the basement with the dog"
              :dir {:up :kitchen}
              :contents #{:dog :crowbar}}
   :guestroom {:desc "\nA small empty bed sits in the corner. A dresser and wardrobe rest against the southern wall. A full-length, but cracked :mirror is hung alongside them."
               :title "in the room with the cracked mirror"
               :dir {:east :hallway}
               :contents #{:mirror}}
   :hidden {:desc "\nYou crawl through the tight opening. You find yourself in an unbelievably stuffy small closet space. As you grope around, you bump a small, cold piece of metal which falls to the floor with startling thunk. Could that be a :key?"
            :title "in the hidden space"
            :dir {:north :guestroom}
            :contents #{:key}}
   :master {:desc "\nThe king size bed in the middle of the room indicates this is probably the master bedroom. You don't notice anything strange. Until you spot the :skeleton tucked away under the covers. Its skull looks like it would crumble into dust at the slightest touch."
            :title "in the master bedroom"
            :dir {:west :hallway}
            :contents #{:skeleton}}
   :bathroom {:desc "\nIt's a bathroom. You could go right now... but maybe you should hold it in. The :toilet lid is down; who knows what's in there..."
              :title "in the bathroom"
              :dir {:south :hallway}
              :contents #{:toilet}}
   :hallway {:desc "\nAt the top of the stairs is another hallway. The walls are faded and reek of mildew. Two rooms are open on the sides, and another one is closed at the end of the hallway."
             :title "standing in the hallway"
             :dir {:down :entrance, :east :master, :west :guestroom, :north :bathroom}
             :contents #{}}})

(def init-adventurer
  {:location :entrance
   :inventory #{}
   :tick 0
   :seen #{}
   :age (-> 50 rand-int (+ 15))
   :gender (nth '(:male :female) (rand-int 2))
   :cleared-dog :false})

(def items
  {:key {:desc "This key has very sharp teeth. Maybe this is just what you need!"
         :name "a strangely shaped key"}
   :mice {:desc "These mice seem to have died not too long ago. Maybe if prepared them properly somehow..."
          :name "three dead mice"}
   :cooked-mice {:desc "The dead mice you found have been well-cooked. The fur is quite charred."
                 :name "three cooked mice"}
   :crowbar {:desc "A warped but sturdy crowbar. You could probably pry off anything you wanted with this."
             :name "a bent crowbar"}
   :skeleton {:desc "How long has this person been dead? Who knows. Their bones make you uneasy."
              :name "a skeleton"}})

(def room-items
  {:door "It looks like the door out."
   :letter "You read the letter. It is very sad; it seems the author of the letter loved his dog a lot, and often fed it well-done steaks."
   :crumpled-paper "Opening up the wadded ball, you find a strange drawing of a hatch and a mirror as well as several measurements and calculations. The paper is crawling with bugs, so you put it back."
   :fireplace "The flames flicker. They are definitely real. And hot."
   :dog "The dog glares at you. Its jaws and paws are guarding the crowbar as if its life depended on it."
   :mirror "The cracked mirror is affixed firmly to the wall."
   :toilet "Carefully peeking into the toilet, you are met with... an empty bowl. There is nothing."})

(def tick-triggers
  {10 "\nYou hear what sound like footsteps. Out of a corner of your eye, you spot a shadow shivering slightly before becoming still.\n"
   25 "\nAn ominous presence is filling the air. You feel like you're suffocating. Better get out quickly.\n"
   45 "\nAll your hairs are standing on end. You feel extremely cold. Your heart is beating uncontrollably. You're drenched in sweat.\n"
   50 "\nSCREEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE!!\n"})

; Provided by Piazza instructions
(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (when-not ((get-in state [:adventurer :seen]) location)
      (print (-> the-map location :desc)))
    (print (str "\nYou are " (-> the-map location :title) ". "))
    (update-in state [:adventurer :seen] #(conj % location))))

(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (do (println "Walking...")
          (assoc-in state [:adventurer :location] dest)))))

; From interaction mp
(defn canonicalize
  "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
  [input]
  (map keyword (-> input str/lower-case
                         (str/replace #"[?!.]" "")
                         (str/split #" +"))))

(defn match [pattern input]
  (loop [pattern pattern
         input input
         vars '()]
    (cond (and (empty? pattern) (empty? input)) (reverse vars)
          (or (empty? pattern) (empty? input)) nil
          (= (first pattern) "@") (recur (rest pattern)
                                         (rest input)
                                         (cons (first input) vars))
          (= (first pattern) (first input)) (recur (rest pattern)
                                                   (rest input)
                                                   vars)
          :fine-be-that-way nil)))

;; REPL functions
(def dir-shortcut {:n :north, :s :south, :e :east, :w :west, :u :up, :d :down})

(defn convert-dir-shortcut [dir]
  (if (contains? dir-shortcut dir) (dir-shortcut dir) dir))

(defn move-rooms [state dir]
  (go state (convert-dir-shortcut dir)))

(defn quit-game [state]
  (do (println "Exiting. Hope you enjoyed the game!\n")
      (System/exit 0)))

(defn __take-item [state item]
  (let [new-state (update-in state [:adventurer :inventory] conj item)
        curr-loc (get-in state [:adventurer :location])]
    (update-in new-state [:map curr-loc :contents] disj item)))

(defn take-item [state item]
  (let [curr-loc (get-in state [:adventurer :location])
        room-contents (get-in state [:map curr-loc :contents])
        get-crow (get-in state [:adventurer :cleared-dog])]
    (if (contains? room-contents item)
        (if (contains? (:items state) item)
          (if (and (= item :crowbar) (= get-crow :false))
            (do (println "The dog won't let you have it. Is there any way to distract him?") state)
            (do (println (str "You picked up " item)) (__take-item state item)))
          (do (println "You cannot pick up that object.") state))
        (do (println "Either the item is not in the room or is unnoteworthy.")
            state))))

(defn __drop-item [state item]
  (let [curr-loc (get-in state [:adventurer :location])
        new-state (update-in state [:map curr-loc :contents] conj item)]
    (update-in new-state [:adventurer :inventory] disj item)))

(defn drop-item [state item]
  (let [inventory-items (get-in state [:adventurer :inventory])]
    (if (contains? inventory-items item) (do (println (str "You dropped " item)) (__drop-item state item))
        (do (println "You do not have that item.") state))))

(defn look-room [state]
  (let [loc (get-in state [:adventurer :location])
        desc (get-in state [:map loc :desc])]
    (do (println desc)
        state)))

(defn examine-item [state item]
  (let [curr-loc (get-in state [:adventurer :location])
        room-contents (get-in state [:map curr-loc :contents])
        inventory-items (get-in state [:adventurer :inventory])]
     (do
       (cond (contains? room-contents item) (if (contains? (:items state) item) (println (get-in (:items state) [item :desc]))
                                                                                (println ((:room-items state) item)))
             (contains? inventory-items item) (println (get-in (:items state) [item :desc]))
             :else (println "Either the item is not present or is unnoteworthy. Please check spelling."))
       state)))

(defn print-inventory [state]
  (let [inventory (get-in state [:adventurer :inventory])]
    (if (empty? inventory)
      (do (println "Your inventory is empty.")
          state)
      (do (println "You have the following in your inventory:")
          (doseq [i inventory] (println (str " + " (:name ((:items state) i)))))
          state))))

(defn end-game [result]
  (if (= result :win) (println "The key fits! You open the door and dash outside. Yay you won! Hope you enjoyed the game :)")
                      (println "Suddenly, you feel the ominous presence behind you. Without a sound, a heavy blow knocks you out of existence. You died :("))
  (System/exit 0))

(defn cook-mice [state]
  (let [state1 (update-in state [:adventurer :inventory] disj :mice)]
    (do (println "You gently cook the mice in the flames. You now have three :cooked-mice.")
        (update-in state1 [:adventurer :inventory] conj :cooked-mice))))

(defn feed-dog [state]
  (let [state1 (assoc-in state [:adventurer :cleared-dog] :true)]
    (do (println "You offer the poor dog the mice. The thin, old canine drops the crowbar and picks up the mice, sauntering over to a corner.")
        (assoc-in state1 [:room-items :dog] "The dog is lying down over in the corner. It looks very sad."))))

(defn open-mirror [state]
  (let [age (get-in state [:adventurer :age])
        gender (get-in state [:adventurer :gender])
        child (gender {:male :boy, :female :girl})
        new-state (assoc-in state [:map :guestroom :dir :south] :hidden)]
    (do (println (str "You pry off the mirror, but looking closely you realize with a jump that a 5 year old " child " is looking back at " age "-year-old you. Creepy. You put the mirror down. You see a dusty opening."))
        (assoc-in new-state [:room-items :mirror] "The cracked mirror is leaning against the wall, next to the hidden opening."))))

(defn attempt-use-item [state item]
  (let [room (get-in state [:adventurer :location])]
    (cond (and (= item :key) (= room :entrance)) (end-game :win)
          (and (= item :mice) (= room :basement)) (do (println "The dog seems interested, but it doesn't move.") state)
          (and (= item :mice) (= room :living-room)) (cook-mice state)
          (and (= item :cooked-mice) (= room :basement)) (feed-dog state)
          (and (= item :crowbar) (= room :guestroom)) (open-mirror state)
          (and (= item :skeleton) (= room :basement)) (do (println "The dog is not interested. A dusty bone isn't too different from a crowbar.") state)
          :else (do (println "Hmmm, I don't think that will work.") state))))

(defn use-item [state item]
  (let [inventory-items (get-in state [:adventurer :inventory])]
    (if (contains? inventory-items item) (attempt-use-item state item)
        (do (println "You do not have that item") state))))

(defn move-shortcuts [state shortcut]
  (cond (or (= shortcut :n) (= shortcut :north) (= shortcut :e) (= shortcut :east)
            (= shortcut :s) (= shortcut :south) (= shortcut :w) (= shortcut :west)
            (= shortcut :u) (= shortcut :up) (= shortcut :d) (= shortcut :down))
        (move-rooms state shortcut)
        :else (do (println "Command not understood. Refer to command list.") state)))

(defn instructions [state]
  (println
    "\n\nInstructions:\n\tYou are random person suddenly teleported into a strange house. Oh no."
    "\n\tYou need to escape before something ... terrible happens to you."
    "\n\tInteractable items are denoted with a colon (e.g. :door)."
    "\n\tOmit the colon when inputting commands, but include dashes."
    "\n\tSome items can only be examined and not picked up."
    "\nCommands: [] denote shortcuts, % denotes a word\n\tgo % - movement, e.g. go [n]orth; only six directions allowed: north, south, east, west, up, down"
    "\n\t[i]nventory - check your inventory"
    "\n\t[l]ook - print long descriptor for room"
    "\n\t[e]xamine % - print description of item"
    "\n\t[t]ake % - pick up item if possible"
    "\n\t[d]rop % - removes item from inventory"
    "\n\t[u]se % - uses item if possible"
    "\n\t[h]elp - prints these instructions"
    "\n\t[q]uit / exit - quits the game\n")
  state)

(def command-set [ [:go "@"] move-rooms
                   [:quit] quit-game [:exit] quit-game [:q] quit-game
                   [:take "@"] take-item [:t "@"] take-item
                   [:drop "@"] drop-item [:d "@"] drop-item
                   [:look] look-room [:l] look-room
                   [:examine "@"] examine-item [:e "@"] examine-item
                   [:inventory] print-inventory [:i] print-inventory
                   [:use "@"] use-item [:u "@"] use-item
                   [:help] instructions [:h] instructions
                   ["@"] move-shortcuts])

(defn update-tick [state func]
  (update-in state [:adventurer :tick] func))

(defn respond [state command]
  "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
  If there is no match, return the original state and result \"I don't know what you mean.\""
  (loop [idx 0]
    (cond (>= idx (count command-set)) (do (println "Instructions not understood. Make sure syntax is correct.") state)
          :else (if-let [vars (match (command-set idx) command)]
                  (apply (command-set (inc idx)) (cons (update-tick state inc) vars))
                  (recur (+ idx 2))))))

(defn intro []
  (println "Welcome! This is the final project for CS 296-25 FA19 at UIUC by Maxwell Jong."
           "\nI hope you enjoy this little puzzle text-adventure game!")
  (instructions nil))

(defn check-tick [state]
  (let [tick (get-in state [:adventurer :tick])
        message (tick-triggers tick)]
    (if (some? message) (println message))
    (if (= tick 50) (end-game :lose))))

; Main function - provided by Piazza instructions
(defn -main
  "Initialize the adventure"
  [& args]
  (do (intro)
   (loop [local-state {:map init-map :adventurer init-adventurer :items items :room-items room-items}]
     (let [pl (status local-state)
           _  (print "\n > ")
           __ (flush)
           command (read-line)]
       ;(println (get-in local-state [:adventurer :tick]))
       (check-tick local-state)
       (recur (respond pl (canonicalize command)))))))
