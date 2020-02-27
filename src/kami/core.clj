(ns kami.core)

(def board [[:r :r :b :r :b]
						[:r :r :r :r :r]
						[:b :b :b :r :b]
						[:r :r :r :r :r]
						[:b :b :b :r :b]])

(defn get-neg [[x y]]
 [[(inc x) y]
	[(dec x) y]
	[x (inc y)]
	[x (dec y)]])

(defn get-same-neighbour
 [point board color]
 (filter #(= color (get-in board %))
				 (get-neg point)))

(defn fill-board
 ([board cp] (fill-board board cp [] []))
 ([board cp pnc pc]
	(let [all-pc (concat pc [cp])
				all-pnc (->> (get-in board cp)
										 (get-same-neighbour cp board)
										 (remove (set (concat pnc pc)))
										 (concat pnc))]
	 (if (zero? (count all-pnc))
		all-pc
		(fill-board board (first all-pnc) (rest all-pnc) all-pc)))))

(fill-board board [1 1])

(defn fill
 [board new-color old-color x y]
 (if (= (get-in board [x y]) old-color)
	board
	(-> board
			(update-in [x y] new-color)
			(fill new-color old-color x (dec y))
			(fill new-color old-color x (inc y))
			(fill new-color old-color (dec x) y)
			(fill new-color old-color (inc x) y))))