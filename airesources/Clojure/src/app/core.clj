(ns app.core)

(import [io.halite Direction GameMap InitPackage Location Move Networking Site])

(defn -main [& args]
	(let [iPackage (Networking/getInit) myID (.-myID iPackage) gameMap (.-gameMap iPackage)]
		(do
			(Networking/sendInit "ClojureBot")
			(while true
				(let [gameMap (Networking/getFrame)]

						(loop [y 0 moves #{}]
							(if (< y (.-height gameMap))

								(loop [x 0 newMoves #{}]
									(if (< x (.-width gameMap))
										(let [location (new Location x y) site (.getSite gameMap location)]
											(if (= (.-owner site) myID)
												(recur (inc x) (conj newMoves (new Move location Direction/NORTH)))
												(recur (inc x) moves)))
										(recur (inc y) (conj moves newMoves))))

								(Networking/sendFrame moves))))))))
