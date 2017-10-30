(ns hlt.math)

(defn rad->deg
  "Translates radians into degrees in the interval [0, 360)."
  [rad]
  (let [deg-unclipped (Math/round (Math/toDegrees rad))]
    ;; Clip to ensure value is in [0, 360), as required by the game engine.
    (int (mod deg-unclipped 360))))

(defprotocol Positionable
  "Positionable is either a location in the game map, or an entity with a
  location in the game map."
  (get-x [this] "Returns the center x-coordinate for this object")
  (get-y [this] "Returns the center y-coordinate for this object"))

(defrecord Position [x y]
  Positionable
  (get-x [_] x)
  (get-y [_] y))

(defn square [x]
  (* x x))

(defn distance-between
  "Returns the distance between the center of a and b."
  [a b]
  (let [dx (- (get-x a) (get-x b))
        dy (- (get-y a) (get-y b))]
    (Math/sqrt (+ (square dx)
                  (square dy)))))

(defn orient-towards
  "Returns the angle from `from` to `to` in radians."
  [from to]
  (let [dx (- (get-x to) (get-x from))
        dy (- (get-y to) (get-y from))]
    (+ (Math/atan2 dy dx)
       (* 2 Math/PI))))

(defn closest-point
  "Returns the closest safe point from `point` towards `goal`. The
  closest safe point is on the radius of `goal` plus an optioanl
  distance, which can be used to avoid collisions."
  ([point goal] (closest-point point goal 0.0))
  ([point goal safe-distance]
   (let [radius (+ (:radius goal) safe-distance)
         angle (orient-towards goal point)
         x (+ (get-x goal) (* radius (Math/cos angle)))
         y (+ (get-y goal) (* radius (Math/sin angle)))]
     (->Position x y))))

(defn segment-circle-intersects?
  "Returns true if the entity intersects with the line segment between p1 and p2,
  false otherwise. An optional fudge factor can be used to ensure a
  little padding around entities that may move, to reduce the
  probability of hitting into them."
  ([p1 p2 entity] (segment-circle-intersects? p1 p2 entity 0.0))
  ([p1 p2 entity fudge-factor]
   ;; Parameterize the segment as start + t * (end - start),
   ;; and substitute into the equation of a circle
   ;; Solve for t
   (let [fudged-radius (+ (:radius entity) fudge-factor)
         x1 (get-x p1)
         y1 (get-y p1)
         x2 (get-x p2)
         y2 (get-y p2)
         dx (- x2 x1)
         dy (- y2 y1)
         a (+ (square dx) (square dy))

         center-x (get-x entity)
         center-y (get-y entity)

         b (* -2 (+ (square x1) (- (* x1 x2))
                    (- (* x1 center-x)) (* center-x x2)
                    (square y1) (- (* y1 y2))
                    (- (* y1 center-y)) (* center-y y2)))]
     (if (== a 0.0)
       ;; start == end
       (<= (distance-between p1 entity) fudged-radius)
       ;; time along segment when closest to the circle (vertex of the quadratic)
       (let [t (min (/ (- b) (* 2 a)) 1.0)]
         (if (< t 0)
           false
           (let [closest-x (+ x1 (* dx t))
                 closest-y (+ y1 (* dy t))
                 closest-distance (distance-between (->Position closest-x closest-y)
                                                    entity)]
             (<= closest-distance fudged-radius))))))))
