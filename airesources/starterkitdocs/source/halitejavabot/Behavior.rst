Behavior
========

.. java:package:: halitejavabot
   :noindex:

.. java:type:: public class Behavior

Constructors
------------
Behavior
^^^^^^^^

.. java:constructor:: public Behavior(long shipId, BehaviorType type, Position target, State state)
   :outertype: Behavior

Methods
-------
brake
^^^^^

.. java:method:: public Move brake(GameMap gameMap, double speed, double angle, int maxAcceleration)
   :outertype: Behavior

cancel
^^^^^^

.. java:method::  void cancel()
   :outertype: Behavior

isFinished
^^^^^^^^^^

.. java:method:: public boolean isFinished(GameMap gameMap)
   :outertype: Behavior

next
^^^^

.. java:method:: public Move next(GameMap gameMap)
   :outertype: Behavior

