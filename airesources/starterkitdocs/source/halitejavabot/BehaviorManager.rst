.. java:import:: java.util Map

.. java:import:: java.util TreeMap

.. java:import:: java.util Vector

BehaviorManager
===============

.. java:package:: halitejavabot
   :noindex:

.. java:type:: public class BehaviorManager

Fields
------
behaviors
^^^^^^^^^

.. java:field::  Map<Long, Behavior> behaviors
   :outertype: BehaviorManager

Constructors
------------
BehaviorManager
^^^^^^^^^^^^^^^

.. java:constructor:: public BehaviorManager()
   :outertype: BehaviorManager

Methods
-------
isExecuting
^^^^^^^^^^^

.. java:method:: public boolean isExecuting(long shipId)
   :outertype: BehaviorManager

update
^^^^^^

.. java:method:: public void update(GameMap gameMap, Vector<Move> moves)
   :outertype: BehaviorManager

warpTo
^^^^^^

.. java:method:: public void warpTo(long shipId, Position target)
   :outertype: BehaviorManager

