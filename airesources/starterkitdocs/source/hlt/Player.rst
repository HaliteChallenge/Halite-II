.. java:import:: java.util Map

.. java:import:: java.util TreeMap

Player
======

.. java:package:: hlt
   :noindex:

.. java:type:: public class Player

Constructors
------------
Player
^^^^^^

.. java:constructor:: public Player(short id)
   :outertype: Player

Methods
-------
addShip
^^^^^^^

.. java:method:: public void addShip(long shipId, Ship ship)
   :outertype: Player

getId
^^^^^

.. java:method:: public short getId()
   :outertype: Player

getShip
^^^^^^^

.. java:method:: public Ship getShip(long entityId)
   :outertype: Player

getShips
^^^^^^^^

.. java:method:: public Map<Long, Ship> getShips()
   :outertype: Player

