.. java:import:: java.util LinkedList

Ship
====

.. java:package:: halitejavabot
   :noindex:

.. java:type:: public class Ship extends Entity

Constructors
------------
Ship
^^^^

.. java:constructor:: public Ship(short owner, LinkedList<String> shipMetadata)
   :outertype: Ship

Methods
-------
getDockedPlanet
^^^^^^^^^^^^^^^

.. java:method:: public long getDockedPlanet()
   :outertype: Ship

getDockingProgress
^^^^^^^^^^^^^^^^^^

.. java:method:: public short getDockingProgress()
   :outertype: Ship

getDockingStatus
^^^^^^^^^^^^^^^^

.. java:method:: public DockingStatus getDockingStatus()
   :outertype: Ship

getVelocity
^^^^^^^^^^^

.. java:method:: public Velocity getVelocity()
   :outertype: Ship

getWeaponCooldown
^^^^^^^^^^^^^^^^^

.. java:method:: public short getWeaponCooldown()
   :outertype: Ship

parseShips
^^^^^^^^^^

.. java:method:: static LinkedList<Ship> parseShips(short owner, LinkedList<String> shipsMetadata)
   :outertype: Ship

