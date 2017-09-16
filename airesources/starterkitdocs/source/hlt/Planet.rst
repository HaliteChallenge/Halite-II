.. java:import:: java.util List

Planet
======

.. java:package:: hlt
   :noindex:

.. java:type:: public class Planet extends Entity

Constructors
------------
Planet
^^^^^^

.. java:constructor:: public Planet(Short owner, long id, double xPos, double yPos, short health, double radius, short dockingSpots, short currentProduction, short remainingProduction, List<Long> dockedShips)
   :outertype: Planet

Methods
-------
getCurrentProduction
^^^^^^^^^^^^^^^^^^^^

.. java:method:: public short getCurrentProduction()
   :outertype: Planet

getDockedShips
^^^^^^^^^^^^^^

.. java:method:: public List<Long> getDockedShips()
   :outertype: Planet

getDockingSpots
^^^^^^^^^^^^^^^

.. java:method:: public short getDockingSpots()
   :outertype: Planet

getRemainingProduction
^^^^^^^^^^^^^^^^^^^^^^

.. java:method:: public short getRemainingProduction()
   :outertype: Planet

isFull
^^^^^^

.. java:method:: public boolean isFull()
   :outertype: Planet

isOwned
^^^^^^^

.. java:method:: public boolean isOwned()
   :outertype: Planet

