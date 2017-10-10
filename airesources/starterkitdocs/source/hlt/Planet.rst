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

.. java:constructor:: public Planet(int owner, int id, double xPos, double yPos, int health, double radius, int dockingSpots, int currentProduction, int remainingProduction, List<Integer> dockedShips)
   :outertype: Planet

Methods
-------
getCurrentProduction
^^^^^^^^^^^^^^^^^^^^

.. java:method:: public int getCurrentProduction()
   :outertype: Planet

getDockedShips
^^^^^^^^^^^^^^

.. java:method:: public List<Integer> getDockedShips()
   :outertype: Planet

getDockingSpots
^^^^^^^^^^^^^^^

.. java:method:: public int getDockingSpots()
   :outertype: Planet

getRemainingProduction
^^^^^^^^^^^^^^^^^^^^^^

.. java:method:: public int getRemainingProduction()
   :outertype: Planet

isFull
^^^^^^

.. java:method:: public boolean isFull()
   :outertype: Planet

isOwned
^^^^^^^

.. java:method:: public boolean isOwned()
   :outertype: Planet

toString
^^^^^^^^

.. java:method:: @Override public String toString()
   :outertype: Planet

