Navigation
==========

.. java:package:: hlt
   :noindex:

.. java:type:: public class Navigation

Constructors
------------
Navigation
^^^^^^^^^^

.. java:constructor:: public Navigation(Ship ship, Entity target)
   :outertype: Navigation

Methods
-------
navigateToDock
^^^^^^^^^^^^^^

.. java:method:: public ThrustMove navigateToDock(GameMap gameMap, int maxThrust)
   :outertype: Navigation

navigateTowards
^^^^^^^^^^^^^^^

.. java:method:: public ThrustMove navigateTowards(GameMap gameMap, Position targetPos, int maxThrust, boolean avoidObstacles, int maxCorrections, int angularStep)
   :outertype: Navigation

