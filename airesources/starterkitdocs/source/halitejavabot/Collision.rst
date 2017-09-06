Collision
=========

.. java:package:: halitejavabot
   :noindex:

.. java:type:: public class Collision

Methods
-------
segmentCircleTest
^^^^^^^^^^^^^^^^^

.. java:method:: public static boolean segmentCircleTest(Position start, Position end, Position center, double radius, double fudge)
   :outertype: Collision

   Test whether a given line segment intersects a circular area.

   :param start: The start of the segment.
   :param end: The end of the segment.
   :param center: The center of the circular area.
   :param radius: The radius of the circular area.
   :param fudge: An additional safety zone to leave when looking for collisions.
   :return: true if the segment intersects, false otherwise

