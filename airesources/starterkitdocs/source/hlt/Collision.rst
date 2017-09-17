Collision
=========

.. java:package:: hlt
   :noindex:

.. java:type:: public class Collision

Methods
-------
segmentCircleIntersect
^^^^^^^^^^^^^^^^^^^^^^

.. java:method:: public static boolean segmentCircleIntersect(Position start, Position end, Entity circle, double fudge)
   :outertype: Collision

   Test whether a given line segment intersects a circular area.

   :param start: The start of the segment.
   :param end: The end of the segment.
   :param circle:: The circle to test against.
   :param fudge: An additional safety zone to leave when looking for collisions. (Probably set it to ship radius 0.5)
   :return: true if the segment intersects, false otherwise

square
^^^^^^

.. java:method:: public static double square(double num)
   :outertype: Collision

