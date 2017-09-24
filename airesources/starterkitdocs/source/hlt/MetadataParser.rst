.. java:import:: java.util ArrayList

.. java:import:: java.util LinkedList

MetadataParser
==============

.. java:package:: hlt
   :noindex:

.. java:type:: public class MetadataParser

Methods
-------
getShipList
^^^^^^^^^^^

.. java:method:: public static LinkedList<Ship> getShipList(short owner, LinkedList<String> shipsMetadata)
   :outertype: MetadataParser

newPlanetFromMetadata
^^^^^^^^^^^^^^^^^^^^^

.. java:method:: public static Planet newPlanetFromMetadata(LinkedList<String> metadata)
   :outertype: MetadataParser

parsePlayerId
^^^^^^^^^^^^^

.. java:method:: public static Short parsePlayerId(LinkedList<String> metadata)
   :outertype: MetadataParser

parsePlayerNum
^^^^^^^^^^^^^^

.. java:method:: public static Short parsePlayerNum(LinkedList<String> metadata)
   :outertype: MetadataParser

