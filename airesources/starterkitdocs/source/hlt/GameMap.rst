.. java:import:: java.util ArrayList

.. java:import:: java.util List

.. java:import:: java.util Map

.. java:import:: java.util TreeMap

.. java:import:: java.util Collections

.. java:import:: java.util Collection

.. java:import:: java.util LinkedList

GameMap
=======

.. java:package:: hlt
   :noindex:

.. java:type:: public class GameMap

Constructors
------------
GameMap
^^^^^^^

.. java:constructor:: public GameMap(short width, short height, short playerId)
   :outertype: GameMap

Methods
-------
getAllPlanets
^^^^^^^^^^^^^

.. java:method:: public Map<Long, Planet> getAllPlanets()
   :outertype: GameMap

getAllPlayers
^^^^^^^^^^^^^

.. java:method:: public List<Player> getAllPlayers()
   :outertype: GameMap

getAllShips
^^^^^^^^^^^

.. java:method:: public List<Ship> getAllShips()
   :outertype: GameMap

getHeight
^^^^^^^^^

.. java:method:: public short getHeight()
   :outertype: GameMap

getMyPlayer
^^^^^^^^^^^

.. java:method:: public Player getMyPlayer()
   :outertype: GameMap

getMyPlayerId
^^^^^^^^^^^^^

.. java:method:: public short getMyPlayerId()
   :outertype: GameMap

getPlanet
^^^^^^^^^

.. java:method:: public Planet getPlanet(long entityId)
   :outertype: GameMap

getShip
^^^^^^^

.. java:method:: public Ship getShip(short playerId, long entityId) throws IndexOutOfBoundsException
   :outertype: GameMap

getWidth
^^^^^^^^

.. java:method:: public short getWidth()
   :outertype: GameMap

nearbyEntitiesByDistance
^^^^^^^^^^^^^^^^^^^^^^^^

.. java:method:: public Map<Double, Entity> nearbyEntitiesByDistance(Entity entity)
   :outertype: GameMap

objectsBetween
^^^^^^^^^^^^^^

.. java:method:: public ArrayList<Entity> objectsBetween(Position start, Position target)
   :outertype: GameMap

updateMap
^^^^^^^^^

.. java:method:: public GameMap updateMap(LinkedList<String> mapMetadata)
   :outertype: GameMap

