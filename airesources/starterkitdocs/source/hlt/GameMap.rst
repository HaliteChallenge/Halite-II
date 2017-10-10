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

.. java:constructor:: public GameMap(int width, int height, int playerId)
   :outertype: GameMap

Methods
-------
getAllPlanets
^^^^^^^^^^^^^

.. java:method:: public Map<Integer, Planet> getAllPlanets()
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

.. java:method:: public int getHeight()
   :outertype: GameMap

getMyPlayer
^^^^^^^^^^^

.. java:method:: public Player getMyPlayer()
   :outertype: GameMap

getMyPlayerId
^^^^^^^^^^^^^

.. java:method:: public int getMyPlayerId()
   :outertype: GameMap

getPlanet
^^^^^^^^^

.. java:method:: public Planet getPlanet(int entityId)
   :outertype: GameMap

getShip
^^^^^^^

.. java:method:: public Ship getShip(int playerId, int entityId) throws IndexOutOfBoundsException
   :outertype: GameMap

getWidth
^^^^^^^^

.. java:method:: public int getWidth()
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

.. java:method:: public GameMap updateMap(Metadata mapMetadata)
   :outertype: GameMap

