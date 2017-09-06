GameMap
=======

.. java:package:: halitejavabot
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
adjustForCollision
^^^^^^^^^^^^^^^^^^

.. java:method:: public ThrustMove.Pair adjustForCollision(Position start, double angle, short thrust)
   :outertype: GameMap

adjustForCollision
^^^^^^^^^^^^^^^^^^

.. java:method:: public ThrustMove.Pair adjustForCollision(Position start, double angle, short thrust, int tries)
   :outertype: GameMap

getClosestPoint
^^^^^^^^^^^^^^^

.. java:method:: public Position getClosestPoint(Position start, Position target, short radius)
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

getPlanets
^^^^^^^^^^

.. java:method:: public Map<Long, Planet> getPlanets()
   :outertype: GameMap

getPlayers
^^^^^^^^^^

.. java:method:: public List<Player> getPlayers()
   :outertype: GameMap

getShip
^^^^^^^

.. java:method:: public Ship getShip(short playerId, long entityId) throws IndexOutOfBoundsException
   :outertype: GameMap

getWidth
^^^^^^^^

.. java:method:: public short getWidth()
   :outertype: GameMap

isPathable
^^^^^^^^^^

.. java:method:: public boolean isPathable(Position start, Position target)
   :outertype: GameMap

positionDelta
^^^^^^^^^^^^^

.. java:method:: public Position positionDelta(Position originalPosition, Position deltaPosition)
   :outertype: GameMap

updateMap
^^^^^^^^^

.. java:method::  GameMap updateMap(LinkedList<String> mapMetadata)
   :outertype: GameMap

