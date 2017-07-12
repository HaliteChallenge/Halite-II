import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

public class BehaviorManager {
    Map<Long, Behavior> behaviors;

    public BehaviorManager() {
        behaviors = new TreeMap<>();
    }

    private void updateBehaviors(GameMap gameMap, Vector<Move> moves, long id, Behavior behavior) {
        if (!behavior.isFinished(gameMap))
            moves.add(behavior.next(gameMap));
        else
            this.behaviors.remove(id);
    }

    void update(GameMap gameMap, Vector<Move> moves) {
        behaviors.forEach((id, behavior) -> updateBehaviors(gameMap, moves, id, behavior));
    }

    boolean isExecuting(long shipId) {
        return behaviors.containsKey(shipId);
    }

    void warpTo(long shipId, Position target) {
        behaviors.put(shipId, new Behavior(shipId, Behavior.BehaviorType.Warp, target, Behavior.State.Moving));
    }
}
