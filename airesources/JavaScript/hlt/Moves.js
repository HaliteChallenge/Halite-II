class Moves {
    static dock(shipId, dockId) {
        return `d ${shipId} ${dockId}`;
    }

    static unDock(shipId) {
        return `u ${shipId}`;
    }
}

module.exports = Moves;
