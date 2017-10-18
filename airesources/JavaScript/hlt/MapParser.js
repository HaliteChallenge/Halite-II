class MapParser {
    constructor(line) {
        this._tokens = line.trim().split(' ');
        this._currentIdx = 0;
    }

    nextShipParams() {
        return {
            id: this.nextInt(),
            xPos: this.nextFloat(),
            yPos: this.nextFloat(),
            health: this.nextInt(),
            velocityX: this.nextFloat(),
            velocityY: this.nextFloat(),
            dockingStatus: this.nextInt(),
            dockedPlanetId: this.nextInt(),
            dockingProgress: this.nextInt(),
            weaponCooldown: this.nextInt()
        };
    }
    
    nextPlanetParams() {
        return {
            id: this.nextInt(),
            xPos: this.nextFloat(),
            yPos: this.nextFloat(),
            health: this.nextInt(),
            radius: this.nextFloat(),
            dockingSpots: this.nextInt(),
            currentProduction: this.nextInt(),
            remainingProduction: this.nextInt(),
            ownerId: this.nextInt() === 1 ? this.nextInt() : this.skipNextAndReturn(null),
            dockedShipIds: this.nextDockedShipIds()
        };
    }

    nextDockedShipIds() {
        const numberOfShips = this.nextInt();
        const shipIds = [];
        for (let shipIdx = 0; shipIdx < numberOfShips; shipIdx++) {
            shipIdx.push(this.nextInt());
        }

        return shipIds;
    }

    nextInt() {
        return parseInt(this.nextToken());
    }

    nextFloat() {
        return parseFloat(this.nextToken());
    }

    skipNextAndReturn(valueReplacement) {
        this.nextToken();
        return valueReplacement;
    }

    nextToken() {
        return this._tokens[this._currentIdx++];
    }

    remainingTokens() {
        return this._tokens.slice(this._currentIdx);
    }
}

module.exports = MapParser;
