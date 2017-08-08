class Player {
    public  vel:     V2 = V2.zero();

    private lastPos: V2;
    public  lerpPos: V2;

    private lastVel: V2 = V2.zero();
    public  lerpVel: V2 = V2.zero();

    public constructor(
        public pos:      V2,
        public mass:     number,
        public appForce: number,
        public friction: number,
        public side:     number,
        public color:    string
    ) {
        this.lastPos = pos;
        this.lerpPos = pos;
    }

    /**
     * Add displacement.
     */
    public addPos(d: V2): void {
        this.pos = this.pos.add(d);
    }

    /**
     * Add velocity.
     */
    public addVel(v: V2): void {
        this.vel = this.vel.add(v);
    }

    public pushPos(newPos: V2): void {
        this.lastPos = this.lerpPos;
        this.pos = newPos;
    }

    public pushVel(newVel: V2): void {
        this.lastVel = this.lerpVel;
        this.vel = newVel;
    }

    /**
     * Sets `this.lerpPos` and `this.lerpVel`.
     */
    public lerp(ratio: number): void {
        this.lerpPos = this.pos.scalarMult(ratio).add(
            this.lastPos.scalarMult(1 - ratio)
        );
    }

    /**
     * Returns a `V2` representing the position of the center of this player.
     */
    public center(): V2 {
        return this.pos.add(v2(this.side / 2, this.side / 2));
    }
}
