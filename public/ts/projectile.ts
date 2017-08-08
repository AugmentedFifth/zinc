class Projectile {
    public  isBroken:    boolean = false;
    private hasBroken:   boolean = false;
    public  isDestroyed: boolean = false;

    public dust: [V2, V2][] = [];
    private dustLifetime: number = 0;

    public static readonly dustRadius: number = 4;

    private static readonly dustLifespan: number = 1000;

    public constructor(
        public readonly id:     number,
        public          pos:    V2,
        public          vel:    V2,
        public          angPos: number,
        public readonly angVel: number,
        public readonly radius: number
    ) {}

    public update(dt: number): void {
        if (this.isBroken) {
            if (!this.hasBroken) {
                this.dust = [];
                const dustCount = randInt(6, 13);
                for (let i = 0; i < dustCount; ++i) {
                    this.dust.push([
                        this.pos.clone(),
                        this.vel
                            .scalarMult(0.75)
                            .add(v2(
                                Math.random() - 0.5,
                                Math.random() - 0.5
                            ))
                    ]);
                }
                this.hasBroken = true;
            } else {
                this.dustLifetime += dt;
                if (this.dustLifetime >= Projectile.dustLifespan) {
                    this.isDestroyed = true;
                    return;
                }
                this.dust = this.dust.map(([pos, vel]) => {
                    const newVel = vel.add(v2(0, Main.sparkGravity * dt));
                    const newPos = pos.add(newVel.scalarMult(dt));
                    const ret: [V2, V2] = [newPos, newVel];
                    return ret;
                }).filter(([pos]) => Main.isInCanvas(pos));
            }
        } else {
            this.hasBroken = false;
            this.isDestroyed = false;
            if (!this.isDestroyed) {
                this.pos = this.pos.add(this.vel.scalarMult(dt));
                this.angPos = this.angPos + this.angVel * dt;
            }
        }
    }

    public addPos(d: V2): void {
        this.pos = this.pos.add(d);
    }

    public clone(): Projectile {
        const clone = new Projectile(
            this.id,
            this.pos,
            this.vel,
            this.angPos,
            this.angVel,
            this.radius
        );

        clone.isBroken     = this.isBroken;
        clone.hasBroken    = this.hasBroken;
        clone.isDestroyed  = this.isDestroyed;
        clone.dust         = this.dust.slice();
        clone.dustLifetime = this.dustLifetime;

        return clone;
    }
}

/**
 * Utility function for creating a new `Projectile` object using serialized
 * data recieved from the server.
 */
function projectile(id: number, pos: V2, vel: V2, phase: number): Projectile {
    const angVel = Main.maxProjAngVel * vel.norm() / Main.maxProjVel;
    const angPos = Math.random() * 2 * Math.PI;
    const p = new Projectile(id, pos, vel, angPos, angVel, 12);
    p.isBroken = phase !== 0;
    return p;
}
