function Projectile(id, pos, vel, angPos, angVel, radius) {
    "use strict";
    this.id = id;
    this.pos = pos;
    this.vel = vel;
    this.angPos = angPos;
    this.angVel = angVel;
    this.radius = radius;

    this.isBroken = false;
    this.hasBroken = false;
    this.isDestroyed = false;
    this.dust = [];
    this.dustLifetime = 0;
    this.dustLifespan = 1000;
    this.dustRadius = 4;
}

/**
 * Utility function for creating a new `Projectile` object using serialized
 * data recieved from the server.
 *
 * @param {number} id
 * @param {V2} pos
 * @param {V2} vel
 * @param {number} phase
 * @return {Projectile}
 */
function projectile(id, pos, vel, phase) {
    "use strict";
    const angVel = Main.maxProjAngVel * vel.norm() / Main.maxProjVel;
    const angPos = Math.random() * 2 * Math.PI;
    const p = new Projectile(id, pos, vel, angPos, angVel, 12);
    p.isBroken = phase !== 0;
    return p;
}

Projectile.prototype.update = function(dt) {
    "use strict";
    if (this.isBroken) {
        if (!this.hasBroken) {
            const dustCount = randInt(6, 11);
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
            if (this.dustLifetime >= this.dustLifespan) {
                this.isDestroyed = true;
                return;
            }
            this.dust = this.dust.map(([pos, vel]) => {
                const newVel = vel.add(v2(0, Main.sparkGravity * dt));
                const newPos = pos.add(newVel.scalarMult(dt));
                return [newPos, newVel];
            }).filter(d => Main.isInCanvas(d[0]));
        }
    } else if (!this.isDestroyed) {
        this.pos = this.pos.add(this.vel.scalarMult(dt));
        this.angPos = this.angPos + this.angVel * dt;
    }
};

Projectile.prototype.addPos = function(d) {
    "use strict";
    this.pos = this.pos.add(d);
};

Projectile.prototype.clone = function() {
    "use strict";
    const clone = new Projectile(
        this.id,
        this.pos,
        this.vel,
        this.angPos,
        this.angVel,
        this.radius
    );

    clone.isBroken = this.isBroken;
    clone.hasBroken = this.hasBroken;
    clone.isDestroyed = this.isDestroyed;
    clone.dust = this.dust.slice();
    clone.dustLifetime = this.dustLifetime;
    clone.dustLifespan = this.dustLifespan;
    clone.dustRadius = this.dustRadius;

    return clone;
};
