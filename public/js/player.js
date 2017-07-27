/**
 * Creates a new `Player` with the specified starting position and the
 * specified maximum applied force.
 *
 * @param {V2} pos - The starting position for this new `Player`.
 * @param {number} mass - The mass of this player.
 * @param {number} appForce - The maximum force applied by a controller to this
 *                            player.
 * @param {number} friction - Frictional coefficient for this player.
 * @param {number} side - The length of one side of this player.
 * @return {Player}
 */
function Player(pos, mass, appForce, friction, side, color) {
    "use strict";
    this.mass = mass;
    this.appForce = appForce;
    this.pos = pos;
    this.vel = V2.zero();
    this.friction = friction;
    this.side = side;
    this.color = color;

    this.lastPos = pos;
    this.lerpPos = pos;
    this.lastVel = V2.zero();
    this.lerpVel = V2.zero();
}

/**
 * Add displacement.
 *
 * @param {V2} d
 * @return {void}
 */
Player.prototype.addPos = function(d) {
    "use strict";
    this.pos = this.pos.add(d);
};

/**
 * Add velocity.
 *
 * @param {V2} v
 * @return {void}
 */
Player.prototype.addVel = function(v) {
    "use strict";
    this.vel = this.vel.add(v);
};

Player.prototype.pushPos = function(newPos) {
    "use strict";
    this.lastPos = this.lerpPos;
    this.pos = newPos;
};

Player.prototype.pushVel = function(newVel) {
    "use strict";
    this.lastVel = this.lerpVel;
    this.vel = newVel;
};

/**
 * Sets `this.lerpPos` and `this.lerpVel`.
 *
 * @param {number} t1
 * @param {number} dt
 * @return {void}
 */
Player.prototype.lerp = function(ratio) {
    "use strict";
    this.lerpPos = this.pos.scalarMult(ratio).add(this.lastPos.scalarMult(1 - ratio));
};
