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
function Player(pos, mass, appForce, friction, side) {
    "use strict";
    this.mass = mass;
    this.appForce = appForce;
    this.pos = pos;
    this.vel = V2.zero();
    this.friction = friction;
    this.side = side;
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
