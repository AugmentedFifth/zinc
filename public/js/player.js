/**
 * Creates a new `Player` with the specified starting position and the
 * specified maximum applied force.
 *
 * @param {V2} pos - The starting position for this new `Player`.
 * @param {number} mass - The mass of this player.
 * @param {number} appForce - The maximum force applied by a controller to this
 *                            player.
 * @param {number} friction - Frictional coefficient for this player.
 * @return {Player}
 */
function Player(pos, mass, appForce, friction) {
    "use strict";
    this.mass = mass;
    this.appForce = appForce;
    this.pos = pos;
    this.vel = V2.zero();
    this.accel = V2.zero();
    this.friction = friction;
}
