/**
 * Creates a new `Player` with the specified starting position and the
 * specified maximum applied force.
 *
 * @param {V2} pos - The starting position for this new `Player`.
 * @param {number} mass - The mass of this new `Player`.
 * @param {number} appForce - The maximum force applied by a controller to this
 *                            new `Player`.
 * @return {Player}
 */
function Player(pos, mass, appForce) {
    "use strict";
    this.mass = mass;
    this.appForce = appForce;
    this.pos = pos;
    this.vel = V2.zero();
    this.accel = V2.zero();
}
