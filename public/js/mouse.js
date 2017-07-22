function registerMouse(canvas, eventListeners, buttons) {
    "use strict";

    const mouseState = Object.create(null);

    // Circular buffer to store mouse position history.
    mouseState.mouseLocs = new CircularBuffer(168);

    const _mousemove = e => {
        const boundingRect = canvas.getBoundingClientRect();
        mouseState.mouseLocs.cons(
            v2(e.clientX - boundingRect.left, e.clientY - boundingRect.top)
        );
    };
    canvas.addEventListener("mousemove", _mousemove);
    eventListeners.register(canvas, "mousemove", _mousemove);

    const _mouseenter = () => mouseState.mouseLocs.clear();
    canvas.addEventListener("mouseenter", _mouseenter);
    eventListeners.register(canvas, "mouseenter", _mouseenter);

    // Circular buffer to store mouse particle effect state.
    mouseState.mouseSparks = new CircularBuffer(64);
    mouseState.lastSparkPos = V2.zero();

    // Cursor click animation state and trigger.
    mouseState.clickAnim = null;
    const _click = e => {
        const boundingRect = canvas.getBoundingClientRect();
        const clickPos = v2(
            e.clientX - boundingRect.left,
            e.clientY - boundingRect.top
        );
        mouseState.clickAnim = [clickPos, Main.clickAnimDur];

        if (Main.currentLoops.size !== 1) {
            return;
        }
        const clickedButton = buttons.find(
            ([box]) => box.contains(clickPos)
        );
        if (clickedButton) {
            const callback = clickedButton[3];
            if (callback) {
                callback();
            }
        }
    };
    canvas.addEventListener("click", _click);
    eventListeners.register(canvas, "click", _click);

    return mouseState;
}

function drawMouseTrail(ctx, mouseState) {
    "use strict";

    ctx.save();

    ctx.lineWidth = 2;
    const oldestMouseLoc = mouseState.mouseLocs.peek();
    let [oldX, oldY] =
        oldestMouseLoc ?
            [oldestMouseLoc.x, oldestMouseLoc.y] :
            [0, 0];
    let mouseLocCounter = 0;
    mouseState.mouseLocs.forEachTail(loc => {
        if (mouseLocCounter % 24 === 0) {
            ctx.strokeStyle =
                `rgba(144, 144, 144, ${mouseLocCounter / 256})`;
            ctx.beginPath();
            ctx.moveTo(oldX, oldY);
            ctx.lineTo(loc.x, loc.y);
            ctx.closePath();
            ctx.stroke();

            [oldX, oldY] = [loc.x, loc.y];
        }
        mouseLocCounter++;
    });

    ctx.beginPath();
    ctx.moveTo(oldX, oldY);
    const newestMouseLoc = mouseState.mouseLocs.get();
    if (newestMouseLoc) {
        ctx.lineTo(newestMouseLoc.x, newestMouseLoc.y);

        if (!newestMouseLoc.equals(mouseState.lastSparkPos)) {
            // Add a spark.
            const xVel = 0.375 * Math.random() *
                         (Math.random() < 0.5 ? 1 : -1);
            const yVel = -0.5 * Math.random() + 0.125;
            mouseState.lastSparkPos = newestMouseLoc;
            mouseState.mouseSparks.cons([
                newestMouseLoc,
                v2(xVel, yVel),
                0,
                Math.floor(128 + 96 * Math.random())
            ]);
        }
    }
    ctx.closePath();
    ctx.stroke();

    ctx.restore();
}

function drawAndUpdateMouseSparks(ctx, mouseState, dt) {
    "use strict";

    // Draw mouse spark particles.
    ctx.save();

    ctx.globalCompositeOperation = "lighter";
    mouseState.mouseSparks.forEachBuffer(spark => {
        if (!spark) {
            return;
        }
        const [pos, , age, green] = spark;
        const a = Math.max((750 - age) / 1000, 0.0625);
        ctx.fillStyle = `rgba(242, ${green}, 144, ${a})`;
        ctx.fillRect(pos.x - 1, pos.y - 1, 2, 2);
    });
    ctx.restore();

    // Update state of mouse particles.
    mouseState.mouseSparks.map(([pos, vel, age, green]) => {
        const newVel = vel.add(v2(0, Main.sparkGravity * dt));
        const newPos = pos.add(newVel.scalarMult(dt));
        if (!Main.isInCanvas(newPos)) {
            return null;
        }
        return [newPos, newVel, age + dt, green];
    });
}

function drawClickEffect(ctx, mouseState, dt) {
    "use strict";

    if (mouseState.clickAnim) {
        ctx.save();

        ctx.lineWidth = 3;
        ctx.lineCap = "square";
        ctx.globalCompositeOperation = "screen";
        const [pos, t] = mouseState.clickAnim;
        const progress = t / Main.clickAnimDur;
        ctx.strokeStyle = `rgba(96, 96, 96, ${progress})`;
        Main.crosshairQuad.forEach(vec => {
            ctx.beginPath();
            const shifted = pos.add(vec);
            const regresado = pos.add(
                vec.scalarMult(1 - progress * progress)
            );
            ctx.moveTo(shifted.x, shifted.y);
            ctx.lineTo(regresado.x, regresado.y);
            ctx.stroke();
            ctx.closePath();
        });

        if (t >= dt) {
            mouseState.clickAnim = [pos, t - dt];
        } else {
            mouseState.clickAnim = null;
        }

        ctx.restore();
    }
}
