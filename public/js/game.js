Main.game = (canvas, ctx, ws) => {
    "use strict";

    // Holding a local copy of event listeners so they can be unloaded.
    const eventListeners = new EventRegistrar();

    // Initialize patterns.
    const darkBg = document.getElementById("45-deg-dark-jean-pattern");
    const darkBgPattern = ctx.createPattern(darkBg, "repeat");

    const textBg = document.getElementById("pink-dust-pattern");
    const textBgPattern = ctx.createPattern(textBg, "repeat");

    const buttonBg = document.getElementById("grey-linen-pattern");
    const buttonBgPattern = ctx.createPattern(buttonBg, "repeat");

    // Game state.
    const player = new Player(
        v2(randInt(100, Main.width - 100), randInt(100, Main.height - 100)),
        12,
        3e-2,
        1e-3,
        48
    );
    let keypressLog = [];
    const color = getRand([
        "#914882",
        "#6a4891",
        "#486291",
        "#48918a",
        "#6da076",
        "#cebe6b",
        "#ce876b",
        "#993d3d",
        "#7c5e52",
        "#afafaf",
    ]);

    // Generating button data.
    const _mainCallback = Main.getTransition(
        "game",
        "mainMenu",
        1,
        eventListeners
    );
    const mainCallback = () => {
        // TODO: Tell server that player is leaving.
        _mainCallback();
    };

    const buttons =
        [ [rect(1155, 660, 114, 50), 5, "main", mainCallback]
        ];

    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Resistering mouse state.
    const mouseState = registerMouse(canvas, eventListeners, buttons);

    // Player controls.
    const controllerKeys = new Map([
        ["w", v2(0, -1)],
        ["a", v2(-1, 0)],
        ["s", v2(0,  1)],
        ["d", v2(1,  0)],
    ]);

    const _keydown = e => {
        const now = window.performance.now();
        const key = e.key.toLowerCase();
        if (controllerKeys.has(key)) {
            keypressLog.push([now, key, true]);
        }
    };
    window.addEventListener("keydown", _keydown);
    eventListeners.register(window, "keydown", _keydown);

    const _keyup = e => {
        const now = window.performance.now();
        const key = e.key.toLowerCase();
        if (controllerKeys.has(key)) {
            keypressLog.push([now, key, false]);
        }
    };
    window.addEventListener("keyup", _keyup);
    eventListeners.register(window, "keyup", _keyup);

    // dat GUI.
    const datGui = new dat.GUI();
    datGui.add(player, "mass", 1, 48);
    datGui.add(player, "appForce", 8e-3, 8e-2);
    datGui.add(player, "friction", 4e-4, 6e-3);
    datGui.add(player, "side", 2, 128);

    // Game main loop.
    function aboutPage(displacement, dt) {
        // Fill in the background.
        ctx.save();
        ctx.fillStyle = darkBgPattern;
        ctx.fillRect(0, 0, Main.width, Main.height);
        ctx.restore();

        // Draw buttons.
        drawButtons(
            ctx,
            mouseState,
            buttons,
            buttonBgPattern,
            textBgPattern,
            screwAngles,
            32,
            false
        );

        // TODO: Draw info/status.

        /* Update local player position/physics. */
        // Get a copy of the keypress log to work with and then start a new
        // stack. We cut off any keys that are still down at this point, and
        // copy them over to the new stack.
        const keypressLogCopy = keypressLog;
        keypressLog = [];
        const now = window.performance.now();

        // Calculate accelerations for this frame based on keypress log.
        const pressed = new Set();
        let t0, t_;
        for (let i = 0; i < keypressLogCopy.length; ++i) {
            const [t1, key, down] = keypressLogCopy[i];

            if (t0 !== undefined) {
                const thisDt = t1 - t0;
                if (thisDt > 0) {
                    /* jshint loopfunc: true */
                    const dir = pressed.foldl(
                        (d, k) => d.add(controllerKeys.get(k)),
                        V2.zero()
                    ).normalize();
                    /* jshint loopfunc: false */
                    const thisAccel =
                        dir.scalarMult(player.appForce / player.mass);
                    player.addVel(thisAccel.scalarMult(thisDt));
                }
            }

            if (down) {
                pressed.add(key);
                if (t0 === undefined) {
                    t_ = t1;
                }
            } else {
                pressed.delete(key);
            }
            t0 = t1;
        }
        let leftoverDir = V2.zero();
        pressed.forEach(key => {
            keypressLog.unshift([now, key, true]);
            leftoverDir = leftoverDir.add(controllerKeys.get(key));
        });
        if (!leftoverDir.null()) {
            leftoverDir = leftoverDir.normalize();
            const thisDt = now - t_;
            const thisAccel =
                leftoverDir.scalarMult(player.appForce / player.mass);
            player.addVel(thisAccel.scalarMult(thisDt));
        }

        // Apply frictional forces.
        const frictionalDvNorm = -player.friction * dt;
        if (
            player.vel.null() ||
            Math.abs(frictionalDvNorm) >= player.vel.norm()
        ) {
            player.vel = V2.zero();
        } else {
            const frictionDv =
                player.vel.normalize().scalarMult(frictionalDvNorm);
            player.addVel(frictionDv);
        }

        // Update position based on new velocity.
        player.addPos(player.vel.scalarMult(dt));

        // Collision detection.
        if (player.pos.y <= 0) { // Hit top
            player.vel.y = -player.vel.y;
            player.pos.y = 0;
        } else if (player.pos.y >= Main.height - player.side) { // Hit bottom
            player.vel.y = -player.vel.y;
            player.pos.y = Main.height - player.side;
        }
        if (player.pos.x >= Main.width - player.side) { // Hit right
            player.vel.x = -player.vel.x;
            player.pos.x = Main.width - player.side;
        } else if (player.pos.x <= 0) { // Hit left
            player.vel.x = -player.vel.x;
            player.pos.x = 0;
        }

        // TODO: Send inputs to server.

        // TODO: Interpolate other client positions.

        // Draw player.
        ctx.save();

        ctx.fillStyle = color;
        ctx.fillRect(player.pos.x, player.pos.y, player.side, player.side);

        ctx.restore();

        // TODO: Draw other players.

        // Draw mouse trail.
        drawMouseTrail(ctx, mouseState);

        // Draw mouse movement particle effects.
        drawAndUpdateMouseSparks(ctx, mouseState, dt);

        // Draw mouse click effect.
        drawClickEffect(ctx, mouseState, dt);
    }

    return aboutPage;
};
