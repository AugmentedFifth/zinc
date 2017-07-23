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
    const playerSide = 48;
    const player = new Player(
        v2(randInt(50, Main.width - 50), randInt(50, Main.height - 50)),
        8,
        3e-4
    );
    let keypressLog = [];
    let friction = -4e-4;
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
        [ [rect(900, 660, 114, 50), 5, "main", mainCallback]
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
            if (
                !keypressLog.some(([k, , t1]) => k === key && t1 === -1)
            ) {
                keypressLog.push([key, now, -1]);
            }
        }
    };
    window.addEventListener("keydown", _keydown);
    eventListeners.register(window, "keydown", _keydown);

    const _keyup = e => {
        const now = window.performance.now();
        const key = e.key.toLowerCase();
        if (controllerKeys.has(key)) {
            for (let i = keypressLog.length - 1; i >= 0; --i) {
                if (keypressLog[i][0] === key) {
                    keypressLog[i][2] = now;
                }
            }
        }
    };
    window.addEventListener("keyup", _keyup);
    eventListeners.register(window, "keyup", _keyup);

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
            screwAngles
        );

        // TODO: Draw info/status.

        /* Update local player position/physics. */
        // Get a copy of the keypress log to work with and then start a new
        // stack. We cut off any keys that are still down at this point, and
        // copy them over to the new stack.
        let keypressLogCopy = keypressLog;
        keypressLog = keypressLog.filter(kp => kp[2] === -1);
        const now = window.performance.now();

        // Apply frictional forces.
        const frictionalDvNorm = friction * dt;
        if (
            player.vel.null() ||
            Math.abs(frictionalDvNorm) >= player.vel.norm()
        ) {
            player.vel = V2.zero();
        } else {
            const frictionDv =
                player.vel.normalize().scalarMult(frictionalDvNorm);
            player.vel = player.vel.add(frictionDv);
        }

        // Calculate accelerations for this frame based on keypress log.
        let keypressLogCopyOrig = null;
        let partiallyProcessed = [];
        do {
            for (let i = 0; i < keypressLogCopy.length; ++i) {
                const [key, t0, _t1] = keypressLogCopy[i];
                const t1 = _t1 === -1 ? now : _t1;
                const nextT0 =
                    i === keypressLogCopy.length - 1 ?
                        Math.min(t1, ...partiallyProcessed.map(p => p[2])) :
                        Math.min(
                            t1,
                            keypressLogCopy[i + 1][1],
                            ...partiallyProcessed.map(p => p[2])
                        );
                const thisDt = nextT0 - t0;
                let thisDir = controllerKeys.get(key);
                for (let j = 0; j < partiallyProcessed.length; ++j) {
                    thisDir = thisDir.add(
                        controllerKeys.get(partiallyProcessed[j][0])
                    );
                }
                thisDir = thisDir.normalize();
                const thisAccel =
                    thisDir.scalarMult(player.appForce / player.mass);
                player.vel = player.vel.add(thisAccel.scalarMult(thisDt));
                /* jshint loopfunc: true */
                partiallyProcessed =
                    partiallyProcessed
                        .filter(p => p[2] > nextT0)
                        .map(([k, , t]) => [k, nextT0, t]);
                /* jshint loopfunc: false */
                if (t1 > nextT0) {
                    partiallyProcessed.push([key, nextT0, t1]);
                }
            }
            if (keypressLogCopyOrig === null) {
                keypressLogCopyOrig = keypressLogCopy;
            }
            keypressLogCopy = partiallyProcessed;
            partiallyProcessed = [];
        } while (keypressLogCopy.length > 0);

        // Update position based on new velocity.
        player.pos = player.pos.add(player.vel.scalarMult(dt));

        // TODO: Send inputs to server.

        // TODO: Interpolate other client positions.

        // Draw player.
        ctx.save();

        ctx.fillStyle = color;
        ctx.fillRect(player.pos.x, player.pos.y, playerSide, playerSide);

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
