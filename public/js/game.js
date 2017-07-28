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

    const naturalBlack = document.getElementById("natural-black-pattern");
    const naturalBlackPattern = ctx.createPattern(naturalBlack, "repeat");

    // Game state.
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
    const player = new Player(
        v2(50, 50),
        12,
        3e-2,
        1e-3,
        48,
        color
    );
    /*
    const playerShadow = new Player(
        v2(50, 50),
        12,
        3e-2,
        1e-3,
        48,
        color
    );
    */

    let keypressLog = [];
    let movementSendCounter = 0;
    const waitingForConfirmation = new Map();

    // Sending initial game-start "hello".
    const hereIsMyGameInfoBytes = [0x02];
    Main.data.hexRgbToBytes(player.color).forEach(
        b => hereIsMyGameInfoBytes.push(b)
    );
    ws.send(new Uint8Array(hereIsMyGameInfoBytes).buffer);

    // Generating button data.
    let promptOpen = false;
    const modalBoxRect = rect(
        Main.width / 2 - 180,
        Main.height / 2 - 120,
        360,
        240
    );

    const _mainCallback = Main.getTransition(
        "game",
        "mainMenu",
        1,
        eventListeners
    );
    const mainCallback = () => {
        if (promptOpen) {
            // Tell server that player is leaving.
            ws.send(new Uint8Array([0x04]).buffer);

            _mainCallback();
        }
    };
    const mainPromptCallback  = () => promptOpen = true;
    const closePromptCallback = () => promptOpen = false;

    const _modalClick = e => {
        if (!promptOpen) {
            return;
        }

        const boundingRect = canvas.getBoundingClientRect();
        const clickPos = v2(
            e.clientX - boundingRect.left,
            e.clientY - boundingRect.top
        );

        if (Main.currentLoops.size !== 1) {
            return;
        }
        if (!modalBoxRect.contains(clickPos)) {
            promptOpen = false;
        }
    };
    canvas.addEventListener("click", _modalClick);
    eventListeners.register(canvas, "click", _modalClick);

    const buttons =
        [ [ rect(1155, 660, 114, 50)
          , 5
          , "main"
          , mainPromptCallback
          ]
        , [ rect(Main.width / 2 - 35 - 114, 400, 114, 50)
          , 5
          , "no"
          , closePromptCallback
          ]
        , [ rect(Main.width / 2 + 35, 400, 114, 50)
          , 5
          , "yes"
          , mainCallback
          ]
        ];

    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Font size to display usernames.
    const usernameFontSize = 16; // pixels

    // Resistering mouse state.
    const mouseState = registerMouse(canvas, eventListeners, buttons);

    // Player control buttons.
    const controllerKeys = new Map([
        ["w", v2(0, -1)],
        ["a", v2(-1, 0)],
        ["s", v2(0,  1)],
        ["d", v2(1,  0)],
    ]);
    const controllerKeyIndices = new Map([
        ["w", 0x00],
        ["a", 0x01],
        ["s", 0x02],
        ["d", 0x03],
    ]);

    // In-game chat.
    const chatHandler = new ChatHandler(
        eventListeners,
        ws,
        x => keypressLog.push(x),
        controllerKeyIndices
    );

    // Player controls.
    const _keydown = e => {
        if (!chatHandler.active) {
            const now = window.performance.now();
            const key = e.key.toLowerCase();
            if (controllerKeys.has(key)) {
                keypressLog.push([now, key, true]);
            }
        }
    };
    window.addEventListener("keydown", _keydown);
    eventListeners.register(window, "keydown", _keydown);

    const _keyup = e => {
        if (!chatHandler.active) {
            const now = window.performance.now();
            const key = e.key.toLowerCase();
            if (controllerKeys.has(key)) {
                keypressLog.push([now, key, false]);
            }
        }
    };
    window.addEventListener("keyup", _keyup);
    eventListeners.register(window, "keyup", _keyup);

    // Info on other players.
    const otherPlayers = new Map();
    let recvCount = 0;
    let lastRecv, recvDtAvg;
    let lastRecvDt;

    // Server interaction.
    Main.wsRecvCallback = data => {
        const now = window.performance.now();
        recvCount++;
        if (lastRecv !== undefined) {
            const recvDt = now - lastRecv;
            if (recvDtAvg !== undefined) {
                recvDtAvg = (recvDtAvg * (recvCount - 1) + recvDt) / recvCount;
            } else {
                recvDtAvg = recvDt;
            }
            lastRecvDt = recvDt;
        }
        lastRecv = now;

        const bytes = new Uint8Array(data.data);
        if (bytes[0] !== 0x02 && bytes[0] !== 0x03) {
            console.log(
                "Bad packet. Expecting leading 0x02 or 0x03 byte, " +
                    `got: ${bytes}`
            );
            return;
        }

        // Chat packet handling.
        if (bytes[0] === 0x03) {
            const nameLength = bytes[1];
            let name = "";
            let offset = 2;
            for (let i = 0; i < nameLength; ++i) {
                name += String.fromCharCode(bytes[offset]);
                offset++;
            }

            if (name !== Main.username && !otherPlayers.has(name)) {
                console.log(
                    `Chat packet from absent player "${name}": ${bytes}`
                );
                return;
            }

            let chatMsg = "";
            while (offset < bytes.length) {
                chatMsg += String.fromCharCode(bytes[offset]);
                offset++;
            }

            chatHandler.addChatBubble(name, chatMsg);

            return;
        }

        const view = new DataView(data.data, 1);
        let offset = 0;
        const playersSeen = new Set();
        while (offset < view.byteLength) {
            try {
                const nameLen = view.getUint8(offset);
                offset++;
                let name = "";
                for (let j = 0; j < nameLen; ++j) {
                    name += String.fromCharCode(view.getUint8(offset));
                    offset++;
                }
                const isOtherPlayer = name !== Main.username;
                const px = view.getFloat64(offset, true);
                offset += 8;
                const py = view.getFloat64(offset, true);
                offset += 8;
                const vx = view.getFloat64(offset, true);
                offset += 8;
                const vy = view.getFloat64(offset, true);
                offset += 8;
                const red = view.getUint8(offset).toString(16);
                offset++;
                const green = view.getUint8(offset).toString(16);
                offset++;
                const blue = view.getUint8(offset).toString(16);
                offset++;
                const lastOrdinal = view.getUint32(offset, true);
                offset += 4;
                if (isOtherPlayer) {
                    if (otherPlayers.has(name)) {
                        const otherPlayer = otherPlayers.get(name);
                        otherPlayer.pushPos(v2(px, py));
                        otherPlayer.pushVel(v2(vx, vy));
                        otherPlayer.color = `#${red}${green}${blue}`;
                    } else {
                        const otherPlayer = new Player(
                            v2(px, py),
                            12,
                            3e-2,
                            1e-3,
                            48,
                            `#${red}${green}${blue}`
                        );
                        otherPlayer.vel = v2(vx, vy);
                        otherPlayers.set(name, otherPlayer);
                    }
                    playersSeen.add(name);
                } else {
                    //playerShadow.pushPos(v2(px, py));
                    //playerShadow.pushVel(v2(vx, vy));
                    //playerShadow.color = `#${red}${green}${blue}`;
                    const toConfirm = waitingForConfirmation.get(lastOrdinal);
                    if (toConfirm !== undefined) {
                        const diff = v2(px, py).sub(toConfirm);
                        player.addPos(diff);
                    }
                    waitingForConfirmation.clearKeysUpTo(lastOrdinal);
                }
            } catch (e) {
                console.log(
                    "Wrong byte length. Offset:",
                    offset,
                    "bytes:",
                    bytes.slice(1),
                    e
                );
                break;
            }
        }

        otherPlayers.forEach((otherPlayer, name) => {
            if (!playersSeen.has(name)) {
                otherPlayers.delete(name);
            }
        });
    };

    // Game main loop.
    function aboutPage(displacement, dt) {
        // Fill in the background.
        ctx.save();
        ctx.fillStyle = darkBgPattern;
        ctx.fillRect(0, 0, Main.width, Main.height);
        ctx.restore();

        // Chat box GUI.
        if (chatHandler.active) {
            // Draw text.
            ctx.font = "18px 'Source Code Pro', monospace";
            ctx.textAlign = "left";
            ctx.fillStyle = "#9ab";
            ctx.fillText(chatHandler.text, 24, Main.height - 24);

            // Draw cursor.
            chatHandler.incrementCursorTime(dt);
            if (chatHandler.cursorTime > chatHandler.cursorPeriod / 2) {
                const textWidth = ctx.measureText(chatHandler.text).width;
                ctx.fillStyle = "rgba(212, 212, 212, 0.5)";
                ctx.fillRect(
                    textWidth + 25,
                    Main.height - 28 + 3,
                    12,
                    18 - 16
                );
            }
        }

        // Draw modal if necessary.
        if (promptOpen) {
            ctx.save();
            // Modal background.
            ctx.fillStyle = "rgba(0, 0, 0, 0.36)";
            ctx.fillRect(0, 0, Main.width, Main.height);

            // Modal box.
            ctx.fillStyle = naturalBlackPattern;
            ctx.fillRect(
                modalBoxRect.x,
                modalBoxRect.y,
                modalBoxRect.width,
                modalBoxRect.height
            );
            ctx.strokeStyle = "#aaa";
            ctx.strokeRect(
                modalBoxRect.x,
                modalBoxRect.y,
                modalBoxRect.width,
                modalBoxRect.height
            );

            // Modal text.
            ctx.font = "28px 'Noto Sans', sans-serif";
            ctx.textAlign = "center";
            ctx.textBaseline = "alphabetic";
            ctx.fillStyle = "#ccc";
            ctx.fillText(
                "are you sure you want",
                Main.width / 2,
                Main.height / 2 - 70
            );
            ctx.fillText(
                "to leave the game?",
                Main.width / 2,
                Main.height / 2 - 32
            );

            ctx.restore();
        }

        // Draw buttons.
        drawButtons(
            ctx,
            mouseState,
            promptOpen ? buttons : buttons.slice(0, 1),
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

        // Saving calculated position to be confirmed by server later.
        waitingForConfirmation.set(movementSendCounter, player.pos.clone());

        // Send inputs to server.
        const hereAreMyMovementsBytes = [0x03];
        const pushByte = b => hereAreMyMovementsBytes.push(b);
        Main.data.i32ToBytes(movementSendCounter).forEach(pushByte);
        movementSendCounter++;
        Main.data.f64ToBytes(now).forEach(pushByte);
        Main.data.f64ToBytes(dt).forEach(pushByte);
        for (let i = 0; i < keypressLogCopy.length; ++i) {
            const [t, key, down] = keypressLogCopy[i];
            Main.data.f64ToBytes(t).forEach(pushByte);
            pushByte(controllerKeyIndices.get(key));
            if (down) {
                pushByte(0x01);
            } else {
                pushByte(0x00);
            }
        }
        ws.send(new Uint8Array(hereAreMyMovementsBytes).buffer);

        // Draw player.
        ctx.save();

        ctx.fillStyle = player.color;
        ctx.fillRect(player.pos.x, player.pos.y, player.side, player.side);
        // Drawing player name.
        ctx.font = `${usernameFontSize}px 'Noto Sans', sans-serif`;
        ctx.textAlign = "center";
        ctx.textBaseline = "alphabetic";
        ctx.fillStyle = "#ccc";
        ctx.fillText(
            Main.username,
            player.pos.x + player.side / 2,
            player.pos.y + player.side + usernameFontSize
        );

        ctx.restore();

        // Drawing player shadow based on server side data for testing.
        /*
        ctx.save();

        if (recvDtAvg !== undefined && lastRecv !== undefined) {
            const recvDt = window.performance.now() - lastRecv;
            playerShadow.lerp(Math.min(Math.max(recvDt / recvDtAvg, 0), 1));
        }

        ctx.fillStyle = playerShadow.color;
        ctx.globalCompositeOperation = "screen";
        ctx.fillRect(
            playerShadow.lerpPos.x,
            playerShadow.lerpPos.y,
            playerShadow.side,
            playerShadow.side
        );

        ctx.restore();
        */

        // Draw other players.
        //console.log(otherPlayers);
        otherPlayers.forEach((p, name) => {
            ctx.save();

            if (recvDtAvg !== undefined && lastRecv !== undefined) {
                const recvDt = window.performance.now() - lastRecv;
                p.lerp(Math.min(Math.max(recvDt / recvDtAvg, 0), 1));
            }

            ctx.fillStyle = p.color;
            ctx.fillRect(
                p.lerpPos.x,
                p.lerpPos.y,
                p.side,
                p.side
            );

            // Drawing player name.
            ctx.font = `${usernameFontSize}px 'Noto Sans', sans-serif`;
            ctx.textAlign = "center";
            ctx.textBaseline = "alphabetic";
            ctx.fillStyle = "#ccc";
            ctx.fillText(
                name,
                p.lerpPos.x + p.side / 2,
                p.lerpPos.y + p.side + usernameFontSize
            );

            ctx.restore();
        });

        // Chat bubbles.
        ctx.save();

        ctx.font = "18px 'Source Code Pro', monospace";
        ctx.textAlign = "center";
        ctx.textBaseline = "alphabetic";
        ctx.fillStyle = "#9ab";

        chatHandler.newFrame().forEach(([ , msg], playerName) => {
            const textPos = (() => {
                if (playerName === Main.username) {
                    return v2(
                        player.pos.x + player.side / 2,
                        player.pos.y - 6
                    );
                } else {
                    const otherPlayer = otherPlayers.get(playerName);
                    if (otherPlayer === undefined) {
                        console.log(
                            `Chat from absent player "${playerName}": ${msg}`
                        );
                        return;
                    }
                    return v2(
                        otherPlayer.lerpPos.x + otherPlayer.side / 2,
                        otherPlayer.lerpPos.y - 6
                    );
                }
            })();

            if (textPos === undefined) {
                return;
            }

            const split = chatHandler.splitMsg(msg);
            split.forEach((line, i) => {
                ctx.fillText(
                    line,
                    textPos.x,
                    textPos.y - (split.length - 1 - i) * 21
                );
            });
        });

        ctx.restore();

        // Draw mouse trail.
        drawMouseTrail(ctx, mouseState);

        // Draw mouse movement particle effects.
        drawAndUpdateMouseSparks(ctx, mouseState, dt);

        // Draw mouse click effect.
        drawClickEffect(ctx, mouseState, dt);
    }

    return aboutPage;
};
