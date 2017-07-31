Main.serverSelect = (canvas, ctx, ws) => {
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

    // Generating button data.
    let doRequestServerList = true;
    const _newGameCallback = Main.getTransition(
        "serverSelect",
        "newGame",
        3,
        eventListeners
    );
    const newGameCallback = () => {
        doRequestServerList = false;
        Main.wsRecvCallback = null;
        _newGameCallback();
    };

    const _mainCallback = Main.getTransition(
        "serverSelect",
        "mainMenu",
        2,
        eventListeners
    );
    const mainCallback = () => {
        doRequestServerList = false;
        Main.wsRecvCallback = null;
        _mainCallback();
    };

    const buttons =
        [ [rect(150, 550, 325, 100), 7, "new",  newGameCallback]
        , [rect(805, 550, 325, 100), 7, "main", mainCallback]
        ];

    let serverListRects = [];

    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Resistering mouse state.
    const mouseState = registerMouse(canvas, eventListeners, buttons);

    // Loading wheel state.
    let loadingWheelAngle = 0;
    const loadingWheelVel = 1 / 384;
    const loadingWheelSide = 24;
    const loadingWheelPseudoRadius = 256;
    const loadingWheelTrailLen = 12;
    const loadingWheelTrailTheta = 0.1;
    function squareAtAngle(c, s, theta) {
        if (theta >= 7 * Math.PI / 4 || theta < Math.PI / 4) {
            // Right
            const x = c.x + s / 2;
            const y = c.y + s * Math.tan(theta) / 2;
            return v2(x, y);
        }
        if (theta >= Math.PI / 4 && theta < 3 * Math.PI / 4) {
            // Top
            const x = c.x + s * Math.tan(Math.PI / 2 - theta) / 2;
            const y = c.y + s / 2;
            return v2(x, y);
        }
        if (theta >= 3 * Math.PI / 4 && theta < 5 * Math.PI / 4) {
            // Left
            const x = c.x - s / 2;
            const y = c.y + s * Math.tan(Math.PI - theta) / 2;
            return v2(x, y);
        }
        // Bottom
        const x = c.x - s * Math.tan(3 * Math.PI / 2 - theta) / 2;
        const y = c.y - s / 2;
        return v2(x, y);
    }

    // Requesting a list of servers and updating periodically.
    let serverList = null;
    const requestServerListPacket = new Uint8Array([0x00]).buffer;
    const recvServerListCallback = data => {
        const bytes = new Uint8Array(data.data);
        if (bytes[0] !== 0x00) {
            console.log(
                `Bad packet. Expecting leading 0x00 byte, got: ${bytes}`
            );
            return;
        }
        const newServerList = [];
        let newServer = "";
        let j = -1;
        for (let i = 1; i < bytes.length; ++i) {
            if (j === 0) {
                newServerList.push([newServer, bytes[i]]);
                newServer = "";
            } else if (j === -1) {
                j = bytes[i];
                continue;
            } else {
                newServer += String.fromCharCode(bytes[i]);
            }
            j--;
        }
        serverList = newServerList;
    };
    function requestServerList() {
        if (doRequestServerList) {
            Main.wsRecvCallback = recvServerListCallback;
            ws.send(requestServerListPacket);
            window.setTimeout(requestServerList, 5000);
            console.log("requested server list.");
        }
    }
    requestServerList();

    // Add functionality to server list items.
    const joinGameConfirmCallback = data => {
        const bytes = new Uint8Array(data.data);
        if (bytes[0] !== 0x01) {
            console.log(
                `Bad packet. Expecting leading 0x01 byte, got: ${bytes}`
            );
            return;
        }
        if (bytes[1] === 1) {
            alertText =
                [ "An error has occured in joining the game."
                , "Please try again later."
                ];
        } else if (bytes[1] === 2) {
            alertText =
                [ "It looks like someone else has that username already."
                , "Change your name and try again."
                ];
        } else if (bytes[1] === 3) {
            alertText =
                [ "It looks like there's no such game with that name."
                , "Please try again later."
                ];
        } else {
            doRequestServerList = false;
            const joinGameCallback = Main.getTransition(
                "serverSelect",
                "game",
                2,
                eventListeners
            );
            Main.currGame = Main.serverToJoinName;
            joinGameCallback();
        }
    };

    const _clickServerName = e => {
        const boundingRect = canvas.getBoundingClientRect();
        const clickPos = v2(
            e.clientX - boundingRect.left,
            e.clientY - boundingRect.top
        );

        if (Main.currentLoops.size !== 1) {
            return;
        }
        const hoveredServerName = serverListRects.find(([ , box]) =>
            box.contains(clickPos)
        );
        if (hoveredServerName) {
            Main.serverToJoinName = hoveredServerName[0];
            if (Main.username) {
                const joinGameBytes = [0x05];
                joinGameBytes.push(Main.username.length);
                for (let i = 0; i < Main.username.length; ++i) {
                    joinGameBytes.push(Main.username.charCodeAt(i));
                }
                joinGameBytes.push(Main.serverToJoinName.length);
                for (let i = 0; i < Main.serverToJoinName.length; ++i) {
                    joinGameBytes.push(Main.serverToJoinName.charCodeAt(i));
                }

                Main.wsRecvCallback = joinGameConfirmCallback;
                ws.send(new Uint8Array(joinGameBytes).buffer);
            } else {
                doRequestServerList = false;
                Main.wsRecvCallback = null;
                Main.getTransition(
                    "serverSelect",
                    "joinGame",
                    3,
                    eventListeners
                )();
            }
        }
    };
    canvas.addEventListener("click", _clickServerName);
    eventListeners.register(canvas, "click", _clickServerName);

    // Server select menu main loop.
    function serverSelect(displacement, dt) {
        // Fill in the background.
        ctx.save();
        ctx.fillStyle = darkBgPattern;
        ctx.fillRect(0, 0, Main.width, Main.height);
        ctx.restore();

        // Draw title text.
        ctx.save();
        ctx.font = "96px 'Noto Sans', sans-serif";
        ctx.textAlign = "center";
        ctx.fillStyle = textBgPattern;
        ctx.fillText("games", Main.width / 2, 112);
        ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
        ctx.lineWidth = 2;
        ctx.strokeText("games", Main.width / 2, 112);
        ctx.restore();

        if (serverList === null) {
            // Draw loading wheel and update its state.
            ctx.save();
            for (let i = 0; i < loadingWheelTrailLen; ++i) {
                ctx.fillStyle =
                    `rgba(16, 16, 16, ${1 - i / loadingWheelTrailLen})`;
                const angle = loadingWheelAngle - i * loadingWheelTrailTheta;
                const sqPos = squareAtAngle(
                    v2(Main.width / 2, Main.height / 2),
                    loadingWheelPseudoRadius,
                    angle >= 0 ? angle : angle + 2 * Math.PI
                );
                ctx.fillRect(
                    sqPos.x - loadingWheelSide / 2,
                    sqPos.y - loadingWheelSide / 2,
                    loadingWheelSide,
                    loadingWheelSide
                );
            }
            ctx.restore();
            loadingWheelAngle += dt * loadingWheelVel;
            loadingWheelAngle %= Math.PI * 2;
        } else {
            // Display server list.
            ctx.save();
            const fontHeight = 24;
            ctx.font = `${fontHeight}px 'Noto Sans', sans-serif`;
            ctx.textAlign = "center";
            ctx.fillStyle = "#777";
            const newServerListRects = [];
            if (serverList.length > 0) {
                serverList.forEach(([serverName, playerCount], i) => {
                    const text =
                        `${serverName}, ` +
                            `${playerCount} / ${Main.maxPlayerCount} players`;
                    const drawHeight = 212 + i * 54;
                    ctx.fillText(
                        text,
                        Main.width / 2,
                        drawHeight
                    );
                    const width = ctx.measureText(text).width;
                    newServerListRects.push([
                        serverName,
                        rect(
                            Main.width / 2 - width / 2 - 10,
                            drawHeight - fontHeight,
                            width + 20,
                            fontHeight + 8
                        )
                    ]);
                });
                serverListRects = newServerListRects;
            } else {
                ctx.fillText(
                    "No games were found. You can create a new one below.",
                    Main.width / 2,
                    286
                );
            }
            ctx.restore();
        }

        // Highlight server names if they are hovered over by mouse.
        const newestMouseLoc = mouseState.mouseLocs.get();
        if (newestMouseLoc) {
            const hoveredServerName = serverListRects.find(([ , box]) =>
                box.contains(newestMouseLoc)
            );
            if (hoveredServerName) {
                ctx.save();

                ctx.fillStyle = "rgba(232, 232, 232, 0.3)";
                const [ , box] = hoveredServerName;
                ctx.fillRect(box.x, box.y, box.width, box.height);

                ctx.restore();
            }
        }

        // Draw buttons.
        drawButtons(
            ctx,
            mouseState,
            buttons,
            buttonBgPattern,
            textBgPattern,
            screwAngles
        );

        // Draw mouse trail.
        drawMouseTrail(ctx, mouseState);

        // Draw mouse movement particle effects.
        drawAndUpdateMouseSparks(ctx, mouseState, dt);

        // Draw mouse click effect.
        drawClickEffect(ctx, mouseState, dt);
    }

    return serverSelect;
};
