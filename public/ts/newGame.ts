Main.loops.newGame = (canvas, ctx, ws) => {
    // Holding a local copy of event listeners so they can be unloaded.
    const eventListeners = new EventRegistrar();

    // Initialize patterns.
    const darkBgPattern = ctx.createPattern(
        document.getElementById(
            "45-deg-dark-jean-pattern"
        ) as HTMLImageElement,
        "repeat"
    );

    const textBgPattern = ctx.createPattern(
        document.getElementById(
            "pink-dust-pattern"
        ) as HTMLImageElement,
        "repeat"
    );

    const buttonBgPattern = ctx.createPattern(
        document.getElementById(
            "grey-linen-pattern"
        ) as HTMLImageElement,
        "repeat"
    );

    const naturalBlackPattern = ctx.createPattern(
        document.getElementById(
            "natural-black-pattern"
        ) as HTMLImageElement,
        "repeat"
    );

    // Generating button data.
    const backCallback = Main.getTransition(
        "newGame",
        "serverSelect",
        1,
        eventListeners
    );

    const buttons: Button[] =
        [ [rect(150, 550, 325, 100), 7, "start", startCallback]
        , [rect(805, 550, 325, 100), 7, "back",  backCallback]
        ];

    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Form boxes to be typed into.
    const formBoxes: FormBox[] = Main.username ?
        [ [rect(240, 380, 800, 45), "", false, 32]
        ] :
        [ [rect(240, 230, 800, 45), "", false, 24]
        , [rect(240, 380, 800, 45), "", false, 32]
        ];

    let gameName: string;
    let username: string;
    let alertText: string[] = [];

    const wrongLength: (fb: FormBox) => boolean =
        ([ , text, , maxLen]) => text.length < 2 || text.length > maxLen;
    const illegalChar =
        (char: string) => char.charCodeAt(0) < 32 || char.charCodeAt(0) > 126;
    const newGameConfirmCallback = (data: MessageEvent) => {
        const bytes = new Uint8Array(data.data);
        if (bytes[0] !== 0x01) {
            console.log(
                `Bad packet. Expecting leading 0x01 byte, got: ${bytes}`
            );
            return;
        }
        if (bytes[1] === 1) {
            alertText =
                [ "An error has occured in creating your new game."
                , "Please try again later."
                ];
        } else if (bytes[1] === 2) {
            alertText =
                [ "It looks like someone else has that username already."
                , "Change your name and try again."
                ];
        } else if (bytes[1] === 3) {
            alertText =
                [ "It looks like there's already a game called that."
                , "Change your game's name and try again."
                ];
        } else {
            const startGameCallback = Main.getTransition(
                "newGame",
                "game",
                2,
                eventListeners
            );
            Main.username = username;
            Main.currGame = gameName;
            startGameCallback();
        }
    };
    function startCallback(): void {
        if (formBoxes.some(wrongLength)) {
            alertText =
                [ "Make sure that both names are at least 2 characters"
                , "long and that your name and the game name do not"
                , "exceed 24 and 32 characters, respectively."
                ];
            return;
        }
        for (const [ , text] of formBoxes) {
            for (const char of text) {
                if (illegalChar(char)) {
                    alertText = [`Illegal character: ${char}`];
                    return;
                }
            }
        }

        const createNewGameBytes = [0x01];
        if (Main.username) {
            createNewGameBytes.push(Main.username.length);
            for (let j = 0; j < Main.username.length; ++j) {
                createNewGameBytes.push(Main.username.charCodeAt(j));
            }

            const text = formBoxes[0][1];
            createNewGameBytes.push(text.length);
            for (let j = 0; j < text.length; ++j) {
                createNewGameBytes.push(text.charCodeAt(j));
            }
            username = Main.username;
            gameName = text;
        } else {
            for (let i = 0; i < formBoxes.length; ++i) {
                const text = formBoxes[i][1];
                createNewGameBytes.push(text.length);
                for (let j = 0; j < text.length; ++j) {
                    createNewGameBytes.push(text.charCodeAt(j));
                }
                if (i === 0) {
                    username = text;
                } else {
                    gameName = text;
                }
            }
        }

        Main.wsRecvCallback = newGameConfirmCallback;
        ws.send(new Uint8Array(createNewGameBytes).buffer);
    }

    const formCursorPeriod = 1792;
    let formCursorTime = 0;

    const _formClick = (e: MouseEvent) => {
        const boundingRect = canvas.getBoundingClientRect();
        const clickPos = v2(
            e.clientX - boundingRect.left,
            e.clientY - boundingRect.top
        );
        const clickedFormIx = formBoxes.findIndex(
            ([box]) => box.contains(clickPos)
        );
        for (let i = 0; i < formBoxes.length; ++i) {
            formBoxes[i][2] = false;
        }
        if (~clickedFormIx) {
            formBoxes[clickedFormIx][2] = true;
        }
    };
    canvas.addEventListener("click", _formClick);
    eventListeners.register(canvas, "click", _formClick as EventListener);

    const _formKeydown = (e: KeyboardEvent) => {
        const keydownedFormIx = formBoxes.findIndex(fb => fb[2]);
        if (~keydownedFormIx) {
            if (e.key === "Backspace" || e.key === "Delete") {
                formBoxes[keydownedFormIx][1] =
                    formBoxes[keydownedFormIx][1].slice(0, -1);
            } else if (
                e.key.length === 1 &&
                formBoxes[keydownedFormIx][1].length <
                    formBoxes[keydownedFormIx][3]
            ) {
                formBoxes[keydownedFormIx][1] += e.key;
            }
        }
    };
    window.addEventListener("keydown", _formKeydown);
    eventListeners.register(window, "keydown", _formKeydown as EventListener);

    // Resistering mouse state.
    const mouseState = new MouseState(canvas, eventListeners, buttons);

    // New game page main loop.
    function newGame(displacement: V2, dt: number): void {
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
        ctx.fillText("creating a new game", Main.width / 2, 112);
        ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
        ctx.lineWidth = 2;
        ctx.strokeText("creating a new game", Main.width / 2, 112);
        ctx.restore();

        // Draw form boxes.
        formBoxes.forEach(([{x, y, width, height}, text, active]) => {
            ctx.save();

            // Draw box.
            ctx.fillStyle = naturalBlackPattern;
            ctx.fillRect(x, y, width, height);
            ctx.lineWidth = 4;
            ctx.strokeStyle = "rgba(212, 212, 212, 0.5)";
            ctx.strokeRect(x, y, width, height);

            // Draw text.
            ctx.font = "32px 'Source Code Pro', monospace";
            ctx.textAlign = "left";
            ctx.fillStyle = "#9ab";
            ctx.fillText(text, x + 6, y + height - 12);

            // Draw cursor.
            formCursorTime += dt;
            formCursorTime %= formCursorPeriod;
            if (!active || formCursorTime > formCursorPeriod / 2) {
                const textWidth = ctx.measureText(text).width;
                ctx.fillStyle = "rgba(212, 212, 212, 0.5)";
                ctx.fillRect(x + textWidth + 8, y + 8, 12, height - 16);
            }

            ctx.restore();
        });

        // Draw form labels.
        ctx.save();
        ctx.font = "36px 'Noto Sans', sans-serif";
        ctx.textAlign = "center";
        ctx.fillStyle = "#777";
        if (Main.username) {
            ctx.fillText(`your name: ${Main.username}`, Main.width / 2, 210);
        } else {
            ctx.fillText("your name", Main.width / 2, 210);
        }
        ctx.fillText("game name", Main.width / 2, 360);
        ctx.restore();

        // Draw alert text.
        if (alertText.length > 0) {
            ctx.save();
            ctx.font = "28px 'Noto Sans', sans-serif";
            ctx.textAlign = "center";
            ctx.fillStyle = "rgba(232, 112, 128, 0.75)";
            alertText.forEach(
                (text, i) => ctx.fillText(text, Main.width / 2, 462 + i * 32)
            );
            ctx.restore();
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
        mouseState.drawMouseTrail(ctx);

        // Draw mouse movement particle effects.
        mouseState.drawAndUpdateMouseSparks(ctx, dt);

        // Draw mouse click effect.
        mouseState.drawClickEffect(ctx, dt);
    }

    return newGame;
};
