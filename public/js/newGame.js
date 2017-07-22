Main.newGame = (canvas, ctx) => {
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

    // Generating button data.
    const startCallback = () => {
        // ...
    };

    const backCallback = Main.getTransition(
        "newGame",
        "serverSelect",
        1,
        eventListeners
    );

    const buttons =
        [ [rect(150, 550, 325, 100), 7, "start", startCallback]
        , [rect(805, 550, 325, 100), 7, "back",  backCallback]
        ];

    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Form boxes to be typed into.
    const formBoxes =
        [ [rect(240, 200, 800, 45), "", false]
        , [rect(240, 350, 800, 45), "", false]
        ];

    const formCursorPeriod = 1792;
    let formCursorTime = 0;

    const _formClick = e => {
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
    eventListeners.register(canvas, "click", _formClick);

    const _formKeydown = e => {
        const keydownedFormIx = formBoxes.findIndex(fb => fb[2]);
        if (~keydownedFormIx) {
            if (e.key === "Backspace" || e.key === "Delete") {
                formBoxes[keydownedFormIx][1] =
                    formBoxes[keydownedFormIx][1].slice(0, -1);
            } else if (e.key.length === 1) {
                formBoxes[keydownedFormIx][1] += e.key;
            }
        }
    };
    window.addEventListener("keydown", _formKeydown);
    eventListeners.register(window, "keydown", _formKeydown);

    // Resistering mouse state.
    const mouseState = registerMouse(canvas, eventListeners, buttons);

    // New game page main loop.
    function newGame(displacement, dt) {
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

    return newGame;
};
