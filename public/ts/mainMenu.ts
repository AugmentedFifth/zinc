Main.loops.mainMenu = (canvas, ctx) => {
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

    // Generating button data.
    const playCallback = Main.getTransition(
        "mainMenu",
        "serverSelect",
        0,
        eventListeners
    );
    const aboutCallback = Main.getTransition(
        "mainMenu",
        "aboutPage",
        1,
        eventListeners
    );

    const buttons: Button[] =
        [ [rect(150, 550, 325, 100), 7, "play",  playCallback]
        , [rect(805, 550, 325, 100), 7, "about", aboutCallback]
        ];
    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    const mouseState = new MouseState(canvas, eventListeners, buttons);

    // Main menu loop.
    function mainMenu(displacement: V2, dt: number): void {
        // Fill in the background.
        ctx.save();
        ctx.fillStyle = darkBgPattern;
        ctx.fillRect(0, 0, Main.width, Main.height);
        ctx.restore();

        // Draw title text.
        ctx.save();
        ctx.font = "192px 'Noto Sans', sans-serif";
        ctx.textAlign = "center";
        ctx.fillStyle = textBgPattern;
        ctx.fillText("zinc", Main.width / 2, 175);
        ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
        ctx.lineWidth = 2;
        ctx.strokeText("zinc", Main.width / 2, 175);
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

        // Draw mouse trail.
        mouseState.drawMouseTrail(ctx);

        // Draw mouse movement particle effects.
        mouseState.drawAndUpdateMouseSparks(ctx, dt);

        // Draw mouse click effect.
        mouseState.drawClickEffect(ctx, dt);
    }

    return mainMenu;
};
