Main.aboutPage = (canvas, ctx) => {
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
    const mainCallback = Main.getTransition(
        "aboutPage",
        "mainMenu",
        3,
        eventListeners
    );
    const sourceCallback = () => {
        const newTab = window.open(
            "https://www.github.com/AugmentedFifth/zinc",
            "_blank"
        );
        newTab.focus();
    };

    const buttons =
        [ [rect(150, 550, 325, 100), 7, "main",   mainCallback]
        , [rect(805, 550, 325, 100), 7, "source", sourceCallback]
        ];

    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Resistering mouse state.
    const mouseState = registerMouse(canvas, eventListeners, buttons);

    // Content text for this page.
    const contentText =
        [ "zinc is a free and open source HTML5 browser game."
        , "it require no plugins, running natively in modern browsers."
        , "zinc is licensed under version 3 of the GNU Affero General"
        , "Public License."
        , "all source code for both the server and client side is thus"
        , "publicly available for free. see below."
        ];

    // About page main loop.
    function aboutPage(displacement, dt) {
        // Fill in the background.
        ctx.save();
        ctx.fillStyle = darkBgPattern;
        ctx.fillRect(0, 0, Main.width, Main.height);
        ctx.restore();

        // Draw title text.
        ctx.save();
        ctx.font = "128px 'Noto Sans', sans-serif";
        ctx.textAlign = "center";
        ctx.fillStyle = textBgPattern;
        ctx.fillText("about", Main.width / 2, 140);
        ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
        ctx.lineWidth = 2;
        ctx.strokeText("about", Main.width / 2, 140);
        ctx.restore();

        // Draw content text.
        ctx.save();
        ctx.font = "36px 'Noto Sans', sans-serif";
        ctx.textAlign = "center";
        ctx.fillStyle = "#777";
        contentText.forEach(
            (text, i) => ctx.fillText(text, Main.width / 2, 212 + i * 54)
        );
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
        drawMouseTrail(ctx, mouseState);

        // Draw mouse movement particle effects.
        drawAndUpdateMouseSparks(ctx, mouseState, dt);

        // Draw mouse click effect.
        drawClickEffect(ctx, mouseState, dt);
    }

    return aboutPage;
};
