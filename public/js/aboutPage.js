Main.aboutPage = (canvas, ctx) => {
    "use strict";

    // Initialize patterns.
    const darkBg = document.getElementById("45-deg-dark-jean-pattern");
    const darkBgPattern = ctx.createPattern(darkBg, "repeat");

    //const textBg = document.getElementById("pink-dust-pattern");
    //const textBgPattern = ctx.createPattern(textBg, "repeat");

    //const buttonBg = document.getElementById("grey-linen-pattern");
    //const buttonBgPattern = ctx.createPattern(buttonBg, "repeat");

    function aboutPage(displacement, dt) {
        // Fill in the background.
        ctx.save();
        ctx.fillStyle = darkBgPattern;
        ctx.fillRect(0, 0, Main.width, Main.height);
        ctx.restore();
    }

    return aboutPage;
};
