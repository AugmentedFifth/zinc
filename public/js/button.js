function drawButtons(ctx,
                     mouseState,
                     buttons,
                     buttonBgPattern,
                     textBgPattern,
                     screwAngles,
                     fontSize,
                     outline) {
    "use strict";

    if (fontSize === undefined) {
        fontSize = 64;
    }
    if (outline === undefined) {
        outline = true;
    }

    ctx.save();

    const newestMouseLoc = mouseState.mouseLocs.get();
    buttons.forEach(([box, screwRadius, text], i) => {
        // Main button body.
        ctx.fillStyle = buttonBgPattern;
        ctx.globalCompositeOperation = "luminosity";
        ctx.fillRect(box.x, box.y, box.width, box.height);
        ctx.globalCompositeOperation = "source-over";
        ctx.lineWidth = 4;
        ctx.strokeStyle = "#202020";
        ctx.strokeRect(box.x, box.y, box.width, box.height);
        if (newestMouseLoc && box.contains(newestMouseLoc)) {
            ctx.globalCompositeOperation = "darken";
            ctx.fillStyle = "#fff";
            ctx.fillRect(box.x, box.y, box.width, box.height);
            ctx.strokeStyle = "#fff";
            ctx.strokeRect(box.x, box.y, box.width, box.height);
        }

        // Screws.
        ctx.strokeStyle = "#7f7f7f";
        ctx.fillStyle = textBgPattern;
        ctx.lineWidth = 1;
        const screwOffset = 2 * screwRadius + 1;
        const minX = box.x + screwOffset;
        const maxX = box.x + box.width - screwOffset;
        const minY = box.y + screwOffset;
        const maxY = box.y + box.height - screwOffset;
        [ v2(minX, minY)
        , v2(minX, maxY)
        , v2(maxX, minY)
        , v2(maxX, maxY)
        ].forEach((loc, j) => {
            ctx.beginPath();
            ctx.arc(loc.x, loc.y, screwRadius, 0, Math.PI * 2, false);
            ctx.fill();
            ctx.stroke();
            const xOffset = screwRadius * Math.cos(screwAngles[4 * i + j]);
            const yOffset = screwRadius * Math.sin(screwAngles[4 * i + j]);
            ctx.moveTo(
                loc.x - xOffset,
                loc.y - yOffset
            );
            ctx.lineTo(
                loc.x + xOffset,
                loc.y + yOffset
            );
            ctx.stroke();
            ctx.closePath();
        });

        // Text.
        ctx.font = `${fontSize}px 'Noto Sans', sans-serif`;
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.fillStyle = textBgPattern;
        const yOffset = -4; // Magic number lmao
        ctx.fillText(
            text,
            box.x + box.width / 2,
            box.y + box.height / 2 + yOffset
        );
        if (outline) {
            ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
            ctx.lineWidth = 2;
            ctx.strokeText(
                text,
                box.x + box.width / 2,
                box.y + box.height / 2 + yOffset
            );
        }
    });

    ctx.restore();
}
