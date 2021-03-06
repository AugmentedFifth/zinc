function drawButtons(
    ctx:             CanvasRenderingContext2D,
    mouseState:      AbstractMouseState,
    buttons:         Button[],
    buttonBgPattern: FillStyle,
    textBgPattern:   FillStyle,
    screwAngles:     Float64Array,
    fontSize:        number  = 64,
    outline:         boolean = true
): void {
    ctx.save();

    buttons.forEach(([box, screwRadius, text], i) => {
        // Main button body.
        ctx.fillStyle = buttonBgPattern;
        ctx.globalCompositeOperation = "luminosity";
        ctx.fillRect(box.x, box.y, box.width, box.height);
        ctx.globalCompositeOperation = "source-over";
        ctx.lineWidth = 4;
        ctx.strokeStyle = "#202020";
        ctx.strokeRect(box.x, box.y, box.width, box.height);
        if (
            mouseState.newestMouseLoc &&
            box.contains(mouseState.newestMouseLoc)
        ) {
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
        ctx.fillText(
            text,
            box.x + box.width / 2,
            box.y + box.height / 2
        );
        if (outline) {
            ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
            ctx.lineWidth = 2;
            ctx.strokeText(
                text,
                box.x + box.width / 2,
                box.y + box.height / 2
            );
        }
    });

    ctx.restore();
}
