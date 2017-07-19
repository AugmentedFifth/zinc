const Main = {
    doLoop: true,
    lastLoop: 0,
    width: 1280,
    height: 720,
    sparkGravity: 0.005,
    isInCanvas: pos => {
        "use strict";
        return Main.rectContains(0, 0, Main.width, Main.height, pos);
    },
    rectContains: (rectX, rectY, width, height, pos) => {
        "use strict";
        return pos.x >= rectX &&
               pos.y >= rectY &&
               pos.x <= rectX + width &&
               pos.y <= rectY + height;
    },
    noSupportFailure: (htmlName, plainName) => {
        "use strict";

        const fallback =
            "<p>Uh oh! It looks like your browser doesn't support " +
                `<code>${htmlName}</code>. ` +
                '<a href="https://www.mozilla.org/en-US/firefox/new/">' +
                "You can download Firefox for free here.</a></p>";
        document.getElementById("body").innerHTML = fallback;

        const noSupportMsg =
            "Uh oh! It looks like your browser doesn't support " +
                `${plainName}. You can download Firefox for free here: ` +
                "https://www.mozilla.org/en-US/firefox/new/";
        throw new Error(noSupportMsg);
    }
};

window.addEventListener("load", () => {
    "use strict";

    // Initializing `canvas` context.
    const canvas = document.getElementById("canvas");
    const ctx =
        canvas.getContext ?
            canvas.getContext("2d") :
            Main.noSupportFailure("&lt;canvas&gt;", "<canvas>");

    // Initializing `WebSocket`, connecting to and getting UID from server.
    if (!WebSocket) {
        Main.noSupportFailure("WebSocket", "WebSocket");
    }

    const ws = new WebSocket("ws://50.53.140.240:3000/ws");
    ws.binaryType = "arraybuffer";
    const hello = new Uint8Array(
        [0x05, 0x58, 0xe6, 0x39, 0x0a, 0xc4, 0x13, 0x24]
    ).buffer;

    window.addEventListener(
        "beforeunload",
        () => ws.close(4001, "Page unload")
    );

    ws.addEventListener("open", opener => {
        console.log("WebSocket is open.", opener);
        ws.send(hello);
    });

    ws.addEventListener("close", closer => {
        console.log("WebSocket is closed.", closer);
    });

    ws.addEventListener("message", data => {
        console.log("WebSocket recv:", data);
        if (!Main.uuid) {
            Main.uuid = data.data;
            if (Main.uuid.byteLength !== 16) {
                const errMsg =
                    "UUID expected size (in bytes): 16, " +
                        `got: ${Main.uuid.byteLength}`;
                ws.close(4002, errMsg);
                throw new Error(errMsg);
            }
        }
    });

    ws.addEventListener("error", err => {
        console.log("WebSocket error:", err);
        console.log(err.target.url, err.target.readyState);
    });

    // Setting up constants that will be used in main menu draw loop.
    const darkBg = document.getElementById("45-deg-dark-jean-pattern");
    const darkBgPattern = ctx.createPattern(darkBg, "repeat");

    const textBg = document.getElementById("pink-dust-pattern");
    const textBgPattern = ctx.createPattern(textBg, "repeat");

    const buttonBg = document.getElementById("grey-linen-pattern");
    const buttonBgPattern = ctx.createPattern(buttonBg, "repeat");

    // Circular buffer to store mouse position history.
    const mouseLocs = new CircularBuffer(168);
    canvas.addEventListener(
        "mousemove",
        e => {
            const rect = canvas.getBoundingClientRect();
            mouseLocs.cons(v2(e.clientX - rect.left, e.clientY - rect.top));
        }
    );
    canvas.addEventListener("mouseenter", () => mouseLocs.clear());

    // Circular buffer to store mouse particle effect state.
    const mouseSparks = new CircularBuffer(64);
    let lastSparkPos = V2.zero();

    // Generating random screw angles and declaring positions ahead of time.
    const buttons =
        [ [v2(150, 550), v2(325, 100), 7, "play"]
        , [v2(805, 550), v2(325, 100), 7, "about"]
        ];
    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Cursor click animation state and trigger.
    let clickAnim = null;
    const clickAnimDur = 250;
    canvas.addEventListener(
        "click",
        e => clickAnim = [v2(e.clientX, e.clientY), clickAnimDur]
    );

    // Main menu loop.
    function mainMenu(timestamp) {
        // Request next animation frame right up front.
        if (Main.doLoop) {
            requestAnimationFrame(mainMenu);
        }

        // Update our dt for this frame.
        const dt = Main.lastLoop ? timestamp - Main.lastLoop : 0;
        Main.lastLoop = timestamp;

        // Clear canvas.
        ctx.moveTo(0, 0);
        ctx.clearRect(0, 0, Main.width, Main.height);

        // Save state.
        ctx.save();

        // Fill in the background.
        ctx.save();
        ctx.fillStyle = darkBgPattern;
        ctx.fillRect(0, 0, Main.width, Main.height);
        ctx.restore();

        // Draw centering lines for debugging.
        /*
        ctx.save();
        ctx.lineWidth = 2;
        ctx.strokeStyle = "rgba(144, 44, 44, 0.75)";
        ctx.setLineDash([4, 4]);
        ctx.beginPath();
        ctx.moveTo(Main.width / 2, 0);
        ctx.lineTo(Main.width / 2, Main.height);
        ctx.moveTo(0, Main.height / 2);
        ctx.lineTo(Main.width, Main.height / 2);
        ctx.stroke();
        ctx.restore();
        */

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
        ctx.save();
        // Grabbing roughly current mouse position; is also used further below.
        const newestMouseLoc = mouseLocs.get();
        buttons.forEach(([bLoc, dims, screwRadius, text], i) => {
            // Main button body.
            ctx.fillStyle = buttonBgPattern;
            ctx.globalCompositeOperation = "luminosity";
            ctx.fillRect(bLoc.x, bLoc.y, dims.x, dims.y);
            ctx.globalCompositeOperation = "source-over";
            ctx.lineWidth = 4;
            ctx.strokeStyle = "#202020";
            ctx.strokeRect(bLoc.x, bLoc.y, dims.x, dims.y);
            if (
                newestMouseLoc &&
                Main.rectContains(
                    bLoc.x,
                    bLoc.y,
                    dims.x,
                    dims.y,
                    newestMouseLoc
                )
            ) {
                ctx.globalCompositeOperation = "darken";
                ctx.fillStyle = "#fff";
                ctx.fillRect(bLoc.x, bLoc.y, dims.x, dims.y);
                ctx.strokeStyle = "#fff";
                ctx.strokeRect(bLoc.x, bLoc.y, dims.x, dims.y);
            }

            // Screws.
            ctx.strokeStyle = "#7f7f7f";
            ctx.fillStyle = textBgPattern;
            ctx.lineWidth = 1;
            const screwOffset = 2 * screwRadius + 1;
            const minX = bLoc.x + screwOffset;
            const maxX = bLoc.x + dims.x - screwOffset;
            const minY = bLoc.y + screwOffset;
            const maxY = bLoc.y + dims.y - screwOffset;
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
            });

            // Text.
            ctx.font = "64px 'Noto Sans', sans-serif";
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";
            ctx.fillStyle = textBgPattern;
            const yOffset = -4; // Magic number lmao
            ctx.fillText(
                text,
                bLoc.x + dims.x / 2,
                bLoc.y + dims.y / 2 + yOffset
            );
            ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
            ctx.lineWidth = 2;
            ctx.strokeText(
                text,
                bLoc.x + dims.x / 2,
                bLoc.y + dims.y / 2 + yOffset
            );
        });
        ctx.restore();

        // Draw mouse trails.
        ctx.save();
        ctx.lineWidth = 2;
        const oldestMouseLoc = mouseLocs.peek();
        let [oldX, oldY] =
            oldestMouseLoc ?
                [oldestMouseLoc.x, oldestMouseLoc.y] :
                [0, 0];
        let mouseLocCounter = 0;
        mouseLocs.forEachTail(
            loc => {
                if (mouseLocCounter % 24 === 0) {
                    ctx.strokeStyle =
                        `rgba(144, 144, 144, ${mouseLocCounter / 256})`;
                    ctx.beginPath();
                    ctx.moveTo(oldX, oldY);
                    ctx.lineTo(loc.x, loc.y);
                    ctx.closePath();
                    ctx.stroke();
                    [oldX, oldY] = [loc.x, loc.y];
                }
                mouseLocCounter++;
            }
        );
        ctx.beginPath();
        ctx.moveTo(oldX, oldY);
        if (newestMouseLoc) {
            ctx.lineTo(newestMouseLoc.x, newestMouseLoc.y);

            if (!newestMouseLoc.equals(lastSparkPos)) {
                // Add a spark.
                const xVel = 0.375 * Math.random() *
                             (Math.random() < 0.5 ? 1 : -1);
                const yVel = -0.5 * Math.random() + 0.125;
                lastSparkPos = newestMouseLoc;
                mouseSparks.cons(
                    [ newestMouseLoc
                    , v2(xVel, yVel)
                    , 0
                    , Math.floor(128 + 96 * Math.random())
                    ]
                );
            }
        }
        ctx.closePath();
        ctx.stroke();
        ctx.restore();

        // Draw mouse particle effects.
        ctx.save();
        ctx.globalCompositeOperation = "lighter";
        mouseSparks.forEachBuffer(spark => {
            if (!spark) {
                return;
            }
            const [pos, vel, age, green] = spark; // jshint ignore: line
            const a = Math.max((750 - age) / 1000, 0.0625);
            ctx.fillStyle = `rgba(242, ${green}, 144, ${a})`;
            ctx.fillRect(pos.x - 1, pos.y - 1, 2, 2);
        });
        ctx.restore();

        // Update state of mouse particles.
        mouseSparks.map(([pos, vel, age, green]) => {
            const newVel = vel.add(v2(0, Main.sparkGravity * dt));
            const newPos = pos.add(newVel.scalarMult(dt));
            if (!Main.isInCanvas(newPos)) {
                return null;
            }
            return [newPos, newVel, age + dt, green];
        });

        // Restore all.
        ctx.restore();
    }

    // Start main game loop.
    requestAnimationFrame(mainMenu);
    console.log("loaderino");
});
