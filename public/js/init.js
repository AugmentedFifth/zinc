const Main = {
    doLoop: true,
    currentLoops: new Map([["mainMenu", V2.zero()]]),
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

Main.init = () => {
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

    const registeredLoops = new Map();

    function draw(timestamp) {
        // Request next animation frame right up front.
        if (Main.doLoop) {
            window.requestAnimationFrame(draw);
        }

        // Update our dt for this frame.
        const dt = Main.lastLoop ? timestamp - Main.lastLoop : 0;
        Main.lastLoop = timestamp;

        // Clear canvas.
        ctx.moveTo(0, 0);
        ctx.clearRect(0, 0, Main.width, Main.height);

        // Run all current draw loops sequentially.
        Main.currentLoops.forEach((displacement, loopId) => {
            // Save state.
            ctx.save();

            // Displace entire drawing if necessary.
            if (!displacement.null()) {
                ctx.translate(displacement.x, displacement.y);
            }

            // Run loop specified by ID, registering first if necessary.
            if (registeredLoops.has(loopId)) {
                registeredLoops.get(loopId)(displacement, dt);
            } else {
                const closuredLoop = Main[loopId](canvas, ctx);
                registeredLoops.set(loopId, closuredLoop);
                closuredLoop(displacement, dt);
            }

            // Restore all.
            ctx.restore();
        });

        // Unregister any locally registered loops that aren't current.
        for (const loopId of registeredLoops.keys()) {
            if (!Main.currentLoops.has(loopId)) {
                registeredLoops.delete(loopId);
            }
        }
    }

    // Start main game loop.
    window.requestAnimationFrame(draw);
};

window.addEventListener("load", Main.init);
