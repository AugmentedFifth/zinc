const Main = {
    doLoop: true,
    lastLoop: 0,
    width: 1280,
    height: 720,
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

    const ws = new WebSocket("ws://127.0.0.1:8000/ws");
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

    // Set up constants that will be used in main menu draw loop.
    const darkBg = document.getElementById("45-deg-dark-jean-pattern");
    const darkBgPattern = ctx.createPattern(darkBg, "repeat");

    const textBg = document.getElementById("pink-dust-pattern");
    const textBgPattern = ctx.createPattern(textBg, "repeat");

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
        // /*
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
        // */

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

        // Restore all.
        ctx.restore();
    }

    // Start main game loop.
    requestAnimationFrame(mainMenu);
    console.log("loaderino");
});