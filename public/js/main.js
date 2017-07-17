const Main = {
    doLoop: true,
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
        throw Error(noSupportMsg);
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
                throw Error(errMsg);
            }
        }
    });

    ws.addEventListener("error", err => {
        console.log("WebSocket error:", err);
        console.log(err.target.url, err.target.readyState);
    });

    // Main game loop.
    function main(timestamp) {
        if (Main.doLoop) {
            requestAnimationFrame(main);
        }

        //
    }

    // Start main game loop.
    //requestAnimationFrame(main);
    console.log("loaderino");
});
