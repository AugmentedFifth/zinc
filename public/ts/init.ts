class LoopIndexer {
    [index: string]: ( canvas: HTMLCanvasElement
                     , ctx:    CanvasRenderingContext2D
                     , ws:     WebSocket
                     ) => (displacement: V2, dt: number) => void;
}

class Main {
    public static doLoop: boolean = true;
    public static currentLoops: Map<string, V2> =
        new Map([["mainMenu", V2.zero()]]);
    public static wsRecvCallback: ((data: MessageEvent) => void) | null = null;
    public static lastLoop: number = 0;

    public static currGame: string;
    public static serverToJoinName: string;
    public static username: string;

    // tslint:disable-next-line:no-any
    public static uuid: any;

    public static readonly data: Data = new Data();

    public static readonly width:  number = 1280;
    public static readonly height: number = 720;
    public static readonly canvasRect: Rect = rect(0, 0, 1280, 720);

    public static readonly maxPlayerCount: number = 2;

    public static readonly clickAnimDur: number = 250; // milliseconds
    public static readonly crosshairQuad: [V2, V2, V2, V2] = [ v2(15,  0)
                                                             , v2(-15, 0)
                                                             , v2(0,  15)
                                                             , v2(0, -15)
                                                             ];
    public static readonly sparkGravity: number = 0.005;

    public static readonly transitionTime: number = 697.2; // milliseconds

    public static readonly maxProjVel:    number = 2.5;
    public static readonly maxProjAngVel: number = 0.02;

    public static loops: LoopIndexer = new LoopIndexer();

    public static isInCanvas(arg1: number | V2, arg2?: number): boolean {
        return Main.canvasRect.contains(arg1, arg2);
    }

    public static noSupportFailure(
        htmlName: string,
        plainName: string
    ): never {
        const fallback =
            "<p>Uh oh! It looks like your browser doesn't support " +
                `<code>${htmlName}</code>. ` +
                '<a href="https://www.mozilla.org/en-US/firefox/new/">' +
                "You can download Firefox for free here.</a></p>";
        const body = document.getElementById("body");
        if (body !== null) {
            body.innerHTML = fallback;
        }

        const noSupportMsg =
            "Uh oh! It looks like your browser doesn't support " +
                `${plainName}. You can download Firefox for free here: ` +
                "https://www.mozilla.org/en-US/firefox/new/";
        throw new Error(noSupportMsg);
    }

    public static init: () => void;

    public static getTransition: ( thisName:       string
                          ,        destName:       string
                          ,        dir:            Direction
                          ,        eventListeners: EventRegistrar
                          ) => (t?: number) => void;
}

Main.init = () => {
    // Disabling right mouse button to make gameplay less error-prone.
    document.addEventListener("contextmenu", e => e.preventDefault(), false);

    // Initializing `canvas` context.
    const canvas = document.getElementById("canvas") as HTMLCanvasElement;
    const ctx =
        canvas.getContext ?
            canvas.getContext("2d") as CanvasRenderingContext2D :
            Main.noSupportFailure("&lt;canvas&gt;", "<canvas>");

    // Initializing `WebSocket`, connecting to and getting UID from server.
    if (!WebSocket) {
        Main.noSupportFailure("WebSocket", "WebSocket");
    }

    const ws = new WebSocket("ws://127.0.0.1:3000/ws");
    ws.binaryType = "arraybuffer";
    const hello = new Uint8Array(
        [0x05, 0x58, 0xE6, 0x39, 0x0A, 0xC4, 0x13, 0x24]
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
        //console.log("WebSocket recv:", data);
        if (!Main.uuid) {
            Main.uuid = data.data;
            if (Main.uuid.byteLength !== 16) {
                const errMsg =
                    "UUID expected size (in bytes): 16, " +
                        `got: ${Main.uuid.byteLength}`;
                ws.close(4002, errMsg);
                throw new Error(errMsg);
            }
        } else if (Main.wsRecvCallback) {
            Main.wsRecvCallback(data);
        }
    });

    ws.addEventListener("error", err => console.log("WebSocket error:", err));

    /**
     * Function for making new transition callbacks.
     *
     * @param {string} thisName - The name of the page to be transitioned from,
     *                            as registered in `Main.currentLoops`.
     * @param {string} destName - The name of the page to be transitioned to.
     * @param {Direction} dir - Direction of the page to transition to,
     *                          relative to the current page.
     * @param {EventRegistrar} eventListeners - An `EventRegistrar` of event
     *                                          listeners registered by the
     *                                          current page.
     * @return {(t?: number) => void} - The new callback.
     */
    Main.getTransition = function(
        thisName: string,
        destName: string,
        dir: Direction,
        eventListeners: EventRegistrar
    ): (t?: number) => void {
        if (![0, 1, 2, 3].includes(dir)) {
            throw new Error(
                "Main.getTransition(): Expected dir to be in [0, 1, 2, 3]. " +
                    `Got: ${dir}`
            );
        }

        const relevantDim = dir === 0 || dir === 2 ? Main.height : Main.width;

        function transitionCallback(t: number = 0): void {
            if (t === 0) {
                eventListeners.forEach(
                    (target, type, fn) => target.removeEventListener(type, fn)
                );
            }

            const disp = bezier2(
                0,
                0.75,
                relevantDim,
                t / Main.transitionTime
            );

            if (Main.currentLoops.has(destName)) {
                if (t >= Main.transitionTime) {
                    Main.currentLoops.delete(thisName);
                    Main.currentLoops.set(destName, V2.zero());
                } else {
                    if (dir === 0) {
                        (Main.currentLoops.get(thisName) as V2).y = disp;
                        (Main.currentLoops.get(destName) as V2).y =
                            -relevantDim + disp + 1;
                    } else if (dir === 1) {
                        (Main.currentLoops.get(thisName) as V2).x = -disp;
                        (Main.currentLoops.get(destName) as V2).x =
                            relevantDim - disp - 1;
                    } else if (dir === 2) {
                        (Main.currentLoops.get(thisName) as V2).y = -disp;
                        (Main.currentLoops.get(destName) as V2).y =
                            relevantDim - disp - 1;
                    } else if (dir === 3) {
                        (Main.currentLoops.get(thisName) as V2).x = disp;
                        (Main.currentLoops.get(destName) as V2).x =
                            -relevantDim + disp + 1;
                    }
                }
            } else {
                if (dir === 0) {
                    (Main.currentLoops.get(thisName) as V2).y = disp;
                    Main.currentLoops.set(
                        destName,
                        v2(0, -relevantDim + disp)
                    );
                } else if (dir === 1) {
                    (Main.currentLoops.get(thisName) as V2).x = -disp;
                    Main.currentLoops.set(
                        destName,
                        v2(relevantDim - disp, 0)
                    );
                } else if (dir === 2) {
                    (Main.currentLoops.get(thisName) as V2).y = -disp;
                    Main.currentLoops.set(
                        destName,
                        v2(0, relevantDim - disp)
                    );
                } else if (dir === 3) {
                    (Main.currentLoops.get(thisName) as V2).x = disp;
                    Main.currentLoops.set(
                        destName,
                        v2(-relevantDim + disp, 0)
                    );
                }
            }

            if (Main.currentLoops.has(thisName)) {
                window.setTimeout(() => transitionCallback(t + 16.6), 16.6);
            }
        }

        return transitionCallback;
    };

    const registeredLoops = new Map();

    function draw(timestamp: number): void {
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
            // Save state (all).
            ctx.save();

            // Displace entire drawing if necessary.
            if (!displacement.null()) {
                ctx.translate(displacement.x, displacement.y);
            }

            // Run loop specified by ID, registering first if necessary.
            if (registeredLoops.has(loopId)) {
                registeredLoops.get(loopId)(displacement, dt);
            } else {
                const closuredLoop = Main.loops[loopId](canvas, ctx, ws);
                registeredLoops.set(loopId, closuredLoop);
                closuredLoop(displacement, dt);
            }

            // Restore all.
            ctx.restore();
        });

        // Unregister any locally registered loops that aren't current.
        registeredLoops.forEach((loop, loopId) => {
            if (!Main.currentLoops.has(loopId)) {
                registeredLoops.delete(loopId);
            }
        });
    }

    // Start main game loop.
    window.requestAnimationFrame(draw);
};

window.addEventListener("load", Main.init);
