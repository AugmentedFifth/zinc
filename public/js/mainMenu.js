Main.mainMenu = (canvas, ctx) => {
    "use strict";

    // Holding a local copy of event listeners so they can be unloaded.
    const eventListeners = new Map();

    // Initialize patterns.
    const darkBg = document.getElementById("45-deg-dark-jean-pattern");
    const darkBgPattern = ctx.createPattern(darkBg, "repeat");

    const textBg = document.getElementById("pink-dust-pattern");
    const textBgPattern = ctx.createPattern(textBg, "repeat");

    const buttonBg = document.getElementById("grey-linen-pattern");
    const buttonBgPattern = ctx.createPattern(buttonBg, "repeat");

    // Circular buffer to store mouse position history.
    const mouseLocs = new CircularBuffer(168);

    const _mousemove = e => {
        const rect = canvas.getBoundingClientRect();
        mouseLocs.cons(v2(e.clientX - rect.left, e.clientY - rect.top));
    };
    canvas.addEventListener("mousemove", _mousemove);
    eventListeners.set("mousemove", _mousemove);

    const _mouseenter = () => mouseLocs.clear();
    canvas.addEventListener("mouseenter", _mouseenter);
    eventListeners.set("mouseenter", _mouseenter);

    // Circular buffer to store mouse particle effect state.
    const mouseSparks = new CircularBuffer(64);
    let lastSparkPos = V2.zero();

    // Generating button data.
    const aboutCallback = Main.getTransition(
        "mainMenu",
        "aboutPage",
        1,
        eventListeners
    );

    const buttons =
        [ [rect(150, 550, 325, 100), 7, "play"]
        , [rect(805, 550, 325, 100), 7, "about", aboutCallback]
        ];
    const screwAngles =
        new Float64Array(buttons.length * 4)
            .map(() => Math.PI * Math.random());

    // Cursor click animation state and trigger.
    let clickAnim = null;
    const clickAnimDur = 250;
    const _click = e => {
        const boundingRect = canvas.getBoundingClientRect();
        const clickPos = v2(
            e.clientX - boundingRect.left,
            e.clientY - boundingRect.top
        );
        clickAnim = [clickPos, clickAnimDur];

        if (Main.currentLoops.size !== 1) {
            return;
        }
        const clickedButton = buttons.find(
            ([box]) => box.contains(clickPos)
        );
        if (clickedButton) {
            const callback = clickedButton[3];
            if (callback) {
                callback();
            }
        }
    };
    canvas.addEventListener("click", _click);
    eventListeners.set("click", _click);
    const crosshairQuad = [v2(15, 0), v2(-15, 0), v2(0, 15), v2(0, -15)];

    // Main menu loop.
    function mainMenu(displacement, dt) {
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
        ctx.save();
        // Grabbing roughly current mouse position; is also used further below.
        const newestMouseLoc = mouseLocs.get();
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
            ctx.font = "64px 'Noto Sans', sans-serif";
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";
            ctx.fillStyle = textBgPattern;
            const yOffset = -4; // Magic number lmao
            ctx.fillText(
                text,
                box.x + box.width / 2,
                box.y + box.height / 2 + yOffset
            );
            ctx.strokeStyle = "rgba(144, 144, 144, 0.5)";
            ctx.lineWidth = 2;
            ctx.strokeText(
                text,
                box.x + box.width / 2,
                box.y + box.height / 2 + yOffset
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
        mouseLocs.forEachTail(loc => {
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
        });
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

        // Draw mouse click effect.
        if (clickAnim) {
            ctx.save();
            ctx.lineWidth = 3;
            ctx.lineCap = "square";
            ctx.globalCompositeOperation = "screen";
            const [pos, t] = clickAnim;
            const progress = t / clickAnimDur;
            ctx.strokeStyle = `rgba(96, 96, 96, ${progress})`;
            crosshairQuad.forEach(vec => {
                ctx.beginPath();
                const shifted = pos.add(vec);
                const regresado = pos.add(
                    vec.scalarMult(1 - progress * progress)
                );
                ctx.moveTo(shifted.x, shifted.y);
                ctx.lineTo(regresado.x, regresado.y);
                ctx.stroke();
                ctx.closePath();
            });
            if (t >= dt) {
                clickAnim = [pos, t - dt];
            } else {
                clickAnim = null;
            }
            ctx.restore();
        }
    }

    return mainMenu;
};
