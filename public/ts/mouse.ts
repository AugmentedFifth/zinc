abstract class AbstractMouseState {
    public newestMouseLoc: V2 | null = null;

    public clickAnim: [V2, number] | null = null;

    public constructor(
        canvas:         HTMLCanvasElement,
        eventListeners: EventRegistrar,
        buttons:        Button[]
    ) {
        // Cursor click animation state and trigger.
        const _click = (e: MouseEvent) => {
            const boundingRect = canvas.getBoundingClientRect();
            const clickPos = v2(
                e.clientX - boundingRect.left,
                e.clientY - boundingRect.top
            );
            this.clickAnim = [clickPos, Main.clickAnimDur];

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
        eventListeners.register(canvas, "click", _click);
    }

    public drawClickEffect(ctx: CanvasRenderingContext2D, dt: number): void {
        if (this.clickAnim !== null) {
            ctx.save();

            ctx.lineWidth = 3;
            ctx.lineCap = "square";
            ctx.globalCompositeOperation = "screen";
            const [pos, t] = this.clickAnim;
            const progress = t / Main.clickAnimDur;
            ctx.strokeStyle = `rgba(96, 96, 96, ${progress})`;
            Main.crosshairQuad.forEach(vec => {
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
                this.clickAnim = [pos, t - dt];
            } else {
                this.clickAnim = null;
            }

            ctx.restore();
        }
    }
}

class MouseState extends AbstractMouseState {
    public mouseLocs: CircularBuffer<V2> = new CircularBuffer(168);

    /** Circular buffer to store mouse particle effect state. */
    public mouseSparks: CircularBuffer<[V2, V2, number, number]> =
        new CircularBuffer(64);

    public lastSparkPos: V2 = V2.zero();

    public constructor(
        canvas:         HTMLCanvasElement,
        eventListeners: EventRegistrar,
        buttons:        Button[]
    ) {
        super(canvas, eventListeners, buttons);

        const _mousemove = (e: MouseEvent) => {
            const boundingRect = canvas.getBoundingClientRect();
            const mouseLoc = v2(
                e.clientX - boundingRect.left,
                e.clientY - boundingRect.top
            );
            this.mouseLocs.cons(mouseLoc);
            this.newestMouseLoc = mouseLoc;
        };
        canvas.addEventListener("mousemove", _mousemove);
        eventListeners.register(canvas, "mousemove", _mousemove);

        const _mouseenter = () => this.mouseLocs.clear();
        canvas.addEventListener("mouseenter", _mouseenter);
        eventListeners.register(canvas, "mouseenter", _mouseenter);
    }

    public drawMouseTrail(ctx: CanvasRenderingContext2D): void {
        ctx.save();

        ctx.lineWidth = 2;
        const oldestMouseLoc = this.mouseLocs.peek();
        let [oldX, oldY] =
            oldestMouseLoc ?
                [oldestMouseLoc.x, oldestMouseLoc.y] :
                [0, 0];
        let mouseLocCounter = 0;
        this.mouseLocs.forEachTail(loc => {
            if (loc === null) {
                return;
            }
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
        if (this.newestMouseLoc !== null) {
            ctx.lineTo(this.newestMouseLoc.x, this.newestMouseLoc.y);

            if (!this.newestMouseLoc.equals(this.lastSparkPos)) {
                // Add a spark.
                const xVel = 0.375 * Math.random() *
                             (Math.random() < 0.5 ? 1 : -1);
                const yVel = -0.5 * Math.random() + 0.125;
                this.lastSparkPos = this.newestMouseLoc;
                this.mouseSparks.cons([
                    this.newestMouseLoc.clone(),
                    v2(xVel, yVel),
                    0,
                    Math.floor(128 + 96 * Math.random())
                ]);
            }
        }
        ctx.closePath();
        ctx.stroke();

        ctx.restore();
    }

    public drawAndUpdateMouseSparks(
        ctx: CanvasRenderingContext2D,
        dt: number
    ): void {
        ctx.save();

        ctx.globalCompositeOperation = "lighter";
        this.mouseSparks.forEachBuffer(spark => {
            if (!spark) {
                return;
            }
            const [pos, , age, green] = spark;
            const a = Math.max((750 - age) / 1000, 0.0625);
            ctx.fillStyle = `rgba(242, ${green}, 144, ${a})`;
            ctx.fillRect(pos.x - 1, pos.y - 1, 2, 2);
        });
        ctx.restore();

        // Update state of mouse particles.
        this.mouseSparks.map(spark => {
            if (spark === null) {
                return null;
            }
            const [pos, vel, age, green] = spark;
            const newVel = vel.add(v2(0, Main.sparkGravity * dt));
            const newPos = pos.add(newVel.scalarMult(dt));
            if (!Main.isInCanvas(newPos)) {
                return null;
            }
            return [newPos, newVel, age + dt, green];
        });
    }
}

class MouseStateNoTrail extends AbstractMouseState {
    public constructor(
        canvas:         HTMLCanvasElement,
        eventListeners: EventRegistrar,
        buttons:        Button[]
    ) {
        super(canvas, eventListeners, buttons);

        const _mousemove = (e: MouseEvent) => {
            const boundingRect = canvas.getBoundingClientRect();
            this.newestMouseLoc = v2(
                e.clientX - boundingRect.left,
                e.clientY - boundingRect.top
            );
        };
        canvas.addEventListener("mousemove", _mousemove);
        eventListeners.register(canvas, "mousemove", _mousemove);
    }
}
