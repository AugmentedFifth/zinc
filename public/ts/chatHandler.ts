class ChatHandler {
    public active: boolean = false;

    public text: string = "";

    public cursorTime: number = 0;

    private chatBubbles = new Map<string, [number, string]>();

    static readonly cursorPeriod: number = 1024;

    private static readonly maxTextLength:     number = 96;
    private static readonly chatDisplayPeriod: number = 5000;
    private static readonly maxLineLen:        number = 24;

    constructor(
        eventListeners:       EventRegistrar,
        ws:                   WebSocket,
        keypressLogPush:      (kp: [number, string, boolean]) => void,
        controllerKeyIndices: Map<string, number>
    ) {
        const _chatKeydown = (e: KeyboardEvent) => {
            if (this.active) {
                e.preventDefault();
                if (e.key === "Enter") {
                    if (this.text) {
                        const sendChatBytes = [0x06];
                        for (let i = 0; i < this.text.length; ++i) {
                            sendChatBytes.push(this.text.charCodeAt(i));
                        }
                        ws.send(new Uint8Array(sendChatBytes).buffer);
                    }
                    this.active = false;
                    this.text = "";
                } else if (e.key === "Escape") {
                    this.active = false;
                    this.text = "";
                } else if (e.key === "Delete" || e.key === "Backspace") {
                    this.text = this.text.slice(0, -1);
                } else if (
                    e.key.length === 1 &&
                    this.text.length < ChatHandler.maxTextLength
                ) {
                    this.text += e.key;
                }
            } else {
                if (e.key === "Enter") {
                    const now = window.performance.now();

                    controllerKeyIndices.forEach(
                        (i, key) => keypressLogPush([now, key, false])
                    );

                    this.active = true;
                }
            }
        };

        window.addEventListener("keydown", _chatKeydown);
        eventListeners.register(window, "keydown", _chatKeydown);
    }

    incrementCursorTime(dt: number): void {
        this.cursorTime = (this.cursorTime + dt) % ChatHandler.cursorPeriod;
    }

    addChatBubble(playerName: string, msg: string): void {
        this.chatBubbles.set(playerName, [window.performance.now(), msg]);
    }

    newFrame(): Map<string, [number, string]> {
        const now = window.performance.now();
        this.chatBubbles.filter(
            ([t]) => t + ChatHandler.chatDisplayPeriod >= now
        );
        return this.chatBubbles;
    }

    static splitMsg(msg: string): string[] {
        const split = msg.split(" ");
        const lines = [];
        let line = "";
        let i = 0;
        while (i < split.length) {
            if (line.length >= ChatHandler.maxLineLen) {
                lines.push(line);
                line = "";
            }
            if (line.length > 0) {
                line += " ";
            }
            line += split[i];
            i++;
        }
        if (line) {
            lines.push(line);
        }
        return lines;
    }
}
