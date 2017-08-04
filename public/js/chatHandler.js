function ChatHandler(eventListeners,
                     ws,
                     keypressLogPush,
                     controllerKeyIndices) {
    "use strict";

    this.active = false;
    this.text = "";
    this.maxTextLength = 96;
    this.cursorTime = 0;
    this.cursorPeriod = 1024;
    this.chatDisplayPeriod = 5000;
    this.chatBubbles = new Map();
    this.maxLineLen = 24;

    const _chatKeydown = e => {
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
                this.text.length < this.maxTextLength
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

ChatHandler.prototype.incrementCursorTime = function(dt) {
    "use strict";
    this.cursorTime = (this.cursorTime + dt) % this.cursorPeriod;
};

ChatHandler.prototype.addChatBubble = function(playerName, msg) {
    "use strict";
    this.chatBubbles.set(playerName, [window.performance.now(), msg]);
};

ChatHandler.prototype.newFrame = function() {
    "use strict";
    const now = window.performance.now();
    this.chatBubbles.filter(([t]) => t + this.chatDisplayPeriod >= now);
    return this.chatBubbles;
};

ChatHandler.prototype.splitMsg = function(msg) {
    "use strict";
    const split = msg.split(" ");
    const lines = [];
    let line = "";
    let i = 0;
    while (i < split.length) {
        if (line.length >= this.maxLineLen) {
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
};
