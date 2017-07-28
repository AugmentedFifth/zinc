# opcodes

## *send* opcodes (server -> client)

| leading byte | semantics                                      |
|--------------|------------------------------------------------|
| `0x00`       | This is a list of active games/servers.        |
| `0x01`       | New game or join game request accepted/denied. |
| `0x02`       | Here's the state of the game.                  |
| `0x03`       | Here's a chat message that someone sent.       |

## *recv* opcodes (client -> server)

| leading byte | semantics                                      |
|--------------|------------------------------------------------|
| `0x00`       | Send me a list of active games/servers.        |
| `0x01`       | Request a new game.                            |
| `0x02`       | Here is info about my game.                    |
| `0x03`       | Here are my inputs.                            |
| `0x04`       | I'm leaving my game.                           |
| `0x05`       | I'd like to join this game.                    |
| `0x06`       | Here's a chat message I'm sending.             |
