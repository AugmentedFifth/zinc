# opcodes

## *send* opcodes (server -> client)

| leading byte | semantics                               |
|--------------|-----------------------------------------|
| `0x00`       | This is a list of active games/servers. |
| `0x01`       | New game request accepted/denied.       |

## *recv* opcodes (client -> server)

| leading byte | semantics                               |
|--------------|-----------------------------------------|
| `0x00`       | Send me a list of active games/servers. |
| `0x01`       | Request a new game.                     |
| `0x02`       | Here is info about my game.             |
| `0x03`       | Here are my movements.                  |
| `0x04`       | I'm leaving my game.                    |
