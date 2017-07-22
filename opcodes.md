# opcodes

## *send* opcodes (server -> client)

| leading byte | semantics                               |
|--------------|-----------------------------------------|
| 0            | This is a list of active games/servers. |
| 1            | New game request accepted/denied.       |

## *recv* opcodes (client -> server)

| leading byte | semantics                               |
|--------------|-----------------------------------------|
| 0            | Send me a list of active games/servers. |
| 1            | Request a new game.                     |
