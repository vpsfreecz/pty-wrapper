pty-wrapper
===========

Run executable wrapped in pseudoterminal with UNIX domain socket
for receiving data and controlling it.

Uses simple JSON protocol, `keys` field refers to keys to send to
the executable, rows and cols (if set) control the size of the
pseudoterminal. Sample protocol messages:

```json
{"keys": "pq", "rows": null, "cols" :null}
{"keys":null,"rows":10,"cols":20}
```

Client side recieves raw data as output from the executable.

Usage
-----

```bash
nix build
# run server
./result/bin/pty-wrapper /tmp/testsock.ipc htop

# in another terminal, run sample client
sudo ./result/bin/pty-wrapper-client /tmp/testsock.ipc
```

By defualt access to UNIX socket is restricted to root (UID 0)
hence the use of `sudo` for client.

Client can send events to server, it reads lines from standard
input and sends these to wrapped application. If line starts
with character `!`, it sends a request to change pseudoterminal
size to 10, 20.
