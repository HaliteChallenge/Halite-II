# Python 3 Sample Bots

## Running

These all depend on `hlt.py` in `airesources/Python3`. You can set 
`PYTHONPATH` to include this directory so that these bots can import the
starter kit:

    $ PYTHONPATH=../airesources/Python3 halite "python3 null.py" "python3 null.py"
    
## Strategies

Overall, these bots perform fairly poorly.

- `settlermk2` is an improvement on the `settler` bot provided as a
  starter, prioritizing closer planets.
- `crashlander` follows `settlermk2`, but takes extra ships and crashes them
  into nearby planets.
- `dogfighter` follows `settlermk2`, but moves extra ships to attack enemy
  ones.
- `fastsettler` follow `settlermk2`, but uses `hlt.warp` to get to planets
  faster.