from pathlib import Path
from z3 import Int, Solver

inp = (Path(__file__).parent / '..' / 'inputs' / 'day24.txt').read_text().strip()
lines = []
for l in inp.splitlines():
    a, b = l.split(' @ ')
    pos = [int(w) for w in a.split(', ')]
    vel = [int(w) for w in b.split(', ')]
    lines.append((pos, vel))

fx,  fy,  fz  = Int("fx"),  Int("fy"),  Int("fz")
fdx, fdy, fdz = Int("fdx"), Int("fdy"), Int("fdz")
s = Solver()
for i, ((x,y,z), (dx,dy,dz)) in enumerate(lines):
    t = Int(f"t{i}")
    s.add(t >= 0)
    s.add(x + dx * t == fx + fdx * t)
    s.add(y + dy * t == fy + fdy * t)
    s.add(z + dz * t == fz + fdz * t)
assert str(s.check()) == 'sat'

print(s.model().eval(fx + fy + fz))