n = 200
pythagoras = [(a+1,b+1,c+1) for c in range(n) for b in range(c) for a in range(b) if (a+1)**2 + (b+1)**2 == (c+1)**2]
print pythagoras
