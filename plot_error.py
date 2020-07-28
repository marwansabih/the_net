import matplotlib 
import matplotlib.pyplot as plt
import numpy as np

def approx(steps,xs):
  ys = []
  for i in list(range(0, len(xs)//steps)):
    start = i * steps
    ys.append(np.average(xs[start:start+steps]))
  return ys	

f = open ("error.log" , "r")
lines = f.read()
li = (lines.split())#[500:]
yss =  list( map ( lambda a : float (a), li ))
ys = approx(30,yss)
xs = list( range (1, len(ys)+1))
print (len (yss))
fig, ax = plt.subplots()
ax.plot(xs, ys)

ax.set(xlabel='iteration_steps', ylabel='Error in probality prediction',
       title='Errorplot')
ax.grid()

fig.savefig("test.png")
plt.show()

