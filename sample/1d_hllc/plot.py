import matplotlib.pyplot as plt
import numpy as np
from formura_data_load import load_data

gamma = 1.4
gamma1 = gamma - 1.0

def plot(ylabel, data, ref_data):
    plt.xlim([1.8,2.6])
    plt.ylim([np.min(ref_data)-0.1,np.max(ref_data)+0.1])
    plt.xlabel("x")
    plt.ylabel(ylabel)
    plt.plot(ref_x, ref_data, color='black')
    plt.plot(x, data, '+', color='black')

ref = np.loadtxt('ref_data/exact.dat')
ref_x = ref[:,0]

data = load_data("data", 100, "config.yaml")
x   = data[0,0,:,0]
rho = data[0,0,:,3]
p   = data[0,0,:,4]
T   = data[0,0,:,5]
u   = data[0,0,:,6]
v   = data[0,0,:,7]
w   = data[0,0,:,8]

plt.figure(dpi=150)
plt.subplot(221)
plot("rho", rho, ref[:,1])
plt.subplot(222)
plot("u", u, ref[:,2])
plt.subplot(223)
plot("p", p, ref[:,3])
plt.subplot(224)
plot("T", T, gamma1*ref[:,4])
plt.tight_layout()
plt.savefig('x_plot.png')
plt.show()

