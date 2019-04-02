import matplotlib.pyplot as plt
import numpy as np
from formura_data_load import load_data


def plot(ylabel, data, ref_data):
    plt.xlim([-0.5,0.5])
    plt.ylabel(ylabel)
    plt.plot(ref_x, ref_data, label="CANS+", color="black", linewidth = 0.5)
    plt.plot(x, data, '+', markersize=3.5, label="formura", color="black")
    plt.legend()
    plt.grid()


ref = np.loadtxt("ref_data/Bx2.dat")
ref_x = ref[:,0]

data = load_data("data", 1600, "config.yaml")
x   = data[0,0,:,0] - 2.0
rho = data[0,0,:,3]
p   = data[0,0,:,4]
T   = data[0,0,:,5]
u   = data[0,0,:,6]
v   = data[0,0,:,7]
w   = data[0,0,:,8]
B_x = data[0,0,:,9]
B_y = data[0,0,:,10]
B_z = data[0,0,:,11]

plt.figure(figsize=(12.8, 9.6))
plt.subplot(331)
plot("rho", rho, ref[:,3])
plt.subplot(332)
plot("p", p, ref[:,7])
plt.subplot(333)
plot("T", T, ref[:,7]/ref[:,3])
plt.subplot(334)
plot("u", u, ref[:,4])
plt.subplot(335)
plot("v", v, ref[:,5])
plt.subplot(336)
plot("w", w, ref[:,6])
plt.subplot(337)
plot("B_x", B_x, ref[:,8])
plt.subplot(338)
plot("B_y", B_y, ref[:,9])
plt.subplot(339)
plot("B_z", B_z, ref[:,10])
plt.tight_layout()
plt.savefig('x_plot.png')
plt.show()

