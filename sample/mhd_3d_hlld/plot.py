import matplotlib.pyplot as plt
import numpy as np
from formura_data_load import load_data

data = load_data("data", 1000, 'config.yaml')
data_XY = data[0,:,:,:]
x = data_XY[0,:,0]
y = data_XY[:,0,1]
X = data_XY[:,:,0]
Y = data_XY[:,:,1]
T = data_XY[:,:,5]

fig1 = plt.figure()
ax1 = fig1.add_subplot(111)
c = ax1.pcolor(X, Y, T, cmap="gray")
fig1.colorbar(c)
ax1.set_aspect('equal')
ax1.set_title("Temperature")
ax1.set_xlabel("x")
ax1.set_ylabel("y")
fig1.tight_layout()
fig1.savefig('Temperature.png', dpi=150)

T100 = np.loadtxt("ref_data/T100_3rd.dat")
fig2 = plt.figure()
ax2 = fig2.add_subplot(211)
ax2.plot(x, T[100,:], '+', label="formura", color='black')
ax2.plot(T100[:,0], T100[:,1], label="CANS+", color='black', linewidth=0.7)
ax2.set_xlim([0.0, np.pi*2.0])
ax2.grid()
ax2.legend()
ax2.set_ylabel("T")
ax2.set_title("y=pi")

T64 = np.loadtxt("ref_data/T64_3rd.dat")
ax3 = fig2.add_subplot(212, sharex=ax2)
ax3.plot(x, T[64,:], '+', label="formura", color='black')
ax3.plot(T64[:,0], T64[:,1], label="CANS+", color='black', linewidth=0.7)
ax3.grid()
ax3.legend()
ax3.set_xlabel("x")
ax3.set_ylabel("T")
ax3.set_title("y=0.64*pi")
fig2.tight_layout()
fig2.savefig('y_cross_section.png', dpi=150)

B_x = data_XY[:,:,9]
B_y = data_XY[:,:,10]
dx = x[1]-x[0]
dy = y[1]-y[0]
divB = np.zeros(np.shape(B_x))
for j in range(np.shape(B_x)[0]):
    for i in range(np.shape(B_x)[1]):
        divB[j-1,i-1] = (B_x[j,i] - B_x[j,i-2])/(2.0*dx) + (B_y[j,i] - B_y[j-2,i])/(2.0*dy)
fig3 = plt.figure()
ax4 = fig3.add_subplot(111)
c = ax4.pcolor(X, Y, np.abs(divB), cmap="gray", vmin=0.0)
fig3.colorbar(c)
ax4.set_aspect('equal')
ax4.set_title("|div B|")
ax4.set_xlabel("x")
ax4.set_ylabel("y")
fig3.tight_layout()
fig3.savefig('divB.png', dpi=150)

plt.show()

