import numpy as np
import yaml

def read_yaml(yaml_path):
    with open(yaml_path, "r") as f:
        config = yaml.load(f)
    mpi_shape = config['mpi_shape'] if 'mpi_shape' in config else [1]
    grid_per_node = config['grid_per_node']
    return (mpi_shape, grid_per_node)

def origin_index(x):
    dx = np.diff(x)
    edge = np.where(dx < 0.0)[0]
    return edge[0] + 1 if (len(edge) != 0) else 0
    
def load_data(prefix, step, yaml_path):
    mpi_shape, grid_per_node = read_yaml(yaml_path)
    if (len(grid_per_node) == 1):
        grid_per_node.append(1)
        mpi_shape.append(1)
    if (len(grid_per_node) == 2):
        grid_per_node.append(1)
        mpi_shape.append(1)

    data_list = [ [ [ 
        np.loadtxt('%s/%d_%d.dat'%(prefix, step, nx + ny*mpi_shape[0] + nz*mpi_shape[0]*mpi_shape[1]))
        for nx in range(mpi_shape[0]) ]
        for ny in range(mpi_shape[1]) ]
        for nz in range(mpi_shape[2]) ]
    col = np.shape(data_list[0][0][0])[1]
    data_cube = [ [ [ 
        [ data_list[nz][ny][nx].reshape(grid_per_node[2], grid_per_node[1], grid_per_node[0], col) ]
        for nx in range(mpi_shape[0]) ]
        for ny in range(mpi_shape[1]) ]
        for nz in range(mpi_shape[2]) ]
    data = np.block(data_cube)
    origin_i = origin_index(data[0,0,:,0])
    origin_j = origin_index(data[0,:,0,1])
    origin_k = origin_index(data[:,0,0,2])
    data = np.roll(data, -origin_i, axis=2)
    data = np.roll(data, -origin_j, axis=1)
    data = np.roll(data, -origin_k, axis=0)
    return data

