import numpy as np
import matplotlib.pyplot as plt
from joblib import delayed,Parallel
plt.rcParams['figure.dpi'] = 200

numProc = 16
N = 20
sigma = 2.576e2
li = [0,0]
hi = [100*sigma,100*sigma]
start = 0
end = 2540
skip = 1

def plotter(i):
    data = np.loadtxt('/home/jposey/Desktop/pltMD/plt'+str(i).zfill(9)+'.dat',delimiter=',')

    fig,ax = plt.subplots()
    for ii in range(1,N):
        ax.add_patch(plt.Circle((data[ii][0],data[ii][1]),sigma/2))
    plt.xlim([li[0],hi[0]])
    plt.ylim([li[1],hi[1]])
    plt.title('t = '+str(data[0][0])+' ps')
    ax.set_aspect('equal')
    plt.savefig('/home/jposey/git/SimpleMD/plt'+str(i).zfill(9)+'.png')
    plt.close()

Parallel(n_jobs=numProc)(delayed(plotter)(ii) for ii in range(start,end+skip,skip))