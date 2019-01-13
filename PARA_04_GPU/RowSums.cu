#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>

// CUDA example:  finds row sums of an integer matrix m

// find1elt() finds the row sum of one row of the nxn matrix m, 
// storing the result in the corresponding position in the 
// rowsum array rs; matrix is in 1-dimensional, row-major order

// this is the "kernel", which each thread on the GPU executes
__global__ void find1elt(int *m, int *rs, int n)
{
   // this thread will handle row # rownum
   int rownum = blockIdx.x;  
   int sum = 0;
   for (int k = 0; k < n; k++)
      sum += m[rownum*n+k];
   rs[rownum] = sum;
}

// the remaining code is executed on the CPU
int main(int argc, char **argv)
{
    int n = atoi(argv[1]);  // number of matrix rows/cols
    int *hm, // host matrix
        *dm, // device matrix
        *hrs, // host rowsums
        *drs; // device rowsums
    // size of matrix in bytes
    int msize = n * n * sizeof(int);  
    // allocate space for host matrix
    hm = (int *) malloc(msize);  
    // as a test, fill matrix with consecutive integers
    int t = 0,i,j;
    for (i = 0; i < n; i++) {
       for (j = 0; j < n; j++) {
          hm[i*n+j] = t++;
       }
    }
    // allocate matrix space at device 
    cudaMalloc((void **)&dm,msize);
    // copy host matrix to device matrix
    cudaMemcpy(dm,hm,msize,cudaMemcpyHostToDevice);
    // allocate host, device rowsum arrays
    int rssize = n * sizeof(int);
    hrs = (int *) malloc(rssize);  
    cudaMalloc((void **)&drs,rssize);
    // set up threads structure parameters
    dim3 dimGrid(n,1);  // n blocks in the grid
    dim3 dimBlock(1,1,1);  // 1 thread per block
    // launch the kernel
    find1elt<<<dimGrid,dimBlock>>>(dm,drs,n);
    // wait until kernel finishes
    cudaThreadSynchronize();
    // copy row vector from device to host
    cudaMemcpy(hrs,drs,rssize,cudaMemcpyDeviceToHost);
    // check results
    if (n < 10) for(int i=0; i<n; i++) printf("%d\n",hrs[i]);
    // clean up, very important 
    free(hm);
    cudaFree(dm);
    free(hrs);
    cudaFree(drs);
}
