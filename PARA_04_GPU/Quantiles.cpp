
// Quantiles.cpp, Thrust example

// calculate every k-th element in given numbers, going from 
// smallest to largest; k obtained from command line and fed 
// into the ismultk() functor

// these are the ik/n * 100 percentiles, i = 1, 2, ...

#include <stdio.h>

#include <thrust/device_vector.h>
#include <thrust/sort.h>
#include <thrust/sequence.h>
#include <thrust/copy.h>  

// functor
struct ismultk {
   const int increm;  // k in above comments
   // get k from call
   ismultk(int _increm): increm(_increm) {}
   __device__ bool operator()(const int i)
   {  return i != 0 && (i % increm) == 0;
   }
};

int main(int argc, char **argv)
{  int x[15] = {6,12,5,13,3,5,4,5,8,88,1,11,9,22,168};  
   int n=15;
   // create int vector dx on the device, init. to 
   // x[0], x[1], ..., x[n-1]
   thrust::device_vector<int> dx(x,x+n);
   // sort dx in-place
   thrust::sort(dx.begin(),dx.end());
   // create a vector seq of length n
   thrust::device_vector<int> seq(n);
   // fill seq with 0,1,2,...n-1
   thrust::sequence(seq.begin(),seq.end(),0);
   // set up space to store our quantiles
   thrust::device_vector<int> out(n);
   // obtain k from command line
   int incr = atoi(argv[1]);  
   // for each i in seq, call ismultk() on this i, and if get a 
   // true result, put dx[i] into out; return pointer to (one 
   // element past) the end of this output array
   thrust::device_vector<int>::iterator newend = 
      thrust::copy_if(dx.begin(),dx.end(),seq.begin(),out.begin(),
         ismultk(incr));
   // print results
   thrust::copy(out.begin(), newend,
      std::ostream_iterator<int>(std::cout, " "));
   std::cout << "\n";
}
