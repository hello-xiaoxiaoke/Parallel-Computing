
#include <stdio.h>

int sqr(int *x, int n) {
	int i;
	for (i = 0; i < n; i++)
		x[i] = x[i] * x[i];
}

int main() {
	int y[10], i;
	for (i = 0; i < 5; i++)
		scanf("%d", &y[i]);
	sqr(y, 10);
	for(i = 0; i < 5; i++)
		printf("%d\n", y[i]);
}
