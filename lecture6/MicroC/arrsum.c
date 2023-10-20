
void main(int n) {
    int m;
    if (20 < n) { m = 20; } else { m = n; }

    int arr[20];

    squares(m, &arr);
    print &arr;
    println;

    // int arr[4];
    // arr[0] = 7;
    // arr[1] = 3;
    // arr[2] = 9;
    // arr[3] = 8;

    println;

    int j;
    j = 0;
    while (j < 3) {
        print arr[j];
        j = j + 1;
        println;
    }

    int sum;
    sum = 0;


    arrsum(m, arr, &sum);

    print sum;
    println;
}

void squares(int num, int *arrp[]) {
    print &arrp;
    println;
    print *arrp;
    println;
    int i;
    i = 0;

    while (i < num) {
        *arrp[i] = i * i;
        print *arrp[i];
        i = i + 1;
    }
    println;
}

void arrsum (int n, int arr[], int *sump) {
    int i;
    i = 0;

    while (i < n) {
        print arr[i];
        *sump = *sump + arr[i];
        i = i + 1;
    }
}
