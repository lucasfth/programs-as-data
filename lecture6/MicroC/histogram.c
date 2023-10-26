
void main(int n)
{
    int arr[7];
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 1;
    arr[3] = 1;
    arr[4] = 1;
    arr[5] = 2;
    arr[6] = 0;

    int max;
    max = 3;

    int freq[4];
    freq[0] = 0;
    freq[1] = 0;
    freq[2] = 0;
    freq[3] = 0;

    histogram(n, arr, max, freq);

    int i;
    // i = 0;

    for(i = 0; i < 4; i = i + 1)
    {
        print freq[i];
    }
    
    // while (i < 4)
    // {
    //     print freq[i];
    //     i = i + 1;
    // }
}

void histogram(int n, int ns[], int max, int freq[])
{
    int i;
    // i = 0;

    for (i = 0; i < n; i = i + 1)
    {
        freq[ns[i]] = freq[ns[i]] + 1;
    }

    // while (i < n)
    // {
    //     freq[ns[i]] = freq[ns[i]] + 1;
    //     i = i + 1;
    // }

    
}