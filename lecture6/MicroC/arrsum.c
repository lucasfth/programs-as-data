int *sump;

void main(int n)
{
    int arr[4];
    arr[0] = 7;
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;

    int sum;
    sum = 0;

    arrsum(n, arr, sump);

    print *sump;
}
/*
While Loop version
void arrsum(int n, int arr[], int *sump)
{
    int i;
    i = 0;
    int sum;
    sum = 0;

    if (n <= 4)
    {
        while (i < n)
        {
            sum = sum + arr[i];
            i = i + 1;
        }
    }

    *sump = sum;
}
*/

//For loop version
void arrsum(int n, int arr[], int *sump)
{
    int i;
    int sum;
    sum = 0;

    if (n <= 4)
    {
        for (i = 0; i < n; i = i + 1)
        {
            sum = sum + arr[i];
        }
    }

    *sump = sum;
}