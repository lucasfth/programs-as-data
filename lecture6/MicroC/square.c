
int *sump;

void main(int n)
{
    int arr[20];

    squares(n, arr);

    arrsum(n, arr, sump);

    print *sump;
}

/*
While loop version
void squares(int n, int arr[])
{
    int i;
    i = 0;
    if (n <= 20)
    {
        while (i < n)
        {
            arr[i] = i * i;
            i = i + 1;
        }
    }
}
*/

//For loop
void squares(int n, int arr[])
{
    int i;
    if (n <= 20)
    {
        for(i = 0; i < n; i = i +1){
            arr[i] = i * i;
        }
    }
}

/*
While loop version
void arrsum(int n, int arr[], int *sump)
{
    int i;
    i = 0;
    int sum;
    sum = 0;
    while (i < n)
    {
        sum = sum + arr[i];
        i = i + 1;
    }
    *sump = sum;
}
*/

//For loop
void arrsum(int n, int arr[], int *sump)
{
    int i;
    int sum;
    sum = 0;
    
    for(i = 0; i < n; i = i+1){
        sum = sum + arr[i];
    }

    *sump = sum;
}
