int (*printf)(const char *, ...) = (void *)0x00ef0020;

void main()
{
    int a, b, c, i, sum = 0;
    printf("%s, %s%c\n", "Hello", "World", '!');
    a = 1;
    b = 2;
    c = a + b;
    printf("%d + %d = %d\n", a, b, c);
    printf("&a: %p, &b: %p, &c: %p\n", &a, &b, &c);
    for (i = 1; i <= 10000; i++) sum += i;
    printf("1 + 2 + ... + 9999 + 10000 = %d\n", sum);
}
