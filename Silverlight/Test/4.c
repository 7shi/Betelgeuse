int (*fputc)(int, void *) = (void *)0x00ef0004;

void printstr(char *s)
{
    for (; *s; s++) fputc(*s, 0);
}

void entry()
{
    printstr("Hello, World!");
}
