#include "libc.h"

int main(int argc, char *argv[])
{
    int i;
    char buf[16];
    FILE *f = fopen("test.txt", "w");
    if (f)
    {
        for (i = 0; i < argc; i++)
            fprintf(f, "argv[%d] = \"%s\"\n", i, argv[i]);
        memset(buf, 'a', 4);
        buf[4] = 0;
        fprintf(f, "%s\n", buf);
        strncat(buf, "bbbb", sizeof(buf));
        fprintf(f, "%s\n", buf);
        strncpy(buf, "abcd", sizeof(buf));
        fprintf(f, "%s\n", buf);
        fprintf(f, "%d\n", strcmp("a", "b"));
        fprintf(f, "%d\n", strcmp("a", "ab"));
        fprintf(f, "%d\n", strcmp("ab", "a"));
        fprintf(f, "[%08x] [%8d]\n", 0x1234, 1234);
        fclose(f);
    }
    f = fopen("test.txt", "r");
    if (f)
    {
        int ch;
        while ((ch = fgetc(f)) != -1)
            printf("%c", (char)ch);
        fclose(f);
    }
    return 0;
}
