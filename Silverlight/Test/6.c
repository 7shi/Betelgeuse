typedef short int16_t;
typedef int int32_t;
typedef long int64_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long uint64_t;
typedef unsigned long size_t;
typedef void FILE;

void (*exit)(int) = (void *)0x00ef0000;
int (*fputc)(int, FILE *) = (void *)0x00ef0004;
int (*fgetc)(FILE *) = (void *)0x00ef0008;
FILE *(*fopen)(const char *, const char *) = (void *)0x00ef000c;
int (*fclose)(FILE *) = (void *)0x00ef0010;
int (*fwrite)(const void *, size_t, size_t, FILE *) = (void *)0x00ef0014;
int (*fread)(void *, size_t, size_t, FILE *) = (void *)0x00ef0018;
int (*fseek)(FILE *, long, int) = (void *)0x00ef001c;
int (*printf)(const char *, ...) = (void *)0x00ef0020;
int (*fprintf)(FILE *, const char *, ...) = (void *)0x00ef0024;
int (*snprintf)(char *, size_t, const char *, ...) = (void *)0x00ef0028;

int strcmp(const char *, const char *);
char *strncpy(char *, const char *, size_t);
char *strncat(char *, const char *, size_t);
size_t strlen(const char *);
void *memcpy(void *, const void *, size_t);
void *memset(void *, int, size_t);

/* -------------------------------- */

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

/* libc implementation */

int strcmp(const char *a, const char *b)
{
    for (; *a || *b; a++, b++)
    {
        if (*a < *b) return -1; else if (*a > *b) return 1;
    }
    return 0;
}

char *strncpy(char *dst, const char *src, size_t size)
{
    for (; size > 0; size--, dst++, src++)
    {
        *dst = *src;
        if (!*src) break;
    }
    return dst;
}

char *strncat(char *dst, const char *src, size_t size)
{
    for (; size > 0 && *dst; size--, dst++);
    strncpy(dst, src, size);
    return dst;
}

size_t strlen(const char *s)
{
    size_t ret = 0;
    for (; *s; s++, ret++);
    return ret;
}

void *memcpy(void *dst, const void *src, size_t size)
{
    char *d = (char *)dst;
    const char *s = (const char *)src;
    for (; size > 0; size--, d++, s++)
        *d = *s;
    return dst;
}

void *memset(void *dst, int c, size_t len)
{
    char *d = (char *)dst;
    int i;
    for (i = 0; i < len; i++, d++) *d = (char)c;
    return dst;
}
