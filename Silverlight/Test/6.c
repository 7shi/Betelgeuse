typedef short int16_t;
typedef int int32_t;
typedef long int64_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long uint64_t;

typedef void FILE;

void (*exit)(int) = (void *)0x00ef0000;
int (*fputc)(int, FILE *) = (void *)0x00ef0004;
int (*fgetc)(FILE *) = (void *)0x00ef0008;
FILE *(*fopen)(const char *, const char *) = (void *)0x00ef000c;
int (*fclose)(FILE *) = (void *)0x00ef0010;
int (*fread)(void *, int, int, FILE *) = (void *)0x00ef0014;
int (*fwrite)(const void *, int, int, FILE *) = (void *)0x00ef0018;
int (*fseek)(FILE *, int, int) = (void *)0x00ef001c;

int printf(const char *, ...);
int fprintf(FILE *, const char *, ...);
int snprintf(char *, int, const char *, ...);
int strcmp(const char *, const char *);
char *strcpy(char *, const char *);
char *strcat(char *, const char *);
int strcmp(const char *, const char *);
void *memset(void *, int, int);

/* -------------------------------- */

void entry()
{
    int a = 1, b = 2;
    char buf[32];
    printf("buf: %p\n", buf);
    snprintf(buf, sizeof(buf), "%x + %x = %x", a, b, a + b);
    fprintf(0, "[%s]", buf);
}

/* -------------------------------- */

void fsnputc(int ch, FILE *f, char **sb, int *len)
{
    if (sb)
    {
        if ((*len)-- > 0) *((*sb)++) = *len ? ch : 0;
    }
    else
        fputc(ch, f);
}

int fsnprintstr(const char *s, FILE *f, char **sb, int *len)
{
    int ret = 0;
    for (; *s; s++, ret++) fsnputc(*s, f, sb, len);
    return ret;
}

int fsnprintlong(long v, FILE *f, char **sb, int *len)
{
    char buf[32];
    char *p;
    unsigned long uv = (unsigned long)v;
    int ret = 0;
    if (v == 0)
    {
        fsnputc('0', f, sb, len);
        return 1;
    }
    else if (v < 0)
    {
        fsnputc('-', f, sb, len);
        ret++;
        uv = 0 - uv;
    }
    p = buf + sizeof(buf) - 1;
    *p = '\0';
    for (; uv; uv /= 10)
        *(--p) = '0' + (uv % 10);
    return ret + fsnprintstr(p, f, sb, len);
}

int fsnprinthex(unsigned long v, int w, FILE *f, char **sb, int *len)
{
    char buf[32];
    char *p, *start;
    int ret = 0;
    if (v == 0)
    {
        fsnputc('0', f, sb, len);
        return 1;
    }
    p = buf + sizeof(buf) - 1;
    *p = '\0';
    for (; v; v >>= 4)
        *(--p) = "0123456789abcdef"[v & 15];
    if (w < 0) w = 0;
    if (w > 16) w = 16;
    start = buf + sizeof(buf) - 1 - w;
    while (p > start) *(--p) = '0';
    return ret + fsnprintstr(p, f, sb, len);
}

int vfsnprintf(FILE *f, char **sb, int *len, const char *format, void **arg)
{
    const char *p = format;
    int ret = 0;
    for (; *p; p++)
    {
        if (*p == '%')
        {
            switch (*(++p))
            {
            case 'd':
                ret += fsnprintlong(*(int *)(arg++), f, sb, len);
                break;
            case 'x':
                ret += fsnprinthex(*(int *)(arg++), 0, f, sb, len);
                break;
            case 'p':
                fsnprintstr("0x", f, sb, len);
                ret += fsnprinthex(*(int *)(arg++), 16, f, sb, len) + 2;
                break;
            case 'c':
                fsnputc(*(char *)(arg++), f, sb, len);
                ret++;
                break;
            case 's':
                ret += fsnprintstr(*(const char **)(arg++), f, sb, len);
                break;
            case '\0':
                fsnputc('%', f, sb, len);
                ret++;
                p--;
                break;
            default:
                fsnputc('%', f, sb, len);
                fsnputc(*p, f, sb, len);
                ret += 2;
                break;
            }
        }
        else
        {
            fsnputc(*p, f, sb, len);
            ret++;
        }
    }
    return ret;
}

int printf(const char * format, ...)
{
    return vfsnprintf(0, 0, 0, format, ((void **)&format) + 9);
}

int fprintf(FILE *f, const char *format, ...)
{
    return vfsnprintf(f, 0, 0, format, ((void **)&format) + 9);
}

int snprintf(char *sb, int len, const char *format, ...)
{
    return vfsnprintf(0, &sb, &len, format, ((void **)&format) + 11);
}
