/* libc declaration */

typedef short int16_t;
typedef int int32_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long size_t;
typedef void FILE;

#ifdef __alpha
typedef long int64_t;
typedef unsigned long uint64_t;

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
#else
typedef long long int64_t;
typedef unsigned long long uint64_t;

#ifdef _MSC_VER
#define snprintf _snprintf
#endif

int printf(const char *, ...);
int snprintf(char *, size_t, const char *, ...);
int fprintf(FILE *, const char *, ...);
FILE *fopen(const char *, const char *);
int fclose(FILE *);
int fread(void *, size_t, size_t, FILE *);
int fwrite(const void *, size_t, size_t, FILE *);
int fseek(FILE *, long, int);
int fgetc(FILE *);
int strcmp(const char *, const char *);
char *strncpy(char *, const char *, size_t);
char *strncat(char *, const char *, size_t);
size_t strlen(const char *);
void *memcpy(void *, const void *, size_t);
void *memset(void *, int, size_t);
#endif

/* extractor implementation */

uint64_t text_addr, text_size;
char text_buf[65536];

int read_text_file(FILE *f)
{
    int i;
    char buf[64], shstrtab[256];
    uint16_t e_shstrndx, e_shentsize, e_shnum;
    uint64_t e_shoff, stroff, strsize, textoff = 0;

    if (fread(buf, 64, 1, f) == 0)
    {
        printf("can not read ELF header.\n");
        return 0;
    }
    if (buf[0] != 0x7f || buf[1] != 'E' || buf[2] != 'L' || buf[3] != 'F')
    {
        printf("EI_MAG != { 0x7f, 'E', 'L', 'F' }\n");
        return 0;
    }
    if (buf[4] != 2)
    {
        printf("EI_CLASS != ELFCLASS64\n");
        return 0;
    }
    if (buf[5] != 1)
    {
        printf("EI_DATA != ELFDATA2LSB\n");
        return 0;
    }

    e_shoff = *(uint64_t *)&buf[40];
    e_shentsize = *(uint16_t *)&buf[58];
    e_shnum = *(uint16_t *)&buf[60];
    e_shstrndx = *(uint16_t *)&buf[62];

    if (e_shoff == 0)
    {
        printf("e_shoff == 0\n");
        return 0;
    }
    if (e_shstrndx == 0)
    {
        printf("e_shstrndx == 0\n");
        return 0;
    }
    if (fseek(f, (int)(e_shoff + e_shstrndx * e_shentsize + 24), 0) != 0 ||
        fread(&stroff, sizeof(stroff), 1, f) == 0 ||
        fread(&strsize, sizeof(strsize), 1, f) == 0 ||
        strsize > sizeof(shstrtab) ||
        fseek(f, (int)stroff, 0) != 0 ||
        fread(shstrtab, (int)strsize, 1, f) == 0)
    {
        printf("can not read shstrtab\n");
        return 0;
    }

    if (fseek(f, (int)e_shoff, 0) != 0)
    {
        printf("can not read section headers\n");
        return 0;
    }
    for (i = 0; i < e_shnum; i++)
    {
        const char *name;
        if (fread(buf, 64, 1, f) == 0) break;
        name = shstrtab + *(uint32_t *)buf;
        if (strcmp(name, ".text") == 0)
        {
            text_addr = *(uint64_t *)&buf[16];
            textoff = *(uint64_t *)&buf[24];
            text_size = *(uint64_t *)&buf[32];
            break;
        }
    }
    if (textoff == 0 ||
        text_size > sizeof(text_buf) ||
        fseek(f, (int)textoff, 0) != 0 ||
        fread(text_buf, (int)text_size, 1, f) == 0)
    {
        printf("can not read .text section\n");
        return 0;
    }

    return 1;
}

int read_text(const char *fn)
{
    int ret = 0;
    FILE *f = fopen(fn, "rb");
    if (f)
    {
        ret = read_text_file(f);
        fclose(f);
    }
    return ret;
}

void exec(const char *src, const char *dst)
{
    printf("%s -> %s\n", src, dst);
    if (read_text(src))
    {
        FILE *f;
        printf("text_addr: 0x%016x\n", text_addr);
        printf("text_size: 0x%016x\n", text_size);
        f = fopen(dst, "wb");
        if (f)
        {
            fwrite(text_buf, (int)text_size, 1, f);
            fclose(f);
        }
    }
}

#ifdef _MSC_VER
#define CURDIR "../Test/"
#else
#define CURDIR
#endif

const char *tests[] =
{
    "1", "2", "3", "4", "5", "6", "7t", "7d", "7a", 0
};

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        const char **t;
        for (t = tests; *t; t++)
        {
            char src[32], dst[32];
            snprintf(src, sizeof(src), CURDIR"%s", *t);
            snprintf(dst, sizeof(dst), CURDIR"%s.bin", *t);
            exec(src, dst);
        }
    }
    else
    {
        int i;
        for (i = 1; i < argc; i++)
        {
            char dst[256];
            snprintf(dst, sizeof(dst), "%s.bin", argv[i]);
            exec(argv[i], dst);
        }
    }
    return 0;
}

/* libc implementation */

#ifdef __alpha
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
#endif
