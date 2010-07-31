#pragma once

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
int (*strcmp)(const char *, const char *) = (void *)0x00ef002c;
char *(*strncpy)(char *, const char *, size_t) = (void *)0x00ef0030;
char *(*strncat)(char *, const char *, size_t) = (void *)0x00ef0034;
size_t (*strlen)(const char *) = (void *)0x00ef0038;
void *(*memcpy)(void *, const void *, size_t) = (void *)0x00ef003c;
void *(*memset)(void *, int, size_t) = (void *)0x00ef0040;
void *(*lfind)(const void *, const void *, size_t *, size_t, int (*)(const void *, const void *)) = (void *)0x00ef0044;
void *(*bsearch)(const void *, const void *, size_t, size_t, int (*)(const void *, const void *)) = (void *)0x00ef0048;
int (*stricmp)(const char *, const char *) = (void *)0x00ef004c;
unsigned long (*strtoul)(const char *, char **, int) = (void *)0x00ef0050;
int (*isdigit)(int) = (void *)0x00ef0054;
int (*isupper)(int) = (void *)0x00ef0058;
int (*islower)(int) = (void *)0x00ef005c;
int (*isalpha)(int) = (void *)0x00ef0060;
int (*isalnum)(int) = (void *)0x00ef0064;
char *(*fgets)(char *, size_t, FILE *) = (void *)0x00ef0068;
int __divl = 0x7c00f000, __divlu = 0x7c00f001, __divq = 0x7c00f002, __divqu = 0x7c00f003;
#else
typedef long long int64_t;
typedef unsigned long long uint64_t;

#ifdef _MSC_VER
#define snprintf _snprintf
#define lfind _lfind
#define stricmp _stricmp
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
void *lfind(const void *, const void *, size_t *, size_t, int (*)(const void *, const void *));
void *bsearch(const void *, const void *, size_t, size_t, int (*)(const void *, const void *));
int stricmp(const char *, const char *);
unsigned long strtoul(const char *, char **, int);
int isdigit(int);
int isupper(int);
int islower(int);
int isalpha(int);
int isalnum(int);
char *fgets(char *, size_t, FILE *);

#ifdef __INTERIX
int stricmp(const char *a, const char *b)
{
    for (; *a || *b; a++, b++)
    {
        char ca = *a, cb = *b;
        if (isupper(ca)) ca += 32;
        if (isupper(cb)) cb += 32;
        if (ca < cb) return -1; else if (ca > cb) return 1;
    }
    return 0;
}
#endif

#endif
