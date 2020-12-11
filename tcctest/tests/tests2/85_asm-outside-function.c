extern int printf (const char *, ...);
extern void vide(void);
__asm("vide: ret");

int main() {
    vide();
    printf ("okay\n");
    return 0;
}
