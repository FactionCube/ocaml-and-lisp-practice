#include <stdio.h>
#include <stdlib.h>

// gcc -g test.c -o test

typedef unsigned char byte;

struct foo_t {
	unsigned int x;
	unsigned int y;
};

int func0 ( byte a, byte b, byte c )
{
	if ( a <= b ) 
	{
		return b;
	} else {
		return c;
	}
}

struct foo_t* set_struct_a ( struct foo_t * foo)
{
	foo->x = 0xABABABAB;
	foo->y = 0xCDCDCDCD;

	return foo;
}

struct foo_t* set_struct_b ( struct foo_t * foo)
{
	foo->x = 0xDEADBEEF;
	foo->y = 0xCAFEBABE;
	return foo;
}


int main () {

	struct foo_t * boo = (struct foo_t *) malloc(sizeof (struct foo_t));
	struct foo_t * coo = (struct foo_t *) malloc(sizeof (struct foo_t));
	char ret = func0 ( 0x99 , 0x88 , 0x77);
	set_struct_a ( boo);
	set_struct_b ( coo);

	printf("boo.x = %x  coo.x = %x\n", boo->x, coo->x);

	free (boo);
	free (coo);

	return 0;

}

