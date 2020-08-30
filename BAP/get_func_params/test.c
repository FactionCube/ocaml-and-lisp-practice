#include <stdio.h>
#include <stdlib.h>

// gcc -g test.c -o test

typedef unsigned char byte;

struct foo_t {
	unsigned int x;
	unsigned int y;
	unsigned int z;
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
	foo->z = 0xDEDEDEDE;

	return foo;
}

struct foo_t* set_struct_b ( struct foo_t * foo)
{
	foo->x = 0xDEADBEEF;
	foo->y = 0xCAFEBABE;
	foo->z = 0xD00DBAAF;
	return foo;
}


int main () {

	struct foo_t * foo = (struct foo_t *) malloc(sizeof (struct foo_t));
	char ret = func0 ( 0x99 , 0x88 , 0x77);
	struct foo_t * boo = set_struct_a ( foo);
	struct foo_t * coo = set_struct_b ( foo);

	free (foo);
	return 0;
}

