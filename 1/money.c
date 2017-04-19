#include <stdio.h>

struct twoxtwomat {
	double a1;
	double a2;
	double b1;
	double b2;
};

struct twoxtwomat inverse(struct twoxtwomat a)
{
	struct twoxtwomat ret;
	double det = a.a1 * a.b2 - a.a2 * a.b1;

	ret.a1 = a.b2 / det;
	ret.a2 = -a.a2 / det;
	ret.b1 = -a.b1 / det;
	ret.b2 = a.a1 / det;

	return (ret);
}



int main()
{
	struct twoxtwomat co;
	struct twoxtwomat co_inv;
	double x, y;
	co.a1 = 3;
	co.a2 = 5;
	co.b1 = 2;
	co.b2 = 7;

	co_inv = inverse(co);
	x = co_inv.a1 * 7500 + co_inv.a2 * 8850;
	y = co_inv.b1 * 7500 + co_inv.b2 * 8850;

	printf("武: %f, 景子: %f", x, y);

	return (0);
}
