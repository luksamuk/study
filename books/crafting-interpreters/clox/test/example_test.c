#include <criterion/criterion.h>
#include <clox/foo.h>

Test(example, foo) {
	cr_assert_eq(5, sum(2, 3));
}

