|\microcvar| x, y;
x = 5;
y = {a: x * -10};
if (input) {
    x = x + 1;
} else {
    x = x * -10;
}
y = {a: x};