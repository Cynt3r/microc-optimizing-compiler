main() {
    |\microcvar| n, f;
    n = input;
    f = 1;

    while (n > 0) {
        f = f * n;
        n = n - 1;
    }

    return f;
}