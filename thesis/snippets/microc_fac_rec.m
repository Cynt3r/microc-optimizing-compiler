fac(n) {
    |\microcvar| f;
    if (n == 0) { 
        f = 1; 
    } else { 
        f = n * fac(n - 1); 
    }
    return f;
}

main() {
    |\microcvar| n;
    n = input;
    return fac(n);
}